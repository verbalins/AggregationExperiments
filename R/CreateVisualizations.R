source("R/VisualizeData.R")
library(kableExtra)
library(Hmisc)
library(patchwork)
library(flextable)
library(parallel)

# Export plots
save_plot <- function(x, p) {
  ggsave(
    filename = paste0("img/", if_else(str_detect(x, "avg|Runtime"), x, paste0("other/", x)), ".pdf"),
    plot = p,
    device = cairo_pdf,
    units = "px",
    height = 2500,
    width = 2000,
  )
}

rmse <- function(obs, pred) {
  sqrt(sum((obs - pred)^2)/length(obs))
}

nrmse <- function(obs, pred) {
  rmse(obs, pred) / mean(obs)
}


# Load and transform data
df <- dplyr::bind_rows(get_data_from_db(db = "data/Detailed.db") %>%
                         mutate(ExpName = "Detailed") %>%
                         add_experiment_parameters(),
                       get_data_from_db(db = "data/Simplified.db") %>%
                         mutate(ExpName = "Simplified") %>%
                         add_experiment_parameters())

data_with_errors <- df %>%
  filter(BufferSize > 0) %>%
  select(PT:ExpName,-all_of(c("ID", "StatTHP", "TargetMDT", "Runtime"))) %>%
  pivot_longer(cols = LT_avg:JPH_sd, names_to = "Variable") %>%
  pivot_wider(names_from = ExpName, values_from = value) %>%
  nest_by(Experiment, Variable) %>%
  summarise(NRMSE = nrmse(data$Detailed, data$Simplified)) %>%
  pivot_wider(names_from = Variable, values_from = NRMSE)

data_for_table <- data_with_errors %>%
  ungroup() %>%
  right_join(y=df %>%
               filter(BufferSize > 0) %>%
               select(Experiment, NumberMachines, Setting, Runtime, ExpName, Observation) %>%
               pivot_wider(names_from=ExpName, values_from=Runtime, names_glue="Runtime_{ExpName}") %>%
               group_by(Setting, NumberMachines, Experiment) %>%
               summarise(across(where(is.double), mean))) %>%
  group_by(Setting,
           NumberMachines = cut(NumberMachines,
                                breaks = c(5,25,50,100,200,350,500), include.lowest=TRUE, labels=c("5-25", "25-50", "50-100", "100-250", "250-350", "350-500"))) %>%
  summarise(across(contains(c("_", "Runtime")), mean)) %>%
  select(Setting, NumberMachines, contains(c("_avg","Runtime"))) %>%
  ungroup() %>%
  left_join(y = df %>%
              select(PT:Setting) %>%
              distinct()) %>%
  relocate(PT:MDT, .after = Setting)

kable(data_for_table,
      format = "latex",
      booktabs = TRUE,
      caption = "\\gls{nrmse} for average values of \\gls{jph}, \\gls{lt}, and \\gls{wip}, binned by $\\beta$. Average Runtime values in seconds for \\textit{Detailed} and \\textit{Simplified}. The results are only for $\\gamma > 0$",
      digits = 3,
      label = "average_results",
      col.names = c("$\\alpha$", "PT", "Avb", "MDT", "$\\beta$", "JPH", "LT", "WIP", "Detailed", "Simplified"),
      table.envir = "table*",
      align = "lllllrrrrr") %>%
  collapse_rows(columns=c(1,2,3,4), latex_hline = "linespace", target=1) %>%
  add_header_above(c("$\\alpha$" = 4, " " = 1, "NRMSE (\\SI{}{\\percent})" = 3, "Average Runtime (s)" = 2)) %>%
  save_kable(file = "img/experimenttable.tex")

grouped <- df %>%
  group_data() %>%
  arrange(Experiment)

compute_rmse <- function(df, attr) {
  df %>%
    select(Setting:BufferSize, -all_of(c("ID", "StatTHP", "TargetMDT")), all_of({{attr}}), ExpName) %>%
    pivot_wider(names_from = ExpName, values_from = {{attr}}) %>%
    nest_by(Setting, Experiment, BufferSize, NumberMachines) %>%
    mutate(MAE = mean(abs(data$Detailed - data$Simplified)),
           RMSE = rmse(data$Detailed, data$Simplified),
           NRMSE = nrmse(data$Detailed, data$Simplified))
}

compute_error_and_save <- function(data, g, filename="", metric = "NRMSE") {
  invisible(mclapply(data |> select(LT_avg:JPH_sd) |> colnames(),
                   function(x) save_plot(paste0(x, filename), compute_rmse(data, x) |>
                                           plot_compare_error(attr=x, metric=metric) |>
                                           plot_combine_with_table(g = g)), mc.cores=12))
}

ex_table <- df %>%
  select(PT:MDT) %>%
  distinct() %>%
  t()

alpha_unicode <-
  c("\U03b1\U2081",
    "\U03b1\U2082",
    "\U03b1\U2083",
    "\U03b1\U2084",
    "\U03b1\U2085")

colnames(ex_table) <- alpha_unicode

ex_table_grob <-
  flextable(data = ex_table |>
              as_tibble(rownames = " ") |>
              mutate(' ' = c("PT (s)", "Avb (%)", "MDT (s)"))) |>
  align(align = "center", part = "all") |>
  align(j = 1, align = "left", part = "all") |>
  hline_top(part="all", border = officer::fp_border(width = 3)) |>
  hline_bottom(part = "body", border = officer::fp_border(width = 3)) |>
  gen_grob(scaling = "full")

df |> compute_error_and_save(g = ex_table_grob,
                 filename="_500") # Default 500 beta and all gamma

df |> # Only 200 beta
  filter(NumberMachines <= 200) |>
  compute_error_and_save(g = ex_table_grob,
             filename="_200")

df |> # 200 beta and gamma > 0
  filter(NumberMachines <= 200,
         BufferSize != 0) |>
  compute_error_and_save(g = ex_table_grob,
             filename="_200_0gamma")

# Runtime graph
p <- grouped %>%
  mutate(Setting = factor(Setting,
                          levels = c("1","2","3","4","5"),
                          labels = alpha_unicode),
         ExpName = str_replace(ExpName, "Aggregated", "Simplified"),
         ExpName = factor(ExpName,
                          levels = c("Simplified", "Detailed"))) %>% # Alpha_n
  ggplot(aes(NumberMachines, Runtime, group = interaction(ExpName, BufferSize), color = ExpName)) +
  geom_line(alpha=0.4) +
  geom_smooth(aes(group=ExpName), se = FALSE) +
  scale_y_continuous(breaks = seq(0, 200, by = 50), limits = c(0, 250)) +
  scale_color_discrete(breaks = c('Detailed', 'Simplified')) +
  facet_wrap(~Setting, nrow = 5, strip.position = "right", drop = TRUE) +
  ylab("Runtime in seconds") +
  xlab(paste("Number of buffer/machine pairs in sequence,", "\u03b2")) +
  guides(color=guide_legend(ncol = 1, title="Model type")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom",
        legend.justification = "left")

plot_combine_with_table(p, g = ex_table_grob) |>
  save_plot(x="Runtime")

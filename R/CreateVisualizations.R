source("R/VisualizeData.R")
library(kableExtra)
library(Hmisc)
library(patchwork)
library(flextable)
library(scales)
library(latex2exp)
library(showtext)

font_add("Segoe", "seguisym.ttf")
showtext_auto()
set_flextable_defaults(font.family = "Segoe")

# Download data if not already done.
download_data()

con <- dbConnect(RSQLite::SQLite(), "db/simplification_results.db")
df <- tbl(con, "results") |> collect()
dbDisconnect(con)

get_data_for_table <- function(data) {

  data_with_errors <- data %>%
    filter(BufferSize > 0) %>%
    select(-all_of(c("StatTHP", "TargetMDT", "Runtime"))) %>%
    arrange(Experiment, ExpName) %>%
    pivot_longer(cols = contains(c("_")), names_to = "Variable") %>%
    pivot_wider(names_from = ExpName, values_from = value) %>%
    nest_by(Experiment, Variable) %>%
    summarise(NRMSE = nrmse(data$Detailed, data$Simplified)) %>%
    pivot_wider(names_from = Variable, values_from = NRMSE)

  data_for_table <- data_with_errors %>%
    ungroup() %>%
    right_join(y=data %>%
                 filter(BufferSize > 0) %>%
                 select(Experiment, NumberMachines, Setting, Runtime, ExpName, Observation) %>%
                 pivot_wider(names_from=ExpName, values_from=Runtime, names_glue="Runtime_{ExpName}") %>%
                 group_by(Setting, NumberMachines, Experiment) %>%
                 summarise(across(where(is.double), mean))) %>%
    group_by(Setting,
             NumberMachines = cut(NumberMachines,
                                  breaks = c(5,25,50,100,200,350,500), include.lowest=TRUE, labels=c("5-25", "26-50", "51-100", "101-200", "201-350", "351-500"))) %>%
    summarise(across(contains(c("_", "Runtime")), mean)) %>%
    select(Setting, NumberMachines, contains(c("_avg","Runtime"))) %>%
    ungroup() %>%
    left_join(y = df %>%
                select(PT:Setting) %>%
                distinct()) %>%
    relocate(PT:MDT, .after = Setting)
}

save_table <- function(df, tbl_format = "latex", save = TRUE, booktabs = TRUE, filename="experimenttable") {
  kable(df |> get_data_for_table() |>
          mutate(across(c("JPH_avg", "WIP_avg", "LT_avg"), ~ 100 * .x)),
        format = tbl_format,
        escape = FALSE,
        booktabs = TRUE,
        caption = "\\gls{nrmse} for average values of \\gls{jph}, \\gls{lt}, and \\gls{wip}, binned by $\\beta$. Average Runtime values in seconds for \\textit{Detailed} and \\textit{Simplified}.",
        digits = 3,
        label = "average_results",
        col.names = c("$\\alpha$", "PT (s)", "Avb (\\SI{}{\\percent})", "MDT (s)", "$\\beta$", "JPH", "LT", "WIP", "\\textit{Detailed}", "\\textit{Simplified}"),
        table.envir = "table*",
        align = "lccclrrrrr") %>%
  collapse_rows(columns=c(1,2,3,4), latex_hline = "linespace", target=1) %>%
  add_header_above(c("$\\\\alpha$" = 4, " " = 1, "NRMSE (\\\\SI{}{\\\\percent})" = 3, "Average Runtime (s)" = 2), escape=FALSE) %>%
  save_kable(file = paste0("img/", filename, ".tex"))
}

df |> save_table()

grouped <- df %>%
  group_data() %>%
  arrange(Experiment)

compute_rmse <- function(data, attr) {
  data %>%
    select(Setting:BufferSize, -all_of(c("StatTHP", "TargetMDT")), all_of({{attr}}), ExpName) %>%
    pivot_wider(names_from = ExpName, values_from = {{attr}}) %>%
    nest_by(Setting, Experiment, BufferSize, NumberMachines) %>%
    mutate(MAE = mean(abs(data$Detailed - data$Simplified)),
           RMSE = rmse(data$Detailed, data$Simplified),
           NRMSE = nrmse(data$Detailed, data$Simplified))
}

get_summary_for_experiment <- function(data, experiment) {
  data |>
    filter(Experiment == experiment) |>
    select(contains("_"), "ExpName") |>
    group_by(ExpName) |>
    dplyr::summarize(across(everything(), mean))
}

compute_error_and_save <- function(data, g, filename="", metric = "NRMSE") {
  invisible(lapply(data |> select(JPH_avg:LT_sd) |> colnames(),
                   function(x) save_plot(paste0(x, filename), compute_rmse(data, x) |>
                                           plot_compare_error_combined(attr=x, metric=metric, g = g))))
}

ex_table <- df %>%
  select(PT:MDT) %>%
  distinct() %>%
  t()

alpha_unicode <-
  stri_unescape_unicode(paste0("\\u", "03b1", "\\u", "208",
                               seq_along(unique(as.character(df$Setting))), sep=''))

colnames(ex_table) <- alpha_unicode

ex_table_grob <-
  flextable(data = ex_table |>
              as_tibble(rownames = " ") |>
              mutate(' ' = c("PT (s)", "Avb (%)", "MDT (s)"))) |>
  align(align = "center", part = "all") |>
  align(j = 1, align = "left", part = "all") |>
  gen_grob(scaling = "full")

df |> compute_error_and_save(g = ex_table_grob,
                                 filename = "_500")

# Runtime graph
p <- grouped %>%
  mutate(Setting = factor(Setting,
                          levels = as.character(unique(df$Setting)),
                          labels = alpha_unicode),
         ExpName = factor(ExpName,
                          levels = c("Simplified", "Detailed"))) %>% # Alpha_n
  ggplot(aes(NumberMachines, Runtime, group = interaction(ExpName, BufferSize), color = ExpName)) +
  geom_line(alpha=0.4) +
  geom_smooth(aes(group=ExpName), se = FALSE) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  #scale_y_continuous(trans = "log10") +
  #scale_y_continuous(breaks = seq(0, 200, by = 50), limits = c(0, 250)) +
  scale_color_discrete(breaks = c('Detailed', 'Simplified')) +
  facet_wrap(~Setting, nrow = 5, strip.position = "right", drop = TRUE) +
  ylab(expression("Model Runtime (s), log"[10])) +
  xlab(paste("Number of buffer/machine pairs in sequence,", "\u03b2")) +
  guides(color=guide_legend(title="Model type")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom",
        text = element_text(family = "Segoe"))

save_plot("Runtime", p)

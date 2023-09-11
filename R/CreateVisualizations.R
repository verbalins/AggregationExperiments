source("R/VisualizeData.R")
library(ragg)
library(xtable)
library(kableExtra)
library(Hmisc)

# Export plots
save_plot <- function(x, p) {
  ggsave(
    filename = paste0("img/", x, ".pdf"),
    plot = p,
    # bg = "transparent",
    device = grDevices::cairo_pdf,
    #res = 300,
    units = "px",
    #limitsize = FALSE,
    height = 2500,
    width = 2000,
    # background = "transparent"
  )
}

rmse <- function(x, y) {
  sqrt(sum((y-x)^2)/length(x))
}

rmses <- function(x, y) {
  rmse(x, y) / mean(y)
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
  select(Setting:ExpName,-all_of(c("ID", "StatTHP", "TargetMDT", "Runtime"))) %>%
  pivot_longer(cols = LT_avg:JPH_sd, names_to = "Variable") %>%
  pivot_wider(names_from = ExpName, values_from = value) %>%
  nest_by(Experiment, Variable) %>%
  #summarise(RMSES = sqrt(sum((data$Detailed - data$Simplified) ^ 2)/length(data$Detailed))/mean(data$Detailed)) %>%
  summarise(RMSES = rmses(data$Simplified, data$Detailed)) %>%
  pivot_wider(names_from = Variable, values_from = RMSES)

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
  select(Setting, NumberMachines, contains(c("_avg","Runtime")))

kable(data_for_table,
      format = "latex",
      booktabs = TRUE,
      caption = "Scaled RMSE for average values of \\gls{jph}, \\gls{lt}, and \\gls{wip}, binned by $\\beta$. Average Runtime values in seconds for detailed and simplified model. The results are only for $\\gamma > 0$",
      digits = 3,
      label = "average_results",
      col.names = c("$\\alpha$", "$\\beta$", "JPH", "LT", "WIP", "Detailed", "Simplified"),
      table.envir = "table*",
      align = "llrrrrr") %>%
  collapse_rows(columns=1, latex_hline = "linespace") %>%
  add_header_above(c(" " = 2, "Average Scaled RMSE" = 3, "Average Runtime (s)" = 2)) %>%
  save_kable(file = "img/experimenttable.tex")

grouped <- df %>%
  group_data() %>%
  arrange(Experiment)

compute_rmse <- function(df, attr) {
  df %>%
    select(Setting:BufferSize, -all_of(c("ID", "StatTHP", "TargetMDT")), {{attr}}, ExpName) %>%
    pivot_wider(names_from = ExpName, values_from = {{attr}}) %>%
    nest_by(Setting, Experiment, BufferSize, NumberMachines) %>%
    mutate(MAE = mean(abs(data$Detailed - data$Simplified)),
           RMSE = rmse(data$Detailed, data$Simplified),
           RMSES = rmses(data$Detailed, data$Simplified))
}

invisible(lapply(df %>% filter(NumberMachines <= 200, BufferSize != 0) %>% select(LT_avg:JPH_sd) %>% colnames(),
                 function(x) save_plot(x, compute_rmse(df %>% filter(NumberMachines <= 200, BufferSize != 0), x) %>%
                                         plot_compare_error(attr=x, metric="RMSES"))))

invisible(lapply(df %>% filter(BufferSize != 0) %>% select(LT_avg:JPH_sd) %>% colnames(),
                 function(x) save_plot(paste0(x, "_500"), compute_rmse(df %>% filter(BufferSize != 0), x) %>%
                                         plot_compare_error(attr=x, metric="RMSES"))))

# Runtime graph
p <- grouped %>%
  mutate(Setting = factor(Setting,
                          levels = c("1","2","3","4","5"),
                          labels = c("\u03b1\u2081",
                                     "\u03b1\u2082",
                                     "\u03b1\u2083",
                                     "\u03b1\u2084",
                                     "\u03b1\u2085"))) %>% # Alpha_n
  ggplot(aes(NumberMachines, Runtime, group = interaction(ExpName, BufferSize), color = ExpName)) +
  geom_line(alpha=0.4) +
  geom_smooth(aes(group=ExpName), se = FALSE) +
  scale_y_continuous(breaks = seq(0, 200, by = 50), limits = c(0, 250)) +
  facet_wrap(~Setting, nrow = 5, strip.position = "right", drop = TRUE) +
  ylab("Runtime in seconds") +
  xlab(paste("Number of buffer/machine pairs in sequence,", "\u03b2")) +
  guides(color=guide_legend(title="Model type")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom")

save_plot("Runtime", p)

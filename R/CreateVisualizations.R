source("R/VisualizeData.R")
library(ragg)
library(xtable)
library(kableExtra)
library(Hmisc)

# Export plots
save_plot <- function(x, p) {
  ggsave(
    filename = paste0("img/", x, ".png"),
    plot = p,
    # bg = "transparent",
    device = ragg::agg_png,
    res = 300,
    units = "in",
    limitsize = FALSE,
    height = 2500,
    width = 2000,
    # background = "transparent"
  )
}

rsq <- function(x, y) cor(x, y) ^ 2
rmse <- function(x, y) {
  sqrt(mean((y-x)^2))
}

# Load and transform data
df <- dplyr::bind_rows(get_data_from_db(db = "data/Detailed.db") %>%
                         mutate(ExpName = "Detailed") %>%
                         add_experiment_parameters(),
                       get_data_from_db(db = "data/Aggregated.db") %>%
                         mutate(ExpName = "Aggregated") %>%
                         add_experiment_parameters())

data_with_errors <- df %>%
  filter(BufferSize > 0) %>%
  select(Setting:ExpName,-c(ID, StatTHP, TargetMDT, Runtime)) %>%
  pivot_longer(cols = LT_avg:JPH_sd, names_to = "Variable") %>%
  pivot_wider(names_from = ExpName, values_from = value) %>%
  group_by(Experiment, Variable) %>%
  summarise(Error = Aggregated - Detailed, M = mean(Detailed)) %>%
  summarise(#MAE = mean(abs(Error)),
            #RMSE = sqrt(mean(Error ^ 2)),
            RMSES = sqrt(mean(Error ^ 2))/mean(M)) %>%
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
      caption = "Scaled RMSE for average values of \\gls{jph}, \\gls{lt}, and \\gls{wip}. Average Runtime values in seconds for detailed and simplified model. Only BufferSize > 0.",
      digits = 3,
      label = "average_results",
      col.names = c("Setting", "NumberMachines", "JPH", "LT", "WIP", "Detailed", "Aggregated"),
      table.envir = "table*",
      align = "llrrrrr") %>%
  collapse_rows(columns=1, latex_hline = "linespace") %>%
  add_header_above(c(" " = 2, "Average Scaled RMSE" = 3, "Average Runtime (s)" = 2))

grouped <- df %>%
  group_data() %>%
  arrange(Experiment)

compute_rmse <- function(df, attr) {
  df %>%
    select(Setting:BufferSize, -c(ID, StatTHP, TargetMDT), {{attr}}, ExpName) %>%
    pivot_wider(names_from = ExpName, values_from = {{attr}}) %>%
    nest_by(Setting, Experiment, BufferSize,NumberMachines) %>%
    mutate(MAE = mean(abs(data$Aggregated - data$Detailed)),
           RMSE = rmse(data$Detailed, data$Aggregated),
           RMSES = RMSE/mean(data$Detailed))
}

invisible(lapply(df %>% select("LT_avg":"JPH_sd") %>% colnames(),
                 function(x) save_plot(x, compute_rmse(df, x) %>% plot_compare_error(attr=x, metric="RMSES"))))

# Boxplot showing influence of shorter lines
delta %>%
  ggplot(aes(as.factor(BufferSize), WIP_avg, colour=as.factor(cut_interval(NumberMachines, 10)))) + geom_boxplot()

df %>%
  ungroup() %>%
  pivot_wider(id_cols=Experiment,names_from=ExpName, values_from=JPH_avg) %>%
  select(Detailed, Aggregated) %>% rsq(.[1], .[2])

avb <- 0.98
pt <- 60.0

wip_values <- df %>%
  ungroup() %>%
  select(ExpName, NumberMachines, BufferSize, WIP_avg, WIP_max, WIP_min) %>%
  mutate(RatioSimulated = WIP_max-WIP_avg,
         RatioTheory = (NumberMachines + (NumberMachines * BufferSize))-WIP_avg,
         ESimulated = ((RatioSimulated*pt)/avb),
         ETheory = ((RatioTheory*pt)/avb))

# Show effect of parameter setting for average and maximum wip
wip_values %>%
  group_by(ExpName) %>%
  pivot_longer(cols = c(RatioSimulated, RatioTheory)) %>%
  ggplot(mapping = aes(NumberMachines, color = name, group=BufferSize)) +
  facet_wrap(vars(ExpName)) +
  geom_line(data = . %>% filter(name=="RatioSimulated"), aes(y=value)) +
  geom_line(data = . %>% filter(name=="RatioTheory"), aes(y=value))

wip_values %>%
  group_by(ExpName) %>%
  pivot_longer(cols = c(ESimulated, ETheory)) %>%
  ggplot(mapping = aes(NumberMachines, color = name, group=BufferSize)) +
  facet_wrap(vars(ExpName)) +
  geom_line(data = . %>% filter(name=="ESimulated"), aes(y=value)) +
  geom_line(data = . %>% filter(name=="ETheory"), aes(y=value))


buffer <- 5

create_exp_values <- function(n_machines, n_buffer, parameter) {
  val <- wip_values %>% filter(NumberMachines == n_machines, BufferSize == n_buffer, ExpName =="Detailed")
  setNames(data.frame(rexp(n=10000, rate = 1/val[[parameter]])),
           paste(parameter, paste0("n",n_machines), sep="_"))
}

p <- map(c("ETheory", "ESimulated"),
    ~map(c(5,25,50,100,200,400), create_exp_values, buffer, .x)) %>%
  map_dfc(~as.data.frame(.)) %>%
  pivot_longer(everything(),
               names_to = c("type", "name"),
               names_sep = "_") %>%
  ggplot(aes(factor(name, levels=c("n5","n25","n50","n100","n200","n400")),value, colour=type)) + geom_boxplot()

plotly::ggplotly(p) %>%layout(boxmode = "group")

data.frame(x = rexp(10000, rate = 1/((3459.3-3236.1)*pt)/avb),
           y = rexp(10000, rate = 1/((5500-3236.1)*pt)/avb)) %>%
  pivot_longer(cols=c(x,y)) %>%
  ggplot(aes(value,fill=name)) + geom_histogram()

data.frame(x = rexp(10000, rate = 1/((446.3-336.3)*pt)/avb),
           y = rexp(10000, rate = 1/((550-336.3)*pt)/avb)) %>%
  pivot_longer(cols=c(x,y)) %>%
  ggplot(aes(value,fill=name)) + geom_histogram()

# Runtime table data
df %>%
  filter(BufferSize > 0) %>%
  nest_by(Setting,
           cut(NumberMachines, breaks=c(5,25,50,100,200,500), include.lowest=TRUE, labels=c(">=5", "25", "50", "100", ">200"))) %>%
  dplyr::summarise(across(LT_avg:Runtime, mean)) #%>%

# Runtime graph
p <- grouped %>%
  mutate(Setting = factor(Setting,
                          levels = c("1","2","3","4","5"),
                          labels = c("\u03b1\u2081",
                                     "\u03b1\u2082",
                                     "\u03b1\u2083",
                                     "\u03b1\u2084",
                                     "\u03b1\u2085"))) %>%
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

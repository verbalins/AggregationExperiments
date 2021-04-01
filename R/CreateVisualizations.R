source("R/VisualizeData.R")
library(ragg)

# Export plots
save_plot <- function(x, p) {
  # p <- p +
  #   theme(
  #     panel.background = element_rect(fill = "transparent"), # bg of the panel
  #     plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
  #     panel.grid.major = element_blank(), # get rid of major grid
  #     panel.grid.minor = element_blank(), # get rid of minor grid
  #     legend.background = element_rect(fill = "transparent"), # get rid of legend bg
  #     legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  #   )
  ggsave(
    filename = paste0("img/", x, ".png"),
    plot = p,
    # bg = "transparent",
    device = ragg::agg_png,
    res = 300,
    units = "in",
    limitsize = FALSE,
    height = 2000,
    width = 2000,
    # background = "transparent"
  )
}

# Load and transform data
df <- get_data_from_db(db = "data/NewSimulationResults_98.db")
grouped <- group_data(df)
delta <- delta_values(grouped)

df_85 <- get_data_from_db(db = "data/NewSimulationResults_85.db")
grouped_85 <- group_data(df_85)
delta_85 <- delta_values(grouped_85)

# Export plots for 98
res <- delta %>% select("LT_avg":"JPH_max") %>% colnames()
invisible(lapply(res, function(x) save_plot(x, compare_singleinputdist_buffersize(x, delta))))

invisible(lapply(res, function(x) save_plot(x, compare_singleinputdist_buffersize(x, delta_85))))

# Boxplot showing influence of shorter lines
delta %>%
  filter(InputDistribution == "No Failure", ExpName == "Aggregated") %>%
  ggplot(aes(as.factor(BufferSize), WIP_avg, colour=as.factor(cut_interval(NumberMachines, 10)))) + geom_boxplot()

#
grouped %>%
  ungroup() %>%
  filter(ExpName != "AggregatedPlace", InputDistribution == "No Failure") %>%
  pivot_wider(id_cols=Experiment,names_from=ExpName, values_from=JPH_avg) %>%
  select(Detailed, Aggregated) %>% rsq(.[1], .[2])

avb <- 0.98
pt <- 60.0

wip_values <- grouped %>%
  ungroup() %>%
  filter(InputDistribution == "No Failure", ExpName != "AggregatedPlace") %>%
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
grouped %>%
  filter(as.numeric(InputDistribution)==1, ExpName != "AggregatedPlace", BufferSize > 0) %>%
  group_by(ExpName,
           cut(NumberMachines, breaks=c(5,25,50,100,200,500), include.lowest=TRUE, labels=c(">=5", "25", "50", "100", ">200"))) %>%
  dplyr::summarise(Runtime = mean(Runtime)) %>%
  pivot_wider(names_from = ExpName, values_from = )

# Data is in a SQLite 3 db called AggregationExp.db
get_data_from_db <- function(FUN = get_all_data, db) {
  con <- dbConnect(RSQLite::SQLite(), db)

  results_db <- tbl(con, "Result")

  df <- FUN(results_db)

  dbDisconnect(con)

  return(df %>% select(-InputDistribution))
}

get_all_data <- function(sqlconn) {
  df <- sqlconn %>% dplyr::collect() %>%
    mutate(Runtime = ifelse(Runtime < 0, Runtime+86400, Runtime))
}

get_and_convert_data <- function(sqlqconn) {
  df <- sqlconn %>% dplyr::collect() %>%
    mutate(Runtime = ifelse(Runtime < 0, Runtime+86400, Runtime),
           JPH = map(JPH, convert_blob),
           LT = map(LT, convert_blob),
           WIP = map(WIP, convert_blob))
}

convert_blob <- function(blob) {
  zlib <- import("zlib")
  return(list(as.numeric(str_split_1(toString(zlib$decompress(unlist(blob))), pattern = ","))))
}

add_experiment_parameters <- function(df) {
  data.frame(PT = unlist(map(c(60,60,60,60,600), rep, 11000)),
             Avb = unlist(map(c(85,98,85,98,98), rep, 11000)),
             MDT = unlist(map(c(600,600,1800,1800,600), rep, 11000)),
             Setting = unlist(map(1:5, rep, 11000))) %>%
    dplyr::bind_cols(df %>% arrange(Experiment, Observation))
}

summarised_5limit <- function(sqlconn) {
  df <- sqlconn %>%
    group_by(NumberMachines, BufferSize, InputDistribution, Experiment, ExpName) %>%
    #filter(BufferSize == 5, ExpName != "AggregatedPlace") %>%
    summarise(across(LT_avg:Runtime, mean),
              n = n()) %>%
    dplyr::collect()
}

group_data <- function(df) {
  df <- df %>% group_by(Experiment, Setting, NumberMachines, BufferSize, ExpName) %>%
    mutate(Observation = n()) %>%
    summarise(across(LT_avg:Runtime, mean)) %>%
    arrange(Experiment)
}

calc_baseline <- function(df) {
  df <- df %>% group_by(Experiment, ExpName) %>%
    mutate(across(LT_avg:Runtime, mean))
}

get_missing_experiments <- function(sqlconn) {
  sqlconn %>%
    count(Experiment, ExpName, Observation) %>%
    dplyr::collect()
}

check_missing <- function(db = "data/NewSimulationResults_98_Errors.db") {
  get_data_from_db(get_missing_experiments, db) %>%
    count(Experiment) #%>%
    #filter(n < 3) %>%
    #nrow() == 0
}

run_script <- function() {
  if (!check_missing()) {
    return(FALSE)
  }
  df <- get_data_from_db()
  grouped <- group_data(df)
  delta <- delta_values(grouped)

  ggplotly(df %>% ggplot(aes(NumberMachines, Runtime, group = interaction(ExpName, BufferSize), color = ExpName)) +
             geom_line(alpha=0.4) +
             geom_smooth(aes(group=ExpName), size=0.5) +
             labs(title = "Runtime versus model size",
                  subtitle = "Grouped") +
             ylab("Runtime in seconds") +
             xlab("Number of buffer/machine pairs in sequence") +
             guides(color=guide_legend(title="Model type"))) #+
  #theme_light(base_size=9, base_family="Roboto") +
  #theme(plot.subtitle = element_text(color="#666666"),
  #plot.title = element_text(family="Roboto Condensed"),
  #plot.caption = element_text(color="#AAAAAA", size=6))

  #ggsave("Comparison.png", width=8, height=4.5, dpi="retina")

  #df %>% ggplot(aes(Experiment, JPH, color = ExpName)) + geom_line()
}

delta_values_ratio <- function(grouped) {
  grouped <- grouped %>%
    ungroup() %>%
    group_by(Experiment) %>%
    do(LT_avg = .$LT_avg/.$LT_avg[1],
       LT_min = .$LT_min/.$LT_min[1],
       LT_max = .$LT_max/.$LT_max[1],
       WIP_avg = .$WIP_avg/.$WIP_avg[1],
       WIP_min = .$WIP_min/.$WIP_min[1],
       WIP_max = .$WIP_max/.$WIP_max[1],
       JPH_avg = .$JPH_avg/.$JPH_avg[1],
       JPH_min = .$JPH_min/.$JPH_min[1],
       JPH_max = .$JPH_max/.$JPH_max[1],
       ExpName = data.frame(ExpName = factor(seq(1:2))),
       Runtime = .$Runtime/.$Runtime[1]) %>%
    unnest(cols = c(LT_avg, LT_min, LT_max, WIP_avg, WIP_min, WIP_max,
                    JPH_avg, JPH_min, JPH_max, ExpName, Runtime)) %>%
    mutate(NumberMachines = grouped$NumberMachines,
           BufferSize = grouped$BufferSize,
           Setting = grouped$Setting)

  levels(grouped$ExpName) <- c("Detailed", "Aggregated")
  return(grouped)
}

delta_values <- function(grouped) {
  grouped <- grouped %>%
    ungroup() %>%
    group_by(Experiment) %>%
    do(LT_avg = .$LT_avg[1]-.$LT_avg,
       LT_min = .$LT_min[1]-.$LT_min,
       LT_max = .$LT_max[1]-.$LT_max,
       WIP_avg = .$WIP_avg[1]-.$WIP_avg,
       WIP_min = .$WIP_min[1]-.$WIP_min,
       WIP_max = .$WIP_max[1]-.$WIP_max,
       JPH_avg = .$JPH_avg[1]-.$JPH_avg,
       JPH_min = .$JPH_min[1]-.$JPH_min,
       JPH_max = .$JPH_max[1]-.$JPH_max,
       Runtime = .$Runtime[1]-.$Runtime) %>%
    unnest(cols = c(LT_avg, LT_min, LT_max, WIP_avg, WIP_min, WIP_max,
                    JPH_avg, JPH_min, JPH_max, ExpName, Runtime)) %>%
    mutate(Setting = grouped$Setting)

  levels(grouped$ExpName) <- c("Detailed", "Aggregated")
  #levels(grouped$InputDistribution) <- c("No Failure", "Avb", "sqrt(Avb)")
  return(grouped)
}

workspace <- function() {
  x <- grouped_85 %>% filter(ExpName == "Detailed", InputDistribution == "No Failure") %>% .$JPH_avg
  y <- grouped_85 %>% filter(ExpName == "Aggregated", InputDistribution == "No Failure") %>% .$JPH_avg
  fv <- data.frame(x,y) %>% lm(y ~ x,.) %>% fitted.values()

  data.frame(x, y,
             BufferSize = rep(rep(0:10, each = 1), 100),
             NumberMachines = unique(test$NumberMachines),
             Experiment = unique(test$Experiment)) %>%
  plot_ly(
    x = ~x,
    y = ~y,
    color = ~BufferSize,
    customdata = ~Experiment,
    mode = "markers",
    type = "scatter",
    hovertemplate = paste('Detailed: %{x:.2f}',
                          '<br>Aggregated: %{y:.2f}',
                          '<br>BufferSize: %{BufferSize}',
                          '<br>Experiment: %{customdata}')) %>%
  add_lines(x = ~x, y = ~fv$residuals)
}

## call required packages
require(pacman)
p_load(tidyverse, DBI, RSQLite, plotly, magrittr, broom, GGally, lattice, Hmisc,
       latex2exp, RColorBrewer, FSA, emmeans, reticulate, twosamples)

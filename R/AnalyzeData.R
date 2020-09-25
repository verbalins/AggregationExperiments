# Data is in a SQLite 3 db called AggregationExp.db
get_data_from_db <- function(FUN = get_all_data, db = "data/NewSimulationResults_98_Errors.db") {
  con <- dbConnect(RSQLite::SQLite(), db)

  results_db <- tbl(con, "Result")

  df <- FUN(results_db)

  dbDisconnect(con)

  return(df)
}

get_all_data <- function(sqlconn) {
  df <- sqlconn %>% collect() %>%
    mutate(Runtime = ifelse(Runtime < 0, Runtime+86400, Runtime),
           ExpName = factor(ExpName, levels = c("Detailed", "Aggregated", "AggregatedPlace")),
           InputDistribution = factor(ceil(Experiment/1100), labels = c("No Failure", "Avb", "sqrt(Avb)")))
}

summarised_5limit <- function(sqlconn) {
  df <- sqlconn %>%
    group_by(NumberMachines, BufferSize, InputDistribution, Experiment, ExpName) %>%
    #filter(BufferSize == 5, ExpName != "AggregatedPlace") %>%
    summarise(across(LT_avg:Runtime, mean),
              n = n()) %>%
    collect()
}

group_data <- function(df) {
  df <- df %>% group_by(NumberMachines, BufferSize, InputDistribution, Experiment, ExpName) %>%
    mutate(Observation = n()) %>%
    summarise(across(LT_avg:Runtime, mean),
              n = n()) %>%
    arrange(Experiment)
}

calc_baseline <- function(df) {
  df <- df %>% group_by(Experiment, ExpName) %>%
    mutate(across(LT_avg:Runtime, mean))
}

get_missing_experiments <- function(sqlconn) {
  sqlconn %>%
    count(Experiment, ExpName, Observation) %>%
    collect()
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

delta_values <- function(grouped) {
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
       ExpName = data.frame(ExpName = factor(seq(1:3)))) %>%
    unnest(cols = c(LT_avg, LT_min, LT_max, WIP_avg, WIP_min, WIP_max,
                    JPH_avg, JPH_min, JPH_max, ExpName)) %>%
    mutate(InputDistribution = as.factor(ceil(Experiment/1100)),
           NumberMachines = grouped$NumberMachines,
           BufferSize = grouped$BufferSize)

  levels(grouped$ExpName) <- c("Detailed", "Aggregated", "AggregatedPlace")
  levels(grouped$InputDistribution) <- c("No Failure", "Avb", "sqrt(Avb)")
  return(grouped)
}

get_fraction <- function(df, col) {
  df %>% mutate(col = lapply())
}

## call required packages
require(pacman)
p_load(tidyverse, DBI, RSQLite, plotly, magrittr, broom, GGally, lattice, Hmisc,
       latex2exp, RColorBrewer, FSA, emmeans) #psych

reticulate::source_python("uncompresszlib.py")

import_wip_data <- function() {
  con <- dbConnect(RSQLite::SQLite(), "db/results_detailed.db")
  import_wip <- tbl(con, "Results") %>% select(-JPH, -LT) %>% collect()
  library(multidplyr)
  cluster <- multidplyr::new_cluster(10)
  multidplyr::cluster_send(cluster, reticulate::source_python("uncompresszlib.py"))
  import_wip_c <- import_wip %>%
    partition(cluster)
  decompressed <- import_wip_c %>%
    mutate(dplyr::across(any_of(c("JPH", "WIP", "LT")),
                         \(x) unlist(purrr::map(x, \(y) round(mean(decompress_zlib(y))))))) %>%
    collect()
}

import_data <- function(db) {
  con <- dbConnect(RSQLite::SQLite(), db)
  import_wip <- tbl(con, "Results") %>% collect()

  library(multidplyr)
  cluster <- multidplyr::new_cluster(10)
  multidplyr::cluster_send(cluster, reticulate::source_python("uncompresszlib.py"))

  import_wip_c <- import_wip %>%
    partition(cluster)

  decompressed <- import_wip_c %>%
    mutate(dplyr::across(any_of(c("JPH", "WIP", "LT")),
                         \(x) unlist(purrr::map(x, \(y) decompress_zlib(y))))) %>%
    collect()

  rm(cluster, import_wip_c)
  return(decompressed)
}

download_data <- function() {
  if (is_empty(list.files(path="db"))) {

    # Download the files to the db folder.
    if (pacman::p_isloaded("osfr")) {
      osfr::osf_retrieve_file("https://osf.io/3vfwp") %>%
        osfr::osf_download(path = "db/", progress = TRUE)
      osfr::osf_retrieve_file("https://osf.io/9e4mr") %>%
        osfr::osf_download(path = "db/", progress = TRUE)
    } else {
      download.file("https://osf.io/3vfwp/download", "db/results_detailed.db")
      download.file("https://osf.io/9e4mr/download", "db/results_simplified.db")
    }

    # Compare checksums
    if(tools::md5sum("db/results_simplified.db") == "c57ff82a37123efaae736782b91d51ca" &&
        tools::md5sum("db/results_detailed.db") == "3781380e0177f0771c765798e19f0207") {
      print("Download succeeded!")
    } else {
      print("Download failed.")
    }
  }
}

# Data is in a SQLite 3 db
get_data_from_db <- function(FUN = get_all_data, db) {
  con <- dbConnect(RSQLite::SQLite(), db)

  if (stringr::str_starts(db, "db")) {
    results_db <- tbl(con, "Results")
    df <- FUN(results_db) %>%
      get_and_convert_data()
  } else {
    results_db <- tbl(con, "Result")
    df <- FUN(results_db)
  }

  dbDisconnect(con)

  return(df)
}

get_all_data <- function(sqlconn) {
  df <- sqlconn %>% dplyr::collect() %>%
    mutate(Runtime = ifelse(Runtime < 0, Runtime+86400, Runtime))
}

convert_data <- function(data) {
  zlib <- reticulate::import("zlib")
  data %>%
    dplyr::mutate(Runtime = ifelse(Runtime < 0, Runtime+86400, Runtime),
           #dplyr::across(any_of(c("JPH", "WIP", "LT")), purrr::map, convert_blob))
           dplyr::across(any_of(c("JPH", "WIP", "LT")), purrr::map, \(x) as.numeric(stringr::str_split_1(toString(zlib$decompress(unlist(x))), pattern = ","))))
}





convert_blob <- function(blob) {
  zlib <- import("zlib")
  #return(list(as.numeric(str_split_1(toString(zlib$decompress(unlist(blob))), pattern = ","))))
  return(as.numeric(stringr::str_split_1(toString(zlib$decompress(unlist(blob))), pattern = ",")))
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

## call required packages
require(pacman)
p_load(tidyverse, DBI, RSQLite, plotly, GGally, lattice, Hmisc,
       latex2exp, RColorBrewer, reticulate, twosamples, osfr, multidplyr)

source("R/AnalyzeData.R")
library(latex2exp)

# How to compare, we have several different KPIs. Facet on input distribution.
# Create a visualisation for each metric with min, avg, and max.
temp <- function(df) {
  df %>% filter(InputDistribution == 0) %>%
    plot_ly(x = ~Experiment, name = "LT avg.", type = "scatter", mode = "lines",
            line = list(color = "red", width = 4)) %>%
    add_trace(y = ~LT_min, name = "LT min",
              line = list(color="red", width = 4, dash = "dash")) %>%
    add_trace(y = ~LT_max, name = "LT max",
              line = list(color="red", width = 4, dash = "dash"))

  grouped %>%
    filter(InputDistribution == 0) %>%
    select(starts_with("LT"), ExpName) %>%
    ggplot(aes(Experiment, y = LT_avg, color = ExpName)) + geom_point()
}

parallell_coordinates <- function(df) {
  df %>% filter(Runtime > 0) %>%
    plot_ly(type = 'parcoords',
            line = list(color = ~Runtime,
                        colorscale = 'Jet',
                        showscale = FALSE,
                        reversescale = FALSE),
            dimensions = list(
              list(range = c(1,3),
                   tickvals = c(1,2,3),
                   ticktext = c("Detailed", "Aggregated", "AggregatedPlace"),
                   label = "Exp Type", values = ~as.numeric(ExpName)),
              list(range = c(~min(NumberMachines),~max(NumberMachines)),
                   label = 'Number Machines', values = ~NumberMachines),
              list(range = c(~min(BufferSize),~max(BufferSize)),
                   label = "Buffer Size", values = ~BufferSize),
              list(range = c(~min(Runtime),~max(Runtime)),
                   label = "Runtime", values = ~Runtime)
            )
    )
}

plotly_splom <- function(df) {

  pl_colorscale=list(c(0.0, '#19d3f3'),
                     c(0.333, '#19d3f3'),
                     c(0.333, '#e763fa'),
                     c(0.666, '#e763fa'),
                     c(0.666, '#636efa'),
                     c(1, '#636efa'))

  axis = list(showline=FALSE,
              zeroline=FALSE,
              gridcolor='#ffff',
              ticklen=4)

  df %>%
    filter(Experiment <= 1000) %>%
    plot_ly() %>%
    add_trace(type = "splom",
              dimensions = list(
                list(label = "LT_min", values = ~LT_min),
                list(label = "LT_avg", values = ~LT_avg),
                list(label = "LT_max", values = ~LT_max),
                list(label = "NrMachines", values = ~NumberMachines)
              ),
              text = ~ExpName,
              marker = list(
                color = as.integer(df$ExpName),
                colorscale = pl_colorscale,
                size = 5,
                line = list(
                  width = 1,
                  color = 'rgb(230,230,230)'
                )
              )
    ) %>%
    layout(
      title= 'Comparison',
      hovermode='closest',
      dragmode= 'select',
      plot_bgcolor='rgba(240,240,240, 0.95)',
      xaxis=list(domain=NULL, showline=F, zeroline=F, gridcolor='#ffff', ticklen=4),
      yaxis=list(domain=NULL, showline=F, zeroline=F, gridcolor='#ffff', ticklen=4),
      xaxis2=axis,
      xaxis3=axis,
      xaxis4=axis,
      yaxis2=axis,
      yaxis3=axis,
      yaxis4=axis
    )
}

plot_diff <- function(attr, df, inputDist = 0, interactive = FALSE) {
  if (!(inputDist == 0)) {
    df <- df %>% filter(as.numeric(InputDistribution == inputDist))
  }

  gfx <- df %>%
    filter(!ExpName == "Detailed") %>%
    ggplot(aes(NumberMachines, y = !!as.name(attr), color = ExpName, group = interaction(Experiment, ExpName))) +
    geom_point(alpha = 0.5, size = 0.2) +
    stat_smooth(aes(group = ExpName), se = F) +
    geom_hline(yintercept = 1) +
    #scale_y_continuous(limits = c(0.5, 1.5)) +
    scale_x_continuous(minor_breaks = seq(50, 450, by = 50)) +
    #coord_cartesian(ylim = limits) + # Use the same zoom on all the graphs.
    facet_wrap(~InputDistribution, nrow = 3, strip.position = "right", drop = TRUE) +
    labs(title = attr, x = "Number of buffer/machine pairs in sequence", y = "% difference to Detailed",
         color = "Experiment") +
    theme_bw() +
    theme(legend.position = "bottom")

  if (!interactive) {
    print(gfx)
  }
  else{
    ggplotly(gfx)
  }
}

compare_buffersize <- function(attr, df, interactive=FALSE) {
  gfx <- df %>%
    filter(!ExpName == "Detailed") %>%
    ggplot(aes(NumberMachines, y = !!as.name(attr), group = interaction(ExpName, as.factor(BufferSize)))) +
    geom_point(alpha = 0.5, size = 1, aes(shape = ExpName)) +
    geom_hline(yintercept = 1) +
    geom_line(aes(color = as.factor(BufferSize), group = interaction(ExpName, as.factor(BufferSize)))) +
    #stat_smooth(aes(color = BufferSize), se = FALSE) +
    scale_x_continuous(minor_breaks = seq(50, 450, by = 50)) +
    facet_wrap(~InputDistribution, nrow = 3, strip.position = "right", drop = TRUE) +
    labs(title = paste("BufferSize compared to", attr),
         x = "Number of buffer/machine pairs in sequence",
         y = "% difference to Detailed",
         color = "BufferSize",
         shape = "Experiment") +
    theme_bw() +
    theme(legend.position = "bottom")

  if (!interactive) {
    print(gfx)
  }
  else{
    ggplotly(gfx)
  }
}

compare_singleinputdist_buffersize <- function(attr, df, interactive=FALSE) {
  df %>% mutate(BufferSize = as.factor(BufferSize)) %>%
    filter(ExpName == "Aggregated") %>%
    ggplot(aes(NumberMachines, y = !!as.name(attr), group = interaction(ExpName, BufferSize))) +
    geom_point(alpha = 0.5, size = 1)+#, aes(shape = ExpName)) +
    geom_hline(yintercept = 0) +
    geom_line(aes(color = BufferSize, group = interaction(ExpName, BufferSize))) +
    scale_x_continuous(minor_breaks = seq(50, 450, by = 50)) +
    facet_wrap(vars(Setting), nrow = 5, strip.position = "right", drop = TRUE) +
    labs(#title = paste("BufferSize compared to", attr),
         x = "Number of buffer/machine pairs in sequence",
         y = "Error",
         color = "BufferSize",
         shape = "Experiment") +
    theme_bw(base_size = 18) +
    theme(legend.position = "bottom")
}

plot_compare_error <- function(df, attr, metric = "Error", interactive = FALSE) {
  df %>% mutate(BufferSize = as.factor(BufferSize),
                Setting = factor(Setting,
                                 levels = c("1","2","3","4","5"),
                                 labels = c("\u03b1\u2081",
                                            "\u03b1\u2082",
                                            "\u03b1\u2083",
                                            "\u03b1\u2084",
                                            "\u03b1\u2085"))) %>% # Alpha_n
    ggplot(aes(NumberMachines, y = !!as.name(metric), group = interaction(Setting, BufferSize))) +
    #geom_point(alpha = 0.5, size = 1) + #, aes(shape = ExpName)) +
    geom_hline(yintercept = if_else(metric == "Ratio", 1, 0)) +
    geom_line(aes(color = BufferSize, group = interaction(Setting, BufferSize))) +
    scale_x_continuous(minor_breaks = seq(50, 450, by = 50)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + # Use percentage
    facet_wrap(vars(Setting), nrow = 5, strip.position = "right", drop = TRUE) +
    labs(x = "Number of buffer/machine pairs in sequence, \u03b2", # Beta
         y = paste(if_else(metric=="RMSES", "Scaled RMSE", metric), unlist(strsplit(attr, "[_]"))[1]),
         color = "\u03b3", # Gamma
         shape = "Experiment") +
    theme_bw(base_size = 14) +
    theme(legend.position = "bottom")
}

plot_compare_error_ggpubr <- function(df, attr, metric = "Error") {
  df %>% mutate(BufferSize = as.factor(BufferSize),
                Setting = factor(Setting,
                                 levels = c("1","2","3","4","5"),
                                 labels = c("\u03b1\u2081",
                                            "\u03b1\u2082",
                                            "\u03b1\u2083",
                                            "\u03b1\u2084",
                                            "\u03b1\u2085"))) %>%
    ggpubr::ggline(x = "NumberMachines", y = metric, group = "BufferSize", color = "BufferSize",
                   plot_type = "l", ggtheme = theme_gray()) %>%
    ggpubr::facet(facet.by = "Setting", nrow = 5, strip.position = "right") +
    #ggpubr::border(color = "grey", size = 0.5) +
    ggpubr::yscale("percent")

}

matrix_plot <- function(attr, data) {
  cols <- regmatches(colnames(data), regexpr(paste0(attr,"_[a-z]*"), colnames(data)))
  input1 <- lapply(cols, plot_diff, df = data, inputDist = 1)
  input2 <- lapply(cols, plot_diff, df = data, inputDist = 2)
  input3 <- lapply(cols, plot_diff, df = data, inputDist = 3)

  ggmatrix(do.call(c, list(input1, input2, input3)), nrow = 3, ncol = length(input1), byrow = TRUE,
           xAxisLabels = cols,
           showAxisPlotLabels = TRUE,
           yAxisLabels = levels(grouped$InputDistribution),
           legend = c(1,1),
           title = attr) +
    theme(legend.position = "bottom")
}


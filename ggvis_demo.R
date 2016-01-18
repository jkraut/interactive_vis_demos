library(dplyr)
library(ggplot2)
library(ggvis)
library(RColorBrewer)

cocaine %>%
  mutate(month = as.factor(month)) %>%
  ggvis(x = ~month) %>%
  layer_bars() 

cocaine %>%
  mutate(month = as.factor(month)) %>%
  group_by(month) %>%
  summarize(count = n()) %>%
  ggvis(x = ~month, width = band(), y = ~count, y2 = 0) %>%
  layer_rects() %>%
  scale_numeric("y", expand = 0)

cocaine.data <-
  cocaine %>%
    mutate(month = as.factor(month)) %>%
    group_by(month) %>%
    summarize(count = n()) %>%
    mutate(def.color = 'blue')


f <- function(pal) brewer.pal(brewer.pal.info[pal, "maxcolors"], pal)

cocaine.data$color <- f('Set3')

cocaine.data %>%
  ggvis(x = ~count, x2 = 0, y = ~month, height = band()) %>%
    layer_rects(fill := ~color, key := ~count) %>%
    scale_numeric("x", expand = 0)

# try with tooltip
ApplyTooltip <- function(x) {
  if (is.null(x)) return(NULL)
  total.count <- 
    sum(cocaine.data[cocaine.data == x$month, "count"])
  
  paste0("Total Count: ", total.count)
}

cocaine.data %>%
  ggvis(x = ~count, x2 = 0, y = ~month, height = band()) %>%
  layer_rects(fill := ~color, key := ~count) %>%
  scale_numeric("x", expand = 0) %>%
  add_tooltip(ApplyTooltip, "hover")


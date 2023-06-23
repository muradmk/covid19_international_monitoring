library(here)
library(tidyverse)
library(scales)
library(openxlsx)
library(PHACTrendR)
library(table1)
library(outbreakinfo)
library(kableExtra)
library(knitr)
library(purrr)
library(glue)

##### Edits to some functions

# editing function to include date start

plotAllLineagesByLocation <- function (location, other_threshold = 0.05, nday_threshold = 10, 
                                       ndays = 180, other_exclude = NULL, cumulative = FALSE, include_title = TRUE, date_start){
  df <- getGenomicData(query_url = "prevalence-by-location-all-lineages", 
                       location = location, other_threshold = other_threshold, 
                       nday_threshold = nday_threshold, ndays = ndays, other_exclude = other_exclude, 
                       cumulative = cumulative)
  COLORPALETTE = c("#4E79A7", "#f28e2b", "#59a14f","#e15759", "#499894","#B6992D",  "#D37295", "#B07AA1","#9D7660", "#bcbd22",
                            "#aecBe8", "#FFBE7D",  "#8CD17D", "#FF9D9A",  "#86BCB6", "#F1CE63","#FABFD2",  "#D4A6C8", "#D7B5A6",  "#79706E")
  if (is.null(df)) 
    return(df)
  df <- df %>% 
    filter(date >= date_start)
  df$lineage = factor(df$lineage, levels = unique(c("other", 
                                                    df %>% arrange(desc(prevalence_rolling)) %>% pull(lineage))))
  numLineages = levels(df$lineage) %>% length()
  if (numLineages > length(COLORPALETTE)) {
    COLORPALETTE = c(COLORPALETTE, rep("#bab0ab", numLineages - 
                                                  length(COLORPALETTE)))
  }
  p <- ggplot(df, aes(x = date, y = prevalence_rolling, group = lineage, 
                      fill = lineage)) + geom_area(colour = "#555555", size = 0.2) + 
    scale_x_date(date_labels = "%b %Y", expand = c(0, 0)) + 
    scale_y_continuous(labels = scales::percent, expand = c(0, 
                                                            0)) + scale_fill_manual(values = COLORPALETTE) + 
    theme_minimal() + labs(caption = "Data from GISAID (https://gisaid.org/)") + 
    theme(legend.position = "bottom", legend.background = element_rect(fill = "#eeeeec", 
                                                                       colour = NA), panel.grid = element_blank(), axis.ticks = element_line(size = 0.5, 
                                                                                                                                             colour = "#555555"), axis.ticks.length = unit(5, 
                                                                                                                                                                                           "points"), axis.title = element_blank()) + theme(legend.position = "bottom", 
                                                                                                                                                                                                                                            axis.title = element_blank(), plot.caption = element_text(size = 18))
  if (include_title == T) {
    p <- p + ggtitle(paste0("Lineage prevalence in ", str_to_title(location)))
  }
  return(p)
}

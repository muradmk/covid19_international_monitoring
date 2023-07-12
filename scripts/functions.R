# Load in packages to get started
source("scripts/packages.R")

# Creating function to list the countries that have experienced case increase (>20%) and stratify by WHO region
case_inc_country <- function(region_cases){
  country_stat_case <- c()
  for (i in region_cases){
    wtv <- df_case %>% filter(WHO_region==i)
    
    countries <- wtv$Country
    perc_inc <- wtv$case_perc_change
    case_new <- as.character(wtv$Cases_quadriweekly-wtv$Cases_PreviousWeek)
    line <- c()
    
    for (j in seq_along(countries)) {
      line[j] <- paste0(countries[j], " (", perc_inc[j], "; ", case_new[j]," new cases", ")")
    }
    
    stat <- paste0("In the ", i, " WHO region, there are ", nrow(wtv), " countries with case increases of 20% or more.",
                   " These countries, in descending order, are: ", turn_char_vec_to_comma_list(line), ".")
    
    country_stat_case <- append(country_stat_case, stat)
  }
  return(country_stat_case)
}

# Creating function to list the countries that have experienced death increase (>20%) and stratify by WHO region
death_inc_country <- function(region_death){
  country_stat_death <- c()
  for (i in region_death){
    wtv <- df_death %>% filter(WHO_region==i)
    
    countries <- wtv$Country
    perc_inc <- wtv$death_perc_change
    death_new <- as.character(wtv$Deaths_quadriweekly-wtv$Deaths_PreviousWeek)
    line <- c()
    
    for (j in seq_along(countries)) {
      line[j] <- paste0(countries[j], " (", perc_inc[j], "; ", death_new[j]," new deaths", ")")
    }
    
    stat <- paste0("In the ", i, " WHO region, there are ", nrow(wtv), " countries with death increases of 20% or more.",
                   " These countries, in descending order, are: ", turn_char_vec_to_comma_list(line), ".")
    
    country_stat_death <- append(country_stat_death, stat)
  }
  return(country_stat_death)
}

# Creating function to list the countries that have experienced case increase (>20%) and stratify by WHO region
case_inc_region <- function(region_who_cases){
    if(nrow(df_reg_case) > 0){
      for (i in region_who_cases){
        wtv <- df_reg_case %>% filter(WHO_region==i)
        
        print(paste0("In the ", i, " WHO region there was an increase of ",turn_char_vec_to_comma_list(as.character(wtv$Cases_quadriweekly-wtv$Cases_PreviousWeek)),
                     " more new cases compared to the previous 28-day interval."))
        }} else{
      print(paste0("There are no WHO regions with case increases over 1% in the last 28-day reporting interval compared to the preceeding 28-day reporting interval."))
        }
  
}

case_inc_region <- function(region_who_cases){
  country_stat_case <- c()
    for (i in region_who_cases){
    wtv <- df_reg_2 %>% 
      filter(WHO_region==i & Quadriweek > (max(Quadriweek, na.rm = T)- weeks(4)))%>% 
      mutate(case_perc_change = turn_num_to_percent_change(Weekly_Change_Cases))
    
    region <- wtv$WHO_region
    perc_inc <- wtv$case_perc_change
    case_new <- as.character(wtv$Cases_quadriweekly-wtv$Cases_PreviousWeek)
    line <- c()
    
    stat <- paste0("In the ", i, " WHO region, there was a case count change of ", perc_inc, " compared to the previous 28-day interval.", 
                   " This presented a change of ", case_new, " cases compared to the previous 28-day interval.")
    
    country_stat_case <- append(country_stat_case, stat)
  }
  return(country_stat_case)
}



# Creating function to list the countries that have experienced death increase (>20%) and stratify by WHO region
death_inc_region <- function(region_who_death){
  country_stat_case <- c()
  for (i in region_who_death){
    wtv <- df_reg_2 %>% 
      filter(WHO_region==i & Quadriweek > (max(Quadriweek, na.rm = T)- weeks(4)))%>% 
      mutate(death_perc_change = turn_num_to_percent_change(Weekly_Change_Deaths))
    
    region <- wtv$WHO_region
    perc_inc <- wtv$death_perc_change
    death_new <- as.character(wtv$Deaths_quadriweekly-wtv$Deaths_PreviousWeek)
    line <- c()
    
    stat <- paste0("In the ", i, " WHO region, there was a death count change of ", perc_inc, " compared to the previous 28-day interval.", 
                   " This presented a change of ", death_new, " deaths compared to the previous 28-day interval.")
    
    country_stat_case <- append(country_stat_case, stat)
  }
  return(country_stat_case)
}


# Creating a function to plot the lineages in each country that has experienced case increases (>20%)

plotAllLineagesByLocation_case_inc <- function(n_month = 6, n_seq = 500, end_date = current_interval){for(i in df_case$Country){
  df_5 <- getGenomicData(query_url = "prevalence-by-location-all-lineages", 
                         location = i, other_threshold = 0.05, 
                         nday_threshold = 10, ndays = 180, other_exclude = NULL, 
                         cumulative = F)
  if(is.null(df_5) == T){#print(paste0(i, " not found"))
    next}
  if(tail(df_5$total_cases,1) < n_seq){
    next
  }
  df_5 <- df_5 %>% 
    filter(date >= (Sys.Date() - months(n_month)),
           date <= end_date) %>% 
    mutate(total_cases = cumsum(lineage_count)
           ,lineage = case_when(
             is.na(lineage) == T ~ NA_character_, 
             lineage == "" ~ NA_character_, 
             lineage == " " ~ NA_character_, 
             lineage == "other" ~ NA_character_, 
             TRUE ~ lineage))
  is_na <- sum(is.na(df_5$lineage))
  filtered_lineage <- na.omit(df_5$lineage)
  df_5 <- df_5[!is.na(df_5$lineage), ]  # Remove rows with NA in lineage column
  COLORPALETTE = c("#4E79A7", "#f28e2b", "#59a14f","#e15759", "#499894","#B6992D",  "#D37295", "#B07AA1","#9D7660", "#bcbd22",
                            "#aecBe8", "#FFBE7D",  "#8CD17D", "#FF9D9A",  "#86BCB6", "#F1CE63","#FABFD2",  "#D4A6C8", "#D7B5A6",  "#79706E")
                            if(nrow(df_5) == 0){print(paste0(i, " not found"))
                              next}
  
  df_5$lineage = factor(df_5$lineage, levels = unique(c(na.omit(df_5) %>% arrange(desc(prevalence_rolling)) %>% pull(lineage))))
  numLineages = levels(df_5$lineage) %>% length()
  if (numLineages > length(COLORPALETTE)) {
    COLORPALETTE = c(COLORPALETTE, rep("#bab0ab", numLineages - 
                                                  length(COLORPALETTE)))
  }
  
  wtv <- df_case %>% filter(Country==i)
  
  perc_inc <- wtv$case_perc_change
  case_new <- as.character(wtv$Cases_quadriweekly-wtv$Cases_PreviousWeek)
  line <- paste0(i, " (", perc_inc, "; ", case_new," new cases", ")")
  
  p <- ggplot(df_5, aes(x = date, y = prevalence_rolling, group = lineage, 
                        fill = lineage)) + geom_area(colour = "#555555", size = 0.2) + 
    scale_x_date(date_labels = "%b %Y", expand = c(0, 0)) + 
    scale_y_continuous(labels = scales::percent, expand = c(0, 
                                                            0)) + scale_fill_manual(values = COLORPALETTE) + 
    theme_minimal() + labs(caption = "Data from GISAID (https://gisaid.org/)") + 
    theme(legend.position = "bottom", 
          legend.background = element_rect(fill = "#eeeeec",colour = NA), 
          panel.grid = element_blank(), 
          axis.ticks = element_line(size = 0.5,colour = "#555555"), 
          axis.ticks.length = unit(5, "points"), 
          axis.title = element_blank()) + 
    theme(legend.position = "bottom", 
          axis.title = element_blank(), 
          plot.caption = element_text(size = 10))+ 
    ggtitle(paste0("Lineage prevalence in ", line, " over the past ", n_month," months."))+
    labs(subtitle = paste0("Limited to countries that have reported >", n_seq, " sequences in that time period."))
  p <- ggplotly(p) %>%
    layout(title = list(text = paste0("Lineage prevalence in ", line, " over the past ", n_month," months.",
                                      '<br>',
                                      '<sup>',
                                      "Limited to countries that have reported >", n_seq, " sequences in that time period."," Excluding ",is_na ,  " missing and non-specified lineages.",
                                      '</sup>')),
           annotations = list(x = 1, y = -0.1, text = "Data from GISAID (https://gisaid.org/)", 
                              showarrow = F, xref='paper', yref='paper', 
                              xanchor='right', yanchor='auto', xshift=0, yshift=0,
                              font=list(size=10, color="black")))
  return(p)
}
}


# Creating a function to plot the lineages in each country that has experienced death increases (>20%)

plotAllLineagesByLocation_death_inc <- function(n_month = 6, n_seq = 500, end_date = current_interval){for(i in df_death$Country){
  df_5 <- getGenomicData(query_url = "prevalence-by-location-all-lineages", 
                         location = i, other_threshold = 0.05, 
                         nday_threshold = 10, ndays = 180, other_exclude = NULL, 
                         cumulative = F)
  if(is.null(df_5) == T){#print(paste0(i, " not found"))
    next}
  df_5 <- df_5 %>% 
    filter(date >= (Sys.Date() - months(n_month)),
           date <= end_date) %>% 
    mutate(total_cases = cumsum(lineage_count)
           ,lineage = case_when(
             is.na(lineage) == T ~ NA_character_, 
             lineage == "" ~ NA_character_, 
             lineage == " " ~ NA_character_, 
             lineage == "other" ~ NA_character_, 
             TRUE ~lineage))
  is_na <- sum(is.na(df_5$lineage))
  filtered_lineage <- na.omit(df_5$lineage)
  df_5 <- df_5[!is.na(df_5$lineage), ]  # Remove rows with NA in lineage column
  COLORPALETTE = c("#4E79A7", "#f28e2b", "#59a14f","#e15759", "#499894","#B6992D",  "#D37295", "#B07AA1","#9D7660", "#bcbd22",
                            "#aecBe8", "#FFBE7D",  "#8CD17D", "#FF9D9A",  "#86BCB6", "#F1CE63","#FABFD2",  "#D4A6C8", "#D7B5A6",  "#79706E")
                            if(nrow(df_5) == 0){print(paste0(i, " not found"))
                              next}
  
  if(tail(df_5$total_cases,1) < n_seq){
    next
  }
  
  df_5$lineage = factor(df_5$lineage, levels = unique(c(df_5 %>% arrange(desc(prevalence_rolling)) %>% pull(lineage))))
  numLineages = levels(df_5$lineage) %>% length()
  if (numLineages > length(COLORPALETTE)) {
    COLORPALETTE = c(COLORPALETTE, rep("#bab0ab", numLineages - 
                                                  length(COLORPALETTE)))
  }
  
  wtv <- df_death %>% filter(Country==i)
  
  perc_inc <- wtv$death_perc_change
  death_new <- as.character(wtv$Deaths_quadriweekly-wtv$Deaths_PreviousWeek)
  line <- paste0(i, " (", perc_inc, "; ", death_new," new deaths", ")")
  
  
  p <- ggplot(df_5, aes(x = date, y = prevalence_rolling, group = lineage, 
                        fill = lineage)) + geom_area(colour = "#555555", size = 0.2) + 
    scale_x_date(date_labels = "%b %Y", expand = c(0, 0)) + 
    scale_y_continuous(labels = scales::percent, expand = c(0, 
                                                            0)) + scale_fill_manual(values = COLORPALETTE) + 
    theme_minimal() + labs(caption = "Data from GISAID (https://gisaid.org/)") + 
    theme(legend.position = "bottom", 
          legend.background = element_rect(fill = "#eeeeec",colour = NA), 
          panel.grid = element_blank(), 
          axis.ticks = element_line(size = 0.5,colour = "#555555"), 
          axis.ticks.length = unit(5, "points"), 
          axis.title = element_blank()) + 
    theme(legend.position = "bottom", 
          axis.title = element_blank(), 
          plot.caption = element_text(size = 10))+ 
    ggtitle(paste0("Lineage prevalence in ", line, " over the past ", n_month," months."))+
    labs(subtitle = paste0("Limited to countries that have reported >", n_seq, " sequences in that time period."))
  p <- ggplotly(p) %>%
    layout(title = list(text = paste0("Lineage prevalence in ", line, " over the past ", n_month," months.",
                                      '<br>',
                                      '<sup>',
                                      "Limited to countries that have reported >", n_seq, " sequences in that time period."," Excluding ",is_na ,  " missing and non-specified lineages.",
                                      '</sup>')),
           annotations = list(x = 1, y = -0.1, text = "Data from GISAID (https://gisaid.org/)", 
                              showarrow = F, xref='paper', yref='paper', 
                              xanchor='right', yanchor='auto', xshift=0, yshift=0,
                              font=list(size=10, color="black")))
  return(p)
}
}


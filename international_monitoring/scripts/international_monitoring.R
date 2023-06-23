##------------------------------------------------------------------------------
##                 
##                    INTERNATIONAL MONITORING CODE
##
## Purpose: this code uses data from the WHO to flag countries with large 
## increases in terms of cases and deaths over the last 28 days compared to 
## the previous 28 days, for inclusion into SIR. 
##
## Authored by: Murad Khrais
## Last updated on: 2023-06-07 by Murad Khrais
##
##
##
## Notes for improvement:
## Add increased percentages in brackets after the country in the countries with increases line
## Empty lineage in the China graph legend (green)
## Is there a way to add the summary case increase or death increase in a low profile way next to the lineage graphs?
## Is there a way to set at least our current dominant lineage as the same colour in every graph? (E.g., XBB.1.5 always the same right now)
## Can probably institute a graph cut off for countries with less than X sequences in the 8 month period. just to help with the length of the document. If a country only has a few sequences we can't really pull meaningful information out anyway. Maybe 1000? Also is there a way to add the 8 month N to the title for the graphs or as sentence before each one?
##
##
##
##
##------------------------------------------------------------------------------

# Load in packages to get started
source("scripts/packages.R")

### Automating most recent epiweek
epiwk    <- floor_date(Sys.Date(), "week") - 7    # 2 Sundays ago
                     
# Load in data from WHO
df_raw <- as.data.frame(read_csv("https://covid19.who.int/WHO-COVID-19-global-data.csv"))

country_region <- df_raw  %>% 
  select(Country, Country_code, WHO_region) %>% 
  unique()

# Derive 28MA for cases and deaths.  
# Create date sequence to automate rolling 28-day-based time interval from 164 weeks ago relative to most recent epiweek
seq_4week <- seq.Date((epiwk - weeks(163) - days(6)),  
                       epiwk, 
                       by = "4 week")
 
df_1 <- df_raw %>%
  filter(Date_reported <= epiwk) %>% 
  mutate(Quadriweek = ifelse(Date_reported %in% seq_4week, Date_reported, NA),
         Quadriweek = as.Date(Quadriweek, origin = "1970-01-01"),
         Days_denom = 1,
         Quadriweek = Quadriweek - days(1)
         ) %>%
  arrange(Date_reported) %>% # sort before filling the weeks
  fill(Quadriweek, .direction = "down") %>% 
  group_by(Country, Quadriweek) %>%
  summarise(Cases_quadriweekly = sum(New_cases, na.rm = TRUE),
            Deaths_quadriweekly = sum(New_deaths, na.rm = TRUE),
            Days_denom  = sum(Days_denom), # this is the number of days we have data for this 28 day interval (for each Country and interval)
            .groups = "drop_last") %>%
  mutate(Cases_Daily_28MA = Cases_quadriweekly/Days_denom,
         Deaths_Daily_28MA = Deaths_quadriweekly/Days_denom) %>%
  arrange(Quadriweek, Country)

current_interval <- max(df_1$Quadriweek, na.rm = T) + weeks(4)
previous_interval <- max(df_1$Quadriweek, na.rm = T)

warning("Current 28-day interval is ending on: ", current_interval, ". Previous 28-day interval is ending on: ", previous_interval ,".")


# calculate percent change (28 day)
df_2 <- df_1 %>%
  arrange(Country, Quadriweek) %>%
  group_by(Country) %>%
  mutate(Cases_PreviousWeek = lag(Cases_quadriweekly,n=1L,default=NA),
         Deaths_PreviousWeek = lag(Deaths_quadriweekly,n=1L,default=NA),
         Weekly_Change_Cases = (Cases_quadriweekly-Cases_PreviousWeek)/Cases_PreviousWeek,
         Weekly_Change_Deaths = (Deaths_quadriweekly-Deaths_PreviousWeek)/Deaths_PreviousWeek) %>%

  # set percent change to 0% if either 28-day interval has <1 (non-zero) deaths
  mutate(Weekly_Change_Deaths = ifelse((Deaths_quadriweekly < 1 & Deaths_quadriweekly != 0) | (Deaths_PreviousWeek < 1 & Deaths_PreviousWeek != 0), 0, Weekly_Change_Deaths)) %>%
  arrange(Country, Quadriweek) %>%
  select(Country, Quadriweek, Cases_quadriweekly, Deaths_quadriweekly, Days_denom, Cases_Daily_28MA, Deaths_Daily_28MA, Cases_PreviousWeek, Deaths_PreviousWeek, Weekly_Change_Cases, Weekly_Change_Deaths) %>%
  distinct()

# merging region and country data

df_2 <- df_2 %>% 
  group_by(Country) %>% 
  left_join(country_region, by = "Country", keep = F)
  

# filtering out increases in cases over the last two 28-day intervals

df_case <- df_2 %>% 
  filter(Quadriweek > (max(Quadriweek, na.rm = T)- weeks(4))) %>% 
  filter(Weekly_Change_Cases >= 0.2 & Weekly_Change_Cases < Inf) %>% 
  filter(Cases_quadriweekly > 100) %>% 
  mutate(case_perc_change = turn_num_to_percent_change(Weekly_Change_Cases)) %>% 
  arrange(desc(Weekly_Change_Cases)) %>% 
  select(Country, WHO_region, Quadriweek, Cases_quadriweekly, Days_denom, Cases_PreviousWeek, Weekly_Change_Cases, case_perc_change)

df_death <- df_2 %>% 
  filter(Quadriweek > (max(Quadriweek, na.rm = T)- weeks(4))) %>% 
  filter(Weekly_Change_Deaths >= 0.2 & Weekly_Change_Deaths < Inf)%>% 
  mutate(death_perc_change = turn_num_to_percent_change(Weekly_Change_Deaths))%>% 
  arrange(desc(Weekly_Change_Deaths)) %>% 
  select(Country, WHO_region, Quadriweek, Deaths_quadriweekly, Days_denom, Deaths_PreviousWeek, Weekly_Change_Deaths, death_perc_change)

df_case_death <-  df_2 %>% 
  filter(Quadriweek > (max(Quadriweek, na.rm = T)- weeks(4))) %>% 
  filter((Weekly_Change_Cases >= 0.2 & Weekly_Change_Cases < Inf) & (Weekly_Change_Deaths >= 0.2 & Weekly_Change_Deaths < Inf))

# Creating list of regions wherein countries have experienced case increases (>20%)
region_cases <- unique(df_case$WHO_region)

# Creating list of regions wherein countries have experienced death increases (>20%)
region_death <- unique(df_death$WHO_region)

country_case_list <- paste0("There are ", nrow(df_case)," countries with case increases over 20% in the last ",
       "28-day reporting interval compared to the preceeding 28-day reporting interval.",
       " These countries are from the following WHO regions: ",turn_char_vec_to_comma_list(region_cases), 
       ".")


country_death_list <- paste0("There are ", nrow(df_death)," countries with death increases over 20% in the last ",
                            "28-day reporting interval compared to the preceeding 28-day reporting interval.",
                            " These countries are from the following WHO regions: ",turn_char_vec_to_comma_list(region_death), 
                            ".")


####### Region-level ##################

# redoing the steps above for region-level overviews

# Derive 28MA for cases and deaths.  

df_reg_1 <- df_raw %>%
  filter(Date_reported <= epiwk) %>% 
  mutate(Quadriweek = ifelse(Date_reported %in% seq_4week, Date_reported, NA),
         Quadriweek = as.Date(Quadriweek, origin = "1970-01-01"),
         Days_denom = 1,
         Quadriweek = Quadriweek - days(1)
  ) %>%
  arrange(Date_reported) %>% # sort before filling the weeks
  fill(Quadriweek, .direction = "down") %>% 
  group_by(WHO_region, Quadriweek) %>%
  summarise(Cases_quadriweekly = sum(New_cases, na.rm = TRUE),
            Deaths_quadriweekly = sum(New_deaths, na.rm = TRUE),
            Days_denom  = sum(Days_denom), # this is the number of days we have data for this 28 day interval (for each Country and interval)
            .groups = "drop_last") %>%
  mutate(Cases_Daily_28MA = Cases_quadriweekly/Days_denom,
         Deaths_Daily_28MA = Deaths_quadriweekly/Days_denom) %>%
  arrange(Quadriweek, WHO_region)


# calculate percent change (28 day)
df_reg_2 <- df_reg_1 %>%
  arrange(WHO_region, Quadriweek) %>%
  group_by(WHO_region) %>%
  mutate(Cases_PreviousWeek = lag(Cases_quadriweekly,n=1L,default=NA),
         Deaths_PreviousWeek = lag(Deaths_quadriweekly,n=1L,default=NA),
         Weekly_Change_Cases = (Cases_quadriweekly-Cases_PreviousWeek)/Cases_PreviousWeek,
         Weekly_Change_Deaths = (Deaths_quadriweekly-Deaths_PreviousWeek)/Deaths_PreviousWeek) %>%
  
  # set percent change to 0% if either 28-day interval has <1 (non-zero) deaths
  mutate(Weekly_Change_Deaths = ifelse((Deaths_quadriweekly < 1 & Deaths_quadriweekly != 0) | (Deaths_PreviousWeek < 1 & Deaths_PreviousWeek != 0), 0, Weekly_Change_Deaths)) %>%
  arrange(WHO_region, Quadriweek) %>%
  select(WHO_region, Quadriweek, Cases_quadriweekly, Deaths_quadriweekly, Days_denom, Cases_Daily_28MA, Deaths_Daily_28MA, Cases_PreviousWeek, Deaths_PreviousWeek, Weekly_Change_Cases, Weekly_Change_Deaths) %>%
  distinct()

# filtering out increases in cases over the last two 28-day intervals

df_reg_case <- df_reg_2 %>% 
  filter(Quadriweek > (max(Quadriweek, na.rm = T)- weeks(4))) %>% 
  filter(Cases_quadriweekly > 100) %>% 
  mutate(case_perc_change = turn_num_to_percent_change(Weekly_Change_Cases)) %>% 
  arrange(desc(Weekly_Change_Cases)) %>% 
  select(WHO_region, Quadriweek, Cases_quadriweekly, Days_denom, Cases_PreviousWeek, Weekly_Change_Cases, case_perc_change)

df_reg_death <- df_reg_2 %>% 
  filter(Quadriweek > (max(Quadriweek, na.rm = T)- weeks(4)) & WHO_region != "Other") %>% 
  mutate(death_perc_change = turn_num_to_percent_change(Weekly_Change_Deaths))%>% 
  arrange(desc(Weekly_Change_Deaths)) %>% 
  select(WHO_region, Quadriweek, Deaths_quadriweekly, Days_denom, Deaths_PreviousWeek, Weekly_Change_Deaths, death_perc_change)

df_reg_case_death <-  df_reg_2 %>% 
  filter(Quadriweek > (max(Quadriweek, na.rm = T)- weeks(4))) %>% 
  filter((Weekly_Change_Cases >= 0.01 & Weekly_Change_Cases < Inf) & (Weekly_Change_Deaths >= 0.2 & Weekly_Change_Deaths < Inf))

# Creating list of regions that have experienced case increases (>1%)
region_who_cases <- unique(df_reg_case$WHO_region)

# Creating list of regions wherein countries have experienced death increases (>20%)
region_who_death <- unique(df_reg_death$WHO_region)

# Load in functions
source("scripts/functions.R")

ifelse(nrow(df_reg_case) > 0, region_case_list <-  paste0("There are ", nrow(df_reg_case)," regions with case increases over 1% in the last ",
                            "28-day reporting interval compared to the preceeding 28-day reporting interval.",
                            " These are the WHO regions: ",turn_char_vec_to_comma_list(region_who_cases), 
                            "."),
        region_case_list <- paste0("There are no WHO regions with case increases over 1% in the last 28-day reporting interval compared to the preceeding 28-day reporting interval."))


ifelse(nrow(df_reg_death) >0, region_death_list <- paste0("There are ", nrow(df_reg_death)," regions with death increases over 1% in the last ",
                             "28-day reporting interval compared to the preceeding 28-day reporting interval.",
                             " These are the WHO regions: ",turn_char_vec_to_comma_list(region_who_death), 
                             "."),
       region_death_list <- paste0("There are no WHO regions with death increases over 1% in the last 28-day reporting interval compared to the preceeding 28-day reporting interval.")
)


###### Variant International Monitoring ###################

# test out individual locations
#plotAllLineagesByLocation(location = "Mongolia", date_start = (Sys.Date() - months(8)))




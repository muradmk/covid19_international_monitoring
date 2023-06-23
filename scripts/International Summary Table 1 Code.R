##------------------------------------------------------------------------------
##                 
##                    INTERNATIONAL SUMMARY TABLE 1 CODE
##
## Purpose: this code uses data from Our World in Data and Oxford (Stringency)
## to produce the upfront table (Table 1) in the International Summary. It is 
## run on a weekly basis, every Tuesday. 
##
## Authored by: Aashna Uppal
## Last updated on: 2022-08-30 by Aashna Uppal
##
##------------------------------------------------------------------------------

##                           HOW TO USE THIS CODE

#comment this line out if you're not Aashna (add a # in front of it)
#this is just an extra line that I need because I need to manually tell R where
#all of my packages are stored. But your R is smarter than my R and does not need
#to be told.
#.libPaths("C:/Users/AUPPAL/Documents/") 

#pick countries (don't worry about order here - they've been programmed to be 
#arranged in order of descending case incidence, with Canada at the top. Also
#don't worry about spacing and such - I've just put them in multiple lines so
#that they fit in my screen's frame. If you double-click on a country's name then
#R will highlight any duplicates, so it's a good way to check if you accidentally
#typed the same country twice. 
chosen_countries <- c("Canada","New Zealand","France","Italy",
                      "Australia","Luxembourg","Taiwan","Portugal","Austria","Israel",
                      "Germany","Switzerland","Belgium","Spain","Chile",
                      "South Korea","Ireland","Denmark","Hong Kong","Japan","United States",
                      "Netherlands","United Kingdom","Brazil","Mexico",
                      "India","South Africa","China","Finland","Latvia","Nepal","Indonesia",
                      "Slovenia","Nigeria","Bangladesh", "Greece", "Liechtenstein", "Bulgaria",
                      "Malaysia","Peru","Dominican Republic","Brunei","Cyprus","Argentina", "Ecuador","Guatemala", "Malta", "Sweden", "Czechia", "Poland")

#if you decide to change the time span for comparison (currently it's 14 days) change
#the parameter below. It's helpful to open up OWID alongside the output of this code
#in order to see the trend in the last two weeks - our table just spits out the 
#tractory based on two static points in time (today and 14 days ago).
current_time_span_in_days <- 14

#if you don't already have the following packages installed, un-comment (i.e. remove
#the "#") the next few lines to install these packages. You only need to install
#packages once!
# install.packages("here")
# install.packages("tidyverse")
# install.packages("scales")
# install.packages("stringr")
# install.packages("openxlsx")
# install.packages("lubridate")

#when you're ready to run, just press Ctrl + Alt + R. (Or you can highlight
#everything and just press Run (top-right). Sit back, relax, and enjoy!
#once it's done running, check the folder where this code is saved and you should 
#find 2 things
#   1. Table 1 International Summary_YYYY-MM-DD.xlsx
#   2. International_Stringency_Data_YYYY-MM-DD.xlsx  

##------------------------------------------------------------------------------
##                              THE ANALYSIS
##------------------------------------------------------------------------------

#load libraries. Should you ever need another package, you can add it here so that
#it can be loaded and used to run this code. Note: you can also manually load
#packages by going to your Packages pane in your RStudio workspace. Click on the 
#little white box to the left of the package name to load. 
library(here)
#library(tidyverse)
#You may get an error in loading tidyverse. If that is the case, add a "#" in front
#of library(tidyverse) and instead remove the "#" in front of both library(dplyr)  
#and library(readr) below
library(tidyverse)
library(scales)
library(openxlsx)

#locate current script and make that the parent folder. This bit utilizes the here 
#package and locates this current script. We do this so that any output from this 
#script (i.e. the two Excel sheets) get saved where we want them to (i.e. in the same
#folder that this script lives). Should you ever wish to change that, you can play
#with the file paths in the "write.csv()" lines (you'll see those pop up later in 
#the analysis)
here::i_am("Script/International Summary Table 1 Code.R")

#load datasets from github. We use both OWID data (the first dataset) and the vaccine
#names dataset (the second one). Both of these come from OWID's github page, which is
#at https://github.com/owid/covid-19-data/tree/master/public/data
data <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")
vaccnames <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/locations.csv")

#turn date column into date format. This is so that R treats it as numeric (i.e.
#so that we can perform operations with it, like finding the maximum date)
#to check if it's a date type, run class(data$date). The "$" operator finds the 
#column within the dataset. The as.Date() is a function that turns variables into
#dates. 
data$date <- as.Date(data$date)

#modify cases, hosp, deaths to be per 100,000. OWID lists them as per million, but
#we don't want that in our tables. You can see in these lines that I'm just overwriting
#the existing variables. I'm also using the base R method of recoding these variables, 
#but this can just as easily but done tidyverse-style. The round() function rounds 
#to the number of decimal places you want - cases, for example, are rounded to 1 decimal
#place but deaths are rounded to 2 decimal places (just because the deaths are so 
#small, so 2 decimal places are needed). 
data$new_cases_smoothed_per_million <- round(data$new_cases_smoothed_per_million/10,1)
data$new_deaths_smoothed_per_million <- round(data$new_deaths_smoothed_per_million/10,2)
data$hosp_patients_per_million <- round(data$hosp_patients_per_million/10,1)
data$weekly_hosp_admissions_per_million <- round(data$weekly_hosp_admissions_per_million/10,1)

#join data with vaccnames. Now our two datasets will be one dataset. I'm joining by
#location since that's unique. left_join() will sort the data for you, so no need
#to sort beforehand. The reason we're doing a left_join() on the "data" dataset is
#because we don't want to lose any OWID data. "location" is the linking ID. 
data1 <- left_join(data, vaccnames, by = "location")

#next, join data with Oxford stringency index.

#basically, we're going to use Oxford instead of OWID for data on stringency. 
#this is because they sometimes don't line up, and Oxford has the true values.

#download stringency data from github. Note, on July 27, 2022, Oxford totally 
#revamped their stringency datasets. But, they continue to publish the legacy 
#file format, which we use. The new datasets separate stringency by vaccination
#status. So they've got many more indicators than the legacy format does. For
#now, we'll continue to use the legacy format. But keep an eye on their github
#page to see if anything changes. See https://github.com/OxCGRT/covid-policy-tracker
#If you're curious, all the changes that happened on July 27, 2022 are documented
#here: https://www.bsg.ox.ac.uk/sites/default/files/OxCGRT%20%E2%80%93%C2%A0What%27s%20changed%20summary%2022%20Jul%202022.pdf

Stringency_Data <- read_csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker-legacy/main/legacy_data_202207/OxCGRT_latest.csv")

#The above file is a simple version of the legacy file. Oxford isn't publishing
#updated versions of the legacy file with notes, which is what we used to use.
#So this file just has indicators and no notes. That means that it'll require 
#some digging to figure out what exactly cause the stringency index to change. 
#But our international summary isn't so focused on public health measures anymore, 
#so this shouldn't be a big issue. 

#remove the STATE_TOTAL rows. We only want the NAT_TOTAL rows. Oxford calculates
#stringency at a sub-national level too, but we don't want that data. 
Stringency_Data <- Stringency_Data %>% filter(Jurisdiction == "NAT_TOTAL")

#format date variable. For some reason the as.Date() function doesn't work as
#well on these dates. So I've used the ymd() function from lubridate to get 
#the date into a usable format. Both functions accomplish the same thing. 
Stringency_Data$Date <- ymd(Stringency_Data$Date) 

#create an ID variable that contains the country name and date in both the data1 
#and Stringency_Data datasets. This'll be used to join the two datasets. The
#paste() function concatenates country and date - the reason you need to concatenate
#these two is because you no longer have a unique identifier (there are repeat
#rows for both country and date if you consider them separately)
data1$ID <- paste(data1$location, data1$date, sep = "_")
Stringency_Data$ID <- paste(Stringency_Data$CountryName, Stringency_Data$Date, sep = "_")

#now join the two datasets by ID. data1 is your "main" dataset again here. 
data2 <- left_join(data1, Stringency_Data, by = "ID")

#now we can start making Table 1: we'll make a detailed version, and a less detailed 
#version of this table

#first we will pick out only the countries we specified at the beginning of the code.
#Back when we first started running this code, we used to pick countries with similar 
#rates of vaccination to canada, which is why this is called "vacc_subset" below, 
#but that's not necessarily the reason why we pick countries now). The %in% operator
#is super handy because it lets us pick out locations that match countries listed
#in the chosen_countries variable.
vacc_subset <- data2 %>% filter(location %in% chosen_countries)

#1. DETAILED TABLE--------------------------------------------------------------

#create empty table. I'm using a matrix here but I could pretty equivalently use
#a data frame. I've specified that the number of columns is 23 - if you're ever
#adding or removing variables, you'll have to change that number. 
table <- matrix(nrow = length(chosen_countries), ncol = 23)

#loop through countries and fill in empty table (It's called PT vacc subset because
#I grabbed this code from another code that looked at PTs instead of countries). The 
#for loop goes from 1 to however many countries you have, hence length(chosen_countries)
for (i in 1:length(chosen_countries)) {
  
  #grab the country in the "i" position of the chosen_countries vector and create
  #a subset of data based on that country
  PT_vacc_subset <- vacc_subset %>% filter(location == chosen_countries[i])
  
  #arrange the data subset by date
  PT_vacc_subset <- arrange(PT_vacc_subset, by = date)
  
  #add the country name to the first column in the table. The convention for 
  #indexing is [row,column], so [i,1] means row i and column 1
  table[i,1] <- chosen_countries[i]
  
  #now go through the rest of the table's columns and add more data
  
  #cases
  #first filter the data subset to only include complete data for cases. That's 
  #what the !is.na does. 
  cases <- PT_vacc_subset %>% filter(!is.na(new_cases_smoothed_per_million))
  #then, if the number of rows in cases (i.e. the filtered subset above) is 0, 
  #just put an NA, or a blank, for the number of cases this week. If the number
  #of rows in the filtered subset above isn't 0, then go in an find the latest
  #and greatest number of cases. Call it "cases_thisweek"
  cases_thisweek <- ifelse(nrow(cases) == 0, NA, cases %>% filter(date == max(date, na.rm = TRUE)) %>% select(new_cases_smoothed_per_million) %>% pull())
  #similarly, if the number of rows in the filtered subset above is 0, just put
  #an NA for the number of cases last week, but if it is not equal to 0, then pull
  #out the number of cases from 14 days ago. Remember that current_time_span_in_days
  #variable you created right at the beginning? This is where it comes in handy. 
  cases_lastweek <- ifelse(nrow(cases) == 0, NA, cases %>% filter(date == max(date, na.rm = TRUE)-current_time_span_in_days) %>% select(new_cases_smoothed_per_million) %>% pull())
  #Now, fill in the table. In the second column, just display the latest and 
  #greatest number of cases. Again if the filtered subset has 0 rows, then just
  #show NA. 
  table[i,2] <- ifelse(nrow(cases) == 0, NA, cases_thisweek)
  #in the third column, display the percent change. Subtract the cases_lastweek
  #variable from cases_thisweek and calculate relative change. The percent() function
  #turns it into a percent, and that accuracy bit at the end shows how many
  #decimal points should be in there. If you want to change to 2 decimal points
  #for example, you'd change the accuracy to = 0.01. 
  table[i,3] <- ifelse(nrow(cases) == 0, NA, percent((cases_thisweek-cases_lastweek)/cases_lastweek, accuracy = 0.1))
  #in the fourth column, you'll display the latest and greatest date. The format
  #function allows us to specify how the date should be displayed. %b is for the 
  #abbreviated month (e.g. Jan, Feb), %d is for the date, and %Y is for the 4-digit year. Should
  #you ever wish to change how the date is displayed, you can change those parameters. 
  table[i,4] <- ifelse(nrow(cases) == 0, NA, format(max(cases$date), "%b %d, %Y"))
  
  #hospitalizations - census
  #same principle as cases, so I haven't put detailed instructions here. 
  hosp <- PT_vacc_subset %>% filter(!is.na(hosp_patients_per_million))
  hosp_thisweek <- ifelse(nrow(hosp) == 0, NA, hosp %>% filter(date == max(date, na.rm = TRUE)) %>% select(hosp_patients_per_million) %>% pull())
  hosp_lastweek <- ifelse(nrow(hosp) == 0, NA, hosp %>% filter(date == max(date, na.rm = TRUE)-current_time_span_in_days) %>% select(hosp_patients_per_million) %>% pull())
  table[i,5] <- ifelse(nrow(hosp) == 0, NA, hosp_thisweek)
  table[i,6] <- ifelse(nrow(hosp) == 0, NA, percent((hosp_thisweek-hosp_lastweek)/hosp_lastweek, accuracy = 0.1))
  table[i,7] <- ifelse(nrow(hosp) == 0, NA, format(max(hosp$date), "%b %d, %Y"))
  
  #hospitalizations - admissions
  #same principle as cases, so I haven't put detailed instructions here. 
  hosp_admin <- PT_vacc_subset %>% filter(!is.na(weekly_hosp_admissions_per_million))
  hosp_admin_thisweek <- ifelse(nrow(hosp_admin) == 0, NA, hosp_admin %>% filter(date == max(date, na.rm = TRUE)) %>% select(weekly_hosp_admissions_per_million) %>% pull())
  hosp_admin_lastweek <- ifelse(nrow(hosp_admin) == 0, NA, hosp_admin %>% filter(date == max(date, na.rm = TRUE)-current_time_span_in_days) %>% select(weekly_hosp_admissions_per_million) %>% pull())
  table[i,8] <- ifelse(nrow(hosp_admin) == 0, NA, hosp_admin_thisweek)
  table[i,9] <- ifelse(nrow(hosp_admin) == 0, NA, percent((hosp_admin_thisweek-hosp_admin_lastweek)/hosp_admin_lastweek, accuracy = 0.1))
  table[i,10] <- ifelse(nrow(hosp_admin) == 0, NA, format(max(hosp_admin$date), "%b %d, %Y"))
  
  #deaths
  #same principle as cases, so I haven't put detailed instructions here. 
  deaths <- PT_vacc_subset %>% filter(!is.na(new_deaths_smoothed_per_million))
  deaths_thisweek <- ifelse(nrow(deaths) == 0, NA, deaths %>% filter(date == max(date, na.rm = TRUE)) %>% select(new_deaths_smoothed_per_million) %>% pull())
  deaths_lastweek <- ifelse(nrow(deaths) == 0, NA, deaths %>% filter(date == max(date, na.rm = TRUE)-current_time_span_in_days) %>% select(new_deaths_smoothed_per_million) %>% pull())
  table[i,11] <- ifelse(nrow(deaths) == 0, NA, deaths_thisweek)
  table[i,12] <- ifelse(nrow(deaths) == 0, NA, percent((deaths_thisweek-deaths_lastweek)/deaths_lastweek, accuracy = 0.1))
  table[i,13] <- ifelse(nrow(deaths) == 0, NA, format(max(deaths$date), "%b %d, %Y"))
  
  #vaccines
  #same principle as cases, so I haven't put detailed instructions here. 
  vacc <- PT_vacc_subset %>% filter(!is.na(people_fully_vaccinated_per_hundred))
  vacc_thisweek <- ifelse(nrow(vacc) == 0, NA, vacc %>% filter(date == max(date, na.rm = TRUE)) %>% select(people_fully_vaccinated_per_hundred) %>% pull())
  vacc_lastweek <- ifelse(nrow(vacc) == 0, NA, vacc %>% filter(date == max(date, na.rm = TRUE)-current_time_span_in_days) %>% select(people_fully_vaccinated_per_hundred) %>% pull())
  table[i,14] <- ifelse(nrow(vacc) == 0, NA, vacc_thisweek)
  table[i,15] <- ifelse(nrow(vacc) == 0, NA, percent((vacc_thisweek-vacc_lastweek)/vacc_lastweek, accuracy = 0.1))
  table[i,16] <- ifelse(nrow(vacc) == 0, NA, format(max(vacc$date), "%b %d, %Y"))
  table[i,17] <- ifelse(nrow(vacc) == 0, NA, vacc$vaccines[nrow(vacc)])
  
  #boosters
  #same principle as cases, so I haven't put detailed instructions here. 
  boost <- PT_vacc_subset %>% filter(!is.na(total_boosters_per_hundred))
  boost_thisweek <- ifelse(nrow(boost) == 0, NA, boost %>% filter(date == max(date, na.rm = TRUE)) %>% select(total_boosters_per_hundred) %>% pull())
  boost_lastweek <- ifelse(nrow(boost) == 0, NA, boost %>% filter(date == max(date, na.rm = TRUE)-current_time_span_in_days) %>% select(total_boosters_per_hundred) %>% pull())
  table[i,18] <- ifelse(nrow(boost) == 0, NA, boost_thisweek)
  table[i,19] <- ifelse(nrow(boost) == 0, NA, percent((boost_thisweek-boost_lastweek)/boost_lastweek, accuracy = 0.1))
  table[i,20] <- ifelse(nrow(boost) == 0, NA, format(max(boost$date), "%b %d, %Y"))
  
  #stringency
  #same principle as cases, so I haven't put detailed instructions here. 
  stringency <- PT_vacc_subset %>% filter(!is.na(StringencyIndex))
  stringency_thisweek <- ifelse(nrow(stringency) == 0, NA, stringency %>% filter(date == max(date, na.rm = TRUE)) %>% select(StringencyIndex) %>% pull())
  stringency_lastweek <- ifelse(nrow(stringency) == 0, NA, stringency %>% filter(date == max(date, na.rm = TRUE)-current_time_span_in_days) %>% select(StringencyIndex) %>% pull())
  table[i,21] <- ifelse(nrow(stringency) == 0, NA, round(stringency_thisweek,0))
  table[i,22] <- ifelse(nrow(stringency) == 0, NA, percent((stringency_thisweek-stringency_lastweek)/stringency_lastweek, accuracy = 0.1))
  table[i,23] <- ifelse(nrow(stringency) == 0, NA, format(max(stringency$date), "%b %d, %Y"))
}

#now turn the filled-in matrix into a dataframe and add column headings. The reason we 
#turn it into a data frame is because the colnames() function works on data frames. 
#not totally sure why I didn't just do a data frame in the first place, but alas. 
#This was initially a matrix, being turned into a data frame.
table_final <- as.data.frame(table)

#Should you ever need to add or remove the variables in this table, make sure to 
#add or remove their respective column names here too. These column names follow
#the order of columns in table_final, so if you're adding a column name, make sure
#it falls where you want it to. If the number of column names don't match the
#number of columns, R will throw an error.
colnames(table_final) <- c("Country","New Cases per 100,000 (7-day MA)","Trend in Cases (last 14 days)",
                           "Latest Date for Cases",
                           "Patients in Hospital per 100,000 (7-day MA)","Trend in Patients in Hospital (last 14 days)",
                           "Latest Date for Patients in Hospital",
                           "Hospital Admissions per 100,000 (7-day MA)","Trend in Hospitalisations (last 14 days)",
                           "Latest Date for Hospitalisations","New Deaths per 100,000 (7-day MA)",
                           "Trend in Deaths (last 14 days)","Latest Date for Deaths",
                           "Percent of Entire Population Fully Vaccinated","Trend in Vaccine Coverage (last 14 days)",
                           "Latest Date for Vaccine Coverage","Vaccines in Use","Booster Doses per 100",
                           "Trend in Booster Doses (last 14 days)","Latest Date for Booster Doses","Stringency Index",
                           "Trend in Stringency (last 14 days)","Latest Date for Stringency")

#change table column for vacc into percent (we do this after the fact because it's 
#not recognized as a number in the calculations above if you change to percent too 
#early). Accuracy = 1 means there are 0 decimal points. If you wanted 1 decimal
#point, change it to accuracy = 0.1.
table_final$`Percent of Entire Population Fully Vaccinated` <- percent(as.numeric(table_final$`Percent of Entire Population Fully Vaccinated`)/100, accuracy = 1)

#rearrange the table in order of descending case incidence. With Canada at the 
#top. There's probably a better way of doing this, but this is the best I could
#come up with at the time. First, I make sure that the case incidence variable
#is numeric. 
table_final$`New Cases per 100,000 (7-day MA)` <- as.numeric(table_final$`New Cases per 100,000 (7-day MA)`)
#part_1 and part_2 are created below so that Canada can appear as the first row
#in the table while all other countries appear below in descending order of case
#incidence
#First, I create a subset with just Canada's row. 
part_1 <- table_final %>% filter(Country == "Canada")
#Then, I create a subset with all rows except for Canada, and I arrange that 
#subset in order of descending case incidence. 
part_2 <- table_final %>% filter(Country != "Canada") %>% arrange(desc(`New Cases per 100,000 (7-day MA)`))
#lastly, I append the two subsets together. 
table_final_ordered <- rbind(part_1, part_2)

#2. LESS DETAILED TABLE (ACTUAL TABLE 1)------------------------------------------
#now we'll create a distilled version of table_final_ordered; one that you can 
#just copy/paste into the Intl Summary document

#first we'll create a function that assigns trajectory groups. Essentially this 
#function takes the % difference between the current value and the value from 14
#days ago (if you change that upfront parameter for timespan in days, this'll know
#to update as well), and it assigns that % difference to a trajectory category. 
#It also relies on a "stringency" input, which you'll see by default, is set to 
#FALSE. Basically for cases, deaths, and hosp, any change that is less than 10% is
#categorized as "stable", but for stringency we only want to categorize 0% change
#as stable, and anything >0% change is assigned to a trajectory category. 
#Note: you can tell that this is a user defined function because it uses function()
#Both "current" and "past" (and "stringency") are LABELS. They're not actually
#variables, just placeholders. When you apply the function, you'll specify what
#current and past actually are, and that's when you'll use actual variable names. 
arrows <- function(current, past, stringency = FALSE) {
  if(!stringency){ #If stringency = FALSE, this bit runs
    if(is.na(past) | is.na(current)){return("-")}
    else if(past == 0 & current == 0){return("stable")}
    else if(past == 0 & current > 0){return("up >100%")}
    else if((current-past)/past >= 1){return("up >100%")}
    else if((current-past)/past > 0.1){return("up 10-100%")}
    else if((current-past)/past >= -0.1 & (current-past)/past <= 0.1){return("stable")}
    else if((current-past)/past < -0.1 & (current-past)/past > -1){return("down 10-100%")}
    else if((current-past)/past <= -1){return("down >100%")}
  } 
  else{ #If stringency = TRUE, this bit runs
    if(is.na(past) | is.na(current)){return("-")}
    else if(past == 0 & current == 0){return("stable")}
    else if(past == 0 & current > 0){return("up >100%")}
    else if((current-past)/past >= 1){return("up >100%")}
    else if((current-past)/past > 0){return("up 0-100%")}
    else if((current-past)/past == 0){return("stable")}
    else if((current-past)/past < 0 & (current-past)/past > -1){return("down 0-100%")}
    else if((current-past)/past <= -1){return("down >100%")}
  }
  
}

#then we'll create a function that glues a number and a date together so that they can 
#appear in the same "cell". Importantly, this function will not print out the date if 
#it's the maximum date observed. It takes as input a number, a date, and the entire
#column of the date variable in question. The reason it also needs that column as input
#is so that it can find the maximum date in that column. Lastly, by default it has a 
#parameter called "percent", which is set to FALSE. If percent = TRUE, then it'll make
#sure to add a percent sign after glueing the number and date together. 
date_paste <- function(number, date, table_date_column, percent = FALSE) {
  if(!percent){ #If percent = FALSE, this bit executes
    if(is.na(number)){return("NA")} #If the number is blank, then this will just return a blank
    else if(!is.na(date) & date == max(as.Date(table_date_column, format = "%b %d, %Y"), na.rm = TRUE)){return(number)}
    else if(!is.na(date) & date < max(as.Date(table_date_column, format = "%b %d, %Y"), na.rm = TRUE)){return(paste(number, paste("(", format(as.Date(date), "%b %d"), ")", sep="")))}
  }else if(percent){ #If percent = TRUE, this bit executes
    if(is.na(number)){return("NA")} #If the number is blank, then this will just return a blank
    else if(!is.na(date) & date == max(as.Date(table_date_column, format = "%b %d, %Y"), na.rm = TRUE)){return(paste(round(number,1), "%", sep=""))}
    else if(!is.na(date) & date < max(as.Date(table_date_column, format = "%b %d, %Y"), na.rm = TRUE)){return(paste(paste(round(number,1), "%", sep=""), paste("(", format(as.Date(date), "%b %d"), ")", sep="")))}
  }
}

#now that we have the two functions we need, we can proceed much like we did for 
#the detailed table. First, I'll create an empty matrix that has as many rows as
#the number of countries in chosen_countries, and as many columns as the variables
#we need. Should you ever need to add or remove variables, make sure to update the
#"ncol" bit here. 
table_condensed <- matrix(nrow = length(chosen_countries), ncol = 15)

#Now we'll loop through the rows of this empty table and fill it in. 
for (i in 1:length(chosen_countries)) {
  
  #grab the country in the "i" position of the countries in the order they appear
  #Use this to create a data subset called data_subset (location is a variable in 
  #the vacc_subset dataset, and Country is a variable in the table_final_ordered
  #dataset)
  data_subset <- vacc_subset %>% filter(location == table_final_ordered$Country[i])
  
  #arrange data_subset by date
  data_subset <- arrange(data_subset, by = date)
  
  #add the country name to the first column in the table
  table_condensed[i,1] <- table_final_ordered$Country[i]
  
  #now fill in the rest of the columns
  
  #cases
  #this works a lot like the first detailed table. First, you filter the dataset
  #and only grab observations where cases aren't blank
  cases <- data_subset %>% filter(!is.na(new_cases_smoothed_per_million))
  #Then, you reach into that filtered subset and grab the latest and greatest
  #number of cases. If the filtered subset has 0 rows, then it'll just return NA
  cases_thisweek <- ifelse(nrow(cases) == 0, NA, cases %>% filter(date == max(date, na.rm = TRUE)) %>% select(new_cases_smoothed_per_million) %>% pull())
  #Then, you reach into the filtered dataset and grab the number of cases from
  #14 days ago. Should you update the timespan at the beginning of this code, 
  #this'll automatically update too. 
  cases_lastweek <- ifelse(nrow(cases) == 0, NA, cases %>% filter(date == max(date, na.rm = TRUE)-current_time_span_in_days) %>% select(new_cases_smoothed_per_million) %>% pull())
  #Then, you fill in the second column. This is where the date_paste() function
  #comes in handy. The inputs of this are the latest and greatest number of cases, 
  #the latest and greatest date, and the column "Latest Date for Cases" from that
  #first detailed table. 
  table_condensed[i,2] <- date_paste(cases_thisweek, max(cases$date, na.rm = TRUE), table_final_ordered$`Latest Date for Cases`)
  #Lastly, you fill in the third column using that arrows() function you created
  #before. The inputs of that are just the two case incidence values - one current
  #value and one from 14 days ago. 
  table_condensed[i,3] <- arrows(cases_thisweek, cases_lastweek)
  
  #hospitalizations
  #works similarly to the cases bit above, so I haven't ellaborated more here
  hosp <- data_subset %>% filter(!is.na(hosp_patients_per_million))
  hosp_thisweek <- ifelse(nrow(hosp) == 0, NA, hosp %>% filter(date == max(date, na.rm = TRUE)) %>% select(hosp_patients_per_million) %>% pull())
  hosp_lastweek <- ifelse(nrow(hosp) == 0, NA, hosp %>% filter(date == max(date, na.rm = TRUE)-current_time_span_in_days) %>% select(hosp_patients_per_million) %>% pull())
  table_condensed[i,4] <- date_paste(hosp_thisweek, max(hosp$date, na.rm = TRUE), table_final_ordered$`Latest Date for Patients in Hospital`)
  table_condensed[i,5] <- arrows(hosp_thisweek, hosp_lastweek)
  
  #deaths
  #works similarly to the cases bit above, so I haven't ellaborated more here
  deaths <- data_subset %>% filter(!is.na(new_deaths_smoothed_per_million))
  deaths_thisweek <- ifelse(nrow(deaths) == 0, NA, deaths %>% filter(date == max(date, na.rm = TRUE)) %>% select(new_deaths_smoothed_per_million) %>% pull())
  deaths_lastweek <- ifelse(nrow(deaths) == 0, NA, deaths %>% filter(date == max(date, na.rm = TRUE)-current_time_span_in_days) %>% select(new_deaths_smoothed_per_million) %>% pull())
  table_condensed[i,6] <- date_paste(deaths_thisweek, max(deaths$date, na.rm = TRUE), table_final_ordered$`Latest Date for Deaths`)
  table_condensed[i,7] <- arrows(deaths_thisweek, deaths_lastweek)
  
  #vaccine
  #works similarly to the cases bit above, so I haven't ellaborated more here
  #Because the trajectory for this indicator is always up, we don't have a column
  #where we call the "arrows" function
  vacc <- data_subset %>% filter(!is.na(people_fully_vaccinated_per_hundred))
  vacc_thisweek <- ifelse(nrow(vacc) == 0, NA, vacc %>% filter(date == max(date, na.rm = TRUE)) %>% select(people_fully_vaccinated_per_hundred) %>% pull())
  table_condensed[i,8] <- date_paste(round(vacc_thisweek,0), max(vacc$date, na.rm = TRUE), table_final_ordered$`Latest Date for Vaccine Coverage`, percent = TRUE)
  
  #boosters
  #works similarly to the cases bit above, so I haven't ellaborated more here
  #Because the trajectory for this indicator is always up, we don't have a column
  #where we call the "arrows" function
  boost <- data_subset %>% filter(!is.na(total_boosters_per_hundred))
  boost_thisweek <- ifelse(nrow(boost) == 0, NA, boost %>% filter(date == max(date, na.rm = TRUE)) %>% select(total_boosters_per_hundred) %>% pull())
  table_condensed[i,9] <- date_paste(round(boost_thisweek,0), max(boost$date, na.rm = TRUE), table_final_ordered$`Latest Date for Booster Doses`)
  
  #stringency
  #works similarly to the cases bit above, so I haven't ellaborated more here
  stringency <- data_subset %>% filter(!is.na(StringencyIndex))
  stringency_thisweek <- ifelse(nrow(stringency) == 0, NA, stringency %>% filter(date == max(date, na.rm = TRUE)) %>% select(StringencyIndex) %>% pull())
  stringency_lastweek <- ifelse(nrow(stringency) == 0, NA, stringency %>% filter(date == max(date, na.rm = TRUE)-current_time_span_in_days) %>% select(StringencyIndex) %>% pull())
  table_condensed[i,10] <- date_paste(round(stringency_thisweek,0), max(stringency$date, na.rm = TRUE), table_final_ordered$`Latest Date for Stringency`)
  table_condensed[i,11] <- arrows(stringency_thisweek, stringency_lastweek, stringency = TRUE)
  
  #percent of maximum value
  #This is something I recently added. I wasn't totally sure where to put them 
  #so these three columns appear at the end of Table 1 for now. They don't need
  #to be copy/pasted into the Intl Summary (yet). But I imagine they will be
  #somehow integrated at some point. 
  cases_percentofmax <- cases_thisweek/(data_subset %>% summarise(max(new_cases_smoothed_per_million, na.rm = TRUE)) %>% pull())
  hosp_percentofmax <- hosp_thisweek/(data_subset %>% summarise(max(hosp_patients_per_million, na.rm = TRUE)) %>% pull())
  death_percentofmax <- deaths_thisweek/(data_subset %>% summarise(max(new_deaths_smoothed_per_million, na.rm = TRUE)) %>% pull())
  #These three values get added to columns 12, 13, and 14
  table_condensed[i,12] <- cases_percentofmax
  table_condensed[i,13] <- hosp_percentofmax
  table_condensed[i,14] <- death_percentofmax
  
  #the very last column is the continent - this just helps filter data
  table_condensed[i,15] <- unique(data_subset$continent)
}

#Now that the matrix is filled in, turn it into a data frame and add column headings
table_condensed_final <- as.data.frame(table_condensed)

#If you add or remove variables from Table 1, make sure to update their respective
#column headings. You'll notice something different about these column headings 
#when compared to the ones in the detailed table. These ones also have the max date
#displayed in them. 
colnames(table_condensed_final) <- c("Country",
                                     paste("Daily cases per 100,000, 7-day MA (as of", format(max(as.Date(table_final_ordered$`Latest Date for Cases`, format = "%b %d, %Y"), na.rm = TRUE), "%b %d"), "unless indicated)"),
                                     "Cases Trajectory",
                                     paste("Patients in hospital per 100,000* (as of", format(max(as.Date(table_final_ordered$`Latest Date for Patients in Hospital`, format = "%b %d, %Y"), na.rm = TRUE), "%b %d"), "unless indicated)"),
                                     "Patients in Hospital Trajectory",
                                     paste("Daily deaths per 100,000, 7-day MA (as of", format(max(as.Date(table_final_ordered$`Latest Date for Deaths`, format = "%b %d, %Y"), na.rm = TRUE), "%b %d"), "unless indicated)"),
                                     "Deaths Trajectory",
                                     paste("Percent (%) of entire population fully vaccinated (as of", format(max(as.Date(table_final_ordered$`Latest Date for Vaccine Coverage`, format = "%b %d, %Y"), na.rm = TRUE), "%b %d"), "unless indicated)"),
                                     paste("Booster doses administered per 100 (as of", format(max(as.Date(table_final_ordered$`Latest Date for Booster Doses`, format = "%b %d, %Y"), na.rm = TRUE), "%b %d"), "unless indicated)"),
                                     paste("Stringency Index (as of", format(max(as.Date(table_final_ordered$`Latest Date for Stringency`, format = "%b %d, %Y"), na.rm = TRUE), "%b %d"), "unless indicated)"),
                                     "Stringency Trajectory", 
                                     "Current Case Incidence as % of Maximum Value", 
                                     "Current Patients in Hospital as % of Maximum Value",
                                     "Current Death Rate as % of Maximum Value",
                                     "Continent")

#Change last three columns into numeric and round them to two decimal places. 
#I'm just making sure they're numeric so that I can perform some conditional 
#formatting with them later on in the code. This uses the round() function, which
#you can add the number of decimal points to 
table_condensed_final <- table_condensed_final %>%
  mutate(`Current Case Incidence as % of Maximum Value` = round(as.numeric(`Current Case Incidence as % of Maximum Value`), 2), 
         `Current Patients in Hospital as % of Maximum Value` = round(as.numeric(`Current Patients in Hospital as % of Maximum Value`), 2), 
         `Current Death Rate as % of Maximum Value` = round(as.numeric(`Current Death Rate as % of Maximum Value`), 2))

#3. LEADERBOARD-----------------------------------------------------------------
#now that we've made the two tables, we'll pull out the "top-10" countries for 
#cases, hosp, deaths, vaccines, and stringency

#cases 
#I'll walk through this one. We're using the data1 dataset (i.e. the dataset 
#from the beginning that hasn't been subsetted at all)
top10_cases <- data1 %>% 
  #We filter out the rows that have blank case values
  filter(!is.na(new_cases_smoothed_per_million)) %>%
  #Then we arrange the dataset by country and by date
  arrange(location, date) %>% 
  #Then we group by country 
  group_by(location) %>% 
  #Then we make sure to grab the latest and greatest observation for each country, 
  #noting that the maximum date won't be the same across all countries
  summarise_all(last) %>%
  #Arrange the dataset by order of descending case incidence 
  arrange(desc(new_cases_smoothed_per_million)) %>%
  #Grab the top 10 observations
  slice(1:10) %>%
  #Only pick out the columns we need
  select(location, new_cases_smoothed_per_million, date) %>%
  #Then we round the case incidence to 0 decimal places
  mutate(new_cases_smoothed_per_million = round(new_cases_smoothed_per_million, 0))
#Lastly, we change the column names of this table
colnames(top10_cases) <- c("Country","New Cases per 100,000 (7-day MA)","Date")

#hosp 
#Works the same way as cases above
top10_hosp <- data1 %>% 
  filter(!is.na(hosp_patients_per_million)) %>%
  arrange(location, date) %>% 
  group_by(location) %>% 
  summarise_all(last) %>%
  arrange(desc(hosp_patients_per_million)) %>%
  slice(1:10) %>%
  select(location, hosp_patients_per_million, date) %>%
  mutate(hosp_patients_per_million = round(hosp_patients_per_million, 1))
colnames(top10_hosp) <- c("Country","Patients in Hospital per 100,000 (7-day MA)","Date")

#deaths
#Works the same way as cases above
top10_deaths <- data1 %>% 
  filter(!is.na(new_deaths_smoothed_per_million)) %>%
  arrange(location, date) %>% 
  group_by(location) %>% 
  summarise_all(last) %>%
  arrange(desc(new_deaths_smoothed_per_million)) %>%
  slice(1:10) %>%
  select(location, new_deaths_smoothed_per_million, date) %>%
  mutate(new_deaths_smoothed_per_million = round(new_deaths_smoothed_per_million, 2))
colnames(top10_deaths) <- c("Country","New Deaths per 100,000 (7-day MA)","Date")

#vacc
#Works the same way as cases above
top10_vacc <- data1 %>% 
  filter(!is.na(people_fully_vaccinated_per_hundred)) %>%
  arrange(location, date) %>% 
  group_by(location) %>% 
  summarise_all(last) %>%
  arrange(desc(people_fully_vaccinated_per_hundred)) %>%
  slice(1:10) %>%
  select(location, people_fully_vaccinated_per_hundred, date) %>%
  mutate(people_fully_vaccinated_per_hundred = percent(people_fully_vaccinated_per_hundred/100, accuracy = 1))
colnames(top10_vacc) <- c("Country","Percent Fully Vaccinated","Date")

#booster
#Works the same way as cases above
top10_boost <- data1 %>% 
  filter(!is.na(total_boosters_per_hundred)) %>%
  arrange(location, date) %>% 
  group_by(location) %>% 
  summarise_all(last) %>%
  arrange(desc(total_boosters_per_hundred)) %>%
  slice(1:10) %>%
  select(location, total_boosters_per_hundred, date) %>%
  mutate(total_boosters_per_hundred = round(total_boosters_per_hundred, 0))
colnames(top10_boost) <- c("Country","Booster Doses per 100","Date")

#stringency
#Works the same way as cases above
top10_string <- data2 %>% 
  filter(!is.na(StringencyIndex)) %>%
  arrange(location, date) %>% 
  group_by(location) %>% 
  summarise_all(last) %>%
  arrange(desc(StringencyIndex)) %>%
  slice(1:10) %>%
  select(location, StringencyIndex, date) %>%
  mutate(StringencyIndex = round(StringencyIndex, 0))
colnames(top10_string) <- c("Country","Stringency Index","Date")


#for the cases, we'll also print a string for the key messages. This isn't something
#that's needed anymore but still prints. I won't explain it in greater detail 
#because we don't use it anymore. 
all <- c()
for (i in 1:nrow(top10_cases)) {
  if(top10_cases[i,3] %>% pull() < max(data1$date, na.rm = TRUE)){all[i] <- paste(top10_cases[i,1], paste("(",top10_cases[i,2],";",sep = ""), paste(format(top10_cases[i,3] %>% pull(), "%b %d"), ")", sep=""))}
  else{all[i] <- paste(top10_cases[i,1], paste("(",top10_cases[i,2],")",sep = ""))}
}
all <- append(all,"and",after = length(all)-1)
test <- all[1:(length(all)-2)]
all <- append(test, paste(all[(length(all)-1):length(all)], collapse = " "), after = length(test))
all_together <- paste(all[!is.na(all)], collapse = ", ")
cases_string <- paste("Top 10 countries based on COVID-19 incidence per 100,000 population (using a 7-day moving average of cases per 100,000) as of",paste(format(max(data1$date, na.rm = TRUE), "%B %d, %Y"), ":", sep=""), paste(all_together, ".", sep = ""))

#Finally, save an Excel file with 3 tabs. The first step in doing so is creating
#a blank workbook. I'm using the openxlsx package to do all of this. 
workbook <- createWorkbook()

#Now add three sheets - one for the condensed table, one for the detailed table, 
#and one for the leaderboard
addWorksheet(workbook, "Table 1")
addWorksheet(workbook, "Detailed Data")
addWorksheet(workbook, "Leaderboard")

#In the condensed table sheet, put in table_condensed_final
writeData(workbook, sheet = "Table 1", x = table_condensed_final)

#In the detailed table sheet, put in table_final_ordered
writeData(workbook, sheet = "Detailed Data", x = table_final_ordered)

#In the leaderboard sheet, put each of those small top-10 tables. Specify the
#rows and columns that the top-left corner of the tables should start at. If you
#were to add a top10 table, you would have to add a line of code below to specify
#what row and column that table should start at. 
writeData(workbook, sheet = "Leaderboard", x = cases_string, startRow = 1)
writeData(workbook, sheet = "Leaderboard", x = top10_cases, startRow = 3, startCol = 1)
writeData(workbook, sheet = "Leaderboard", x = top10_deaths, startRow = 3, startCol = 5)
writeData(workbook, sheet = "Leaderboard", x = top10_hosp, startRow = 3, startCol = 9)
writeData(workbook, sheet = "Leaderboard", x = top10_vacc, startRow = 3, startCol = 13)
writeData(workbook, sheet = "Leaderboard", x = top10_boost, startRow = 3, startCol = 17)
writeData(workbook, sheet = "Leaderboard", x = top10_string, startRow = 3, startCol = 21)

#Remember those last three columns in Table 1 with the % of maximum value? Well, 
#this is where the conditional formatting comes in. Basically, I would like to 
#add colour to them based on their value. The first step in doing so is defining
#my colour categories. These are arbitrary. I just assign a colour for each category
#using the createStyle() function. 
above_100 <- createStyle(bgFill = "#900C3F")
between_75_to_100 <- createStyle(bgFill = "#C70039")
between_50_to_75 <- createStyle(bgFill = "#FF5733")
between_25_to_50 <- createStyle(bgFill = "#FFC300")
below_25 <- createStyle(bgFill = "#DAF7A6")

#Now I go into that Table 1 tab, go into columns 12-14 since that's where these 
#three variables live, and I assign the style "below_25" if the values in those 
#columns are below 0.25.
conditionalFormatting(workbook, "Table 1",
                      cols = 12:14,
                      rows = 1:length(chosen_countries)+1, 
                      rule = "<0.25", style = below_25)

#Then, I do the same thing, but I look at values above or equal to 0.25 and I assign
#them the colour from "between_25_to_50"
conditionalFormatting(workbook, "Table 1",
                      cols = 12:14,
                      rows = 1:length(chosen_countries)+1, 
                      rule = ">=0.25", style = between_25_to_50)

#Same thing, but for above 0.5
conditionalFormatting(workbook, "Table 1",
                      cols = 12:14,
                      rows = 1:length(chosen_countries)+1, 
                      rule = ">=0.5", style = between_50_to_75)

#Same thing, but for above 0.75
conditionalFormatting(workbook, "Table 1",
                      cols = 12:14,
                      rows = 1:length(chosen_countries)+1, 
                      rule = ">=0.75", style = between_75_to_100)

#Same thing, but for above 1
conditionalFormatting(workbook, "Table 1",
                      cols = 12:14,
                      rows = 1:length(chosen_countries)+1, 
                      rule = ">=1", style = above_100)

#Now save the Excel file. It'll save into the same folder where this code is saved. 
#But if you want to update that, and say, save it in another folder, you could
#play with the file path below. Because we have "overwrite = TRUE", you don't have to 
#delete any of the old workbooks - it'll just make sure to replace any old workbook
#with a new one, should you run this code more than once on the same day. Make sure, 
#however, that you do not have the workbook open when it's being overwritten. 
saveWorkbook(workbook, paste0(here(),"/Output - Table 1/Table 1 International Summary_", Sys.Date(), ".xlsx"), overwrite = TRUE)

#EXTRA FILE FOR STRINGENCY------------------------------------------------------------------------------------
#In this bit, we're going to save an extra Excel file that only has information on stringency

#first, create empty excel workbook
oxford_wb_withnotes <- createWorkbook()

#then, create a sheet that'll hold each country's stringency data. Loop through each
#sheet and fill it in with that country's data.
for(i in 1:length(chosen_countries)){
  
  #subset for the country - choose the country in the "i"th position in our
  #chosen_countries vector. 
  Country_Stringency <- Stringency_Data %>% filter(CountryName == chosen_countries[i])
  
  #create worksheet, and name it with that country's name
  addWorksheet(oxford_wb_withnotes, chosen_countries[i])
  
  #add country's data to worksheet
  writeData(oxford_wb_withnotes, sheet = chosen_countries[i], Country_Stringency)
  
  #add freeze-panes so that it's easy to navigate through the country's data
  freezePane(oxford_wb_withnotes, chosen_countries[i], firstActiveRow = 2, firstActiveCol = 7)
}

#save excel workbook. Again you can play with the file path here. Suppose you had 
#another folder within the folder that this code was saved, and you wanted the
#International Stringency Data file to be saved there. All you'd have to do is
#add that folder's name in front of /International_Stringency_Data_. Something like
#"/Data Folder/International_Stringency_Data_"
saveWorkbook(oxford_wb_withnotes, paste0(here(), "/Output - Stringency Data/International_Stringency_Data_", Sys.Date(), ".xlsx"), overwrite = TRUE)


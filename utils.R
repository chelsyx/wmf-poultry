#Dependent libs
library(reshape2)
library(ggplot2)
library(toOrdinal)
library(magrittr)
library(polloi)
library(xts)
library(dplyr)
library(tidyr)

# Capitalize the first letter
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
      sep="", collapse=" ")
}

## Read in data
read_data <- function() {
  
  all_country_data <- polloi::read_dataset("portal/all_country_data.tsv", col_types = "Dcididid")
  first_visits_country <- polloi::read_dataset("portal/first_visits_country.tsv", col_types = "Dccid")
  last_action_country <- polloi::read_dataset("portal/last_action_country.tsv", col_types = "Dccid")
  most_common_country <- polloi::read_dataset("portal/most_common_country.tsv", col_types = "Dccid")
  
  all_country_data_prop <- all_country_data %>%
    group_by(date) %>%
    mutate(event_prop=round(events/sum(events),4)*100, visit_prop=round(n_visit/sum(n_visit),4)*100, session_prop=round(n_session/sum(n_session),4)*100) %>%
    select(date,country,event_prop,ctr,visit_prop,ctr_visit,session_prop,ctr_session)  
  colnames(all_country_data) <- c("Date", "Country", "No. Events", 
	  "Overall Clickthrough Rate", "No. Visit", "Clickthrough Rate Per Visit", 
	  "No. Session", "Clickthrough Rate Per Session")
  colnames(all_country_data_prop) <- c("Date", "Country", "No. Events", 
	  "Overall Clickthrough Rate", "No. Visit", "Clickthrough Rate Per Visit", 
	  "No. Session", "Clickthrough Rate Per Session")
  all_country_data <<- all_country_data %>% arrange(Date, Country)
  all_country_data_prop <<- all_country_data_prop %>% ungroup() %>% arrange(Date, Country)
  
  first_visits_country <- first_visits_country[!duplicated(first_visits_country[,1:3],fromLast=T),]
  colnames(first_visits_country) <- c("Date", "Action", "Country", "No. Session", "Proportion")
  first_visits_country$Proportion <- first_visits_country$Proportion*100
  first_visits_country_prop <- tidyr::spread(first_visits_country[,-4], key=Action, value=Proportion, fill=0)
  first_visits_country <- tidyr::spread(first_visits_country[,-5], key=Action, value=`No. Session`, fill=0)
  colnames(first_visits_country_prop) <- sapply(colnames(first_visits_country_prop), simpleCap)
  colnames(first_visits_country) <- sapply(colnames(first_visits_country), simpleCap)
  first_visits_country <<- first_visits_country %>% arrange(Date, Country)
  first_visits_country_prop <<- first_visits_country_prop %>% ungroup() %>% arrange(Date, Country)

  last_action_country <- last_action_country[!duplicated(last_action_country[,1:3],fromLast=T),]
  colnames(last_action_country) <- c("Date", "Action", "Country", "Events", "Proportion")
  last_action_country$Proportion <- last_action_country$Proportion*100
  last_action_country_prop <- tidyr::spread(last_action_country[,-4], key=Action, value=Proportion, fill=0)
  last_action_country <- tidyr::spread(last_action_country[,-5], key=Action, value=Events, fill=0)
  colnames(last_action_country_prop) <- sapply(colnames(last_action_country_prop), simpleCap)
  colnames(last_action_country) <- sapply(colnames(last_action_country), simpleCap)
  last_action_country <<- last_action_country %>% arrange(Date, Country)
  last_action_country_prop <<- last_action_country_prop %>% ungroup() %>% arrange(Date, Country)
 
  colnames(most_common_country) <- c("Date", "Action", "Country", "No. Visit", "Proportion")
  most_common_country$Proportion <- most_common_country$Proportion*100
  most_common_country_prop <- tidyr::spread(most_common_country[,-4], key=Action, value=Proportion, fill=0)
  most_common_country <- tidyr::spread(most_common_country[,-5], key=Action, value=`No. Visit`, fill=0)
  colnames(most_common_country_prop) <- sapply(colnames(most_common_country_prop), simpleCap)
  colnames(most_common_country) <- sapply(colnames(most_common_country), simpleCap)
  most_common_country <<- most_common_country %>% arrange(Date, Country)
  most_common_country_prop <<- most_common_country_prop %>% ungroup() %>% arrange(Date, Country)

}

fill_out <- function(x, start_date, end_date, fill = 0) {
  temp <- data.frame(date = seq(start_date, end_date, "day"))
  y <- dplyr::right_join(x, temp, by = "date")
  y[is.na(y)] <- fill
  return(y)
}
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
  data("countrycode_data", package="countrycode")
  countrycode_data$country.name[c(44,54,143)] <- c("Cape Verde", "Congo, The Democratic Republic of the", "Macedonia, Republic of" )
  countrycode_data$continent[countrycode_data$country.name %in% c("British Indian Ocean Territory","Christmas Island","Taiwan, Province of China")] <- "Asia"
  
  
  all_country_data <- all_country_data[!duplicated(all_country_data[,1:2],fromLast=T),]
  all_country_data_prop <- all_country_data %>%
    group_by(date) %>%
    mutate(event_prop=round(events/sum(events),4)*100, visit_prop=round(n_visit/sum(n_visit),4)*100, session_prop=round(n_session/sum(n_session),4)*100) %>%
    select(date,country,event_prop,ctr,visit_prop,ctr_visit,session_prop,ctr_session) %>% ungroup()  
  us_mask <- grepl("^U\\.S\\.", all_country_data$country)
  us_data <- all_country_data[us_mask,]
  all_country_data <- us_data %>%
    mutate(clicks = events*ctr, click_v=n_visit*ctr_visit, click_s=n_session*ctr_session) %>%
    group_by(date) %>%
    summarise(country="United States", events=sum(events), ctr=round(sum(clicks)/sum(events),4),
              n_visit=sum(n_visit), ctr_visit=round(sum(click_v)/sum(n_visit),4),
              n_session=sum(n_session), ctr_session=round(sum(click_s)/sum(n_session),4)) %>%
    rbind(all_country_data[!us_mask,]) %>%
    arrange(date, country)
  us_mask <- grepl("^U\\.S\\.", all_country_data_prop$country)
  us_data_prop <- all_country_data_prop[us_mask,]
  all_country_data_prop <- us_data_prop %>%
    group_by(date) %>%
    summarise(country="United States", event_prop=sum(event_prop), 
              visit_prop=sum(visit_prop), session_prop=sum(session_prop)) %>%
    left_join(all_country_data[, c("date","country","ctr","ctr_visit","ctr_session")], by=c("date","country")) %>%
    rbind(all_country_data_prop[!us_mask,]) %>%
    select(date,country,event_prop,ctr,visit_prop,ctr_visit,session_prop,ctr_session) %>% 
    arrange(date, country)
  colnames(all_country_data) <- c("Date", "Country", "No. Events", 
	  "Overall Clickthrough Rate", "No. Visit", "Clickthrough Rate Per Visit", 
	  "No. Session", "Clickthrough Rate Per Session")
  colnames(all_country_data_prop) <- c("Date", "Country", "No. Events", 
	  "Overall Clickthrough Rate", "No. Visit", "Clickthrough Rate Per Visit", 
	  "No. Session", "Clickthrough Rate Per Session")
  colnames(us_data) <- c("Date", "Country", "No. Events", 
	  "Overall Clickthrough Rate", "No. Visit", "Clickthrough Rate Per Visit", 
	  "No. Session", "Clickthrough Rate Per Session")
  colnames(us_data_prop) <- c("Date", "Country", "No. Events", 
	  "Overall Clickthrough Rate", "No. Visit", "Clickthrough Rate Per Visit", 
	  "No. Session", "Clickthrough Rate Per Session")
  region_mask <- match(stringi::stri_trans_general(all_country_data$Country, "Latin-ASCII"), countrycode_data$country.name)
  all_country_data$Region <- countrycode_data$continent[region_mask]
  all_country_data$Region[is.na(all_country_data$Region)] <- "Other"
  region_mask <- match(stringi::stri_trans_general(all_country_data_prop$Country, "Latin-ASCII"), countrycode_data$country.name)
  all_country_data_prop$Region <- countrycode_data$continent[region_mask]
  all_country_data_prop$Region[is.na(all_country_data_prop$Region)] <- "Other"  
  all_country_data <<- all_country_data[, c(1:2, 9, 3:8)] %>% arrange(Date, Country)
  all_country_data_prop <<- all_country_data_prop[, c(1:2, 9, 3:8)] %>% arrange(Date, Country)
  us_data <<- us_data %>% mutate(Region="Americas") %>% select(c(1:2, 9, 3:8)) %>% arrange(Date, Country)
  us_data_prop <<- us_data_prop %>% mutate(Region="Americas") %>% select(c(1:2, 9, 3:8)) %>% arrange(Date, Country)

  
  first_visits_country <- first_visits_country[!duplicated(first_visits_country[,1:3],fromLast=T),]
  colnames(first_visits_country) <- c("Date", "Action", "Country", "No. Session", "Proportion")
  first_visits_country$Proportion <- first_visits_country$Proportion*100
  first_visits_country_prop <- tidyr::spread(first_visits_country[,-4], key=Action, value=Proportion, fill=0)
  first_visits_country <- tidyr::spread(first_visits_country[,-5], key=Action, value=`No. Session`, fill=0)
  colnames(first_visits_country_prop) <- sapply(colnames(first_visits_country_prop), simpleCap)
  colnames(first_visits_country) <- sapply(colnames(first_visits_country), simpleCap) 
  us_mask <- grepl("^U\\.S\\.", first_visits_country$Country)
  first_visits_us <- first_visits_country[us_mask, ]
  first_visits_country <- first_visits_us %>% select(-Country) %>% group_by(Date) %>%
    summarise_each(funs(sum)) %>% mutate(Country="United States") %>%
    rbind(first_visits_country[!us_mask,])
  us_mask <- grepl("^U\\.S\\.", first_visits_country_prop$Country)
  first_visits_us_prop <- first_visits_country_prop[us_mask, ]
  first_visits_country_prop <- first_visits_us_prop %>% select(-Country) %>% group_by(Date) %>%
    summarise_each(funs(sum)) %>% mutate(Country="United States") %>%
    rbind(first_visits_country_prop[!us_mask,])   
  region_mask <- match(stringi::stri_trans_general(first_visits_country$Country, "Latin-ASCII"), countrycode_data$country.name)
  first_visits_country$Region <- countrycode_data$continent[region_mask]
  first_visits_country$Region[is.na(first_visits_country$Region)] <- "Other"
  region_mask <- match(stringi::stri_trans_general(first_visits_country_prop$Country, "Latin-ASCII"), countrycode_data$country.name)
  first_visits_country_prop$Region <- countrycode_data$continent[region_mask]
  first_visits_country_prop$Region[is.na(first_visits_country_prop$Region)] <- "Other"  
  first_visits_country <<- first_visits_country[, c(1, 8:9, 2:7)] %>% arrange(Date, Country)
  first_visits_country_prop <<- first_visits_country_prop[, c(1, 8:9, 2:7)] %>% arrange(Date, Country)
  first_visits_us <<- first_visits_us %>% mutate(Region="Americas") %>% select(c(1:2, 9, 3:8)) %>% arrange(Date, Country)
  first_visits_us_prop <<- first_visits_us_prop %>% mutate(Region="Americas") %>% select(c(1:2, 9, 3:8)) %>% arrange(Date, Country)


  last_action_country <- last_action_country[!duplicated(last_action_country[,1:3],fromLast=T),]
  colnames(last_action_country) <- c("Date", "Action", "Country", "Events", "Proportion")
  last_action_country$Proportion <- last_action_country$Proportion*100
  last_action_country_prop <- tidyr::spread(last_action_country[,-4], key=Action, value=Proportion, fill=0)
  last_action_country <- tidyr::spread(last_action_country[,-5], key=Action, value=Events, fill=0)
  colnames(last_action_country_prop) <- sapply(colnames(last_action_country_prop), simpleCap)
  colnames(last_action_country) <- sapply(colnames(last_action_country), simpleCap)
  us_mask <- grepl("^U\\.S\\.", last_action_country$Country)
  last_action_us <- last_action_country[us_mask, ]
  last_action_country <- last_action_us %>% select(-Country) %>% group_by(Date) %>%
    summarise_each(funs(sum)) %>% mutate(Country="United States") %>%
    rbind(last_action_country[!us_mask,])
  us_mask <- grepl("^U\\.S\\.", last_action_country_prop$Country)
  last_action_us_prop <- last_action_country_prop[us_mask, ]
  last_action_country_prop <- last_action_us_prop %>% select(-Country) %>% group_by(Date) %>%
    summarise_each(funs(sum)) %>% mutate(Country="United States") %>%
    rbind(last_action_country_prop[!us_mask,])   
  region_mask <- match(stringi::stri_trans_general(last_action_country$Country, "Latin-ASCII"), countrycode_data$country.name)
  last_action_country$Region <- countrycode_data$continent[region_mask]
  last_action_country$Region[is.na(last_action_country$Region)] <- "Other"
  region_mask <- match(stringi::stri_trans_general(last_action_country_prop$Country, "Latin-ASCII"), countrycode_data$country.name)
  last_action_country_prop$Region <- countrycode_data$continent[region_mask]
  last_action_country_prop$Region[is.na(last_action_country_prop$Region)] <- "Other"    
  last_action_country <<- last_action_country[, c(1, 8:9, 2:7)] %>% arrange(Date, Country)
  last_action_country_prop <<- last_action_country_prop[, c(1, 8:9, 2:7)] %>% arrange(Date, Country)
  last_action_us <<- last_action_us %>% mutate(Region="Americas") %>% select(c(1:2, 9, 3:8)) %>% arrange(Date, Country)
  last_action_us_prop <<- last_action_us_prop %>% mutate(Region="Americas") %>% select(c(1:2, 9, 3:8)) %>% arrange(Date, Country)  
 
 
  most_common_country <- most_common_country[!duplicated(most_common_country[,1:3],fromLast=T),]
  colnames(most_common_country) <- c("Date", "Action", "Country", "No. Visit", "Proportion")
  most_common_country$Proportion <- most_common_country$Proportion*100
  most_common_country_prop <- tidyr::spread(most_common_country[,-4], key=Action, value=Proportion, fill=0)
  most_common_country <- tidyr::spread(most_common_country[,-5], key=Action, value=`No. Visit`, fill=0)
  colnames(most_common_country_prop) <- sapply(colnames(most_common_country_prop), simpleCap)
  colnames(most_common_country) <- sapply(colnames(most_common_country), simpleCap)
  us_mask <- grepl("^U\\.S\\.", most_common_country$Country)
  most_common_us <- most_common_country[us_mask, ]
  most_common_country <- most_common_us %>% select(-Country) %>% group_by(Date) %>%
    summarise_each(funs(sum)) %>% mutate(Country="United States") %>%
    rbind(most_common_country[!us_mask,])
  us_mask <- grepl("^U\\.S\\.", most_common_country_prop$Country)
  most_common_us_prop <- most_common_country_prop[us_mask, ]
  most_common_country_prop <- most_common_us_prop %>% select(-Country) %>% group_by(Date) %>%
    summarise_each(funs(sum)) %>% mutate(Country="United States") %>%
    rbind(most_common_country_prop[!us_mask,])     
  region_mask <- match(stringi::stri_trans_general(most_common_country$Country, "Latin-ASCII"), countrycode_data$country.name)
  most_common_country$Region <- countrycode_data$continent[region_mask]
  most_common_country$Region[is.na(most_common_country$Region)] <- "Other"
  region_mask <- match(stringi::stri_trans_general(most_common_country_prop$Country, "Latin-ASCII"), countrycode_data$country.name)
  most_common_country_prop$Region <- countrycode_data$continent[region_mask]
  most_common_country_prop$Region[is.na(most_common_country_prop$Region)] <- "Other"    
  most_common_country <<- most_common_country[, c(1, 7:8, 2:6)] %>% arrange(Date, Country)
  most_common_country_prop <<- most_common_country_prop[, c(1, 7:8, 2:6)] %>% arrange(Date, Country)
  most_common_us <<- most_common_us %>% mutate(Region="Americas") %>% select(c(1:2, 8, 3:7)) %>% arrange(Date, Country)
  most_common_us_prop <<- most_common_us_prop %>% mutate(Region="Americas") %>% select(c(1:2, 8, 3:7)) %>% arrange(Date, Country) 
}

fill_out <- function(x, start_date, end_date, fill = 0) {
  temp <- data.frame(date = seq(start_date, end_date, "day"))
  y <- dplyr::right_join(x, temp, by = "date")
  y[is.na(y)] <- fill
  return(y)
}
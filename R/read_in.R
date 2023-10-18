# Load packages
library(dplyr)
library(tidyverse)
library(cowplot)
library(skimr)
library(ggsci)
library(janitor)
library(meta)
library(stringr)
library(tibble)
library(readr)
library(glue)
library(epitools)
library(scales)
library(writexl)
library(egg)

read_in <- function(database_results_name){

wd <- here::here()
data_path <- glue("/results/{database_results_name}/IP/")


#load general population results
Allpop_MC_Age <- read_csv(paste0(wd,data_path, "Allpop_MC_Age.csv"))
Allpop_MC_AllandSex <- read_csv(paste0(wd,data_path, "Allpop_MC_AllandSex.csv"))

#load other population results
Inf_Age <- read_csv(paste0(wd,data_path, "Inf_Age.csv"))
Inf_AllandSex <- read_csv(paste0(wd,data_path, "Inf_AllandSex.csv"))

if (database_results_name != "Pharmetrics") {

Testneg_Age <- read_csv(paste0(wd,data_path, "Testneg_Age.csv"))
Testneg_AllandSex <- read_csv(paste0(wd,data_path, "Testneg_AllandSex.csv"))

}

if (database_results_name != "AUSOM") {

Reinf_Age <- read_csv(paste0(wd,data_path, "Reinf_Age.csv"))
Reinf_AllandSex <- read_csv(paste0(wd,data_path, "Reinf_AllandSex.csv"))
}

# double check outcomes
unique(Allpop_MC_Age$outcome_cohort_name)
unique(Allpop_MC_AllandSex$outcome_cohort_name)

unique(Inf_Age$outcome_cohort_name)
unique(Inf_AllandSex$outcome_cohort_name)

if (database_results_name != "Pharmetrics") {
unique(Testneg_Age$outcome_cohort_name)
unique(Testneg_AllandSex$outcome_cohort_name)
}

if (database_results_name != "AUSOM") {

unique(Reinf_Age$outcome_cohort_name)
unique(Reinf_AllandSex$outcome_cohort_name)

}

#load outcome names
names_conditions <- read_csv("names_conditions.csv")

# double check age groups
unique(Allpop_MC_Age$denominator_age_group)
unique(Allpop_MC_AllandSex$denominator_age_group)

unique(Inf_Age$denominator_age_group)
unique(Inf_AllandSex$denominator_age_group) 

if (database_results_name != "Pharmetrics") {
unique(Testneg_Age$denominator_age_group)
unique(Testneg_AllandSex$denominator_age_group)
}

if (database_results_name != "AUSOM") {

unique(Reinf_Age$denominator_age_group)
unique(Reinf_AllandSex$denominator_age_group)
}

# double check sex
unique(Allpop_MC_Age$denominator_sex)
unique(Allpop_MC_AllandSex$denominator_sex)

unique(Inf_Age$denominator_sex)
unique(Inf_AllandSex$denominator_sex)

if (database_results_name != "Pharmetrics") {
unique(Testneg_Age$denominator_sex)
unique(Testneg_AllandSex$denominator_sex)
}

if (database_results_name != "AUSOM") {

unique(Reinf_Age$denominator_sex)
unique(Reinf_AllandSex$denominator_sex)
}


#double check analysis interval
unique(Allpop_MC_Age$analysis_interval)
unique(Allpop_MC_AllandSex$analysis_interval)

unique(Inf_Age$analysis_interval)
unique(Inf_AllandSex$analysis_interval)

if (database_results_name != "Pharmetrics") {
unique(Testneg_Age$analysis_interval)
unique(Testneg_AllandSex$analysis_interval)
}

if (database_results_name != "AUSOM") {

unique(Reinf_Age$analysis_interval)
unique(Reinf_AllandSex$analysis_interval)

}

# remove inf_ and testneg_ from the outcome_cohort_name

Inf_Age <- Inf_Age %>% 
  mutate(
    outcome_cohort_name = gsub("^.*?_","",outcome_cohort_name))

Inf_AllandSex <- Inf_AllandSex %>% 
  mutate(
    outcome_cohort_name = gsub("^.*?_","",outcome_cohort_name))

if (database_results_name != "Pharmetrics") {
Testneg_Age <- Testneg_Age %>% 
  mutate(
    outcome_cohort_name = gsub("^.*?_","",outcome_cohort_name))

Testneg_AllandSex <- Testneg_AllandSex %>% 
  mutate(
    outcome_cohort_name = gsub("^.*?_","",outcome_cohort_name))
}

if (database_results_name != "AUSOM") {

Reinf_Age <- Reinf_Age %>% 
  mutate(
    outcome_cohort_name = gsub("^.*?_","",outcome_cohort_name))

Reinf_AllandSex <- Reinf_AllandSex %>% 
  mutate(
    outcome_cohort_name = gsub("^.*?_","",outcome_cohort_name))
}

#put them together
incidence_estimates_general_help <- rbind(Allpop_MC_Age,Allpop_MC_AllandSex)

if (database_results_name == "Pharmetrics") {

incidence_estimates_help <- rbind(Inf_Age,Inf_AllandSex,
                                  Reinf_Age,Reinf_AllandSex)

} else if (database_results_name == "AUSOM") {
  incidence_estimates_help <- rbind(Inf_Age,Inf_AllandSex,
                                    Testneg_Age,Testneg_AllandSex)
} else {
  incidence_estimates_help <- rbind(Inf_Age,Inf_AllandSex,
                                    Testneg_Age,Testneg_AllandSex,
                                    Reinf_Age,Reinf_AllandSex)
}


# Tidy dataframes ------------------------------------------------------

# general population

incidence_estimates_general_help <- 
  incidence_estimates_general_help %>%  
  select(analysis_interval, outcome_cohort_name, n_persons, person_years, n_events, 
         incidence_100000_pys, incidence_100000_pys_95CI_lower, incidence_100000_pys_95CI_upper,
         denominator_age_group, denominator_sex, incidence_start_date, incidence_end_date) %>%
  filter(outcome_cohort_name %in% names_conditions$cohort_name,
         !is.na(n_events)
  ) %>% 
  mutate(year_index = year(incidence_start_date),
         month_index = month(incidence_start_date),
         year_month = paste(year_index, sprintf("%02d", month_index), sep = "-"),
         database_name = database_results_name)

# incidence_estimates_general_help_quarters <- 
#   incidence_estimates_general_help %>%  
#   filter(analysis_interval == "months") %>%
#   mutate(quarter = ifelse(year_month %in% c("2020-12","2021-01","2021-02","2021-03"),"2021.1",
#                           ifelse(year_month %in% c("2021-04","2021-05","2021-06"),"2021.2",
#                                  ifelse(year_month %in% c("2021-07","2021-08","2021-09"),"2021.3",
#                                         ifelse(year_month %in% c("2021-10","2021-11","2021-12"),"2021.4",
#                                                ifelse(year_month %in% c("2022-01","2022-02","2022-03"),"2022.1",
#                                                       ifelse(year_month %in% c("2022-04","2022-05","2022-06"),"2022.2",
#                                                              ifelse(year_month %in% c("2022-07","2022-08","2022-09"),"2022.3",
#                                                                     ifelse(year_month %in% c("2022-10","2022-11","2022-12"),"2022.4",
#                                                                            ifelse(year_month %in% c("2023-01","2023-02","2023-03"),"2023.1",
#                                                                                   ifelse(year_month %in% c("2023-04","2023-05","2023-06"),"2023.2",
#                                                                                          ifelse(year_month %in% c("2023-07","2023-08","2023-09"),"2023.3",NA)))))))))))
#   ) %>%
#   filter(!is.na(quarter))

  

# infection and test negative and reinfection cohorts

incidence_estimates_help <- 
  incidence_estimates_help %>%  
  select(denominator_strata_cohort_name,analysis_interval, outcome_cohort_name, n_persons, person_years, n_events, 
         incidence_100000_pys, incidence_100000_pys_95CI_lower, incidence_100000_pys_95CI_upper,
         denominator_age_group, denominator_sex, incidence_start_date, incidence_end_date) %>%
  filter(outcome_cohort_name %in% names_conditions$cohort_name,
         !is.na(n_events)
  ) %>% 
  mutate(year_index = year(incidence_start_date),
         month_index = month(incidence_start_date),
         year_month = paste(year_index, sprintf("%02d", month_index), sep = "-"),
         database_name = database_results_name)


# incidence_estimates_help_quarters <- 
#   incidence_estimates_help %>%  
#   filter(analysis_interval == "months") %>%
#   mutate(quarter = ifelse(year_month %in% c("2020-12","2021-01","2021-02","2021-03"),"2021.1",
#                           ifelse(year_month %in% c("2021-04","2021-05","2021-06"),"2021.2",
#                                  ifelse(year_month %in% c("2021-07","2021-08","2021-09"),"2021.3",
#                                         ifelse(year_month %in% c("2021-10","2021-11","2021-12"),"2021.4",
#                                                ifelse(year_month %in% c("2022-01","2022-02","2022-03"),"2022.1",
#                                                       ifelse(year_month %in% c("2022-04","2022-05","2022-06"),"2022.2",
#                                                              ifelse(year_month %in% c("2022-07","2022-08","2022-09"),"2022.3",
#                                                                     ifelse(year_month %in% c("2022-10","2022-11","2022-12"),"2022.4",
#                                                                            ifelse(year_month %in% c("2023-01","2023-02","2023-03"),"2023.1",
#                                                                                   ifelse(year_month %in% c("2023-04","2023-05","2023-06"),"2023.2",
#                                                                                          ifelse(year_month %in% c("2023-07","2023-08","2023-09"),"2023.3",NA)))))))))))
#   ) %>%
#   filter(!is.na(quarter))


return(list(incidence_estimates_general_help,
            incidence_estimates_help
            ))

}



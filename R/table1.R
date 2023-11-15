library(glue)
library(dplyr)
library(readr)
library(dbplyr)
library(tidyverse)


table_one_put <- function(database_results_name){
#read in tableones and glue them together

wd <- here::here()
data_path <- glue("/results/{database_results_name}/")


# base_id = 1 ("infection"), 2 ("reinfection") and 3 ("test_negative")

bases <- read_csv(paste0(wd,data_path, "tableOne_bases.csv")) %>%
  filter ( strata_level != "Overall")


allpop <- read_csv(paste0(wd,data_path, "tableOne_allpop.csv"))

if (database_results_name != "Pharmetrics") {

table_one_help <- rbind(bases,allpop ) %>%
  select("strata_level", "variable", "estimate_type", "estimate","variable_level") %>%
  filter(variable_level == "Female" | is.na(variable_level),
         variable != "cohort_start_date",
         variable != "cohort_end_date") %>%
  mutate_at(vars(c(estimate)),as.numeric) %>%
  mutate(type = ifelse(estimate_type == "%","percentage",estimate_type),
         estimate = ifelse(variable == "prior_history",round(estimate/365,0),estimate)) %>%
  select(-"estimate_type") %>%
  pivot_wider(names_from = type,
              values_from = estimate) %>%
  mutate(
    value = ifelse(is.na(count),glue("{.data$median} ({.data$q25}"," - ","{.data$q75})"),
                        ifelse(is.na(percentage),count,glue("{.data$count}"," ({round(.data$percentage,0)}%)")))) %>%
  select("strata_level","variable","value") %>%
  pivot_wider(names_from = strata_level,
              values_from = value) %>% 
  select("variable","Overall","1","2","3") %>%
  rename("Entire database" = "Overall",
         "Infection" = "1",
         "Reinfection" = "2",
         "Test negative" = "3",
         "Variable" = "variable") %>%
  mutate(Variable = ifelse(Variable == "number subjects","# patients",
                           ifelse(Variable == "number records","# records",
                                  ifelse(Variable == "age","median age (IQR) [years]",
                                         ifelse(Variable == "prior_history","prior observation time (IQR) [years]",
                                                ifelse(Variable == "future_observation","follow-up time (IQR) [days]",
                                                       ifelse(Variable == "sex","Female, n (%)",Variable))))))) %>%
  add_row("Variable"    = glue("{database_results_name}"),
          "Entire database" = NA ,
          "Infection" = NA,
          "Reinfection" = NA,
          "Test negative" = NA) %>% 
  slice(7,2,1,6,3,5,4)


return(table_one_help)
}

else {
  
  table_one_help <- rbind(bases,allpop ) %>%
    select("strata_level", "variable", "estimate_type", "estimate","variable_level") %>%
    filter(variable_level == "Female" | is.na(variable_level),
           variable != "cohort_start_date",
           variable != "cohort_end_date") %>%
    mutate_at(vars(c(estimate)),as.numeric) %>%
    mutate(type = ifelse(estimate_type == "%","percentage",estimate_type),
           estimate = ifelse(variable == "prior_history",round(estimate/365,0),estimate)) %>%
    select(-"estimate_type") %>%
    pivot_wider(names_from = type,
                values_from = estimate) %>%
    mutate(
      value = ifelse(is.na(count),glue("{.data$median} ({.data$q25}"," - ","{.data$q75})"),
                     ifelse(is.na(percentage),count,glue("{.data$count}"," ({round(.data$percentage,0)}%)")))) %>%
    select("strata_level","variable","value") %>%
    pivot_wider(names_from = strata_level,
                values_from = value) %>% 
    select("variable","Overall","1","2") %>%
    rename("Entire database" = "Overall",
           "Infection" = "1",
           "Reinfection" = "2",
           "Variable" = "variable") %>%
    mutate(Variable = ifelse(Variable == "number subjects","# patients",
                             ifelse(Variable == "number records","# records",
                                    ifelse(Variable == "age","median age (IQR) [years]",
                                           ifelse(Variable == "prior_history","prior observation time (IQR) [years]",
                                                  ifelse(Variable == "future_observation","follow-up time (IQR) [days]",
                                                         ifelse(Variable == "sex","Female, n (%)",Variable))))))) %>%
    add_row("Variable"    = glue("{database_results_name}"),
            "Entire database" = NA ,
            "Infection" = NA,
            "Reinfection" = NA) %>% 
    slice(7,2,1,6,3,5,4) %>%
    add_column("Test negative" = NA)
  
  return(table_one_help)
}
}



all_table_one <- rbind(table_one_put("CPRDGold"),
                       table_one_put("IPCI"),
                       table_one_put("eDOL_CHUM"),
                       table_one_put("IMASIS"),
                       table_one_put("AUSOM"),
                       table_one_put("Pharmetrics"),
                       table_one_put("CORIVA")
                       )

write.csv(all_table_one,here::here("plots_and_tables", "table1.csv"))
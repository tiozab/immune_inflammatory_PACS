# Specify the directory where you want to save the CSV files
output_directory <- here::here("plots_and_tables")


#pharmetrics and UiO does not have test negative information (take it out)
# specific populations
incidence_estimates_help <- rbind(CPRDGOLD[[2]],
                                  IMASIS[[2]],
                                  AUSOM[[2]],
                                  IPCI[[2]],
                                  eDOL_CHUM[[2]],
                                  CORIVA[[2]],
                                  CPRDAurum[[2]])


### FIGURE 1

counts_inf_testneg <- num_denom("infection","test_negative") %>% 
  filter(analysis_interval =="overall")  %>% 
  select("person_years_infection", "n_events_infection", "person_years_test_negative", "n_events_test_negative",
         "database_name","denominator_age_group","denominator_sex","outcome_cohort_name") %>%
  mutate(outcome = ifelse(outcome_cohort_name == "pots","POTS diagnosis", 
                          ifelse(outcome_cohort_name == "dysautonomia","POTS symptoms", 
                                 ifelse(outcome_cohort_name == "me_cfs","ME/CFS diagnosis", 
                                        ifelse(outcome_cohort_name == "me_cfs_symptoms","ME/CFS symptoms",
                                               ifelse(outcome_cohort_name == "ra","RA",
                                                      ifelse(outcome_cohort_name == "juvenile_arthritis","Juvenile arthritis",
                                                             ifelse(outcome_cohort_name == "sle","SLE",
                                                                    ifelse(outcome_cohort_name == "ibd","IBD",
                                                                           ifelse(outcome_cohort_name == "mis","MIS",
                                                                                  ifelse(outcome_cohort_name == "t1dm","T2DM",NA))))))))))
         ) %>%
  rename(py_exposed = person_years_infection,
  event_exposed = n_events_infection,
  py_unexposed = person_years_test_negative,
  event_unexposed = n_events_test_negative,
  database = database_name,
  age = denominator_age_group,
  sex = denominator_sex) %>%
  select ( - "outcome_cohort_name") %>%
  group_by(database) %>% 
  arrange(outcome,age,sex) 

write.csv(counts_inf_testneg,"fig1_meta_counts_all.csv", row.names = FALSE)

counts_inf_testneg <- counts_inf_testneg %>%
  group_split()
  
## write every list into a csv

# Loop through each data frame in the list
for (i in seq_along(counts_inf_testneg)) {
  # Create a unique filename for each data frame
  filename <- paste0(output_directory, "/fig1_meta_counts_", i, ".csv")
  
  # Write the data frame to a CSV file
  write.csv(counts_inf_testneg[[i]], file = filename, row.names = FALSE)
  
}


# general population
incidence_estimates_general_help <- rbind(CPRDGOLD[[1]],
                                          Pharmetrics[[1]],
                                          IMASIS[[1]],
                                          AUSOM[[1]],
                                          IPCI[[1]],
                                          eDOL_CHUM[[1]],
                                          CORIVA[[1]],
                                          CPRDAurum[[1]],
                                          University_of_Oslo[[1]])


### FIGURE 2 (stratified)

counts_health_care_pop <-  incidence_estimates_general_help %>% 
  filter( analysis_interval == "overall",
          denominator_age_group == "0 to 150",
          denominator_sex =="Both")  %>% 
  select("database_name", "outcome_cohort_name", "n_events","person_years"
  ) %>%
  mutate(care_sector = ifelse(database_name %in% c("CPRDGOLD","IPCI","Pharmetrics","CORIVA","University_of_Oslo","CPRDAurum"),"primary care","secondary care"),
         outcome = ifelse(outcome_cohort_name == "pots","POTS diagnosis", 
                          ifelse(outcome_cohort_name == "dysautonomia","POTS symptoms", 
                                 ifelse(outcome_cohort_name == "me_cfs","ME/CFS diagnosis", 
                                        ifelse(outcome_cohort_name == "me_cfs_symptoms","ME/CFS symptoms",
                                               ifelse(outcome_cohort_name == "ra","RA",
                                                      ifelse(outcome_cohort_name == "juvenile_arthritis","Juvenile arthritis",
                                                             ifelse(outcome_cohort_name == "sle","SLE",
                                                                    ifelse(outcome_cohort_name == "ibd","IBD",
                                                                           ifelse(outcome_cohort_name == "mis","MIS",
                                                                                  ifelse(outcome_cohort_name == "t1dm","T2DM",NA))))))))))
  ) %>%
  rename(database = database_name) %>%
  select ( - "outcome_cohort_name") %>%
  group_by(care_sector) %>% 
    group_split()



## write every list into a csv

# Loop through each data frame in the list
for (i in seq_along(counts_health_care_pop)) {
  # Create a unique filename for each data frame
  filename <- paste0(output_directory, "/fig2_health_care_counts_", i, ".csv")
  
  # Write the data frame to a CSV file
  write.csv(counts_health_care_pop[[i]], file = filename, row.names = FALSE)
  
}

### FIGURE 3 time trends

counts_pop_trend <-  incidence_estimates_general_help %>% 
  filter( analysis_interval == "years",
          denominator_age_group == "0 to 150",
          denominator_sex =="Both")  %>% 
  select("database_name", "outcome_cohort_name", "n_events","person_years","year_index"
  )  %>%
  mutate(care_sector = ifelse(database_name %in% c("CPRDGOLD","IPCI","Pharmetrics","CORIVA","University_of_Oslo","CPRDAurum"),"primary care","secondary care"),
         outcome = ifelse(outcome_cohort_name == "pots","POTS diagnosis", 
                          ifelse(outcome_cohort_name == "dysautonomia","POTS symptoms", 
                                 ifelse(outcome_cohort_name == "me_cfs","ME/CFS diagnosis", 
                                        ifelse(outcome_cohort_name == "me_cfs_symptoms","ME/CFS symptoms",
                                               ifelse(outcome_cohort_name == "ra","RA",
                                                      ifelse(outcome_cohort_name == "juvenile_arthritis","Juvenile arthritis",
                                                             ifelse(outcome_cohort_name == "sle","SLE",
                                                                    ifelse(outcome_cohort_name == "ibd","IBD",
                                                                           ifelse(outcome_cohort_name == "mis","MIS",
                                                                                  ifelse(outcome_cohort_name == "t1dm","T2DM",NA))))))))))
  ) %>%
  rename(database = database_name) %>%
  select ( - "outcome_cohort_name") %>%
  group_by(care_sector) %>% 
  group_split()




## write every list into a csv

# Loop through each data frame in the list
for (i in seq_along(counts_pop_trend)) {
  # Create a unique filename for each data frame
  filename <- paste0(output_directory, "/fig3_time_trends_counts_", i, ".csv")
  
  # Write the data frame to a CSV file
  write.csv(counts_pop_trend[[i]], file = filename, row.names = FALSE)
  
}



### FIGURE 4 

counts_pop_age <-  incidence_estimates_general_help %>% 
  filter( analysis_interval == "overall",
          denominator_sex =="Both")  %>% 
  select("database_name", "denominator_age_group","n_events","person_years","outcome_cohort_name"
  )  %>%
  mutate(care_sector = ifelse(database_name %in% c("CPRDGOLD","IPCI","Pharmetrics","CORIVA","University_of_Oslo","CPRDAurum"),"primary care","secondary care"),
         outcome = ifelse(outcome_cohort_name == "pots","POTS diagnosis", 
                          ifelse(outcome_cohort_name == "dysautonomia","POTS symptoms", 
                                 ifelse(outcome_cohort_name == "me_cfs","ME/CFS diagnosis", 
                                        ifelse(outcome_cohort_name == "me_cfs_symptoms","ME/CFS symptoms",
                                               ifelse(outcome_cohort_name == "ra","RA",
                                                      ifelse(outcome_cohort_name == "juvenile_arthritis","Juvenile arthritis",
                                                             ifelse(outcome_cohort_name == "sle","SLE",
                                                                    ifelse(outcome_cohort_name == "ibd","IBD",
                                                                           ifelse(outcome_cohort_name == "mis","MIS",
                                                                                  ifelse(outcome_cohort_name == "t1dm","T2DM",NA))))))))))
  ) %>%
  rename(database = database_name,
         age = denominator_age_group) %>%
  select ( - "outcome_cohort_name") %>%
  group_by(care_sector) %>% 
  group_split()



## write every list into a csv

# Loop through each data frame in the list
for (i in seq_along(counts_pop_age)) {
  # Create a unique filename for each data frame
  filename <- paste0(output_directory, "/fig4_age_counts_", i, ".csv")
  
  # Write the data frame to a CSV file
  write.csv(counts_pop_age[[i]], file = filename, row.names = FALSE)
}


### FIGURE 5

counts_pop_sex <-  incidence_estimates_general_help %>% 
  filter( analysis_interval == "overall",
          denominator_age_group == "0 to 150")  %>% 
  select("database_name","denominator_sex","n_events","person_years","outcome_cohort_name"
  ) %>%
  mutate(outcome = ifelse(outcome_cohort_name == "pots","POTS diagnosis", 
                          ifelse(outcome_cohort_name == "dysautonomia","POTS symptoms", 
                                 ifelse(outcome_cohort_name == "me_cfs","ME/CFS diagnosis", 
                                        ifelse(outcome_cohort_name == "me_cfs_symptoms","ME/CFS symptoms",
                                               ifelse(outcome_cohort_name == "ra","RA",
                                                      ifelse(outcome_cohort_name == "juvenile_arthritis","Juvenile arthritis",
                                                             ifelse(outcome_cohort_name == "sle","SLE",
                                                                    ifelse(outcome_cohort_name == "ibd","IBD",
                                                                           ifelse(outcome_cohort_name == "mis","MIS",
                                                                                  ifelse(outcome_cohort_name == "t1dm","T2DM",NA))))))))))
  ) %>%
  rename(database = database_name,
         sex = denominator_sex ) %>%
  select ( - "outcome_cohort_name") 


  # Write the data frame to a CSV file
  write.csv(counts_pop_sex, "fig5_sex_counts.csv", row.names = FALSE)


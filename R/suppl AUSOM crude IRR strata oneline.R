## read in the results files

source(here::here("R","read_in.R"))
AUSOM <- read_in(database_results_name = "AUSOM")

#load outcome names
names_conditions <- read_csv(here::here("names_conditions.csv"))

  # general population
  incidence_estimates_general_help <- AUSOM[[1]]
  
  # specific populations
  incidence_estimates_help <- AUSOM[[2]]
  
  source(here::here("R","IRR function.R"))

  
  ## infection over test_negative ------------------------------------------------------
  
  overall_temp_inf_testneg <- 
    map_df( setdiff( names_conditions$cohort_name, c("t1dm","dysautonomia","ibd","juvenile_arthritis",
                                                     "me_cfs","me_cfs_symptoms","ra","sle","pots","mis")) %>% set_names,
            cohort_wrap_func,
            interval = "overall",
            age_expre =  "0 to 150",
            sex_expre = "Both",
            numerator = "infection",
            denominator = "test_negative",
            .id = "conditions")  %>% 
    mutate( conditions = factor(conditions,levels = c("t1dm","mis","ibd","sle","juvenile_arthritis","ra","me_cfs_symptoms",
                                                      "me_cfs","dysautonomia","pots"),
                                labels = c("T1DM","MIS","IBD","SLE","Juvenile arthritis","RA","ME/CFS symptoms",
                                           "ME/CFS","Dysautonomia","POTS"))) 
  
 ## no conditions left, do not use AUSOM
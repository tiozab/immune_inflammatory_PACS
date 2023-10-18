

#results -----------------------------------------------------------------------------
#### Overall results in general population

conditions_general <- 
  incidence_estimates_general_help %>% 
  filter( analysis_interval == "overall", 
          denominator_age_group %in% c("0 to 150"),
          denominator_sex =="Both") %>% 
  mutate( outcome_cohort_name = fct_reorder( outcome_cohort_name, incidence_100000_pys, max, .desc = TRUE),
          database_name = fct_reorder( database_name, incidence_100000_pys, mean)
  )

ggplot(conditions_general, aes( x = outcome_cohort_name, y = incidence_100000_pys, group = database_name)) + 
  geom_point( size = 0.5) +
  geom_errorbar(aes(ymin = incidence_100000_pys_95CI_lower, ymax =  incidence_100000_pys_95CI_upper), width=0.2) +
  labs( x = "", y = "Incidence / 100'000 person-years") +
  facet_wrap( vars( database_name), scales = "free_y", drop = TRUE)+
  theme_classic()+
  theme( text = element_text( family = "serif", color = "black"),
         axis.text.x = element_text( angle = 90, size = 10, vjust = 0.5, hjust = 1),
         axis.text.y = element_text( size = 7),
         panel.grid.major = element_blank())


## overall results in infected cohort
conditions_inf <- 
  incidence_estimates_help %>% 
  filter( analysis_interval == "overall", 
          denominator_age_group %in% c("0 to 150"),
          denominator_sex =="Both",
          denominator_strata_cohort_name == "infection") %>% 
  mutate( outcome_cohort_name = fct_reorder( outcome_cohort_name, incidence_100000_pys, max, .desc = TRUE),
          database_name = fct_reorder( database_name, incidence_100000_pys, mean)
  )

ggplot(conditions_inf, aes( x = outcome_cohort_name, y = incidence_100000_pys, group = database_name)) + 
  geom_point( size = 0.5) +
  geom_errorbar(aes(ymin = incidence_100000_pys_95CI_lower, ymax =  incidence_100000_pys_95CI_upper), width=0.2) +
  labs( x = "", y = "Incidence / 100'000 person-years") +
  facet_wrap( vars( database_name), scales = "free_y", drop = TRUE)+
  theme_classic()+
  theme( text = element_text( family = "serif", color = "black"),
         axis.text.x = element_text( angle = 90, size = 10, vjust = 0.5, hjust = 1),
         axis.text.y = element_text( size = 7),
         panel.grid.major = element_blank())



## overall results in re-infected cohort
conditions_reinf <- 
  incidence_estimates_help %>% 
  filter( analysis_interval == "overall", 
          denominator_age_group %in% c("0 to 150"),
          denominator_sex =="Both",
          denominator_strata_cohort_name == "reinfection") %>% 
  mutate( outcome_cohort_name = fct_reorder( outcome_cohort_name, incidence_100000_pys, max, .desc = TRUE),
          database_name = fct_reorder( database_name, incidence_100000_pys, mean)
  )

ggplot(conditions_reinf, aes( x = outcome_cohort_name, y = incidence_100000_pys, group = database_name)) + 
  geom_point( size = 0.5) +
  geom_errorbar(aes(ymin = incidence_100000_pys_95CI_lower, ymax =  incidence_100000_pys_95CI_upper), width=0.2) +
  labs( x = "", y = "Incidence / 100'000 person-years") +
  facet_wrap( vars( database_name), scales = "free_y", drop = TRUE)+
  theme_classic()+
  theme( text = element_text( family = "serif", color = "black"),
         axis.text.x = element_text( angle = 90, size = 10, vjust = 0.5, hjust = 1),
         axis.text.y = element_text( size = 7),
         panel.grid.major = element_blank())



## overall results in test_negative cohort
conditions_testneg <- 
  incidence_estimates_help %>% 
  filter( analysis_interval == "overall", 
          denominator_age_group %in% c("0 to 150"),
          denominator_sex =="Both",
          denominator_strata_cohort_name == "test_negative") %>% 
  mutate( outcome_cohort_name = fct_reorder( outcome_cohort_name, incidence_100000_pys, max, .desc = TRUE),
          database_name = fct_reorder( database_name, incidence_100000_pys, mean)
  )

ggplot(conditions_testneg, aes( x = outcome_cohort_name, y = incidence_100000_pys, group = database_name)) + 
  geom_point( size = 0.5) +
  geom_errorbar(aes(ymin = incidence_100000_pys_95CI_lower, ymax =  incidence_100000_pys_95CI_upper), width=0.2) +
  labs( x = "", y = "Incidence / 100'000 person-years") +
  facet_wrap( vars( database_name), scales = "free_y", drop = TRUE)+
  theme_classic()+
  theme( text = element_text( family = "serif", color = "black"),
         axis.text.x = element_text( angle = 90, size = 10, vjust = 0.5, hjust = 1),
         axis.text.y = element_text( size = 7),
         panel.grid.major = element_blank())




### TIME TRENDS ------------------------------------------------------------
# Trend of incidence by years in general population

conditions_year <- 
  incidence_estimates_general_help %>% 
  filter( analysis_interval == "years", 
          denominator_age_group %in% c("0 to 150"),
          denominator_sex =="Both") 

ggplot(conditions_year, aes( x = factor(year_index), y = incidence_100000_pys, group = database_name, color = database_name)) + 
  geom_point(size = 0.5) +
  geom_errorbar(aes(ymin = incidence_100000_pys_95CI_lower, ymax =  incidence_100000_pys_95CI_upper), width=0.2) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))+
  labs( x = "", y = "Incidence / 100'000 person-years") +
  ggsci::scale_color_lancet( alpha = 0.75)+
  facet_wrap( vars( outcome_cohort_name), scales = "free_y", drop = TRUE)+
  theme_classic()+
  theme( text = element_text( family = "serif", color = "black"),
         axis.text.x = element_text( angle = 90, size = 6, vjust = 0.5, hjust = 1),
         axis.text.y = element_text( size = 8),
         panel.grid.major = element_blank(),
         panel.grid.minor.x = element_blank(),
         legend.background = element_rect(fill='transparent'),
         legend.position = "top")


# Trend of incidence by months in general population

conditions_month  <- 
  incidence_estimates_general_help %>% 
  filter( analysis_interval == "months", 
          denominator_age_group %in% c("0 to 150"),
          denominator_sex =="Both") %>% 
  group_by( database_name, outcome_cohort_name) %>% 
  mutate( number_point = n()) %>% 
  ungroup() %>% 
  filter( number_point > 5) 

ggplot(conditions_month, aes( x = factor(year_month), y = incidence_100000_pys, group = database_name, color = database_name)) + 
  geom_point(size = 0.5) +
  geom_errorbar(aes(ymin = incidence_100000_pys_95CI_lower, ymax =  incidence_100000_pys_95CI_upper), width=0.2) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))+
  labs( x = "", y = "Incidence / 100'000 person-years") +
  ggsci::scale_color_lancet( alpha = 0.75)+
  facet_wrap( vars( outcome_cohort_name), scales = "free_y", drop = TRUE)+
  theme_classic()+
  theme( text = element_text( family = "serif", color = "black"),
         axis.text.x = element_text( angle = 90, size = 6, vjust = 0.5, hjust = 1),
         axis.text.y = element_text( size = 8),
         panel.grid.major = element_blank(),
         panel.grid.minor.x = element_blank(),
         legend.background = element_rect(fill='transparent'),
         legend.position = "top")


#### trends of incidence by year in infected cohort

conditions_in_infection_year <- 
  incidence_estimates_help %>% 
  filter( analysis_interval == "years", 
          denominator_age_group %in% c("0 to 150"),
          denominator_sex =="Both",
          denominator_strata_cohort_name == "infection",
          year_index != 2020) 

ggplot(conditions_in_infection_year, aes( x = factor(year_index), y = incidence_100000_pys, group = database_name, color = database_name)) + 
  geom_point(size = 0.5) +
  geom_errorbar(aes(ymin = incidence_100000_pys_95CI_lower, ymax =  incidence_100000_pys_95CI_upper), width=0.2) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))+
  labs( x = "", y = "Incidence / 100'000 person-years") +
  ggsci::scale_color_lancet( alpha = 0.75)+
  facet_wrap( vars( outcome_cohort_name), scales = "free_y", drop = TRUE)+
  theme_classic()+
  theme( text = element_text( family = "serif", color = "black"),
         axis.text.x = element_text( angle = 90, size = 6, vjust = 0.5, hjust = 1),
         axis.text.y = element_text( size = 8),
         panel.grid.major = element_blank(),
         panel.grid.minor.x = element_blank(),
         legend.background = element_rect(fill='transparent'),
         legend.position = "top")

# Trend of incidence by month in infected cohort 

conditions_in_infection_month <- 
  incidence_estimates_help %>% 
  filter( analysis_interval == "months", 
          denominator_age_group %in% c("0 to 150"),
          denominator_sex =="Both",
          denominator_strata_cohort_name == "infection") %>% 
  group_by( database_name, outcome_cohort_name) %>% 
  mutate( number_point = n()) %>% 
  ungroup() %>% 
  filter( number_point > 5) 

ggplot(conditions_in_infection_month, aes( x = factor(year_month), y = incidence_100000_pys, group = database_name, color = database_name)) + 
  geom_point(size = 0.5) +
  geom_errorbar(aes(ymin = incidence_100000_pys_95CI_lower, ymax =  incidence_100000_pys_95CI_upper), width=0.2) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))+
  labs( x = "", y = "Incidence / 100'000 person-years") +
  ggsci::scale_color_lancet( alpha = 0.75)+
  facet_wrap( vars( outcome_cohort_name), scales = "free_y", drop = TRUE)+
  theme_classic()+
  theme( text = element_text( family = "serif", color = "black"),
         axis.text.x = element_text( angle = 90, size = 6, vjust = 0.5, hjust = 1),
         axis.text.y = element_text( size = 8),
         panel.grid.major = element_blank(),
         panel.grid.minor.x = element_blank(),
         legend.background = element_rect(fill='transparent'),
         legend.position = "top")




# Stratification by age ---------------------------------------
# Incidence by age general population

conditions_age <- 
  incidence_estimates_general_help %>% 
  filter( analysis_interval == "overall", 
          denominator_sex == "Both",
          denominator_age_group != "0 to 150") %>% 
  group_by( database_name, outcome_cohort_name) %>% 
  mutate( number_point = n()) %>% 
  ungroup() %>% 
  filter( number_point > 3) %>% 
  mutate(denominator_age_group = factor(denominator_age_group, levels=c("0 to 6","7 to 11","12 to 18","19 to 40",
                                                                        "41 to 64","65 to 150"))
  )


ggplot(conditions_age, aes( x = denominator_age_group, y = incidence_100000_pys, group = database_name, color = database_name)) + 
  geom_smooth( method = "lm",
               formula = y ~ poly(x, 2),
               se = FALSE) +
  geom_point(size = 0.5) +
  geom_errorbar(aes(ymin = incidence_100000_pys_95CI_lower, ymax =  incidence_100000_pys_95CI_upper), width=0.2) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))+
  labs( x = "", y = "Incidence / 100'000 person-years") +
  ggsci::scale_color_lancet( alpha = 0.75)+
  facet_wrap( vars( outcome_cohort_name), scales = "free_y", drop = TRUE)+
  theme_classic()+
  theme( text = element_text( family = "serif", color = "black"),
         axis.text.x = element_text( angle = 90, size = 8, vjust = 0.5, hjust = 1),
         axis.text.y = element_text( size = 8),
         panel.grid.major = element_blank(),
         panel.grid.minor.x = element_blank(),
         legend.background = element_rect(fill='transparent'),
         legend.position = "top")


### by age in infected cohort

conditions_infectionage <- 
  incidence_estimates_help %>% 
  filter( analysis_interval == "overall", 
          denominator_strata_cohort_name == "infection",
          denominator_sex == "Both",
          denominator_age_group != "0 to 150") %>% 
  group_by( database_name, outcome_cohort_name) %>% 
  mutate( number_point = n()) %>% 
  ungroup() %>% 
  filter( number_point > 3) %>% 
  mutate(denominator_age_group = factor(denominator_age_group, levels=c("0 to 6","7 to 11","12 to 18","19 to 40",
                                                                        "41 to 64","65 to 150"))
  )


ggplot(conditions_infectionage, aes( x = denominator_age_group, y = incidence_100000_pys, group = database_name, color = database_name)) + 
  geom_smooth( method = "lm",
               formula = y ~ poly(x, 2),
               se = FALSE) +
  geom_point(size = 0.5) +
  geom_errorbar(aes(ymin = incidence_100000_pys_95CI_lower, ymax =  incidence_100000_pys_95CI_upper), width=0.2) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))+
  labs( x = "", y = "Incidence / 100'000 person-years") +
  ggsci::scale_color_lancet( alpha = 0.75)+
  facet_wrap( vars( outcome_cohort_name), scales = "free_y", drop = TRUE)+
  theme_classic()+
  theme( text = element_text( family = "serif", color = "black"),
         axis.text.x = element_text( angle = 90, size = 8, vjust = 0.5, hjust = 1),
         axis.text.y = element_text( size = 8),
         panel.grid.major = element_blank(),
         panel.grid.minor.x = element_blank(),
         legend.background = element_rect(fill='transparent'),
         legend.position = "top")



# Stratification by sex ---------------------------------------------------


# Incidence by sex in general population
conditions_sex <- 
  incidence_estimates_general_help %>% 
  filter( analysis_interval == "overall", 
          denominator_age_group == "0 to 150") 

ggplot(conditions_sex, aes( x = database_name, y = incidence_100000_pys, fill = denominator_sex)) + 
  geom_errorbar( aes( ymin = incidence_100000_pys, ymax = incidence_100000_pys_95CI_upper, color = denominator_sex), 
                 position = position_dodge(0.5), width = 0.25) +
  geom_col( position = position_dodge(0.5), width = 0.5) +
  scale_y_continuous( breaks = scales::breaks_extended(5)) +
  ggsci::scale_color_lancet( alpha = 0.75)+
  ggsci::scale_fill_lancet( alpha = 0.75)+
  labs( x = "", y = "Incidence / 100'000 person-years", color = "", fill = "") +
  facet_wrap( vars( outcome_cohort_name), scales = "free", drop = TRUE)+
  theme_classic() +
  theme( text = element_text( family = "serif", color = "black"),
         axis.text.x = element_text( angle = 0, size = 6),
         axis.text.y = element_text( size = 8),
         panel.grid.major = element_blank(),
         legend.background = element_rect(fill='transparent'),
         legend.position = "top")


# Incidence by sex in infected population
conditions_in_infection_sex <- 
  incidence_estimates_help %>% 
  filter( analysis_interval == "overall", 
          denominator_age_group == "0 to 150",
          denominator_strata_cohort_name == "infection")
 

ggplot(conditions_in_infection_sex, aes( x = database_name, y = incidence_100000_pys, fill = denominator_sex)) + 
  geom_errorbar( aes( ymin = incidence_100000_pys, ymax = incidence_100000_pys_95CI_upper, color = denominator_sex), 
                 position = position_dodge(0.5), width = 0.25) +
  geom_col( position = position_dodge(0.5), width = 0.5) +
  scale_y_continuous( breaks = scales::breaks_extended(5)) +
  ggsci::scale_color_lancet( alpha = 0.75)+
  ggsci::scale_fill_lancet( alpha = 0.75)+
  labs( x = "", y = "Incidence / 100'000 person-years", color = "", fill = "") +
  facet_wrap( vars( outcome_cohort_name), scales = "free", drop = TRUE)+
  theme_classic() +
  theme( text = element_text( family = "serif", color = "black"),
         axis.text.x = element_text( angle = 0, size = 6),
         axis.text.y = element_text( size = 8),
         panel.grid.major = element_blank(),
         legend.background = element_rect(fill='transparent'),
         legend.position = "top")





# IRR ---------------------------------------------------------------------
# Transform from Long to wide format


num_denom <- function(numerator,denominator) {

assign(glue("incidence_estimates_wide_{numerator}_{denominator}"),  
  incidence_estimates_help %>% 
  filter( 
    denominator_strata_cohort_name %in% c(numerator, denominator)) %>% 
  pivot_wider( id_cols = c(database_name, analysis_interval, year_index, 
                           denominator_age_group, denominator_sex, 
                           outcome_cohort_name, incidence_start_date
  ), 
  names_from = denominator_strata_cohort_name,
  values_from = c( n_persons, person_years, n_events, incidence_100000_pys)) %>% 
  janitor::clean_names() %>% 
  ungroup()
)
  
}


cohort_wrap_func <- function(conditions,
                             interval = "overall", 
                             age_expre = "0 to 150", 
                             sex_expre = "Both",
                             numerator = numerator,
                             denominator = denominator){
  
  
  IRR_df <- 
    num_denom(numerator,denominator) %>% 
    filter( analysis_interval %in% interval, 
            denominator_age_group %in% age_expre,
            denominator_sex %in% sex_expre
    ) %>% 
    filter( !is.na(incidence_100000_pys_infection), !is.na(incidence_100000_pys_test_negative)) %>% 
    filter( outcome_cohort_name == conditions)
  
  
  
  
  meta_unit_func <- function( input_df = IRR_df){
    
    output_meta <- meta::metainc( event.e = n_events_infection,
                                  time.e = person_years_infection,
                                  event.c = n_events_test_negative,
                                  time.c = person_years_test_negative, 
                                  sm = "IRR",
                                  data = input_df)
    
    output_meta <- 
      tibble( IRR_random = exp(output_meta$TE.random),
              IRR_low_random = exp(output_meta$lower.random),
              IRR_upper_random = exp(output_meta$upper.random),
              IRR_fix = exp(output_meta$TE.fix),
              IRR_low_fix = exp(output_meta$lower.fix),
              IRR_upper_fix = exp(output_meta$upper.fix))
    
    return( output_meta)
    
  }  
  
  output <- meta_unit_func( input_df = IRR_df)
  
  return(output)
  
  
}



overall_temp_inf_testneg <- 
  map_df( setdiff( names_conditions$cohort_name, c("misc","sle","juvenile_arthritis")) %>% set_names,
          cohort_wrap_func,
          interval = "overall",
          age_expre =  "0 to 150",
          sex_expre = "Both",
          numerator = "infection",
          denominator = "test_negative",
          .id = "conditions") %>% 
  mutate( conditions = fct_reorder(conditions, IRR_random, .desc = FALSE))  

female_temp_inf_testneg <- 
  map_df( setdiff( names_conditions$cohort_name, c("juvenile_arthritis","sle","misc")) %>% set_names,
          cohort_wrap_func,
          interval = "overall",
          age_expre =  "0 to 150",
          sex_expre = "Female",
          numerator = "infection",
          denominator = "test_negative",
          .id = "conditions") %>% 
  mutate( conditions = fct_reorder(conditions, IRR_random, .desc = FALSE))  


male_temp_inf_testneg <- 
  map_df( setdiff( names_conditions$cohort_name, c("juvenile_arthritis","sle","misc")) %>% set_names,
          cohort_wrap_func,
          interval = "overall",
          age_expre =  "0 to 150",
          sex_expre = "Male",
          numerator = "infection",
          denominator = "test_negative",
          .id = "conditions") %>% 
  mutate( conditions = fct_reorder(conditions, IRR_random, .desc = FALSE))  



children_temp_inf_testneg <-
  map_df( setdiff( names_conditions$cohort_name, c("ra","juvenile_arthritis","sle","misc")) %>% set_names,
          cohort_wrap_func,
          interval = "overall",
          age_expre =  c("0 to 6","7 to 11","12 to 18"),
          sex_expre = "Both",
          numerator = "infection",
          denominator = "test_negative",
          .id = "conditions") %>% 
  mutate( conditions = fct_reorder(conditions, IRR_random, .desc = FALSE))  


adult_temp_inf_testneg <- 
  map_df( setdiff( names_conditions$cohort_name, c("juvenile_arthritis","sle","misc")) %>% set_names,
          cohort_wrap_func,
          interval = "overall",
          age_expre =  c("19 to 40","41 to 64"),
          sex_expre = "Both",
          numerator = "infection",
          denominator = "test_negative",
          .id = "conditions") %>% 
  mutate( conditions = fct_reorder(conditions, IRR_random, .desc = FALSE))  


elderly_temp_inf_testneg <- 
  map_df( setdiff( names_conditions$cohort_name, c("t1dm","me_cfs","juvenile_arthritis","sle","misc")) %>% set_names,
          cohort_wrap_func,
          interval = "overall",
          age_expre =  "65 to 150",
          sex_expre = "Both",
          numerator = "infection",
          denominator = "test_negative",
          .id = "conditions") %>% 
  mutate( conditions = fct_reorder(conditions, IRR_random, .desc = FALSE))  



all_IRR_inf_testneg <- list( All = overall_temp_inf_testneg, 
                 Female = female_temp_inf_testneg,
                 Male = male_temp_inf_testneg, 
                 children = children_temp_inf_testneg,
                 adult = adult_temp_inf_testneg,
                 elderly = elderly_temp_inf_testneg)



## wait for further databases
# overall_temp_reinf_inf <- 
#   map_df( setdiff( names_conditions$cohort_name, c("juvenile_arthritis","sle")) %>% set_names,
#           cohort_wrap_func,
#           interval = "overall",
#           age_expre =  "0;150",
#           sex_expre = "Both",
#           numerator = "reinfection",
#           denominator = "infection",
#           .id = "conditions") %>% 
#   mutate( conditions = fct_reorder(conditions, IRR_random, .desc = FALSE))  
# 
# female_temp_reinf_inf  <- 
#   map_df( setdiff( names_conditions$cohort_name, c("juvenile_arthritis","sle","misc")) %>% set_names,
#           cohort_wrap_func,
#           interval = "overall",
#           age_expre =  "0;150",
#           sex_expre = "Female",
#           numerator = "reinfection",
#           denominator = "infection",
#           .id = "conditions") %>% 
#   mutate( conditions = fct_reorder(conditions, IRR_random, .desc = FALSE))  
# 
# 
# male_temp_reinf_inf  <- 
#   map_df( setdiff( names_conditions$cohort_name, c("juvenile_arthritis","sle","misc")) %>% set_names,
#           cohort_wrap_func,
#           interval = "overall",
#           age_expre =  "0;150",
#           sex_expre = "Male",
#           numerator = "reinfection",
#           denominator = "infection",
#           .id = "conditions") %>% 
#   mutate( conditions = fct_reorder(conditions, IRR_random, .desc = FALSE))  
# 
# 
# 
# children_temp_reinf_inf  <-
#   map_df( setdiff( names_conditions$cohort_name, c("juvenile_arthritis","sle","misc")) %>% set_names,
#           cohort_wrap_func,
#           interval = "overall",
#           age_expre =  "0;150",
#           sex_expre = "Male",
#           numerator = "reinfection",
#           denominator = "infection",
#           .id = "conditions") %>% 
#   mutate( conditions = fct_reorder(conditions, IRR_random, .desc = FALSE))  
# 
# 
# adult_temp_reinf_inf  <- 
#   map_df( setdiff( names_conditions$cohort_name, c("juvenile_arthritis","sle","misc")) %>% set_names,
#           cohort_wrap_func,
#           interval = "overall",
#           age_expre =  c("19;40","41;64"),
#           sex_expre = "Both",
#           numerator = "reinfection",
#           denominator = "infection",
#           .id = "conditions") %>% 
#   mutate( conditions = fct_reorder(conditions, IRR_random, .desc = FALSE))  
# 
# 
# elderly_temp_reinf_inf  <- 
#   map_df( setdiff( names_conditions$cohort_name, c("juvenile_arthritis","me_cfs","sle","t1dm","misc")) %>% set_names,
#           cohort_wrap_func,
#           interval = "overall",
#           age_expre =  "65;120",
#           sex_expre = "Both",
#           numerator = "reinfection",
#           denominator = "infection",
#           .id = "conditions") %>% 
#   mutate( conditions = fct_reorder(conditions, IRR_random, .desc = FALSE))  
# 
# 
# 
# all_IRR_reinf_inf  <- list( All = overall_temp_reinf_inf , 
#                              Female = female_temp_reinf_inf ,
#                              Male = male_temp_reinf_inf , 
#                              children = children_temp_reinf_inf ,
#                              adult = adult_temp_reinf_inf ,
#                              elderly = elderly_temp_reinf_inf )



plot_func <- function( df){
  
  output <- ggplot( df, aes( x = IRR_random, y = conditions)) + 
    geom_point( position = position_dodge(0.75)) +
    geom_errorbar( aes( xmin = IRR_low_random, xmax = IRR_upper_random), position = position_dodge(0.75), width = 0.5) +
    geom_vline( xintercept = 1) +
    scale_x_continuous(  limits = c(0.2, 12), trans = scales::log2_trans()) +
    guides(color = guide_legend(nrow = 1, byrow = TRUE)) +
    ggsci::scale_color_lancet( alpha = 0.75) +
    labs( x = "", y = "", fill = "IRR") +
    theme_bw()+
    theme( text = element_text( family = "serif", color = "black"),
           axis.text.x = element_text( angle = 90, vjust = 0.5, hjust = 1),
           axis.text.y = element_text( ),
           panel.grid.minor.x = element_blank(),
           legend.background = element_rect(fill='transparent'),
           legend.position = "top")
  
  return(output)
  
  
}

## infection over test negative ---------------------------------------------------------

plot_list <- map( all_IRR_inf_testneg, plot_func)
empty <- ggplot() + theme_void()
manual_legend <- legend <- get_legend(# create some space to the left of the legend
  plot_list$All + theme(legend.box.margin = margin(0, 0, 0, 12)))

main_plot <- plot_grid(plot_list$All+ theme( legend.position = "none"), 
                       empty, 
                       empty, 
                       plot_list$Female + theme( legend.position = "none"), 
                       plot_list$Male + theme( legend.position = "none"), 
                       empty, 
                       plot_list$children + theme( legend.position = "none"), 
                       plot_list$adult + theme( legend.position = "none"), 
                       plot_list$elderly + theme( legend.position = "none"), 
                       nrow = 3, ncol = 3,
                       labels = c("All", "", "", 
                                  "Female", "Male", "", 
                                  "Children&Adolescent", "Adults aged 19-64", "Elderly (aged >64)"), label_size = 7, label_y = 1.01,
                       align = "h")

plot_grid( main_plot, manual_legend, ncol = 1, rel_heights = c(1, 0.05))


## reinfection over infection ---------------------------------------------------------

plot_list <- map( all_IRR_reinf_inf, plot_func)
empty <- ggplot() + theme_void()
manual_legend <- legend <- get_legend(# create some space to the left of the legend
  plot_list$All + theme(legend.box.margin = margin(0, 0, 0, 12)))

main_plot <- plot_grid(plot_list$All+ theme( legend.position = "none"), 
                       empty, 
                       empty, 
                       plot_list$Female + theme( legend.position = "none"), 
                       plot_list$Male + theme( legend.position = "none"), 
                       empty, 
                       plot_list$children + theme( legend.position = "none"), 
                       plot_list$adult + theme( legend.position = "none"), 
                       plot_list$elderly + theme( legend.position = "none"), 
                       nrow = 3, ncol = 3,
                       labels = c("All", "", "", 
                                  "Female", "Male", "", 
                                  "Children&Adolescent", "Adults aged 19-64", "Elderly aged >64"), label_size = 7, label_y = 1.01,
                       align = "h")

plot_grid( main_plot, manual_legend, ncol = 1, rel_heights = c(1, 0.05))

### IRR functions


# IRR ---------------------------------------------------------------------
# Transform from Long to wide format


num_denom <- function(numerator,denominator,interval, age_expre,sex_expre) {
  
  assign(glue("incidence_estimates_wide_{numerator}_{denominator}"),  
         incidence_estimates_help %>% 
           filter( 
             denominator_strata_cohort_name %in% c(numerator, denominator),
             analysis_interval %in% interval, 
             denominator_age_group %in% age_expre,
             denominator_sex %in% sex_expre
             ) %>% 
           dplyr::select(-c("incidence_start_date", "analysis_interval", "denominator_age_group", "denominator_sex", "year_index", "month_index", "year_month")) %>%
           pivot_wider( id_cols = c(database_name,
                                    outcome_cohort_name
           ), 
           names_from = denominator_strata_cohort_name,
           values_from = c( n_persons, person_years, n_events, incidence_100000_pys)) %>% 
           janitor::clean_names() %>% 
           ungroup() 
  )
  
}


cohort_wrap_func_reinf <- function(conditions,
                             interval = "overall", 
                             age_expre = "0 to 150", 
                             sex_expre = "Both",
                             numerator = numerator,
                             denominator = denominator){
  
  
  IRR_df_1 <- 
    num_denom(numerator,denominator,interval,age_expre,sex_expre) %>% 
    filter(!(is.na(incidence_100000_pys_reinfection) | is.na(incidence_100000_pys_infection))) %>% 
    filter( outcome_cohort_name %in% conditions) %>%
    dplyr::group_by(database_name) %>%
    summarise(n_events_reinfection = sum(n_events_reinfection),
              person_years_reinfection = sum(person_years_reinfection),
              n_events_infection = sum(n_events_infection),
              person_years_infection = sum(person_years_infection))
  
  # IRR_df_2 <- 
  #   num_denom(numerator,denominator) %>% 
  #   filter( analysis_interval %in% interval, 
  #           denominator_age_group %in% age_expre,
  #           denominator_sex %in% sex_expre
  #   ) %>% 
  #   filter( !is.na(incidence_100000_pys_reinfection), !is.na(incidence_100000_pys_infection)) %>% 
  #   filter( outcome_cohort_name == conditions) %>%
  #   select(database_name) %>% distinct()
  # 
  # IRR_df <- cbind(IRR_df_1,IRR_df_2)
  
  
  
  
  meta_unit_func <- function( input_df = IRR_df_1){
    
    output_meta <- meta::metainc( event.e = n_events_reinfection,
                                  time.e = person_years_reinfection,
                                  event.c = n_events_infection,
                                  time.c = person_years_infection, 
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
  
  output <- meta_unit_func( input_df = IRR_df_1)
  
  return(output)
  
  
}

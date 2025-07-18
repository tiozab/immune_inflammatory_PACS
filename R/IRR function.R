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
           dplyr::select(-c("incidence_start_date", "analysis_interval", "year_index", "month_index", "year_month")) %>%
           pivot_wider( id_cols = c(database_name,
                                    outcome_cohort_name,
                                    denominator_age_group,
                                    denominator_sex
           ), 
           names_from = denominator_strata_cohort_name,
           values_from = c( n_persons, person_years, n_events, incidence_100000_pys)) %>% 
           janitor::clean_names() %>% 
           ungroup() 
  )
  
}

meta_unit_func <- function(input_df, method_ci){
  
  output_meta <- meta::metainc( event.e = n_events_infection,
                                time.e = person_years_infection,
                                event.c = n_events_test_negative,
                                time.c = person_years_test_negative, 
                                sm = "IRR",
                                data = input_df,
                                method.random.ci = method_ci)
  
  output_meta_final <- 
    tibble( IRR_random = exp(output_meta$TE.random),
            IRR_low_random = exp(output_meta$lower.random),
            IRR_upper_random = exp(output_meta$upper.random),
            IRR_fix = exp(output_meta$TE.fix),
            IRR_low_fix = exp(output_meta$lower.fix),
            IRR_upper_fix = exp(output_meta$upper.fix),
            I2 = output_meta$I2,
            I2_lower = output_meta$lower.I2,
            I2_upper = output_meta$upper.I2)
  
  return(output_meta_final)
  
}  


cohort_wrap_func <- function(conditions,
                             interval = "overall", 
                             age_expre = "0 to 150", 
                             sex_expre = "Both",
                             numerator = numerator,
                             denominator = denominator,
                             method_ci = "classic"){
  
  IRR_df_1 <- 
    num_denom(numerator,denominator,interval,age_expre,sex_expre) %>% 
    filter( !is.na(incidence_100000_pys_infection), !is.na(incidence_100000_pys_test_negative)) %>% 
    filter( outcome_cohort_name == conditions) %>%
    dplyr::group_by(database_name) %>%
    summarise(n_events_infection = sum(n_events_infection),
              person_years_infection = sum(person_years_infection),
              n_events_test_negative = sum(n_events_test_negative),
              person_years_test_negative = sum(person_years_test_negative))
  
  output <- meta_unit_func( input_df = IRR_df_1, method_ci = method_ci)
  
  return(output)
  
  
}

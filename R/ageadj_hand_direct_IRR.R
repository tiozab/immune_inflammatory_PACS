# Direct Standardisation ---------------------------------------------
## using the population distribution (direct method) of the general population
## age-adjusted rate is a weighted average of the age-specific (crude) rates,
#### where the weights are the proportions of persons in the corresponding age groups of a standard population.
#https://seer.cancer.gov/seerstat/tutorials/aarates/printable.html#:~:text=An%20age-adjusted%20rate%20is%20a%20weighted%20average%20of,the%20corresponding%20age%20groups%20of%20a%20standard%20population.


## age ADJUSTED rates only makes sense if we have only one sex (e.g. prostate cancer, breast cancer)
# or we do not expect sex to have a great impact (assumed in this case)
# need age distribution of general population


incidence_estimates_age_distribution <- incidence_estimates_general_help %>%
  select("analysis_interval","outcome_cohort_name","n_events","person_years",
         "denominator_age_group","n_persons", "database_name") %>%
  filter(analysis_interval == "overall",
         denominator_age_group != "0;150") %>%
  rename(ref_persons = n_persons,
         ref_events = n_events,
         ref_person_years = person_years) %>%
  inner_join(
    incidence_estimates_help,
    join_by(
      "analysis_interval" == "analysis_interval",
      "outcome_cohort_name" == "outcome_cohort_name",
      "denominator_age_group" == "denominator_age_group",
      "database_name" == "database_name")
  )


#### need to know what can be added up in order to get the age adjusted output per group
# we need to group by database, by outcome, by target cohort

# overall only group by denominator_strata_cohort_name, outcome_cohort_name, database_name

incidence_estimates_standard_age <- incidence_estimates_age_distribution %>%
  select("database_name","denominator_strata_cohort_name","outcome_cohort_name","denominator_age_group",
         "n_events","person_years", "ref_persons","ref_person_years") %>%
  group_by(denominator_strata_cohort_name, outcome_cohort_name, database_name) %>%
  mutate(ref_persons_total = sum(ref_persons)) %>%
  ungroup() %>%
  mutate(age_distr = ref_persons/ref_persons_total) %>%
  mutate(n_events = n_events * age_distr,
         person_years = person_years * age_distr
         )  %>%
  arrange(denominator_strata_cohort_name, outcome_cohort_name, database_name, denominator_age_group) 



# IRR ---------------------------------------------------------------------
# Transform from Long to wide format


num_denom <- function(numerator,denominator) {
  
  assign(glue("incidence_estimates_wide_{numerator}_{denominator}"),  
         incidence_estimates_standard_age  %>% 
           filter( 
             denominator_strata_cohort_name %in% c(numerator, denominator)) %>% 
           pivot_wider( id_cols = c(database_name, 
                                    outcome_cohort_name,
                                    denominator_age_group
           ), 
           names_from = denominator_strata_cohort_name,
           values_from = c( person_years, n_events)) %>% 
           janitor::clean_names() %>% 
           ungroup()
  )
  
}


cohort_wrap_func <- function(conditions,
                             denominator_age_group = denominator_age_group, 
                             numerator = numerator,
                             denominator = denominator){
  
  
  IRR_df <- 
    num_denom(numerator,denominator) %>% 
    filter( denominator_age_group %in% denominator_age_group,
             ) %>%
    filter( !is.na(n_events_infection) & !is.na(n_events_test_negative))  %>% 
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
  map_df( setdiff( names_conditions$cohort_name, c("juvenile_arthritis","sle","misc")) %>% set_names,
          cohort_wrap_func,
          denominator_age_group = c("0;6","7;11","12;18","19;40","41;64","65;120"),
          numerator = "infection",
          denominator = "test_negative",
          .id = "conditions") %>% 
  mutate( conditions = fct_reorder(conditions, IRR_random, .desc = FALSE))  

children_temp_inf_testneg <-
  map_df( setdiff( names_conditions$cohort_name, c("misc","juvenile_arthritis","sle")) %>% set_names,
          cohort_wrap_func,
          denominator_age_group = c("0;6","7;11","12;18"),
          numerator = "infection",
          denominator = "test_negative",
          .id = "conditions") %>% 
  mutate( conditions = fct_reorder(conditions, IRR_random, .desc = FALSE))  


adult_temp_inf_testneg <-
  map_df( setdiff( names_conditions$cohort_name, c("misc","juvenile_arthritis","sle")) %>% set_names,
          cohort_wrap_func,
          denominator_age_group = c("19;40","41;64"),
          numerator = "infection",
          denominator = "test_negative",
          .id = "conditions") %>% 
  mutate( conditions = fct_reorder(conditions, IRR_random, .desc = FALSE))  


elderly_temp_inf_testneg <-
  map_df( setdiff( names_conditions$cohort_name, c("misc","juvenile_arthritis","sle")) %>% set_names,
          cohort_wrap_func,
          denominator_age_group = c("65;120"),
          numerator = "infection",
          denominator = "test_negative",
          .id = "conditions") %>% 
  mutate( conditions = fct_reorder(conditions, IRR_random, .desc = FALSE))  



all_IRR_inf_testneg <- list( All = overall_temp_inf_testneg, 
                             children = children_temp_inf_testneg,
                             adult = adult_temp_inf_testneg,
                             elderly = elderly_temp_inf_testneg)



# wait for further databases
# overall_temp_reinf_inf <-
#   map_df( setdiff( names_conditions$cohort_name, c("juvenile_arthritis","sle")) %>% set_names,
#           cohort_wrap_func,
#           denominator_age_group = c("0;6","7;11","12;18","19;40","41;64","65;120"),
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
#           denominator_age_group = c("0;6","7;11","12;18"),
#           numerator = "reinfection",
#           denominator = "infection",
#           .id = "conditions") %>%
#   mutate( conditions = fct_reorder(conditions, IRR_random, .desc = FALSE))
# 
# 
# adult_temp_reinf_inf  <-
#   map_df( setdiff( names_conditions$cohort_name, c("juvenile_arthritis","sle","misc")) %>% set_names,
#           cohort_wrap_func,
#           denominator_age_group = c("19;40","41;64"),
#           numerator = "reinfection",
#           denominator = "infection",
#           .id = "conditions") %>%
#   mutate( conditions = fct_reorder(conditions, IRR_random, .desc = FALSE))
# 
# 
# elderly_temp_reinf_inf  <-
#   map_df( setdiff( names_conditions$cohort_name, c("juvenile_arthritis","me_cfs","sle","t1dm","misc")) %>% set_names,
#           cohort_wrap_func,
#           denominator_age_group = c("65;120"),
#           numerator = "reinfection",
#           denominator = "infection",
#           .id = "conditions") %>%
#   mutate( conditions = fct_reorder(conditions, IRR_random, .desc = FALSE))
# 
# 
# 
# all_IRR_reinf_inf  <- list( All = overall_temp_reinf_inf ,
#                              children = children_temp_reinf_inf ,
#                              adult = adult_temp_reinf_inf ,
#                              elderly = elderly_temp_reinf_inf )
# 
# 
# 
# plot_func <- function( df){
#   
#   output <- ggplot( df, aes( x = IRR_random, y = conditions)) + 
#     geom_point( position = position_dodge(0.75)) +
#     geom_errorbar( aes( xmin = IRR_low_random, xmax = IRR_upper_random), position = position_dodge(0.75), width = 0.5) +
#     geom_vline( xintercept = 1) +
#     scale_x_continuous(  limits = c(0.2, 12), trans = scales::log2_trans()) +
#     guides(color = guide_legend(nrow = 1, byrow = TRUE)) +
#     ggsci::scale_color_lancet( alpha = 0.75) +
#     labs( x = "", y = "", fill = "IRR") +
#     theme_bw()+
#     theme( text = element_text( family = "serif", color = "black"),
#            axis.text.x = element_text( angle = 90, vjust = 0.5, hjust = 1),
#            axis.text.y = element_text( ),
#            panel.grid.minor.x = element_blank(),
#            legend.background = element_rect(fill='transparent'),
#            legend.position = "top")
#   
#   return(output)
#   
#   
# }

## infection over test negative ---------------------------------------------------------

plot_list <- map( all_IRR_inf_testneg, plot_func)
empty <- ggplot() + theme_void()
manual_legend <- legend <- get_legend(# create some space to the left of the legend
  plot_list$All + theme(legend.box.margin = margin(0, 0, 0, 12)))

main_plot <- plot_grid(plot_list$All+ theme( legend.position = "none"), 
                       empty, 
                       empty, 
                       plot_list$children + theme( legend.position = "none"), 
                       plot_list$adult + theme( legend.position = "none"), 
                       plot_list$elderly + theme( legend.position = "none"), 
                       nrow = 2, ncol = 3,
                       labels = c("All", "", "", 
                                  "Children & Adolescent", "Adults aged 19-64", "Elderly (aged >64)"), label_size = 7, label_y = 1.01,
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
                       plot_list$children + theme( legend.position = "none"), 
                       plot_list$adult + theme( legend.position = "none"), 
                       plot_list$elderly + theme( legend.position = "none"), 
                       nrow = 3, ncol = 3,
                       labels = c("All", "", "", 
                                  "Female", "Male", "", 
                                  "Children&Adolescent", "Adults aged 19-64", "Elderly aged >64"), label_size = 7, label_y = 1.01,
                       align = "h")

plot_grid( main_plot, manual_legend, ncol = 1, rel_heights = c(1, 0.05))

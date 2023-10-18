
source(here::here("R","getData.R"))
source(here::here("R","IRR function.R"))

#pharmetrics does not have test negative information (take it out)

## infection over test_negative ------------------------------------------------------

overall_temp_inf_testneg <- 
  map_df( setdiff( names_conditions$cohort_name, c("juvenile_arthritis","sle","mis")) %>% set_names,
          cohort_wrap_func,
          interval = "overall",
          age_expre =  "0 to 150",
          sex_expre = "Both",
          numerator = "infection",
          denominator = "test_negative",
          .id = "conditions")  %>% 
  mutate( conditions = factor(conditions,levels = c("t1dm","mis","ibd","sle","juvenile_arthritis","ra","me_cfs_symptoms",
                                                    "me_cfs","dysautonomia","pots"),
                                       labels = c("T2DM","MIS","IBD","SLE","Juvenile arthritis","RA","ME/CFS symptoms",
                                                  "ME/CFS","Dysautonomia","POTS"))) 

female_temp_inf_testneg <- 
  map_df( setdiff( names_conditions$cohort_name, c("juvenile_arthritis","sle","mis")) %>% set_names,
          cohort_wrap_func,
          interval = "overall",
          age_expre =  "0 to 150",
          sex_expre = "Female",
          numerator = "infection",
          denominator = "test_negative",
          .id = "conditions")  %>% 
  mutate( conditions = factor(conditions,levels = c("t1dm","mis","ibd","sle","juvenile_arthritis","ra","me_cfs_symptoms",
                                                    "me_cfs","dysautonomia","pots"),
                              labels = c("T2DM","MIS","IBD","SLE","Juvenile arthritis","RA","ME/CFS symptoms",
                                         "ME/CFS","Dysautonomia","POTS"))) 


male_temp_inf_testneg <- 
  map_df( setdiff( names_conditions$cohort_name, c("juvenile_arthritis","sle","mis")) %>% set_names,
          cohort_wrap_func,
          interval = "overall",
          age_expre =  "0 to 150",
          sex_expre = "Male",
          numerator = "infection",
          denominator = "test_negative",
          .id = "conditions")  %>% 
  mutate( conditions = factor(conditions,levels = c("t1dm","mis","ibd","sle","juvenile_arthritis","ra","me_cfs_symptoms",
                                                    "me_cfs","dysautonomia","pots"),
                              labels = c("T2DM","MIS","IBD","SLE","Juvenile arthritis","RA","ME/CFS symptoms",
                                         "ME/CFS","Dysautonomia","POTS"))) 


children_temp_inf_testneg <-
  map_df( setdiff( names_conditions$cohort_name, c("ra","juvenile_arthritis","sle","mis")) %>% set_names,
          cohort_wrap_func,
          interval = "overall",
          age_expre =  c("0 to 6","7 to 11","12 to 18"),
          sex_expre = "Both",
          numerator = "infection",
          denominator = "test_negative",
          .id = "conditions")  %>% 
  mutate( conditions = factor(conditions,levels = c("t1dm","mis","ibd","sle","juvenile_arthritis","ra","me_cfs_symptoms",
                                                    "me_cfs","dysautonomia","pots"),
                              labels = c("T2DM","MIS","IBD","SLE","Juvenile arthritis","RA","ME/CFS symptoms",
                                         "ME/CFS","Dysautonomia","POTS"))) 


adult_temp_inf_testneg <- 
  map_df( setdiff( names_conditions$cohort_name, c("juvenile_arthritis","sle","mis")) %>% set_names,
          cohort_wrap_func,
          interval = "overall",
          age_expre =  c("19 to 40","41 to 64"),
          sex_expre = "Both",
          numerator = "infection",
          denominator = "test_negative",
          .id = "conditions")  %>% 
  mutate( conditions = factor(conditions,levels = c("t1dm","mis","ibd","sle","juvenile_arthritis","ra","me_cfs_symptoms",
                                                    "me_cfs","dysautonomia","pots"),
                              labels = c("T2DM","MIS","IBD","SLE","Juvenile arthritis","RA","ME/CFS symptoms",
                                         "ME/CFS","Dysautonomia","POTS"))) 


elderly_temp_inf_testneg <- 
  map_df( setdiff( names_conditions$cohort_name, c("juvenile_arthritis","me_cfs","sle","t1dm","mis")) %>% set_names,
          cohort_wrap_func,
          interval = "overall",
          age_expre =  "65 to 150",
          sex_expre = "Both",
          numerator = "infection",
          denominator = "test_negative",
          .id = "conditions")  %>% 
  mutate( conditions = factor(conditions,levels = c("t1dm","mis","ibd","sle","juvenile_arthritis","ra","me_cfs_symptoms",
                                                    "me_cfs","dysautonomia","pots"),
                              labels = c("T2DM","MIS","IBD","SLE","Juvenile arthritis","RA","ME/CFS symptoms",
                                         "ME/CFS","Dysautonomia","POTS"))) 

all_IRR_inf_testneg <- list( All = overall_temp_inf_testneg, 
                             Female = female_temp_inf_testneg,
                             Male = male_temp_inf_testneg, 
                             children = children_temp_inf_testneg,
                             adult = adult_temp_inf_testneg,
                             elderly = elderly_temp_inf_testneg)



plot_func <- function( df){
  
  output <- ggplot( df, aes( x = IRR_random, y = conditions)) + 
    geom_point( size = 1.2, position = position_dodge(0.2)) +
    geom_errorbar( aes( xmin = IRR_low_random, xmax = IRR_upper_random), position = position_dodge(0.75), width = 0.3) +
    geom_vline( xintercept = 1) +
    scale_x_continuous(  limits = c(0.2, 5), trans = scales::log2_trans()) +
    guides(color = guide_legend(nrow = 1, byrow = TRUE)) +
    ggsci::scale_color_lancet( alpha = 0.5) +
    labs( x = "", y = "", fill = "IRR") +
    theme_bw()+
    theme( text = element_text( family = "serif", color = "black"),
           axis.text.x = element_text( size = 10, angle = 90, vjust = 0.5, hjust = 1),
           axis.text.y = element_text( size=10),
           panel.grid.minor.x = element_blank(),
           legend.background = element_rect(fill='transparent'),
           legend.position = "top")
  
  return(output)
  
  
}


plot_list <- map( all_IRR_inf_testneg, plot_func)
empty <- ggplot() + theme_void()
manual_legend <- legend <- get_legend(# create some space to the left of the legend
  plot_list$All + theme(legend.box.margin = margin(0, 0, 0, 12)))

main_plot <- plot_grid(plot_list$All+ theme( legend.position = "none"), 
                       plot_list$elderly + theme( legend.position = "none"), 
                       plot_list$Female + theme( legend.position = "none"), 
                       plot_list$adult + theme( legend.position = "none"), 
                       plot_list$Male + theme( legend.position = "none"), 
                       plot_list$children + theme( legend.position = "none"), 
                      
                       nrow = 3, ncol = 2,
                       labels = c("All",  "Elderly",
                                  "Female", "Adults", 
                                  "Male", "Children"), label_size = 8, label_y = 1.0,
                       align = "w")

pdf("figure1.pdf",         # File name
    width = 6, height = 6, # Width and height in inches
    bg = "white",          # Background color
    colormodel = "cmyk")    # Color model (cmyk is required for most publications)

plot_grid( main_plot, manual_legend, ncol = 1, rel_heights = c(1.5, 0.1))

# Closing the graphical device
dev.off() 
 


## re-infection over infection ------------------------------------------------------
### WAIT MORE DATA
# 
# overall_temp_inf_testneg <- 
#   map_df( setdiff( names_conditions$cohort_name, c("juvenile_arthritis","sle","mis")) %>% set_names,
#           cohort_wrap_func,
#           interval = "overall",
#           age_expre =  "0 to 150",
#           sex_expre = "Both",
#           numerator = "reinfection",
#           denominator = "infection",
#           .id = "conditions") %>% 
#   mutate( conditions = fct_reorder(conditions, IRR_random, .desc = FALSE))  
# 
# female_temp_inf_testneg <- 
#   map_df( setdiff( names_conditions$cohort_name, c("juvenile_arthritis","sle","mis")) %>% set_names,
#           cohort_wrap_func,
#           interval = "overall",
#           age_expre =  "0 to 150",
#           sex_expre = "Female",
#           numerator = "reinfection",
#           denominator = "infection",
#           .id = "conditions") %>% 
#   mutate( conditions = fct_reorder(conditions, IRR_random, .desc = FALSE))  
# 
# 
# male_temp_inf_testneg <- 
#   map_df( setdiff( names_conditions$cohort_name, c("juvenile_arthritis","sle","mis")) %>% set_names,
#           cohort_wrap_func,
#           interval = "overall",
#           age_expre =  "0 to 150",
#           sex_expre = "Male",
#           numerator = "reinfection",
#           denominator = "infection",
#           .id = "conditions") %>% 
#   mutate( conditions = fct_reorder(conditions, IRR_random, .desc = FALSE))  
# 
# 
# 
# children_temp_inf_testneg <-
#   map_df( setdiff( names_conditions$cohort_name, c("ra","juvenile_arthritis","sle","mis")) %>% set_names,
#           cohort_wrap_func,
#           interval = "overall",
#           age_expre =  c("0 to 6","7 to 11","12 to 18"),
#           sex_expre = "Both",
#           numerator = "reinfection",
#           denominator = "infection",
#           .id = "conditions") %>% 
#   mutate( conditions = fct_reorder(conditions, IRR_random, .desc = FALSE))  
# 
# 
# adult_temp_inf_testneg <- 
#   map_df( setdiff( names_conditions$cohort_name, c("juvenile_arthritis","sle","mis")) %>% set_names,
#           cohort_wrap_func,
#           interval = "overall",
#           age_expre =  c("19 to 40","41 to 64"),
#           sex_expre = "Both",
#           numerator = "reinfection",
#           denominator = "infection",
#           .id = "conditions") %>% 
#   mutate( conditions = fct_reorder(conditions, IRR_random, .desc = FALSE))  
# 
# 
# elderly_temp_inf_testneg <- 
#   map_df( setdiff( names_conditions$cohort_name, c("juvenile_arthritis","me_cfs","sle","t1dm","mis")) %>% set_names,
#           cohort_wrap_func,
#           interval = "overall",
#           age_expre =  "65 to 150",
#           sex_expre = "Both",
#           numerator = "reinfection",
#           denominator = "infection",
#           .id = "conditions") %>% 
#   mutate( conditions = fct_reorder(conditions, IRR_random, .desc = FALSE),
#         outcome_cohort_name = factor(outcome_cohort_name,
#                                      levels = c("dysautonomia",
#                                                 "me_cfs_symptoms",
#                                                 "pots", 
#                                                 "me_cfs",
#                                                 "ra",
#                                                 "ibd",
#                                                 "t1dm",
#                                                 "sle",
#                                                 "juvenile_arthritis",
#                                                 "mis"),
#                                      labels = c("Dysautonomia",
#                                                 "ME/CFS symptoms",
#                                                 "POTS",
#                                                 "ME/CFS",
#                                                 "RA",
#                                                 "IBD",
#                                                 "T1DM",
#                                                 "SLE",
#                                                 "Juvenile arthritis",
#                                                 "MIS")), 
#         database_name = factor(database_name, 
#                                levels = c("CPRDGOLD","PharmPlus","IMASIS","eDOL_CHUM","AUSOM","IPCI"), 
#                                labels = c("CPRD Gold","pharMetrics Plus","IMASIS","CHUM","AUSOM","IPCI")),
#         denominator_sex = factor(denominator_sex, levels=c("Female","Male","Both"), 
#                                  labels = c("Female","Male","All"))
# )
# 
# 
# 
# all_IRR_inf_testneg <- list( All = overall_temp_inf_testneg, 
#                              Female = female_temp_inf_testneg,
#                              Male = male_temp_inf_testneg, 
#                              children = children_temp_inf_testneg,
#                              adult = adult_temp_inf_testneg,
#                              elderly = elderly_temp_inf_testneg)
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
# 
# 
# plot_list <- map( all_IRR_inf_testneg, plot_func)
# empty <- ggplot() + theme_void()
# manual_legend <- legend <- get_legend(# create some space to the left of the legend
#   plot_list$All + theme(legend.box.margin = margin(0, 0, 0, 12)))
# 
# main_plot <- plot_grid(plot_list$All+ theme( legend.position = "none"), 
#                        empty, 
#                        empty, 
#                        plot_list$Female + theme( legend.position = "none"), 
#                        plot_list$Male + theme( legend.position = "none"), 
#                        empty, 
#                        plot_list$children + theme( legend.position = "none"), 
#                        plot_list$adult + theme( legend.position = "none"), 
#                        plot_list$elderly + theme( legend.position = "none"), 
#                        nrow = 3, ncol = 3,
#                        labels = c("All", "", "", 
#                                   "Female", "Male", "", 
#                                   "Children & Adolescent", "Adults aged 19-64", "Elderly (aged >64)"), label_size = 6, label_y = 1.1,
#                        align = "w")
# 
# plot_grid( main_plot, manual_legend, ncol = 1, rel_heights = c(1, 0.1))
# 

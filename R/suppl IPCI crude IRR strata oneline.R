## read in the results files

source(here::here("R","read_in.R"))
IPCI <- read_in(database_results_name = "IPCI")

#load outcome names
names_conditions <- read_csv(here::here("names_conditions.csv"))

  # general population
  incidence_estimates_general_help <- IPCI[[1]]
  
  # specific populations
  incidence_estimates_help <- IPCI[[2]]
  
  
  source(here::here("R","IRR function.R"))
  
  ## infection over test_negative ------------------------------------------------------
  
  overall_temp_inf_testneg <- 
    map_df( setdiff( names_conditions$cohort_name, c("ibd","juvenile_arthritis","me_cfs","ra","sle","pots","t1dm","mis")) %>% set_names,
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
    map_df( setdiff( names_conditions$cohort_name, c("ibd","juvenile_arthritis","me_cfs","ra","sle","pots","t1dm","mis")) %>% set_names,
            cohort_wrap_func,
            interval = "overall",
            age_expre =  "0 to 150",
            sex_expre = "Female",
            numerator = "infection",
            denominator = "test_negative",
            .id = "conditions") %>% 
    mutate( conditions = factor(conditions,levels = c("t1dm","mis","ibd","sle","juvenile_arthritis","ra","me_cfs_symptoms",
                                                      "me_cfs","dysautonomia","pots"),
                                labels = c("T2DM","MIS","IBD","SLE","Juvenile arthritis","RA","ME/CFS symptoms",
                                           "ME/CFS","Dysautonomia","POTS"))) 
  
 

  elderly_temp_inf_testneg <- 
    map_df( setdiff( names_conditions$cohort_name, c("ibd","juvenile_arthritis","me_cfs","ra","sle","pots","t1dm","mis")) %>% set_names,
            cohort_wrap_func,
            interval = "overall",
            age_expre =  "65 to 150",
            sex_expre = "Both",
            numerator = "infection",
            denominator = "test_negative",
            .id = "conditions") %>% 
    mutate( conditions = factor(conditions,levels = c("t1dm","mis","ibd","sle","juvenile_arthritis","ra","me_cfs_symptoms",
                                                      "me_cfs","dysautonomia","pots"),
                                labels = c("T2DM","MIS","IBD","SLE","Juvenile arthritis","RA","ME/CFS symptoms",
                                           "ME/CFS","Dysautonomia","POTS"))) 
  
  all_IRR_inf_testneg <- list( All = overall_temp_inf_testneg, 
                               Female = female_temp_inf_testneg,
                              
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
                        
                         plot_list$Female + theme( legend.position = "none"), 
                        
                         plot_list$elderly + theme( legend.position = "none"), 
                         nrow = 3, ncol = 1,
                         labels = c("All",
                                    "Female",  
                                    "Elderly"), label_size = 8, label_y = 1.0,
                         align = "w")
  

pdf("supp_ipci.pdf",         # File name
    width = 3, height = 6, # Width and height in inches
    bg = "white",          # Background color
    colormodel = "cmyk")    # Color model (cmyk is required for most publications)

plot_grid( main_plot, manual_legend, ncol = 1, rel_heights = c(1.5, 0.1))

# Closing the graphical device
dev.off() 
 

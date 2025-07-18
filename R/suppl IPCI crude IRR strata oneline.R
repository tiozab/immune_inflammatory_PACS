  # specific populations
  incidence_estimates_help <- IPCI[[2]]
  
  
  source(here::here("R","IRR function.R"))
  
  ## infection over test_negative ------------------------------------------------------
  
  overall_temp_inf_testneg <- 
    map_df( setdiff( names_conditions$cohort_name, c("mis","t1dm","sle","ra","me_cfs","juvenile_arthritis","ibd","pots")) %>% set_names,
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
                                           "ME/CFS diagnosis","POTS symptoms","POTS diagnosis"))) 
  
  female_temp_inf_testneg <- 
    map_df( setdiff( names_conditions$cohort_name, c("mis","t1dm","sle","ra","me_cfs","juvenile_arthritis","ibd","pots")) %>% set_names,
            cohort_wrap_func,
            interval = "overall",
            age_expre =  "0 to 150",
            sex_expre = "Female",
            numerator = "infection",
            denominator = "test_negative",
            .id = "conditions") %>% 
    mutate( conditions = factor(conditions,levels = c("t1dm","mis","ibd","sle","juvenile_arthritis","ra","me_cfs_symptoms",
                                                      "me_cfs","dysautonomia","pots"),
                                labels = c("T1DM","MIS","IBD","SLE","Juvenile arthritis","RA","ME/CFS symptoms",
                                           "ME/CFS diagnosis","POTS symptoms","POTS diagnosis"))) 
  
  # male_temp_inf_testneg <- 
  #   map_df( setdiff( names_conditions$cohort_name, c("me_cfs_symptoms","dysautonomia","mis","t1dm","sle","ra","me_cfs",
  #                                                    "juvenile_arthritis","ibd","pots")) %>% set_names,
  #           cohort_wrap_func,
  #           interval = "overall",
  #           age_expre =  "0 to 150",
  #           sex_expre = "Male",
  #           numerator = "infection",
  #           denominator = "test_negative",
  #           .id = "conditions") %>% 
  #   mutate( conditions = factor(conditions,levels = c("t1dm","mis","ibd","sle","juvenile_arthritis","ra","me_cfs_symptoms",
  #                                                     "me_cfs","dysautonomia","pots"),
  #                               labels = c("T1DM","MIS","IBD","SLE","Juvenile arthritis","RA","ME/CFS symptoms",
  #                                          "ME/CFS diagnosis","POTS symptoms","POTS diagnosis"))) 
  # 
 
  
  # adults_temp_inf_testneg <- 
  #   map_df( setdiff( names_conditions$cohort_name, c("mis","t1dm","sle","ra","me_cfs","juvenile_arthritis","ibd",
  #                                                    "pots","dysautonomia","me_cfs_symptoms")) %>% set_names,
  #           cohort_wrap_func,
  #           interval = "overall",
  #           age_expre =  c("19 to 40","41 to 64"),
  #           sex_expre = "Both",
  #           numerator = "infection",
  #           denominator = "test_negative",
  #           .id = "conditions") %>% 
  #   mutate( conditions = factor(conditions,levels = c("t1dm","mis","ibd","sle","juvenile_arthritis","ra","me_cfs_symptoms",
  #                                                     "me_cfs","dysautonomia","pots"),
  #                               labels = c("T1DM","MIS","IBD","SLE","Juvenile arthritis","RA","ME/CFS symptoms",
  #                                          "ME/CFS diagnosis","POTS symptoms","POTS diagnosis"))) 
  
  elderly_temp_inf_testneg <- 
    map_df( setdiff( names_conditions$cohort_name, c("mis","t1dm","sle","ra","me_cfs","juvenile_arthritis","ibd","pots")) %>% set_names,
            cohort_wrap_func,
            interval = "overall",
            age_expre =  "65 to 150",
            sex_expre = "Both",
            numerator = "infection",
            denominator = "test_negative",
            .id = "conditions") %>% 
    mutate( conditions = factor(conditions,levels = c("t1dm","mis","ibd","sle","juvenile_arthritis","ra","me_cfs_symptoms",
                                                      "me_cfs","dysautonomia","pots"),
                                labels = c("T1DM","MIS","IBD","SLE","Juvenile arthritis","RA","ME/CFS symptoms",
                                           "ME/CFS diagnosis","POTS symptoms","POTS diagnosis"))) 
  # children_temp_inf_testneg <- 
  #   map_df( setdiff( names_conditions$cohort_name, c("mis","t1dm","sle","ra","me_cfs","juvenile_arthritis","ibd",
  #                                                    "pots","dysautonomia","me_cfs_symptoms")) %>% set_names,
  #           cohort_wrap_func,
  #           interval = "overall",
  #           age_expre =  c("0 to 6","7 to 11","12 to 18"),
  #           sex_expre = "Both",
  #           numerator = "infection",
  #           denominator = "test_negative",
  #           .id = "conditions") %>% 
  #   mutate( conditions = factor(conditions,levels = c("t1dm","mis","ibd","sle","juvenile_arthritis","ra","me_cfs_symptoms",
  #                                                     "me_cfs","dysautonomia","pots"),
  #                               labels = c("T1DM","MIS","IBD","SLE","Juvenile arthritis","RA","ME/CFS symptoms",
  #                                          "ME/CFS diagnosis","POTS symptoms","POTS diagnosis"))) 
  
  
  all_IRR_inf_testneg <- list( All = overall_temp_inf_testneg, 
                               Female = female_temp_inf_testneg,
                              
                               elderly = elderly_temp_inf_testneg)
  
  
  # Specify the Excel file path
  excel_file <- "all_IRR_inf_testneg_IPCI.xlsx"
  
  # Write the list of tibbles to different sheets in Excel
  write_xlsx(all_IRR_inf_testneg, excel_file)  
  
  
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
                         nrow = 1, ncol = 3,
                         labels = c("All",
                                    "Female",  
                                    "Elderly"), label_size = 8, label_y = 1.0,
                         align = "w")
  

pdf("supp_ipci.pdf",         # File name
    width = 9, height = 3, # Width and height in inches
    bg = "white",          # Background color
    colormodel = "cmyk")    # Color model (cmyk is required for most publications)

plot_grid( main_plot, manual_legend, ncol = 1, rel_heights = c(1.5, 0.1))

# Closing the graphical device
dev.off() 
 

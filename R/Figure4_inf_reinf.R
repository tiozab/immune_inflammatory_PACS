#AUSOM does not have reinfection (take it out), UiO has (put it in)
# specific populations
source(here::here("R","IRR function_reinf.R"))
incidence_estimates_help <- rbind(CPRDGOLD[[2]],
                                  IMASIS[[2]],
                                  Pharmetrics[[2]],
                                  IPCI[[2]],
                                  eDOL_CHUM[[2]],
                                  CORIVA[[2]],
                                  CPRDAurum[[2]],
                                  University_of_Oslo[[2]])



# re-infection over infection ------------------------------------------------------

overall_temp_reinf_inf <- 
  map_df( setdiff( names_conditions$cohort_name, c("juvenile_arthritis","mis","ibd","ra","sle","t1dm")) %>% set_names,
          cohort_wrap_func_reinf,
          interval = "overall",
          age_expre =  "0 to 150",
          sex_expre = "Both",
          numerator = "reinfection",
          denominator = "infection",
          .id = "conditions")  %>% 
  mutate( conditions = factor(conditions,levels = c("t1dm","mis","ibd","sle","juvenile_arthritis","ra","me_cfs_symptoms",
                                                    "me_cfs","dysautonomia","pots"),
                              labels = c("T1DM","MIS","IBD","SLE","Juvenile arthritis","RA","ME/CFS symptoms",
                                         "ME/CFS diagnosis","POTS symptoms","POTS diagnosis"))) 




all_IRR_reinf_inf <- list( All = overall_temp_reinf_inf)

# Specify the Excel file path
excel_file <- here::here("Results_final","all_IRR_reinf_inf_ALL.xlsx")

# Write the list of tibbles to different sheets in Excel
write_xlsx(all_IRR_reinf_inf, excel_file)

plot_func <- function( df){
  
  output <- ggplot( df, aes( x = IRR_random, y = conditions)) + 
    geom_point( size = 1.2, position = position_dodge(0.2)) +
    geom_errorbar( aes( xmin = IRR_low_random, xmax = IRR_upper_random), position = position_dodge(0.75), width = 0.3) +
    geom_vline( xintercept = 1) +
    scale_x_continuous(  limits = c(0.3, 4), trans = scales::log2_trans()) +
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


plot_list <- map( all_IRR_reinf_inf, plot_func)
empty <- ggplot() + theme_void()
manual_legend <- legend <- get_legend(# create some space to the left of the legend
  plot_list$All + theme(legend.box.margin = margin(0, 0, 0, 12)))

main_plot <- plot_grid(plot_list$All+ theme( legend.position = "none"),
                       
                       nrow = 1, ncol = 1,
                       labels = c("All"), label_size = 8, label_y = 1.0,
                       align = "w")

pdf(here::here("Results_final","figure4.pdf"),         # File name
    width = 6, height = 6, # Width and height in inches
    bg = "white",          # Background color
    colormodel = "cmyk")    # Color model (cmyk is required for most publications)

plot_grid( main_plot, manual_legend, ncol = 1, rel_heights = c(1.5, 0.1))

# Closing the graphical device
dev.off() 

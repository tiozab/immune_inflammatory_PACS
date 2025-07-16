#pharmetrics and UiO does not have test negative information (take it out)
# specific populations
source(here::here("R","IRR function.R"))
incidence_estimates_help <- rbind(CPRDGOLD[[2]],
                                 IMASIS[[2]],
                                  AUSOM[[2]],
                                  IPCI[[2]],
                                  eDOL_CHUM[[2]],
                                  CORIVA[[2]],
                                  CPRDAurum[[2]])


## infection over test_negative ------------------------------------------------------

overall_temp_inf_testneg <- 
  map_df( setdiff( names_conditions$cohort_name, c("juvenile_arthritis","mis")) %>% set_names,
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
  map_df( setdiff( names_conditions$cohort_name, c("juvenile_arthritis","mis")) %>% set_names,
          cohort_wrap_func,
          interval = "overall",
          age_expre =  "0 to 150",
          sex_expre = "Female",
          numerator = "infection",
          denominator = "test_negative",
          .id = "conditions")  %>% 
  mutate( conditions = factor(conditions,levels = c("t1dm","mis","ibd","sle","juvenile_arthritis","ra","me_cfs_symptoms",
                                                    "me_cfs","dysautonomia","pots"),
                              labels = c("T1DM","MIS","IBD","SLE","Juvenile arthritis","RA","ME/CFS symptoms",
                                         "ME/CFS diagnosis","POTS symptoms","POTS diagnosis"))) 


male_temp_inf_testneg <- 
  map_df( setdiff( names_conditions$cohort_name, c("juvenile_arthritis","mis","sle")) %>% set_names,
          cohort_wrap_func,
          interval = "overall",
          age_expre =  "0 to 150",
          sex_expre = "Male",
          numerator = "infection",
          denominator = "test_negative",
          .id = "conditions")  %>% 
  mutate( conditions = factor(conditions,levels = c("t1dm","mis","ibd","sle","juvenile_arthritis","ra","me_cfs_symptoms",
                                                    "me_cfs","dysautonomia","pots"),
                              labels = c("T1DM","MIS","IBD","SLE","Juvenile arthritis","RA","ME/CFS symptoms",
                                         "ME/CFS diagnosis","POTS symptoms","POTS diagnosis"))) 


## have to take out RA and SLE because of low count
children_temp_inf_testneg <-
  map_df( setdiff( names_conditions$cohort_name, c("juvenile_arthritis","mis","ra","sle")) %>% set_names,
          cohort_wrap_func,
          interval = "overall",
          age_expre =  c("0 to 6","7 to 11","12 to 18"),
          sex_expre = "Both",
          numerator = "infection",
          denominator = "test_negative",
          .id = "conditions")  %>% 
  mutate( conditions = factor(conditions,levels = c("t1dm","mis","ibd","sle","juvenile_arthritis","ra","me_cfs_symptoms",
                                                    "me_cfs","dysautonomia","pots"),
                              labels = c("T1DM","MIS","IBD","SLE","Juvenile arthritis","RA","ME/CFS symptoms",
                                         "ME/CFS diagnosis","POTS symptoms","POTS diagnosis"))) 


adult_temp_inf_testneg <- 
  map_df( setdiff( names_conditions$cohort_name, c("juvenile_arthritis","mis")) %>% set_names,
          cohort_wrap_func,
          interval = "overall",
          age_expre =  c("19 to 40","41 to 64"),
          sex_expre = "Both",
          numerator = "infection",
          denominator = "test_negative",
          .id = "conditions")  %>% 
  mutate( conditions = factor(conditions,levels = c("t1dm","mis","ibd","sle","juvenile_arthritis","ra","me_cfs_symptoms",
                                                    "me_cfs","dysautonomia","pots"),
                              labels = c("T1DM","MIS","IBD","SLE","Juvenile arthritis","RA","ME/CFS symptoms",
                                         "ME/CFS diagnosis","POTS symptoms","POTS diagnosis"))) 


elderly_temp_inf_testneg <- 
  map_df( setdiff( names_conditions$cohort_name, c("juvenile_arthritis","mis","sle")) %>% set_names,
          cohort_wrap_func,
          interval = "overall",
          age_expre =  "65 to 150",
          sex_expre = "Both",
          numerator = "infection",
          denominator = "test_negative",
          .id = "conditions")  %>% 
  mutate( conditions = factor(conditions,levels = c("t1dm","mis","ibd","sle","juvenile_arthritis","ra","me_cfs_symptoms",
                                                    "me_cfs","dysautonomia","pots"),
                              labels = c("T1DM","MIS","IBD","SLE","Juvenile arthritis","RA","ME/CFS symptoms",
                                         "ME/CFS diagnosis","POTS symptoms","POTS diagnosis"))) 

all_IRR_inf_testneg <- list( All = overall_temp_inf_testneg, 
                             Female = female_temp_inf_testneg,
                             Male = male_temp_inf_testneg, 
                             children = children_temp_inf_testneg,
                             adult = adult_temp_inf_testneg,
                             elderly = elderly_temp_inf_testneg)

# Specify the Excel file path
excel_file <- here::here("Results_final","all_IRR_inf_testneg_ALL.xlsx")

# Write the list of tibbles to different sheets in Excel
write_xlsx(all_IRR_inf_testneg, excel_file)

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

pdf(here::here("Results_final","figure2.pdf"),         # File name
    width = 6, height = 6, # Width and height in inches
    bg = "white",          # Background color
    colormodel = "cmyk")    # Color model (cmyk is required for most publications)

plot_grid( main_plot, manual_legend, ncol = 1, rel_heights = c(1.5, 0.1))

# Closing the graphical device
dev.off() 
 

# To get I2 plot for overall inf vs test negative

mean_I2 <- mean(overall_temp_inf_testneg$I2, na.rm = TRUE)*100

ggplot(overall_temp_inf_testneg, aes(x = I2*100, y = conditions)) +
  geom_point(size = 2, color = "#e41a1c") +
  geom_errorbarh(aes(xmin = I2_lower*100, xmax = I2_upper*100), height = 0.3, color = "#e41a1c") +
  geom_vline(xintercept = mean_I2, linetype = "dashed", color = "blue", linewidth = 1) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  labs(
    x = "I² (%)",
    y = "",
    title = "Heterogeneity (I²) across databases by condition"
  ) +
  annotate("text", x = mean_I2 + 5, y = Inf, label = paste0("Avg I² = ", round(mean_I2, 1), "%"), 
           hjust = 0, vjust = 2, color = "blue", size = 3.5) +
  theme_bw() +
  theme(
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    plot.title = element_text(hjust = 0.5)
  )

ggsave(here::here("Results_final", "Supplementary_I2.pdf"))


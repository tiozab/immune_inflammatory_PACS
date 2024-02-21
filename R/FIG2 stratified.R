## put the results from the individual data sources into one file

# general population
incidence_estimates_general_help <- rbind(CPRDGOLD[[1]],
                                          Pharmetrics[[1]],
                                         IMASIS[[1]],
                                          AUSOM[[1]],
                                        IPCI[[1]],
                                          eDOL_CHUM[[1]],
                                          CORIVA[[1]],
                                          CPRDAurum[[1]],
                                          University_of_Oslo[[1]])


# ### crude stratified plot ----------------
## Figure 2 - stratified

conditions_general <-
  incidence_estimates_general_help %>%
  filter( analysis_interval == "overall",
          denominator_age_group == "0 to 150",
          denominator_sex =="Both",
          n_events > 4 ,
          outcome_cohort_name %in% c("dysautonomia", "me_cfs_symptoms","pots",
                                     "me_cfs","mis", "t1dm")
          ) %>%
  mutate(
    outcome_cohort_name = fct_reorder(outcome_cohort_name, incidence_100000_pys, max, .desc = TRUE),
    database_name = fct_reorder(database_name, incidence_100000_pys, mean),
    care_sector = ifelse(database_name %in% c("Pharmetrics", "CORIVA", "University_of_Oslo"), "primary and secondary care",
                         ifelse(database_name %in% c("CPRDGOLD", "IPCI", "CPRDAurum"), "primary care", "secondary care"))
  ) %>%
  mutate(
    outcome_cohort_name = factor(outcome_cohort_name,
                                 levels = c("dysautonomia", "me_cfs_symptoms","pots",  "me_cfs", 
                                           # "ra","ibd",     "sle", "juvenile_arthritis", 
                                            "mis", "t1dm"),
                                 labels = c("POTS symptoms","ME/CFS symptoms","POTS diagnosis","ME/CFS diagnosis",
                                           # "RA","IBD","SLE", "JIA", 
                                            "MIS", "T2DM")), 
         database_name = factor(database_name, 
                                levels = c("Pharmetrics","CORIVA","University_of_Oslo","CPRDGOLD","CPRDAurum","IPCI","IMASIS","eDOL_CHUM","AUSOM"), 
                                labels = c("PharMetrics Plus for Academics","CORIVA","NPR","CPRD Gold","CPRD Aurum","IPCI","IMASIS","CHUM","AUSOM"))
  ) 


# Specify the Excel file path
excel_file <- "conditions_general_fig2.xlsx"

# Write the list of tibbles to different sheets in Excel
write_xlsx(conditions_general, excel_file)  

pdf("figure2_stratified.pdf",         # File name
    width = 9, height = 6, # Width and height in inches
    bg = "white",          # Background color
    colormodel = "srgb")   # Color model 


ggplot(conditions_general, aes( x = outcome_cohort_name, y = incidence_100000_pys, color = database_name)) +
  geom_point( size = 2) +
  geom_errorbar(aes(ymin = incidence_100000_pys_95CI_lower, ymax =  incidence_100000_pys_95CI_upper), width=0.2) +
  labs( x = "", y = " Incidence (per 100'000 person-years) [log scale]") +
  scale_y_continuous(label=label_comma(accuracy= 1), trans="pseudo_log",
                     breaks = c(0,1,5,10,50,100,500,1000,5000,10000),
                     limits = c(0, 10000)) + 
  scale_color_discrete(name = "", guide = 'legend') +
  theme_bw()+
  theme( text = element_text( family = "serif", color = "black"),
         axis.text.x = element_text( angle = 45, size = 10, hjust = 1),
         axis.text.y = element_text( size = 10),
         panel.grid.minor = element_blank(),
         legend.direction = 'horizontal', 
         legend.text = element_text(angle = 0, colour = 'black'),
         legend.position = 'bottom'
         ) +
  facet_wrap(~care_sector, scales = "free_y", ncol = 3)   +
  guides(color = guide_legend(ncol = 3))


# Closing the graphical device
dev.off() 



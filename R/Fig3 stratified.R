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



#Figure 3_stratified

conditions_year  <-
  incidence_estimates_general_help %>%
  filter( analysis_interval == "years",
          denominator_age_group == "0 to 150",
          denominator_sex =="Both",
          n_events > 4 ,
          outcome_cohort_name %in% c("dysautonomia", "me_cfs_symptoms","pots",
                                     "me_cfs","mis", "t1dm")
  ) %>%
  group_by( database_name, outcome_cohort_name) %>%
  mutate( number_point = n()) %>%
  ungroup() %>%
  filter( number_point > 1) %>%
  mutate(
    care_sector = ifelse(database_name %in% c("Pharmetrics", "CORIVA", "University_of_Oslo"), "both",
                         ifelse(database_name %in% c("CPRDGOLD", "IPCI", "CPRDAurum"), "primary care", "secondary care")),
    outcome_cohort_name = factor(outcome_cohort_name,
                                 levels = c("dysautonomia", "pots","me_cfs_symptoms",  "me_cfs", 
                                            # "ra","ibd",     "sle", "juvenile_arthritis", 
                                            "mis", "t1dm"),
                                 labels = c("POTS symptoms","POTS diagnosis","ME/CFS symptoms","ME/CFS diagnosis",
                                            # "RA","IBD","SLE", "JIA", 
                                            "MIS", "T2DM")), 
    database_name = factor(database_name, 
                           levels = c("Pharmetrics","CORIVA","University_of_Oslo","CPRDGOLD","CPRDAurum","IPCI","IMASIS","eDOL_CHUM","AUSOM"), 
                           labels = c("P+","CORIVA","NPR","CPRD Gold","CPRD Aurum","IPCI","IMASIS","CHUM","AUSOM"))
    
  )



# Specify the Excel file path
excel_file <- "conditions_year_fig3.xlsx"

# Write the list of tibbles to different sheets in Excel
write_xlsx(conditions_year, excel_file)  

pdf("figure3_stratified.pdf",         # File name
    width = 5, height = 6, # Width and height in inches
    bg = "white",          # Background color
    colormodel = "srgb")   # Color model


ggplot(conditions_year, aes( x = factor(year_index), y = incidence_100000_pys, group = database_name, color = database_name)) +
  geom_smooth( method = "lm",
               formula = y ~ poly(x, 1),
               se = FALSE) +
  geom_point(size = 0.5) +
  geom_errorbar(aes(ymin = incidence_100000_pys_95CI_lower, ymax =  incidence_100000_pys_95CI_upper), width=0.2) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))+
  labs( x = "", y = " Incidence (per 100'000 person-years) [log scale]") +
  scale_y_continuous(label=label_comma(accuracy= 1), trans="pseudo_log",
                     breaks = c(1,10,100,1000,10000)) +
  scale_color_discrete(name = "") +
  facet_grid(outcome_cohort_name ~ care_sector, scales = "free_y", space = "free_y", switch = "y") +
  theme_classic()+
  theme( text = element_text( family = "serif", color = "black"),
         axis.text.x = element_text( angle = 0, size = 7, vjust = 0.5),
         axis.text.y = element_text( size = 8),
         panel.grid.major = element_blank(),
         panel.grid.minor.x = element_blank(),
         legend.background = element_rect(fill='transparent'),
         legend.position = "right",
         strip.text = element_text(size = 8))+
  guides(color = guide_legend(ncol = 1)) 


# Closing the graphical device
dev.off()


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

# Stratification by age ---------------------------------------
# Incidence by age general population
# Figure 4

conditions_age <- 
  incidence_estimates_general_help %>% 
  filter( analysis_interval == "overall", 
          denominator_sex == "Both",
          denominator_age_group != "0 to 150",
          n_events > 4,
          outcome_cohort_name %in% c("dysautonomia", "me_cfs_symptoms","pots",
                                     "me_cfs","mis", "t1dm") ) %>%
  group_by( database_name, outcome_cohort_name) %>% 
  mutate(number_point = n()) %>% 
  ungroup() %>% 
  filter(number_point > 2) %>% 
  mutate(denominator_age_group = factor(denominator_age_group, levels=c("0 to 6","7 to 11","12 to 18","19 to 40","41 to 64","65 to 150"), 
                                        labels = c("0-6","7-11","12-18","19-40","41-64",">64"))
  ) %>% 
  mutate(       care_sector = ifelse(database_name %in% c("Pharmetrics", "CORIVA", "University_of_Oslo"), "both",
                                     ifelse(database_name %in% c("CPRDGOLD", "IPCI", "CPRDAurum"), "primary care", "secondary care")),
                outcome_cohort_name = factor(outcome_cohort_name,
                                             levels = c("dysautonomia","pots", "me_cfs_symptoms",  "me_cfs", 
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
excel_file <- "conditions_age_fig4_stratified.xlsx"

# Write the list of tibbles to different sheets in Excel
write_xlsx(conditions_age, excel_file)  

pdf("figure4_stratified.pdf",         # File name
    width = 6, height = 6, # Width and height in inches
    bg = "white",          # Background color
    colormodel = "srgb")   # Color model 


ggplot(
  conditions_age, aes( x = denominator_age_group, y = incidence_100000_pys, group = database_name, color = database_name)) + 
  geom_smooth( method = "lm",
               formula = y ~ poly(x, 3),
               se = FALSE) +
  geom_point(size = 0.5) +
  geom_errorbar(aes(ymin = incidence_100000_pys_95CI_lower, ymax =  incidence_100000_pys_95CI_upper), width=0.2) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))+
  labs( x = "age [years]", y = "Incidence (per 100'000 person-years) ") +
  scale_y_continuous(label=label_comma(accuracy= 1), trans="pseudo_log",
                     breaks = c(10,100,1000,10000)) +
  scale_color_discrete(name = "", guide = 'legend') +
  facet_grid(outcome_cohort_name ~ care_sector, scales = "free_y", space = "free_y", switch = "y") +
  theme_classic()+
  theme( text = element_text( family = "serif", color = "black"),
         axis.text.x = element_text( angle = 45, size = 8, hjust = 1),
         axis.text.y = element_text( size = 8),
         panel.grid.major = element_blank(),
         panel.grid.minor.x = element_blank(),
         legend.background = element_rect(fill='transparent'),
         legend.position = "right",
strip.text = element_text(size = 7))+
  guides(color = guide_legend(ncol = 1)) 


# Closing the graphical device
dev.off() 




# Stratification by sex ---------------------------------------------------
# Figure 5

# Incidence by sex in general population
conditions_sex <- 
  incidence_estimates_general_help %>% 
  filter( analysis_interval == "overall",
          denominator_sex != "Both",
          denominator_age_group == "0 to 150",
          n_events > 4,
          outcome_cohort_name %in% c("dysautonomia", "me_cfs_symptoms","pots",
                                     "me_cfs","mis", "t1dm") ) %>% 
  mutate(    care_sector = ifelse(database_name %in% c("Pharmetrics", "CORIVA", "University_of_Oslo"), "both",
                                  ifelse(database_name %in% c("CPRDGOLD", "IPCI", "CPRDAurum"), "primary care", "secondary care")),
             outcome_cohort_name = factor(outcome_cohort_name,
                                          levels = c("dysautonomia", "me_cfs_symptoms","mis",
                                                     # "ra","ibd",     "sle", "juvenile_arthritis", 
                                                      "pots",  "me_cfs", "t1dm"),
                                          labels = c("POTS symptoms","ME/CFS symptoms","MIS", 
                                                     # "RA","IBD","SLE", "JIA", 
                                                     "POTS diagnosis","ME/CFS diagnosis","T2DM")), 
             database_name = factor(database_name, 
                                    levels = c("Pharmetrics","CORIVA","University_of_Oslo","CPRDGOLD","CPRDAurum","IPCI","IMASIS","eDOL_CHUM","AUSOM"), 
                                    labels = c("P+","CORIVA","NPR","CPRD Gold","CPRD Aurum","IPCI","IMASIS","CHUM","AUSOM"))
             
  )


# Specify the Excel file path
excel_file <- "conditions_sex_fig5_stratified.xlsx"

# Write the list of tibbles to different sheets in Excel
write_xlsx(conditions_sex, excel_file)  

pdf("figure5_stratified.pdf",         # File name
    width = 9, height = 6, # Width and height in inches
    bg = "white",          # Background color
    colormodel = "cmyk")   # Color model 


ggplot(conditions_sex, aes( x = database_name, y = incidence_100000_pys,  
                                         fill = denominator_sex)) + 
  geom_errorbar( aes( ymin = incidence_100000_pys, ymax = incidence_100000_pys_95CI_upper), 
                 position = position_dodge(0.5), width = 0.25) +
  geom_col( position = position_dodge(0.5), width = 0.5) + scale_fill_grey() +
  scale_y_continuous( breaks = scales::breaks_extended(5)) +
  labs( x = "", y = "Incidence (per 100'000 person-years) ", color = "", fill = "") + 
  facet_wrap( vars( outcome_cohort_name), scales = "free_y", drop = TRUE)+
  theme_bw() +
  theme( text = element_text( family = "serif", color = "black"),
         axis.text.x = element_text( angle = 45, size = 8, hjust = 1),
         axis.text.y = element_text( size = 8),
         panel.grid.major = element_blank(),
         legend.background = element_rect(fill='transparent'),
         legend.position = "top",
strip.text = element_text(size = 8))+
  guides(color = guide_legend(ncol = 1)) 


# Closing the graphical device
dev.off() 


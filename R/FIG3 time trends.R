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




#### TIME TRENDS -----------------------------------------------------------
#Trend of incidence by years in general population

#Figure 3_nonstratified

conditions_year  <-
  incidence_estimates_general_help %>%
  filter( analysis_interval == "years",
          denominator_age_group == "0 to 150",
          denominator_sex =="Both",
          n_events > 4 ) %>%
  group_by( database_name, outcome_cohort_name) %>%
  mutate( number_point = n()) %>%
  ungroup() %>%
  filter( number_point > 1) %>%
  mutate(outcome_cohort_name = factor(outcome_cohort_name,
                                      levels = c("pots","dysautonomia","me_cfs","me_cfs_symptoms","ra",
                                                 "juvenile_arthritis","sle","ibd","mis","t1dm"),
                                      labels = c("POTS diagnosis","POTS symptoms","ME/CFS diagnosis","ME/CFS symptoms","RA",
                                                 "JIA","SLE","IBD","MIS","T2DM")),
           database_name = factor(database_name, 
                                levels = c("CPRDGOLD","CPRDAurum","IPCI","Pharmetrics","University_of_Oslo","IMASIS","eDOL_CHUM","AUSOM","CORIVA"), 
                                labels = c("CPRD Gold","CPRD Aurum","IPCI","PharMetrics Plus for Academics","NPR","IMASIS","CHUM","AUSOM","CORIVA"))
         
  )


# Specify the Excel file path
excel_file <- "conditions_year_fig3.xlsx"

# Write the list of tibbles to different sheets in Excel
write_xlsx(conditions_year, excel_file)  

pdf("figure3.pdf",         # File name
    width = 9, height = 6, # Width and height in inches
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
  facet_wrap( vars( outcome_cohort_name), scales = "free_y", drop = TRUE)+
  theme_classic()+
  theme( text = element_text( family = "serif", color = "black"),
         axis.text.x = element_text( angle = 0, size = 7, vjust = 0.5),
         axis.text.y = element_text( size = 8),
         panel.grid.major = element_blank(),
         panel.grid.minor.x = element_blank(),
         legend.background = element_rect(fill='transparent'),
         legend.position = "bottom")


# Closing the graphical device
dev.off()


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



# ### crude overall plot ----------------
## Figure 2

conditions_general <-
  incidence_estimates_general_help %>%
  filter( analysis_interval == "overall",
          denominator_age_group == "0 to 150",
          denominator_sex =="Both",
          n_events > 4 ) %>%
  mutate( outcome_cohort_name = fct_reorder( outcome_cohort_name, incidence_100000_pys, max, .desc = TRUE)
  ) %>%
  mutate(
    outcome_cohort_name = factor(outcome_cohort_name,
                                 levels = c("dysautonomia", "me_cfs_symptoms","pots",  "me_cfs", "ra","ibd",
                                            "sle", "juvenile_arthritis",  "mis", "t1dm"),
                                 labels = c("POTS symptoms","ME/CFS symptoms","POTS diagnosis","ME/CFS diagnosis","RA","IBD",
                                            "SLE", "JIA",  "MIS", "T2DM")), 
    database_name = factor(database_name, 
                           levels = c("Pharmetrics","CORIVA","University_of_Oslo","CPRDGOLD","CPRDAurum","IPCI","IMASIS","eDOL_CHUM","AUSOM"), 
                           labels = c("PharMetrics Plus for Academics","NPR","CORIVA","CPRD Gold","CPRD Aurum","IPCI","IMASIS","CHUM","AUSOM"))
  )


# Specify the Excel file path
excel_file <- "conditions_general_fig2.xlsx"

# Write the list of tibbles to different sheets in Excel
write_xlsx(conditions_general, excel_file)  

pdf("figure2.pdf",         # File name
    width = 6, height = 6, # Width and height in inches
    bg = "white",          # Background color
    colormodel = "srgb")   # Color model


ggplot(conditions_general, aes( x = outcome_cohort_name, y = incidence_100000_pys, color = database_name)) +
  geom_point( size = 1) +
  geom_errorbar(aes(ymin = incidence_100000_pys_95CI_lower, ymax =  incidence_100000_pys_95CI_upper), width=0.2) +
  labs( x = "", y = " Incidence (per 100'000 person-years) [log scale]") +
  scale_y_continuous(label=label_comma(accuracy= 1), trans="pseudo_log",
                     breaks = c(10,100,1000,10000)) +
  scale_color_discrete(name = "", guide = 'legend') +
  theme_classic()+
  theme( text = element_text( family = "serif", color = "black"),
         axis.text.x = element_text( angle = 45, size = 12, hjust = 1),
         axis.text.y = element_text( size = 10),
         panel.grid.major = element_blank(),
         legend.position = "bottom",  # Adjust the legend position
         legend.box.margin = margin(5, 5, 5, 5),  # Add margin to the legend box
         legend.text = element_text(size = 10),  # Adjust the size of legend text
         legend.title = element_text(size = 10) 
  ) +
  guides(color = guide_legend(ncol = 2))

# Closing the graphical device
dev.off()

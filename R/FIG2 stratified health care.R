source(here::here("R","getData.R"))

# ### crude overall plot ----------------
## Figure 2 - stratified

conditions_general <-
  incidence_estimates_general_help %>%
  filter( analysis_interval == "overall",
          denominator_age_group == "0 to 150",
          denominator_sex =="Both") %>%
  mutate( outcome_cohort_name = fct_reorder( outcome_cohort_name, incidence_100000_pys, max, .desc = TRUE),
          database_name = fct_reorder( database_name, incidence_100000_pys, mean),
          care_sector = ifelse(database_name %in% c("CPRDGOLD","IPCI","Pharmetrics","CORIVA"),"primary care","secondary care")
  ) %>% 
  mutate(outcome_cohort_name = factor(outcome_cohort_name,
                                      levels = c("pots","dysautonomia","me_cfs","me_cfs_symptoms","ra",
                                                 "juvenile_arthritis","sle","ibd","mis","t1dm"),
                                      labels = c("POTS diagnosis","POTS symptoms","ME/CFS diagnosis","ME/CFS symptoms","RA",
                                                 "Juvenile arthritis","SLE","IBD","MIS","T2DM")), 
         database_name = factor(database_name, 
                                levels = c("CPRDGOLD","IMASIS","Pharmetrics","eDOL_CHUM","IPCI","AUSOM","CORIVA"), 
                                labels = c("CPRD Gold","IMASIS","pharMetrics Plus f.A.","CHUM","IPCI","AUSOM","CORIVA"))
  ) 



pdf("figure2_stratified.pdf",         # File name
    width = 5, height = 6, # Width and height in inches
    bg = "white",          # Background color
    colormodel = "srgb")   # Color model 


ggplot(conditions_general, aes( x = outcome_cohort_name, y = incidence_100000_pys, color = database_name)) +
  geom_point( size = 1) +
  facet_grid(care_sector~.) + 
  geom_errorbar(aes(ymin = incidence_100000_pys_95CI_lower, ymax =  incidence_100000_pys_95CI_upper), width=0.2) +
  labs( x = "", y = " Incidence (per 100'000 person-years) [log scale]") +
  scale_y_continuous(label=label_comma(accuracy= 1), trans="pseudo_log",
                     breaks = c(1,5,10,50,100,500,1000,5000,10000)) + 
  scale_color_discrete(name = "", guide = 'legend') +
  theme_bw()+
  theme( text = element_text( family = "serif", color = "black"),
         axis.text.x = element_text( angle = 45, size = 10, hjust = 1),
         axis.text.y = element_text( size = 10),
         panel.grid.minor = element_blank(),
         legend.direction = 'horizontal', 
         legend.text = element_text(angle = 0, colour = 'black'),
         legend.position = 'bottom'
         )


# Closing the graphical device
dev.off() 


# 
# 
# # ### crude overall plot ----------------
# ## Figure 2
# 
# conditions_general <-
#   incidence_estimates_general_help %>%
#   filter( analysis_interval == "overall",
#           denominator_age_group == "0 to 150",
#           denominator_sex =="Both") %>%
#   mutate( outcome_cohort_name = fct_reorder( outcome_cohort_name, incidence_100000_pys, max, .desc = TRUE),
#           database_name = fct_reorder( database_name, incidence_100000_pys, mean)
#   ) %>% 
#   mutate(outcome_cohort_name = factor(outcome_cohort_name,
#                                       levels = c("pots","dysautonomia","me_cfs","me_cfs_symptoms","ra",
#                                                  "juvenile_arthritis","sle","ibd","mis","t1dm"),
#                                       labels = c("POTS diagnosis","POTS symptoms","ME/CFS diagnosis","ME/CFS symptoms","RA",
#                                                  "Juvenile arthritis","SLE","IBD","MIS","T2DM")), 
#     database_name = factor(database_name, 
#                            levels = c("CPRDGOLD","IMASIS","Pharmetrics","eDOL_CHUM","IPCI","AUSOM","CORIVA"), 
#                            labels = c("CPRD Gold","IMASIS","pharMetrics Plus for Academics","CHUM","IPCI","AUSOM","CORIVA"))
#   )
# 
# 
# pdf("figure2.pdf",         # File name
#     width = 9, height = 6, # Width and height in inches
#     bg = "white",          # Background color
#     colormodel = "srgb")   # Color model 
# 
# 
# ggplot(conditions_general, aes( x = outcome_cohort_name, y = incidence_100000_pys, color = database_name)) +
#   geom_point( size = 1) +
#   geom_errorbar(aes(ymin = incidence_100000_pys_95CI_lower, ymax =  incidence_100000_pys_95CI_upper), width=0.2) +
#   labs( x = "", y = " Incidence (per 100'000 person-years) [log scale]") +
#   scale_y_continuous(label=label_comma(accuracy= 1), trans="pseudo_log",
#                      breaks = c(10,100,1000,10000)) + 
#   scale_color_discrete(name = "", guide = 'legend') +
#   theme_classic()+
#   theme( text = element_text( family = "serif", color = "black"),
#          axis.text.x = element_text( angle = 45, size = 12, hjust = 1),
#          axis.text.y = element_text( size = 10),
#          panel.grid.major = element_blank())
# 
# 
# # Closing the graphical device
# dev.off() 
# 

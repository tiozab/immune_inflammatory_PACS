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
          care_sector = ifelse(database_name %in% c("CPRDGOLD","IPCI","Pharmetrics"),"primary care","secondary care")
  ) %>% 
  mutate(outcome_cohort_name = factor(outcome_cohort_name,
                                      levels = c("pots","dysautonomia","me_cfs","me_cfs_symptoms","ra",
                                                 "juvenile_arthritis","sle","ibd","mis","t1dm"),
                                      labels = c("POTS","Dysautonomia","ME/CFS diagnosis","ME/CFS symptoms","RA",
                                                 "Juvenile arthritis","SLE","IBD","MIS","T2DM")), 
         database_name = factor(database_name, 
                                levels = c("CPRDGOLD","Pharmetrics","IMASIS","eDOL_CHUM","AUSOM","IPCI"), 
                                labels = c("CPRD Gold","Pharmetrics Plus","IMASIS","CHUM","AUSOM","IPCI"))
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
  scale_color_discrete(name = "Database:", guide = 'legend') +
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




# ### crude overall plot ----------------
## Figure 2

conditions_general <-
  incidence_estimates_general_help %>%
  filter( analysis_interval == "overall",
          denominator_age_group == "0 to 150",
          denominator_sex =="Both") %>%
  mutate( outcome_cohort_name = fct_reorder( outcome_cohort_name, incidence_100000_pys, max, .desc = TRUE),
          database_name = fct_reorder( database_name, incidence_100000_pys, mean)
  ) %>% 
  mutate(outcome_cohort_name = factor(outcome_cohort_name,
                                      levels = c("pots","dysautonomia","me_cfs","me_cfs_symptoms","ra",
                                                 "juvenile_arthritis","sle","ibd","mis","t1dm"),
                                      labels = c("POTS","Dysautonomia","ME/CFS diagnosis","ME/CFS symptoms","RA",
                                                 "Juvenile arthritis","SLE","IBD","MIS","T2DM")), 
    database_name = factor(database_name, 
                           levels = c("CPRDGOLD","Pharmetrics","IMASIS","eDOL_CHUM","AUSOM","IPCI"), 
                           labels = c("CPRD Gold","Pharmetrics Plus","IMASIS","CHUM","AUSOM","IPCI"))
  )


pdf("figure2.pdf",         # File name
    width = 9, height = 6, # Width and height in inches
    bg = "white",          # Background color
    colormodel = "srgb")   # Color model 


ggplot(conditions_general, aes( x = outcome_cohort_name, y = incidence_100000_pys, color = database_name)) +
  geom_point( size = 1) +
  geom_errorbar(aes(ymin = incidence_100000_pys_95CI_lower, ymax =  incidence_100000_pys_95CI_upper), width=0.2) +
  labs( x = "", y = " Incidence (per 100'000 person-years) [log scale]") +
  scale_y_continuous(label=label_comma(accuracy= 1), trans="pseudo_log",
                     breaks = c(10,100,1000,10000)) + 
  scale_color_discrete(name = "Database") +
  theme_classic()+
  theme( text = element_text( family = "serif", color = "black"),
         axis.text.x = element_text( angle = 45, size = 12, hjust = 1),
         axis.text.y = element_text( size = 10),
         panel.grid.major = element_blank())


# Closing the graphical device
dev.off() 



#### TIME TRENDS -----------------------------------------------------------
# Trend of incidence by years in general population
# Figure 3
# 
# conditions_year  <-
#   incidence_estimates_general_help %>%
#   filter( analysis_interval == "years",
#           denominator_age_group == "0 to 150",
#           denominator_sex =="Both") %>%
#   group_by( database_name, outcome_cohort_name) %>%
#   mutate( number_point = n()) %>%
#   ungroup() %>%
#   filter( number_point > 1) %>%
#   mutate(outcome_cohort_name = factor(outcome_cohort_name,
#                                       levels = c("pots","dysautonomia","me_cfs","me_cfs_symptoms","ra",
#                                                  "juvenile_arthritis","sle","ibd","mis","t1dm"),
#                                       labels = c("POTS","Dysautonomia","ME/CFS diagnosis","ME/CFS symptoms","RA",
#                                                  "Juvenile arthritis","SLE","IBD","MIS","T2DM")),
#          database_name = factor(database_name,
#                                 levels = c("CPRDGOLD","Pharmetrics","IMASIS","eDOL_CHUM","AUSOM","IPCI"),
#                                 labels = c("CPRD Gold","PharMetrics Plus","IMASIS","CHUM","AUSOM","IPCI"))
#   )
# 
# 
# 
# pdf("figure3.pdf",         # File name
#     width = 9, height = 6, # Width and height in inches
#     bg = "white",          # Background color
#     colormodel = "srgb")   # Color model
# 
# 
# ggplot(conditions_year, aes( x = factor(year_index), y = incidence_100000_pys, group = database_name, color = database_name)) +
#   geom_smooth( method = "lm",
#                formula = y ~ poly(x, 2),
#                se = FALSE) +
#   geom_point(size = 0.5) +
#   geom_errorbar(aes(ymin = incidence_100000_pys_95CI_lower, ymax =  incidence_100000_pys_95CI_upper), width=0.2) +
#   guides(color = guide_legend(nrow = 2, byrow = TRUE))+
#   labs( x = "", y = " Incidence (per 100'000 person-years) [log scale]") +
#   scale_y_continuous(label=label_comma(accuracy= 1), trans="pseudo_log",
#                      breaks = c(1,10,100,1000,10000)) +
#   scale_color_discrete(name = "Database") +
#   facet_wrap( vars( outcome_cohort_name), scales = "free_y", drop = TRUE)+
#   theme_classic()+
#   theme( text = element_text( family = "serif", color = "black"),
#          axis.text.x = element_text( angle = 0, size = 7, vjust = 0.5),
#          axis.text.y = element_text( size = 8),
#          panel.grid.major = element_blank(),
#          panel.grid.minor.x = element_blank(),
#          legend.background = element_rect(fill='transparent'),
#          legend.position = "top")
# 
# 
# # Closing the graphical device
# dev.off()
# 

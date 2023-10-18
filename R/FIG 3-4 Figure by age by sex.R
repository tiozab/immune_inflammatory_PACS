source(here::here("R","getData.R"))

# Stratification by age ---------------------------------------
# Incidence by age general population
# Figure 3

conditions_age <- 
  incidence_estimates_general_help %>% 
  filter( analysis_interval == "overall", 
          denominator_sex == "Both",
          denominator_age_group != "0 to 150") %>% 
  group_by( database_name, outcome_cohort_name) %>% 
  mutate( number_point = n()) %>% 
  ungroup() %>% 
  filter( number_point > 2) %>% 
  mutate(denominator_age_group = factor(denominator_age_group, levels=c("0 to 6","7 to 11","12 to 18","19 to 40","41 to 64","65 to 150"), 
                                        labels = c("0-6","7-11","12-18","19-40","41-64",">64"))
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


pdf("figure3.pdf",         # File name
    width = 9, height = 6, # Width and height in inches
    bg = "white",          # Background color
    colormodel = "srgb")   # Color model 


ggplot(
  conditions_age, aes( x = denominator_age_group, y = incidence_100000_pys, group = database_name, color = database_name)) + 
  geom_smooth( method = "lm",
               formula = y ~ poly(x, 2),
               se = FALSE) +
  geom_point(size = 0.5) +
  geom_errorbar(aes(ymin = incidence_100000_pys_95CI_lower, ymax =  incidence_100000_pys_95CI_upper), width=0.2) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))+
  labs( x = "age [years]", y = "Incidence (per 100'000 person-years) ") +
  scale_color_discrete(name = "Database") +
  facet_wrap( vars( outcome_cohort_name), scales = "free_y", drop = TRUE)+
  theme_classic()+
  theme( text = element_text( family = "serif", color = "black"),
         axis.text.x = element_text( angle = 45, size = 8, hjust = 1),
         axis.text.y = element_text( size = 8),
         panel.grid.major = element_blank(),
         panel.grid.minor.x = element_blank(),
         legend.background = element_rect(fill='transparent'),
         legend.position = "top")


# Closing the graphical device
dev.off() 




# Stratification by sex ---------------------------------------------------
# Figure 4

# Incidence by sex in general population
conditions_sex <- 
  incidence_estimates_general_help %>% 
  filter( analysis_interval == "overall",
          denominator_sex != "Both",
          denominator_age_group == "0 to 150") %>% 
  mutate(outcome_cohort_name = factor(outcome_cohort_name,
                                      levels = c("pots","dysautonomia","me_cfs","me_cfs_symptoms","ra",
                                                 "juvenile_arthritis","sle","ibd","mis","t1dm"),
                                      labels = c("POTS","Dysautonomia","ME/CFS diagnosis","ME/CFS symptoms","RA",
                                                 "Juvenile arthritis","SLE","IBD","MIS","T2DM")), 
         database_name = factor(database_name, 
                                levels = c("CPRDGOLD","Pharmetrics","IMASIS","eDOL_CHUM","AUSOM","IPCI"), 
                                labels = c("CPRD Gold","Pharmetrics Plus","IMASIS","CHUM","AUSOM","IPCI")),
         denominator_sex = factor(denominator_sex, levels=c("Female","Male","Both"), 
                                                 labels = c("Female","Male","All"))
  )


pdf("figure4.pdf",         # File name
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
         legend.position = "top")


# Closing the graphical device
dev.off() 


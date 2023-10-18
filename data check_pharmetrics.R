# Load packages
library(dplyr)
library(tidyverse)
library(cowplot)
library(skimr)
library(ggsci)
library(janitor)
library(meta)
library(stringr)
library(tibble)
library(readr)

wd <- getwd()
data_path <- "/results/Pharmetrics/IP/"

#load general population results
Allpop_MC_Age <- read_csv(paste0(wd,data_path, "Allpop_MC_Age.csv"))
Allpop_MC_AllandSex <- read_csv(paste0(wd,data_path, "Allpop_MC_AllandSex.csv"))

#load other population results
Inf_Age <- read_csv(paste0(wd,data_path, "Inf_Age.csv"))
Inf_AllandSex <- read_csv(paste0(wd,data_path, "Inf_AllandSex.csv"))
Reinf_Age <- read_csv(paste0(wd,data_path, "Reinf_Age.csv"))
Reinf_AllandSex <- read_csv(paste0(wd,data_path, "Reinf_AllandSex.csv"))

# double check outcomes
unique(Allpop_MC_Age$outcome_cohort_name)
unique(Allpop_MC_AllandSex$outcome_cohort_name)

unique(Inf_Age$outcome_cohort_name)
unique(Inf_AllandSex$outcome_cohort_name)

unique(Reinf_Age$outcome_cohort_name)
unique(Reinf_AllandSex$outcome_cohort_name)

#load outcome names
names_conditions <- read_csv("names_conditions.csv")

# double check age groups
unique(Allpop_MC_Age$denominator_age_group)
unique(Allpop_MC_AllandSex$denominator_age_group)

unique(Inf_Age$denominator_age_group)
unique(Inf_AllandSex$denominator_age_group)

unique(Reinf_Age$denominator_age_group)
unique(Reinf_AllandSex$denominator_age_group)

# double check sex
unique(Allpop_MC_Age$denominator_sex)
unique(Allpop_MC_AllandSex$denominator_sex)

unique(Inf_Age$denominator_sex)
unique(Inf_AllandSex$denominator_sex)

unique(Reinf_Age$denominator_sex)
unique(Reinf_AllandSex$denominator_sex)

#double check analysis interval
unique(Allpop_MC_Age$analysis_interval)
unique(Allpop_MC_AllandSex$analysis_interval)

unique(Inf_Age$analysis_interval)
unique(Inf_AllandSex$analysis_interval)

unique(Reinf_Age$analysis_interval)
unique(Reinf_AllandSex$analysis_interval)

# remove inf_ from the outcome_cohort_name

Inf_Age <- Inf_Age %>% 
  mutate(
  outcome_cohort_name = gsub("^.*?_","",outcome_cohort_name))

Inf_AllandSex <- Inf_AllandSex %>% 
  mutate(
    outcome_cohort_name = gsub("^.*?_","",outcome_cohort_name))

Reinf_Age <- Reinf_Age %>% 
  mutate(
    outcome_cohort_name = gsub("^.*?_","",outcome_cohort_name))

Reinf_AllandSex <- Reinf_AllandSex %>% 
  mutate(
    outcome_cohort_name = gsub("^.*?_","",outcome_cohort_name))


#put them together
incidence_estimates_general_help <- rbind(Allpop_MC_Age,Allpop_MC_AllandSex)
incidence_estimates_help <- rbind(Inf_Age,Inf_AllandSex,Reinf_Age,Reinf_AllandSex)



# Tidy dataframes ------------------------------------------------------

# general population

incidence_estimates_general_help <- 
  incidence_estimates_general_help %>%  
  select(analysis_interval, cdm_name, outcome_cohort_name, n_persons, person_years, n_events, 
         incidence_100000_pys, incidence_100000_pys_95CI_lower, incidence_100000_pys_95CI_upper,
         denominator_age_group, denominator_sex, incidence_start_date, incidence_end_date) %>%
  filter(outcome_cohort_name %in% names_conditions$cohort_name,
         !is.na(n_events)
         ) %>% 
  mutate(year_index = year(incidence_start_date),
         month_index = month(incidence_start_date),
         year_month = paste(year_index, sprintf("%02d", month_index), sep = "-"))

#have a look at the table
skimr::skim(incidence_estimates_general_help)


# infection and reinfection cohorts

incidence_estimates_help <- 
  incidence_estimates_help %>%  
  select(denominator_strata_cohort_name,analysis_interval, cdm_name, outcome_cohort_name, n_persons, person_years, n_events, 
         incidence_100000_pys, incidence_100000_pys_95CI_lower, incidence_100000_pys_95CI_upper,
         denominator_age_group, denominator_sex, incidence_start_date, incidence_end_date) %>%
  filter(outcome_cohort_name %in% names_conditions$cohort_name,
         !is.na(n_events)
  ) %>% 
  mutate(year_index = year(incidence_start_date),
         month_index = month(incidence_start_date),
         year_month = paste(year_index, sprintf("%02d", month_index), sep = "-"))

#have a look at the table
skimr::skim(incidence_estimates_help)

#results -----------------------------------------------------------------------------
#### Overall results in general population

conditions_general <- 
  incidence_estimates_general_help %>% 
  filter( analysis_interval == "overall", 
          denominator_age_group == "0;150",
          denominator_sex =="Both") %>% 
  mutate( outcome_cohort_name = fct_reorder( outcome_cohort_name, incidence_100000_pys, max, .desc = TRUE)
  )

ggplot(conditions_general, aes( x = outcome_cohort_name, y = incidence_100000_pys)) + 
  geom_smooth( method = "lm",
               formula = y ~ poly(x, 1),
               se = FALSE) +
  geom_point( size = 0.5) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))+
  labs( x = "", y = "Incidence") +
  ggsci::scale_color_lancet( alpha = 0.75)+
  theme_minimal()+
  theme( text = element_text( family = "serif", color = "black"),
         axis.text.x = element_text( angle = 90, size = 10, vjust = 0.5, hjust = 1),
         axis.text.y = element_text( size = 8),
         panel.grid.major = element_blank(),
         panel.grid.minor.x = element_blank(),
         legend.background = element_rect(fill='transparent'),
         legend.position = "top")


## overall results in infected cohort
conditions_inf <- 
  incidence_estimates_help %>% 
  filter( analysis_interval == "overall", 
          denominator_age_group == "0;150",
          denominator_sex =="Both",
          denominator_strata_cohort_name == "infection" ) %>% 
  mutate( outcome_cohort_name = fct_reorder( outcome_cohort_name, incidence_100000_pys, max, .desc = TRUE)
  )

ggplot(conditions_inf, aes( x = outcome_cohort_name, y = incidence_100000_pys)) + 
  geom_smooth( method = "lm",
               formula = y ~ poly(x, 1),
               se = FALSE) +
  geom_point( size = 0.5) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))+
  labs( x = "", y = "Incidence") +
  ggsci::scale_color_lancet( alpha = 0.75)+
  theme_minimal()+
  theme( text = element_text( family = "serif", color = "black"),
         axis.text.x = element_text( angle = 90, size = 6, vjust = 0.5, hjust = 1),
         axis.text.y = element_text( size = 8),
         panel.grid.major = element_blank(),
         panel.grid.minor.x = element_blank(),
         legend.background = element_rect(fill='transparent'),
         legend.position = "top")



## overall results in reinfection cohort
conditions_reinfection <- 
  incidence_estimates_help %>% 
  filter( analysis_interval == "overall", 
          denominator_age_group == "0;150",
          denominator_sex =="Both",
          denominator_strata_cohort_name == "reinfection" ) %>% 
  mutate( outcome_cohort_name = fct_reorder( outcome_cohort_name, incidence_100000_pys, max, .desc = TRUE)
  )

ggplot(conditions_reinfection, aes( x = outcome_cohort_name, y = incidence_100000_pys)) + 
  geom_smooth( method = "lm",
               formula = y ~ poly(x, 1),
               se = FALSE) +
  geom_point( size = 0.5) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))+
  labs( x = "", y = "Incidence") +
  ggsci::scale_color_lancet( alpha = 0.75)+
  theme_minimal()+
  theme( text = element_text( family = "serif", color = "black"),
         axis.text.x = element_text( angle = 90, size = 6, vjust = 0.5, hjust = 1),
         axis.text.y = element_text( size = 8),
         panel.grid.major = element_blank(),
         panel.grid.minor.x = element_blank(),
         legend.background = element_rect(fill='transparent'),
         legend.position = "top")



### TIME TRENDS ------------------------------------------------------------
# Trend of incidence by years in general population

conditions_year <- 
  incidence_estimates_general_help %>% 
  filter( analysis_interval == "years", 
          denominator_age_group == "0;150",
          denominator_sex =="Both") %>% 
  mutate( outcome_cohort_name = fct_reorder( outcome_cohort_name, incidence_100000_pys, max, .desc = TRUE)
  )

ggplot(conditions_year, aes( x = factor(year_index), y = incidence_100000_pys)) + 
  geom_smooth( method = "lm",
               formula = y ~ poly(x, 1),
               se = FALSE) +
  geom_point( size = 0.5) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))+
  labs( x = "", y = "Incidence") +
  ggsci::scale_color_lancet( alpha = 0.75)+
  facet_wrap( vars( outcome_cohort_name), scales = "free_y", drop = TRUE)+
  theme_minimal()+
  theme( text = element_text( family = "serif", color = "black"),
         axis.text.x = element_text( angle = 90, size = 6, vjust = 0.5, hjust = 1),
         axis.text.y = element_text( size = 8),
         panel.grid.major = element_blank(),
         panel.grid.minor.x = element_blank(),
         legend.background = element_rect(fill='transparent'),
         legend.position = "top")

# Trend of incidence by months in general population

conditions_month <- 
  incidence_estimates_general_help %>% 
  filter( analysis_interval == "months", 
          denominator_age_group == "0;150",
          denominator_sex =="Both") %>% 
  mutate( outcome_cohort_name = fct_reorder( outcome_cohort_name, incidence_100000_pys, max, .desc = TRUE)
  )

ggplot(conditions_month, aes( x = factor(year_month), y = incidence_100000_pys)) + 
  geom_smooth( method = "lm",
               formula = y ~ poly(x, 1),
               se = FALSE) +
  geom_point( size = 0.5) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))+
  labs( x = "", y = "Incidence") +
  ggsci::scale_color_lancet( alpha = 0.75)+
  facet_wrap( vars( outcome_cohort_name), scales = "free_y", drop = TRUE)+
  theme_minimal()+
  theme( text = element_text( family = "serif", color = "black"),
         axis.text.x = element_text( angle = 90, size = 6, vjust = 0.5, hjust = 1),
         axis.text.y = element_text( size = 8),
         panel.grid.major = element_blank(),
         panel.grid.minor.x = element_blank(),
         legend.background = element_rect(fill='transparent'),
         legend.position = "top")


#### trends of incidence by year in infected cohort


conditions_in_infection_year <- 
  incidence_estimates_help %>% 
  filter( analysis_interval == "years", 
          denominator_strata_cohort_name == "infection",
          denominator_age_group == "0;150",
          denominator_sex =="Both") %>% 
  mutate( outcome_cohort_name = fct_reorder( outcome_cohort_name, incidence_100000_pys, max, .desc = TRUE)
  )

ggplot(conditions_in_infection_year, aes( x = factor(year_index), y = incidence_100000_pys)) + 
  geom_smooth( method = "lm",
               formula = y ~ poly(x, 1),
               se = FALSE) +
  geom_point( size = 0.5) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))+
  labs( x = "", y = "Incidence") +
  ggsci::scale_color_lancet( alpha = 0.75)+
  facet_wrap( vars( outcome_cohort_name), scales = "free_y", drop = TRUE)+
  theme_minimal()+
  theme( text = element_text( family = "serif", color = "black"),
         axis.text.x = element_text( angle = 90, size = 6, vjust = 0.5, hjust = 1),
         axis.text.y = element_text( size = 8),
         panel.grid.major = element_blank(),
         panel.grid.minor.x = element_blank(),
         legend.background = element_rect(fill='transparent'),
         legend.position = "top")

# Trend of incidence by month in infected cohort 

conditions_in_infection_month <- 
  incidence_estimates_help %>% 
  filter( analysis_interval == "months", 
          denominator_strata_cohort_name == "infection",
          denominator_age_group == "0;150",
          denominator_sex =="Both") %>% 
  mutate( outcome_cohort_name = fct_reorder( outcome_cohort_name, incidence_100000_pys, max, .desc = TRUE)
  )

ggplot(conditions_in_infection_month, aes( x = factor(year_month), y = incidence_100000_pys)) + 
  geom_smooth( method = "lm",
               formula = y ~ poly(x, 1),
               se = FALSE) +
  geom_point( size = 0.5) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))+
  labs( x = "", y = "Incidence") +
  ggsci::scale_color_lancet( alpha = 0.75)+
  facet_wrap( vars( outcome_cohort_name), scales = "free_y", drop = TRUE)+
  theme_minimal()+
  theme( text = element_text( family = "serif", color = "black"),
         axis.text.x = element_text( angle = 90, size = 6, vjust = 0.5, hjust = 1),
         axis.text.y = element_text( size = 8),
         panel.grid.major = element_blank(),
         panel.grid.minor.x = element_blank(),
         legend.background = element_rect(fill='transparent'),
         legend.position = "top")


#### trends of incidence by year in reinfection cohort


conditions_in_reinfection_year <- 
  incidence_estimates_help %>% 
  filter( analysis_interval == "years", 
          denominator_strata_cohort_name == "reinfection",
          denominator_age_group == "0;150",
          denominator_sex =="Both") %>% 
  mutate( outcome_cohort_name = fct_reorder( outcome_cohort_name, incidence_100000_pys, max, .desc = TRUE)
  )

ggplot(conditions_in_reinfection_year, aes( x = factor(year_index), y = incidence_100000_pys)) + 
  geom_smooth( method = "lm",
               formula = y ~ poly(x, 1),
               se = FALSE) +
  geom_point( size = 0.5) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))+
  labs( x = "", y = "Incidence") +
  ggsci::scale_color_lancet( alpha = 0.75)+
  facet_wrap( vars( outcome_cohort_name), scales = "free_y", drop = TRUE)+
  theme_minimal()+
  theme( text = element_text( family = "serif", color = "black"),
         axis.text.x = element_text( angle = 90, size = 6, vjust = 0.5, hjust = 1),
         axis.text.y = element_text( size = 8),
         panel.grid.major = element_blank(),
         panel.grid.minor.x = element_blank(),
         legend.background = element_rect(fill='transparent'),
         legend.position = "top")

# Trend of incidence by month in reinfection cohort 

conditions_in_reinfection_month <- 
  incidence_estimates_help %>% 
  filter( analysis_interval == "months", 
          denominator_strata_cohort_name == "reinfection",
          denominator_age_group == "0;150",
          denominator_sex =="Both") %>% 
  mutate( outcome_cohort_name = fct_reorder( outcome_cohort_name, incidence_100000_pys, max, .desc = TRUE)
  )

ggplot(conditions_in_reinfection_month, aes( x = factor(year_month), y = incidence_100000_pys)) +
  geom_smooth( method = "lm",
               formula = y ~ poly(x, 1),
               se = FALSE) +
  geom_point( size = 0.5) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))+
  labs( x = "", y = "Incidence") +
  ggsci::scale_color_lancet( alpha = 0.75)+
  facet_wrap( vars( outcome_cohort_name), scales = "free_y", drop = TRUE)+
  theme_minimal()+
  theme( text = element_text( family = "serif", color = "black"),
         axis.text.x = element_text( angle = 90, size = 6, vjust = 0.5, hjust = 1),
         axis.text.y = element_text( size = 8),
         panel.grid.major = element_blank(),
         panel.grid.minor.x = element_blank(),
         legend.background = element_rect(fill='transparent'),
         legend.position = "top")



# Stratification by age ---------------------------------------
# Incidence by age general population

conditions_age <- 
  incidence_estimates_general_help %>% 
  filter( analysis_interval == "overall", 
          denominator_sex == "Both") %>% 
  mutate( outcome_cohort_name = fct_reorder( outcome_cohort_name, incidence_100000_pys, max, .desc = TRUE)
  ) %>% 
  group_by( outcome_cohort_name) %>% 
  mutate( number_point = n()) %>% 
  ungroup() %>% 
  filter( number_point > 3) %>% 
  mutate( outcome_cohort_name = fct_reorder( outcome_cohort_name, incidence_100000_pys, max, .desc = TRUE),
  )

ggplot(conditions_age, aes( x = denominator_age_group, y = incidence_100000_pys)) + 
  geom_smooth( method = "lm",
               formula = y ~ poly(x, 2),
               se = FALSE) +
  geom_point( size = 0.5) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))+
  ggsci::scale_color_lancet( alpha = 0.75) +
  labs( x = "", y = "Incidence") +
  facet_wrap( vars( outcome_cohort_name), scales = "free_y", drop = TRUE)+
  theme_minimal()+
  theme( text = element_text( family = "serif", color = "black"),
         axis.text.x = element_text( angle = 90, size = 6, vjust = 0.5, hjust = 1),
         axis.text.y = element_text( size = 8),
         panel.grid.major = element_blank(),
         legend.background = element_rect(fill='transparent'),
         legend.position = "top")



### by age in infected cohort

conditions_in_infection_age <- 
  incidence_estimates_help %>% 
  filter( analysis_interval == "overall", 
          denominator_strata_cohort_name == "infection",
          denominator_sex == "Both") %>% 
  mutate( outcome_cohort_name = fct_reorder( outcome_cohort_name, incidence_100000_pys, max, .desc = TRUE)
  ) %>% 
  group_by( outcome_cohort_name) %>% 
  mutate( number_point = n()) %>% 
  ungroup() %>% 
  filter( number_point > 3) %>% 
  mutate( outcome_cohort_name = fct_reorder( outcome_cohort_name, incidence_100000_pys, max, .desc = TRUE),
  )

ggplot(conditions_in_infection_age, aes( x = denominator_age_group, y = incidence_100000_pys)) + 
  geom_smooth( method = "lm",
               formula = y ~ poly(x, 2),
               se = FALSE) +
  geom_point( size = 0.5) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))+
  ggsci::scale_color_lancet( alpha = 0.75) +
  labs( x = "", y = "Incidence") +
  facet_wrap( vars( outcome_cohort_name), scales = "free_y", drop = TRUE)+
  theme_minimal()+
  theme( text = element_text( family = "serif", color = "black"),
         axis.text.x = element_text( angle = 90, size = 6, vjust = 0.5, hjust = 1),
         axis.text.y = element_text( size = 8),
         panel.grid.major = element_blank(),
         legend.background = element_rect(fill='transparent'),
         legend.position = "top")



### by age in reinfection cohort

conditions_in_reinfection_age <- 
  incidence_estimates_help %>% 
  filter( analysis_interval == "overall", 
          denominator_strata_cohort_name == "reinfection",
          denominator_sex == "Both") %>% 
  mutate( outcome_cohort_name = fct_reorder( outcome_cohort_name, incidence_100000_pys, max, .desc = TRUE)
  ) %>% 
  group_by( outcome_cohort_name) %>% 
  mutate( number_point = n()) %>% 
  ungroup() %>% 
  filter( number_point > 3) %>% 
  mutate( outcome_cohort_name = fct_reorder( outcome_cohort_name, incidence_100000_pys, max, .desc = TRUE),
  )

ggplot(conditions_in_reinfection_age, aes( x = denominator_age_group, y = incidence_100000_pys)) + 
  geom_smooth( method = "lm",
               formula = y ~ poly(x, 2),
               se = FALSE) +
  geom_point( size = 0.5) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))+
  ggsci::scale_color_lancet( alpha = 0.75) +
  labs( x = "", y = "Incidence") +
  facet_wrap( vars( outcome_cohort_name), scales = "free_y", drop = TRUE)+
  theme_minimal()+
  theme( text = element_text( family = "serif", color = "black"),
         axis.text.x = element_text( angle = 90, size = 6, vjust = 0.5, hjust = 1),
         axis.text.y = element_text( size = 8),
         panel.grid.major = element_blank(),
         legend.background = element_rect(fill='transparent'),
         legend.position = "top")



# Stratification by sex ---------------------------------------------------



# Incidence by sex in general population
conditions_sex <- 
  incidence_estimates_general_help %>% 
  filter( analysis_interval == "overall", 
          denominator_age_group == "0;150") %>% 
  mutate( outcome_cohort_name = fct_reorder( outcome_cohort_name, incidence_100000_pys, max, .desc = TRUE)
  ) %>% 
  group_by( outcome_cohort_name) %>% 
  mutate( number_point = n()) %>% 
  ungroup() %>% 
  filter( number_point >= 3) %>% 
  mutate( outcome_cohort_name = fct_reorder( outcome_cohort_name, incidence_100000_pys, max, .desc = TRUE),
  )


ggplot(conditions_sex, aes( x = cdm_name, y = incidence_100000_pys, fill = denominator_sex)) + 
  geom_errorbar( aes( ymin = incidence_100000_pys, ymax = incidence_100000_pys_95CI_upper, color = denominator_sex), 
                 position = position_dodge(0.5), width = 0.25) +
  geom_col( position = position_dodge(0.5), width = 0.5) +
  scale_y_continuous( breaks = scales::breaks_extended(5)) +
   ggsci::scale_color_lancet( alpha = 0.75)+
  ggsci::scale_fill_lancet( alpha = 0.75)+
  labs( x = "", y = "Incidence", color = "", fill = "") +
  facet_wrap( vars( outcome_cohort_name), scales = "free", drop = TRUE)+
  theme_minimal() +
  theme( text = element_text( family = "serif", color = "black"),
         axis.text.x = element_text( angle = 0, size = 6),
         axis.text.y = element_text( size = 8),
         panel.grid.major = element_blank(),
         legend.background = element_rect(fill='transparent'),
         legend.position = "top")


# Incidence by sex in infected population
conditions_in_infection_sex <- 
  incidence_estimates_help %>% 
  filter( analysis_interval == "overall", 
          denominator_age_group == "0;150",
          denominator_strata_cohort_name == "infection"
          ) %>% 
  mutate( outcome_cohort_name = fct_reorder( outcome_cohort_name, incidence_100000_pys, max, .desc = TRUE)
  ) %>% 
  group_by( outcome_cohort_name) %>% 
  mutate( number_point = n()) %>% 
  ungroup() %>% 
  filter( number_point >= 3) %>% 
  mutate( outcome_cohort_name = fct_reorder( outcome_cohort_name, incidence_100000_pys, max, .desc = TRUE),
  )


ggplot(conditions_in_infection_sex, aes( x = cdm_name, y = incidence_100000_pys, fill = denominator_sex)) + 
  geom_errorbar( aes( ymin = incidence_100000_pys, ymax = incidence_100000_pys_95CI_upper, color = denominator_sex), 
                 position = position_dodge(0.5), width = 0.25) +
  geom_col( position = position_dodge(0.5), width = 0.5) +
  scale_y_continuous( breaks = scales::breaks_extended(5)) +
  ggsci::scale_color_lancet( alpha = 0.75)+
  ggsci::scale_fill_lancet( alpha = 0.75)+
  labs( x = "", y = "Incidence", color = "", fill = "") +
  facet_wrap( vars( outcome_cohort_name), scales = "free", drop = TRUE)+
  theme_minimal() +
  theme( text = element_text( family = "serif", color = "black"),
         axis.text.x = element_text( angle = 0, size = 6),
         axis.text.y = element_text( size = 8),
         panel.grid.major = element_blank(),
         legend.background = element_rect(fill='transparent'),
         legend.position = "top")


# Incidence by sex in reinfection population

conditions_in_reinfection_sex <- 
  incidence_estimates_help %>% 
  filter( analysis_interval == "overall", 
          denominator_age_group == "0;150",
          denominator_strata_cohort_name == "reinfection"
  ) %>% 
  mutate( outcome_cohort_name = fct_reorder( outcome_cohort_name, incidence_100000_pys, max, .desc = TRUE)
  ) %>% 
  group_by( outcome_cohort_name) %>% 
  mutate( number_point = n()) %>% 
  ungroup() %>% 
  filter( number_point >= 3) %>% 
  mutate( outcome_cohort_name = fct_reorder( outcome_cohort_name, incidence_100000_pys, max, .desc = TRUE),
  )


ggplot(conditions_in_reinfection_sex, aes( x = cdm_name, y = incidence_100000_pys, fill = denominator_sex)) + 
  geom_errorbar( aes( ymin = incidence_100000_pys, ymax = incidence_100000_pys_95CI_upper, color = denominator_sex), 
                 position = position_dodge(0.5), width = 0.25) +
  geom_col( position = position_dodge(0.5), width = 0.5) +
  scale_y_continuous( breaks = scales::breaks_extended(5)) +
  ggsci::scale_color_lancet( alpha = 0.75)+
  ggsci::scale_fill_lancet( alpha = 0.75)+
  labs( x = "", y = "Incidence", color = "", fill = "") +
  facet_wrap( vars( outcome_cohort_name), scales = "free", drop = TRUE)+
  theme_minimal() +
  theme( text = element_text( family = "serif", color = "black"),
         axis.text.x = element_text( angle = 0, size = 6),
         axis.text.y = element_text( size = 8),
         panel.grid.major = element_blank(),
         legend.background = element_rect(fill='transparent'),
         legend.position = "top")




# IRR ---------------------------------------------------------------------
# Transform from Long to wide format
incidence_estimates_wide <- 
  incidence_estimates_help %>% 
  filter( 
    denominator_strata_cohort_name %in% c( "reinfection", "infection")) %>% 
  pivot_wider( id_cols = c(cdm_name, analysis_interval, year_index, 
                           denominator_age_group, denominator_sex, 
                           outcome_cohort_name, incidence_start_date
  ), 
  names_from = denominator_strata_cohort_name,
  values_from = c( n_persons, person_years, n_events, incidence_100000_pys)) %>% 
  janitor::clean_names() %>% 
  ungroup()


cohort_wrap_func <- function(conditions,
                                     interval = "overall", 
                                     age_expre = "0;150", 
                                     sex_expre = "Both"){
  
  
  IRR_df <- 
    incidence_estimates_wide %>% 
    filter( interval %in% analysis_interval, 
            denominator_age_group %in% age_expre,
            denominator_sex %in% sex_expre
    ) %>% 
    filter( !is.na(incidence_100000_pys_infection), !is.na(incidence_100000_pys_reinfection)) %>% 
    filter( outcome_cohort_name == conditions)
  
  
  
  
  meta_unit_func <- function( input_df = IRR_df){
    
    output_meta <- meta::metainc( event.e = n_events_reinfection,
                                  time.e = person_years_reinfection,
                                  event.c = n_events_infection,
                                  time.c = person_years_infection, 
                                  sm = "IRR",
                                  data = input_df)
    
    output_meta <- 
      tibble( IRR_random = exp(output_meta$TE.random),
              IRR_low_random = exp(output_meta$lower.random),
              IRR_upper_random = exp(output_meta$upper.random),
              IRR_fix = exp(output_meta$TE.fix),
              IRR_low_fix = exp(output_meta$lower.fix),
              IRR_upper_fix = exp(output_meta$upper.fix))
    
    return( output_meta)
    
  }  
  
  output <- meta_unit_func( input_df = IRR_df)
  
  return(output)
  
  
}



overall_temp <- 
  map_df( setdiff( names_conditions$cohort_name, c("ibd","juvenile_arthritis","ra","sle","t1dm","misc")) %>% set_names,
          cohort_wrap_func,
          interval = "overall",
          age_expre =  "0;150",
          sex_expre = "Both",
          .id = "conditions") %>% 
  mutate( conditions = fct_reorder(conditions, IRR_random, .desc = FALSE))  %>% 
  mutate( Sig_index = case_when( IRR_low_random < 1 ~ "Not significant", TRUE ~ "Significant"),
          Priority_index = case_when( IRR_random >= 1.5 ~ "High priority", 
                                      IRR_random < 1.5 & IRR_random >= 1.1 ~ "Intermediate priority", 
                                      IRR_random < 1.1  ~ "Low priority")) 
male_temp <- 
  map_df( setdiff( names_conditions$cohort_name, c("ibd","juvenile_arthritis","ra","sle","t1dm","misc")) %>% set_names,
          cohort_wrap_func,
          interval = "overall",
          age_expre =  "0;150",
          sex_expre = "Both",
          .id = "conditions") %>% 
  mutate( conditions = fct_reorder(conditions, IRR_random, .desc = FALSE))  %>% 
  mutate( Sig_index = case_when( IRR_low_random < 1 ~ "Not significant", TRUE ~ "Significant"),
          Priority_index = case_when( IRR_random >= 1.5 ~ "High priority", 
                                      IRR_random < 1.5 & IRR_random >= 1.1 ~ "Intermediate priority", 
                                      IRR_random < 1.1  ~ "Low priority")) 

female_temp <- 
  map_df(  setdiff( names_conditions$cohort_name, c("ibd","juvenile_arthritis","ra","sle","t1dm","misc")) %>% set_names, 
           cohort_wrap_func,
           interval = "overall",
           age_expre =  "0;150",
           sex_expre = "Both",
           .id = "conditions") %>% 
  mutate( conditions = fct_reorder(conditions, IRR_random, .desc = FALSE))  %>% 
  mutate( Sig_index = case_when( IRR_low_random < 1 ~ "Not significant", TRUE ~ "Significant"),
          Priority_index = case_when( IRR_random >= 1.5 ~ "High priority", 
                                      IRR_random < 1.5 & IRR_random >= 1.1 ~ "Intermediate priority", 
                                      IRR_random < 1.1  ~ "Low priority")) 

# children_temp <-
#   map_df( setdiff( names_conditions$cohort_name, c("ibd","juvenile_arthritis","ra","sle","t1dm","misc",
#                                                    "dysautonomia","me_cfs","me_cfs_symptoms","pots")) %>% set_names,
#           cohort_wrap_func,
#           interval = "overall",
#           age_expre =  c("0;6", "7;11", "12;18"),
#           sex_expre = "Both",
#           .id = "conditions") %>%
#   mutate( conditions = fct_reorder(conditions, IRR_random, .desc = FALSE))  %>%
#   mutate( Sig_index = case_when( IRR_low_random < 1 ~ "Not significant", TRUE ~ "Significant"),
#           Priority_index = case_when( IRR_random >= 1.5 ~ "High priority",
#                                       IRR_random < 1.5 & IRR_random >= 1.1 ~ "Intermediate priority",
#                                       IRR_random < 1.1  ~ "Low priority"))

adult_temp <- 
  map_df( setdiff( names_conditions$cohort_name, c("ibd","juvenile_arthritis","ra","sle","t1dm","misc")) %>% set_names,
          cohort_wrap_func,
          interval = "overall",
          age_expre =  c("19;40", "41;64"),
          sex_expre = "Both",
          .id = "conditions") %>% 
  mutate( conditions = fct_reorder(conditions, IRR_random, .desc = FALSE))  %>% 
  mutate( Sig_index = case_when( IRR_low_random < 1 ~ "Not significant", TRUE ~ "Significant"),
          Priority_index = case_when( IRR_random >= 1.5 ~ "High priority", 
                                      IRR_random < 1.5 & IRR_random >= 1.1 ~ "Intermediate priority", 
                                      IRR_random < 1.1  ~ "Low priority")) 

elderly_temp <- 
  map_df( setdiff( names_conditions$cohort_name, c("ibd","juvenile_arthritis","ra","sle","t1dm","misc",
                                                   "me_cfs")) %>% set_names,
          cohort_wrap_func,
          interval = "overall",
          age_expre =  c("65;120"),
          sex_expre = "Both",
          .id = "conditions") %>% 
  mutate( conditions = fct_reorder(conditions, IRR_random, .desc = FALSE))  %>% 
  mutate( Sig_index = case_when( IRR_low_random < 1 ~ "Not significant", TRUE ~ "Significant"),
          Priority_index = case_when( IRR_random >= 1.5 ~ "High priority", 
                                      IRR_random < 1.5 & IRR_random >= 1.1 ~ "Intermediate priority", 
                                      IRR_random < 1.1  ~ "Low priority")) 



all_IRR <- list( All = overall_temp, 
                 Male = male_temp, 
                 Female = female_temp,
                 #children = children_temp,
                 adult = adult_temp,
                 elderly = elderly_temp)


plot_func <- function( df){
  
  output <- ggplot( df, aes( x = IRR_random, y = conditions, color = Priority_index)) + 
    geom_point( position = position_dodge(0.75)) +
    geom_errorbar( aes( xmin = IRR_low_random, xmax = IRR_upper_random), position = position_dodge(0.75), width = 0.5) +
    geom_vline( xintercept = 1) +
    scale_x_continuous(  limits = c(0.2, 12), trans = scales::log2_trans()) +
    guides(color = guide_legend(nrow = 1, byrow = TRUE)) +
    ggsci::scale_color_lancet( alpha = 0.75) +
    labs( x = "", y = "", fill = "IRR") +
    #facet_wrap( vars( Groups), scales = "free_y", drop = TRUE) +
    theme_bw()+
    theme( text = element_text( family = "serif", color = "black"),
           axis.text.x = element_text( angle = 90, vjust = 0.5, hjust = 1),
           axis.text.y = element_text( ),
           #panel.grid.major = element_blank(),
           panel.grid.minor.x = element_blank(),
           legend.background = element_rect(fill='transparent'),
           legend.position = "top")
  
  return(output)
  
  
}



plot_list <- map( all_IRR, plot_func)
empty <- ggplot() + theme_void()
manual_legend <- legend <- get_legend(# create some space to the left of the legend
  plot_list$All + theme(legend.box.margin = margin(0, 0, 0, 12)))

main_plot <- plot_grid(plot_list$All+ theme( legend.position = "none"), 
                       empty, 
                       empty, 
                       plot_list$Male + theme( legend.position = "none"), 
                       plot_list$Female + theme( legend.position = "none"), 
                       empty, 
                       plot_list$children + theme( legend.position = "none"), 
                       plot_list$adult + theme( legend.position = "none"), 
                       plot_list$elderly + theme( legend.position = "none"), 
                       nrow = 3, ncol = 3,
                       labels = c("All", "", "", 
                                  "Male", "Female", "", 
                                  "Children&Adolescent", "Middle-aged adults", "Elderly adults"), label_size = 7, label_y = 1.01,
                       align = "h")

plot_grid( main_plot, manual_legend, ncol = 1, rel_heights = c(1, 0.05))



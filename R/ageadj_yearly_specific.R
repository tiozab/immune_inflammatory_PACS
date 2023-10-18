

################################# age adjusted yearly rates in specific cohorts ------------------------------------------------------------------------

############################################################### DIRECT METHOD for adj--------------------------------------------------------------------------------------

#estimates in the general population (overall analysis, all ages, both sexes)
incidence_estimates_IR_general_age_years <- incidence_estimates_general_help %>%
  select("analysis_interval","outcome_cohort_name","n_persons","n_events",
         "denominator_age_group","denominator_sex","incidence_100000_pys","database_name",
         "year_index") %>%#
  filter(analysis_interval == "years",
         denominator_age_group != "0 to 150" ,
         denominator_sex == "Both") %>%
  rename(ref_IR_100000_pys = incidence_100000_pys,
         ref_persons = n_persons,
         ref_events = n_events)

#standardised estimates in the specific populations
incidence_estimates_standard_age_years <- incidence_estimates_help %>% 
  select ("denominator_strata_cohort_name","analysis_interval","outcome_cohort_name","incidence_100000_pys",
          "denominator_age_group","denominator_sex","person_years","n_events","database_name",
          "incidence_100000_pys_95CI_lower", "incidence_100000_pys_95CI_upper","year_index") %>%
  inner_join(incidence_estimates_IR_general_age_years ,
             join_by (
               "analysis_interval" == "analysis_interval",
               "outcome_cohort_name" == "outcome_cohort_name",
               "denominator_age_group" == "denominator_age_group",
               "denominator_sex" == "denominator_sex",
               "database_name" == "database_name",
               "year_index" == "year_index")) %>%
  select("database_name","denominator_strata_cohort_name","outcome_cohort_name","denominator_age_group",
         "n_events","incidence_100000_pys","ref_events","ref_persons","ref_IR_100000_pys",
         "person_years","incidence_100000_pys_95CI_lower", "incidence_100000_pys_95CI_upper","year_index") %>% 
  group_by(database_name, denominator_strata_cohort_name,outcome_cohort_name,year_index)
# need this group by at the end for the group_split


### from Ed use ageadjust.indirect(count,pop,stdcount,stdpop,stdrate = NULL, conf.level = 0.95)

list_for_adjusting <- group_split(incidence_estimates_standard_age_years)

results <- list()
for (i in list_for_adjusting){
  
  results <- c(results, ageadjust.direct(count = i$n_events, 
                                         pop = i$person_years , 
                                         rate = (i$incidence_100000_pys / 100000),
                                         stdpop = i$ref_persons))
  
}

name_list <- list()
for (i in 1:length(list_for_adjusting))
{
  for (j in 0:3) {
    name_list <- c(name_list, glue("{names(results)[(i-1)*4+1+j]},{list_for_adjusting[[i]]$database_name[1]},{list_for_adjusting[[i]]$denominator_strata_cohort_name[1]},{list_for_adjusting[[i]]$outcome_cohort_name[1]},{list_for_adjusting[[i]]$year_index[1]},"))
  }
}

names(results) <- name_list

observed_crude <- sapply(results, "[",1, USE.NAMES = TRUE) %>% data.frame() 

crude <- observed_crude %>%
  filter(grepl("crude", rownames(observed_crude)))
crude <- crude %>%
  mutate(rown = rownames(crude)) %>% as_tibble() 
crude[c('rates','database_name','denominator_strata_cohort_name','outcome_cohort_name','year_index','type')] <- str_split_fixed(crude$rown, ',',6) 
crude <- crude %>%
  rename("crude_ir" = ".") %>%
  select("crude_ir","database_name","denominator_strata_cohort_name","outcome_cohort_name","year_index")

exp_adj <- sapply(results, "[",1, USE.NAMES = TRUE) %>% data.frame() 

adj <- exp_adj %>%
  filter(grepl("adj", rownames(exp_adj)))
adj <- adj %>%
  mutate(rown = rownames(adj)) %>% as_tibble() 
adj[c('rates','database_name','denominator_strata_cohort_name','outcome_cohort_name','year_index','type')] <- str_split_fixed(adj$rown, ',',6) 
adj <- adj %>%
  rename("adj_ir" = ".") %>%
  select("adj_ir","database_name","denominator_strata_cohort_name","outcome_cohort_name","year_index")

sir_lci <- sapply(results, "[",1, USE.NAMES = TRUE) %>% data.frame() 

lci_ir <- sir_lci %>%
  filter(grepl("lci", rownames(sir_lci)))
lci_ir <- lci_ir %>%
  mutate(rown = rownames(lci_ir)) %>% as_tibble() 
lci_ir[c('rates','database_name','denominator_strata_cohort_name','outcome_cohort_name','year_index','type')] <- str_split_fixed(lci_ir$rown, ',',6) 
lci_ir <- lci_ir %>%
  rename("lci_ir" = ".") %>%
  select("lci_ir","database_name","denominator_strata_cohort_name","outcome_cohort_name","year_index")



lci_uci <- sapply(results, "[",1, USE.NAMES = TRUE) %>% data.frame() 

uci_ir <- lci_uci %>%
  filter(grepl("uci", rownames(lci_uci)))
uci_ir <- uci_ir %>%
  mutate(rown = rownames(uci_ir)) %>% as_tibble() 
uci_ir[c('rates','database_name','denominator_strata_cohort_name','outcome_cohort_name','year_index','type')] <- str_split_fixed(uci_ir$rown, ',',6) 
uci_ir <- uci_ir %>%
  rename("uci_ir" = ".") %>%
  select("uci_ir","database_name","denominator_strata_cohort_name","outcome_cohort_name","year_index")




tibble_results <- crude %>%
  
  inner_join(adj, join_by ( "database_name" == "database_name",
                            "denominator_strata_cohort_name" == "denominator_strata_cohort_name",
                            "outcome_cohort_name" == "outcome_cohort_name",
                            "year_index" == "year_index"
  )) %>%
  
  inner_join(lci_ir, join_by ( "database_name" == "database_name",
                               "denominator_strata_cohort_name" == "denominator_strata_cohort_name",
                               "outcome_cohort_name" == "outcome_cohort_name",
                               "year_index" == "year_index")) %>%
  
  inner_join(uci_ir, join_by ( "database_name" == "database_name",
                               "denominator_strata_cohort_name" == "denominator_strata_cohort_name",
                               "outcome_cohort_name" == "outcome_cohort_name",
                               "year_index" == "year_index")) 



crude_adj_rates_years <- tibble_results %>%
  select("database_name","denominator_strata_cohort_name","outcome_cohort_name","year_index","crude_ir","adj_ir","lci_ir","uci_ir")  %>%
  mutate(crude_ir = crude_ir * 100000,
         adj_ir = adj_ir * 100000,
         lci_ir = lci_ir * 100000,
         uci_ir =  uci_ir * 100000) 


########################################### Plots yearly ---------------------------------------------------------------------------------



plot.data <- crude_adj_rates_years  %>% 
  group_by(database_name, denominator_strata_cohort_name, outcome_cohort_name) %>%
  mutate( number_point = n()) %>% 
  ungroup() %>% 
  filter(number_point >= 1) %>%
  group_by(database_name)

list_for_plots <- group_split(plot.data)

## 3 lines per plot, 1 outcome per plot

# create as many lists as there are data bases
p1 <- plot.new()
p2 <- plot.new()


for (i in 1:length(list_for_plots)){
  
  assign(glue("p{i}"),ggplot(as_tibble(list_for_plots[[i]]), aes( x = factor(year_index), y = adj_ir, color = denominator_strata_cohort_name)) + 
           geom_point(size = 0.5) +
           geom_errorbar(aes(ymin = lci_ir, ymax =  uci_ir), width=0.05) +
           guides(color = guide_legend(nrow = 2, byrow = TRUE))+
           labs( x = "", y = "Incidence / 100'000 person-years") +
           ggsci::scale_color_lancet( alpha = 0.75)+
           facet_wrap( vars( outcome_cohort_name), scales = "free_y", drop = TRUE)+
           theme_classic()+
           theme( text = element_text( family = "serif", color = "black"),
                  axis.text.x = element_text( angle = 90, size = 6, vjust = 0.5, hjust = 1),
                  axis.text.y = element_text( size = 8),
                  panel.grid.major = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  legend.background = element_rect(fill='transparent'),
                  legend.position = "top"))
  
}


plot_grid(
  p1, p2,
  ncol = 1,
  labels = c('CPRD Gold','pharmetrics Plus')
)


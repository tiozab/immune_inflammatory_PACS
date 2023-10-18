
# https://www.healthknowledge.org.uk/e-learning/epidemiology/specialists/standardisation
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3406211/pdf/mjms-7-1-010.pdf

# Indirect Standardisation ---------------------------------------------
## using a set of specific rates (indirect method)

# STANDARDISED RATIO 
# choose a reference or standard population: general population
# Multiply the number of people in each group of the population(s) of interest 
##by the specific incidence rate in the comparable age/sex group of the reference population. 
###this will be the expected rate
# Divide the total number of observed outcomes of the population(s) of interest by the expected outcomes


############################## OVERALL ###############

#estimates in the general population (overall analysis, all ages, both sexes)
incidence_estimates_IR_general <- incidence_estimates_general_help %>%
  select("analysis_interval","outcome_cohort_name","n_persons","n_events",
         "denominator_age_group","denominator_sex","incidence_100000_pys","database_name") %>%#
  filter(analysis_interval == "overall",
         denominator_age_group == "0;150" ,
         denominator_sex == "Both") %>%
  rename(ref_IR_100000_pys = incidence_100000_pys,
         ref_persons = n_persons,
         ref_events = n_events)

#standardised estimates in the specific populations
incidence_estimates_standard <- incidence_estimates_help %>% 
  select ("denominator_strata_cohort_name","analysis_interval","outcome_cohort_name","incidence_100000_pys",
          "denominator_age_group","denominator_sex","person_years","n_events","database_name",
          "incidence_100000_pys_95CI_lower", "incidence_100000_pys_95CI_upper") %>%
  inner_join(incidence_estimates_IR_general ,
             join_by (
    "analysis_interval" == "analysis_interval",
    "outcome_cohort_name" == "outcome_cohort_name",
    "denominator_age_group" == "denominator_age_group",
    "denominator_sex" == "denominator_sex",
    "database_name" == "database_name")) %>%
  mutate(expected_n_events = person_years /100000 * ref_IR_100000_pys) %>%
  mutate(standardized_ratio = n_events / expected_n_events) %>%
  select("database_name","denominator_strata_cohort_name","outcome_cohort_name","standardized_ratio",
         "n_events","incidence_100000_pys","ref_events","ref_persons","ref_IR_100000_pys",
         "person_years","incidence_100000_pys_95CI_lower", "incidence_100000_pys_95CI_upper") %>% 
  group_by(database_name, denominator_strata_cohort_name,outcome_cohort_name)
# need this group by at the end for the group_split


## THIS WOULD BE SO MUCH SHORTER!!

## add confidence interval (from Incidence Prevalence Package)
# incRateCiExact <- function(ev, pt) {
#   return(tibble::tibble(
#     incidence_100000_pys_95CI_lower =
#       ((stats::qchisq(p = 0.025, df = 2 * ev) / 2) / pt) * 100000,
#     incidence_100000_pys_95CI_upper =
#       ((stats::qchisq(p = 0.975, df = 2 * (ev + 1)) / 2) / pt) * 100000
#   ))


########### INSTEAD TO GET THE CONFIDENCE INTERVALS:


### from Ed use ageadjust.indirect(count,pop,stdcount,stdpop,stdrate = NULL, conf.level = 0.95)

list_for_adjusting <- group_split(incidence_estimates_standard)

results <- list()
for (i in list_for_adjusting){

results <- c(results, ageadjust.indirect(count = i$n_events, 
                   pop = i$person_years , 
                   stdcount = i$ref_events, 
                   stdrate = (i$ref_IR_100000_pys / 100000),
                   stdpop = i$ref_persons))

}

name_list <- list()
for (i in 1:length(list_for_adjusting))
{
  name_list <- c(name_list, glue("{names(results)[i*2-1]},{list_for_adjusting[[i]]$database_name},{list_for_adjusting[[i]]$denominator_strata_cohort_name},{list_for_adjusting[[i]]$outcome_cohort_name},"))
  name_list <- c(name_list, glue("{names(results)[i*2]},{list_for_adjusting[[i]]$database_name},{list_for_adjusting[[i]]$denominator_strata_cohort_name},{list_for_adjusting[[i]]$outcome_cohort_name},"))

  }

names(results) <- name_list

observed_crude <- sapply(results, "[",1, USE.NAMES = TRUE) %>% data.frame() 

observed <- observed_crude %>%
  filter(grepl("observed", rownames(observed_crude))) 
observed <- observed %>%
  mutate(rown = rownames(observed)) %>% as_tibble() 
observed[c('stand','database_name','denominator_strata_cohort_name','outcome_cohort_name','type')] <- str_split_fixed(observed$rown, ',',5) 
observed <- observed %>%
  rename("observed" = ".") %>%
  select("observed","database_name","denominator_strata_cohort_name","outcome_cohort_name")

crude <- observed_crude %>%
  filter(grepl("crude", rownames(observed_crude)))
crude <- crude %>%
  mutate(rown = rownames(crude)) %>% as_tibble() 
crude[c('rates','database_name','denominator_strata_cohort_name','outcome_cohort_name','type')] <- str_split_fixed(crude$rown, ',',5) 
crude <- crude %>%
  rename("crude_ir" = ".") %>%
  select("crude_ir","database_name","denominator_strata_cohort_name","outcome_cohort_name")

exp_adj <- sapply(results, "[",2, USE.NAMES = TRUE) %>% data.frame() 

exp <- exp_adj %>%
  filter(grepl("exp", rownames(exp_adj))) 
exp <- exp %>%
  mutate(rown = rownames(exp)) %>% as_tibble() 
exp[c('stand','database_name','denominator_strata_cohort_name','outcome_cohort_name','type')] <- str_split_fixed(exp$rown, ',',5) 
exp <- exp %>%
  rename("exp" = ".") %>%
  select("exp","database_name","denominator_strata_cohort_name","outcome_cohort_name")

adj <- exp_adj %>%
  filter(grepl("adj", rownames(exp_adj)))
adj <- adj %>%
  mutate(rown = rownames(adj)) %>% as_tibble() 
adj[c('rates','database_name','denominator_strata_cohort_name','outcome_cohort_name','type')] <- str_split_fixed(adj$rown, ',',5) 
adj <- adj %>%
  rename("adj_ir" = ".") %>%
  select("adj_ir","database_name","denominator_strata_cohort_name","outcome_cohort_name")

sir_lci <- sapply(results, "[",3, USE.NAMES = TRUE) %>% data.frame() 

sir <- sir_lci %>%
  filter(grepl("sir", rownames(sir_lci))) 
sir <- sir %>%
  mutate(rown = rownames(sir)) %>% as_tibble() 
sir[c('stand','database_name','denominator_strata_cohort_name','outcome_cohort_name','type')] <- str_split_fixed(sir$rown, ',',5) 
sir <- sir %>%
  rename("sir" = ".") %>%
  select("sir","database_name","denominator_strata_cohort_name","outcome_cohort_name")

lci_ir <- sir_lci %>%
  filter(grepl("lci", rownames(sir_lci)))
lci_ir <- lci_ir %>%
  mutate(rown = rownames(lci_ir)) %>% as_tibble() 
lci_ir[c('rates','database_name','denominator_strata_cohort_name','outcome_cohort_name','type')] <- str_split_fixed(lci_ir$rown, ',',5) 
lci_ir <- lci_ir %>%
  rename("lci_ir" = ".") %>%
  select("lci_ir","database_name","denominator_strata_cohort_name","outcome_cohort_name")



lci_uci <- sapply(results, "[",4, USE.NAMES = TRUE) %>% data.frame() 

lci <- lci_uci %>%
  filter(grepl("lci", rownames(lci_uci))) 
lci <- lci %>%
  mutate(rown = rownames(lci)) %>% as_tibble() 
lci[c('stand','database_name','denominator_strata_cohort_name','outcome_cohort_name','type')] <- str_split_fixed(lci$rown, ',',5) 
lci <- lci %>%
  rename("lci" = ".") %>%
  select("lci","database_name","denominator_strata_cohort_name","outcome_cohort_name")

uci_ir <- lci_uci %>%
  filter(grepl("uci", rownames(lci_uci)))
uci_ir <- uci_ir %>%
  mutate(rown = rownames(uci_ir)) %>% as_tibble() 
uci_ir[c('rates','database_name','denominator_strata_cohort_name','outcome_cohort_name','type')] <- str_split_fixed(uci_ir$rown, ',',5) 
uci_ir <- uci_ir %>%
  rename("uci_ir" = ".") %>%
  select("uci_ir","database_name","denominator_strata_cohort_name","outcome_cohort_name")


uci_null <- sapply(results, "[",5, USE.NAMES = TRUE) %>% data.frame() 

uci <- uci_null %>%
  filter(grepl("uci", rownames(uci_null))) 
uci <- uci %>%
  mutate(rown = rownames(uci)) %>% as_tibble() 
uci[c('stand','database_name','denominator_strata_cohort_name','outcome_cohort_name','type')] <- str_split_fixed(uci$rown, ',',5) 
uci <- uci %>%
  rename("uci" = ".") %>%
  select("uci","database_name","denominator_strata_cohort_name","outcome_cohort_name")





tibble_results <- observed %>% inner_join(crude, join_by ( "database_name" == "database_name",
                                                              "denominator_strata_cohort_name" == "denominator_strata_cohort_name",
                                                              "outcome_cohort_name" == "outcome_cohort_name")) %>%
  inner_join(exp, join_by ( "database_name" == "database_name",
                            "denominator_strata_cohort_name" == "denominator_strata_cohort_name",
                            "outcome_cohort_name" == "outcome_cohort_name")) %>%
  inner_join(adj, join_by ( "database_name" == "database_name",
                            "denominator_strata_cohort_name" == "denominator_strata_cohort_name",
                            "outcome_cohort_name" == "outcome_cohort_name")) %>%
  inner_join(sir, join_by ( "database_name" == "database_name",
                            "denominator_strata_cohort_name" == "denominator_strata_cohort_name",
                            "outcome_cohort_name" == "outcome_cohort_name")) %>%
  inner_join(lci_ir, join_by ( "database_name" == "database_name",
                               "denominator_strata_cohort_name" == "denominator_strata_cohort_name",
                               "outcome_cohort_name" == "outcome_cohort_name")) %>%
  inner_join(lci, join_by ( "database_name" == "database_name",
                            "denominator_strata_cohort_name" == "denominator_strata_cohort_name",
                            "outcome_cohort_name" == "outcome_cohort_name")) %>%
  inner_join(uci_ir, join_by ( "database_name" == "database_name",
                               "denominator_strata_cohort_name" == "denominator_strata_cohort_name",
                               "outcome_cohort_name" == "outcome_cohort_name")) %>%
  inner_join(uci, join_by ( "database_name" == "database_name",
                            "denominator_strata_cohort_name" == "denominator_strata_cohort_name",
                            "outcome_cohort_name" == "outcome_cohort_name"))


obs_exp_sir <- tibble_results %>%
  select("database_name","denominator_strata_cohort_name","outcome_cohort_name","observed","exp","sir","lci","uci")

### for comparison, merge it with my own results

compare_obs_exp_sir <- obs_exp_sir %>% inner_join(incidence_estimates_standard, join_by ( "database_name" == "database_name",
                                                                 "denominator_strata_cohort_name" == "denominator_strata_cohort_name",
                                                                 "outcome_cohort_name" == "outcome_cohort_name")) %>%
  select ("database_name","denominator_strata_cohort_name","outcome_cohort_name","observed","exp","sir","standardized_ratio")

## they are the same :-)


crude_adj_rates <- tibble_results %>%
  select("database_name","denominator_strata_cohort_name","outcome_cohort_name","crude_ir","adj_ir","lci_ir","uci_ir")  %>%
  mutate(crude_ir = crude_ir * 100000,
         adj_ir = adj_ir * 100000,
         lci_ir = lci_ir * 100000,
         uci_ir =  uci_ir * 100000) %>% inner_join(incidence_estimates_standard, join_by ( "database_name" == "database_name",
                                                                                           "denominator_strata_cohort_name" == "denominator_strata_cohort_name",
                                                                                           "outcome_cohort_name" == "outcome_cohort_name")) %>%
  select ("database_name","denominator_strata_cohort_name","outcome_cohort_name",
          "incidence_100000_pys","incidence_100000_pys_95CI_lower", "incidence_100000_pys_95CI_upper",
          "adj_ir","lci_ir","uci_ir")
  

### for comparison, merge it with my own results

# compare_crude_adj_rates <- crude_adj_rates %>% inner_join(incidence_estimates_standard, join_by ( "database_name" == "database_name",
#                                                                                               "denominator_strata_cohort_name" == "denominator_strata_cohort_name",
#                                                                                               "outcome_cohort_name" == "outcome_cohort_name")) %>%
#  select ("database_name","denominator_strata_cohort_name","outcome_cohort_name","crude_ir","incidence_100000_pys")

## crude, they are the same :-)




########################## Age specific estimates (use obs/exp) #################################

#estimates in the general population (overall analysis, all ages, both sexes)
incidence_estimates_IR_general_age <- incidence_estimates_general_help %>%
  select("analysis_interval","outcome_cohort_name","n_persons","n_events",
         "denominator_age_group","denominator_sex","incidence_100000_pys","database_name") %>%#
  filter(analysis_interval == "overall",
         denominator_age_group != "0;150" ,
         denominator_sex == "Both") %>%
  rename(ref_IR_100000_pys = incidence_100000_pys,
         ref_persons = n_persons,
         ref_events = n_events)

#standardised estimates in the specific populations
incidence_estimates_standard_age <- incidence_estimates_help %>% 
  select ("denominator_strata_cohort_name","analysis_interval","outcome_cohort_name","incidence_100000_pys",
          "denominator_age_group","denominator_sex","person_years","n_events","database_name",
          "incidence_100000_pys_95CI_lower", "incidence_100000_pys_95CI_upper") %>%
  inner_join(incidence_estimates_IR_general_age ,
             join_by (
               "analysis_interval" == "analysis_interval",
               "outcome_cohort_name" == "outcome_cohort_name",
               "denominator_age_group" == "denominator_age_group",
               "denominator_sex" == "denominator_sex",
               "database_name" == "database_name")) %>%
  select("database_name","denominator_strata_cohort_name","outcome_cohort_name","denominator_age_group",
         "n_events","incidence_100000_pys","ref_events","ref_persons","ref_IR_100000_pys",
         "person_years","incidence_100000_pys_95CI_lower", "incidence_100000_pys_95CI_upper") %>% 
  group_by(database_name, denominator_strata_cohort_name,outcome_cohort_name)
# need this group by at the end for the group_split


## THIS WOULD BE SO MUCH SHORTER!!

## add confidence interval (from Incidence Prevalence Package)
# incRateCiExact <- function(ev, pt) {
#   return(tibble::tibble(
#     incidence_100000_pys_95CI_lower =
#       ((stats::qchisq(p = 0.025, df = 2 * ev) / 2) / pt) * 100000,
#     incidence_100000_pys_95CI_upper =
#       ((stats::qchisq(p = 0.975, df = 2 * (ev + 1)) / 2) / pt) * 100000
#   ))


########### INSTEAD TO GET THE CONFIDENCE INTERVALS:


### from Ed use ageadjust.indirect(count,pop,stdcount,stdpop,stdrate = NULL, conf.level = 0.95)

list_for_adjusting <- group_split(incidence_estimates_standard_age)

results <- list()
for (i in list_for_adjusting){
  
  results <- c(results, ageadjust.indirect(count = i$n_events, 
                                           pop = i$person_years , 
                                           stdcount = i$ref_events, 
                                           stdrate = (i$ref_IR_100000_pys / 100000),
                                           stdpop = i$ref_persons))
  
}

name_list <- list()
for (i in 1:length(list_for_adjusting))
{
  name_list <- c(name_list, glue("{names(results)[i*2-1]},{list_for_adjusting[[i]]$database_name[1]},{list_for_adjusting[[i]]$denominator_strata_cohort_name[1]},{list_for_adjusting[[i]]$outcome_cohort_name[1]},"))
  name_list <- c(name_list, glue("{names(results)[i*2]},{list_for_adjusting[[i]]$database_name[1]},{list_for_adjusting[[i]]$denominator_strata_cohort_name[1]},{list_for_adjusting[[i]]$outcome_cohort_name[1]},"))
  
}

names(results) <- name_list

observed_crude <- sapply(results, "[",1, USE.NAMES = TRUE) %>% data.frame() 

crude <- observed_crude %>%
  filter(grepl("crude", rownames(observed_crude)))
crude <- crude %>%
  mutate(rown = rownames(crude)) %>% as_tibble() 
crude[c('rates','database_name','denominator_strata_cohort_name','outcome_cohort_name','type')] <- str_split_fixed(crude$rown, ',',5) 
crude <- crude %>%
  rename("crude_ir" = ".") %>%
  select("crude_ir","database_name","denominator_strata_cohort_name","outcome_cohort_name")


exp_adj <- sapply(results, "[",2, USE.NAMES = TRUE) %>% data.frame() 

adj <- exp_adj %>%
  filter(grepl("adj", rownames(exp_adj)))
adj <- adj %>%
  mutate(rown = rownames(adj)) %>% as_tibble() 
adj[c('rates','database_name','denominator_strata_cohort_name','outcome_cohort_name','type')] <- str_split_fixed(adj$rown, ',',5) 
adj <- adj %>%
  rename("adj_ir" = ".") %>%
  select("adj_ir","database_name","denominator_strata_cohort_name","outcome_cohort_name")


sir_lci <- sapply(results, "[",3, USE.NAMES = TRUE) %>% data.frame() 

lci_ir <- sir_lci %>%
  filter(grepl("lci", rownames(sir_lci)))
lci_ir <- lci_ir %>%
  mutate(rown = rownames(lci_ir)) %>% as_tibble() 
lci_ir[c('rates','database_name','denominator_strata_cohort_name','outcome_cohort_name','type')] <- str_split_fixed(lci_ir$rown, ',',5) 
lci_ir <- lci_ir %>%
  rename("lci_ir" = ".") %>%
  select("lci_ir","database_name","denominator_strata_cohort_name","outcome_cohort_name")



lci_uci <- sapply(results, "[",4, USE.NAMES = TRUE) %>% data.frame() 

uci_ir <- lci_uci %>%
  filter(grepl("uci", rownames(lci_uci)))
uci_ir <- uci_ir %>%
  mutate(rown = rownames(uci_ir)) %>% as_tibble() 
uci_ir[c('rates','database_name','denominator_strata_cohort_name','outcome_cohort_name','type')] <- str_split_fixed(uci_ir$rown, ',',5) 
uci_ir <- uci_ir %>%
  rename("uci_ir" = ".") %>%
  select("uci_ir","database_name","denominator_strata_cohort_name","outcome_cohort_name")



tibble_results <- observed %>% inner_join(crude, join_by ( "database_name" == "database_name",
                                                           "denominator_strata_cohort_name" == "denominator_strata_cohort_name",
                                                           "outcome_cohort_name" == "outcome_cohort_name")) %>%
  inner_join(exp, join_by ( "database_name" == "database_name",
                            "denominator_strata_cohort_name" == "denominator_strata_cohort_name",
                            "outcome_cohort_name" == "outcome_cohort_name")) %>%
  inner_join(adj, join_by ( "database_name" == "database_name",
                            "denominator_strata_cohort_name" == "denominator_strata_cohort_name",
                            "outcome_cohort_name" == "outcome_cohort_name")) %>%
  inner_join(sir, join_by ( "database_name" == "database_name",
                            "denominator_strata_cohort_name" == "denominator_strata_cohort_name",
                            "outcome_cohort_name" == "outcome_cohort_name")) %>%
  inner_join(lci_ir, join_by ( "database_name" == "database_name",
                               "denominator_strata_cohort_name" == "denominator_strata_cohort_name",
                               "outcome_cohort_name" == "outcome_cohort_name")) %>%
  inner_join(lci, join_by ( "database_name" == "database_name",
                            "denominator_strata_cohort_name" == "denominator_strata_cohort_name",
                            "outcome_cohort_name" == "outcome_cohort_name")) %>%
  inner_join(uci_ir, join_by ( "database_name" == "database_name",
                               "denominator_strata_cohort_name" == "denominator_strata_cohort_name",
                               "outcome_cohort_name" == "outcome_cohort_name")) %>%
  inner_join(uci, join_by ( "database_name" == "database_name",
                            "denominator_strata_cohort_name" == "denominator_strata_cohort_name",
                            "outcome_cohort_name" == "outcome_cohort_name"))



### for comparison, merge it with my own results

# crude_adj_rates <- tibble_results %>%
#   select("database_name","denominator_strata_cohort_name","outcome_cohort_name","crude_ir","adj_ir","lci_ir","uci_ir")  %>%
#   mutate(crude_ir = crude_ir * 100000,
#          adj_ir = adj_ir * 100000,
#          lci_ir = lci_ir * 100000,
#          uci_ir =  uci_ir * 100000) 
# 
# compare_crude_adj_rates <- crude_adj_rates %>% inner_join(incidence_estimates_standard, join_by ( "database_name" == "database_name",
#                                                                                               "denominator_strata_cohort_name" == "denominator_strata_cohort_name",
#                                                                                               "outcome_cohort_name" == "outcome_cohort_name")) %>%
#  select ("database_name","denominator_strata_cohort_name","outcome_cohort_name","crude_ir","incidence_100000_pys")

## crude, they are the same :-)
## adjusted not, because here I used the indirect function  



obs_exp_sir <- tibble_results %>%
  select("database_name","denominator_strata_cohort_name","outcome_cohort_name","observed","exp","sir","lci","uci")

### for comparison, merge it with my own results
# 
# compare_obs_exp_sir <- obs_exp_sir %>% inner_join(incidence_estimates_standard, join_by ( "database_name" == "database_name",
#                                                                                           "denominator_strata_cohort_name" == "denominator_strata_cohort_name",
#                                                                                           "outcome_cohort_name" == "outcome_cohort_name")) %>%
#   select ("database_name","denominator_strata_cohort_name","outcome_cohort_name","observed","exp","sir","standardized_ratio")
# 
# 



############################################################### DIRECT METHOD for adj--------------------------------------------------------------------------------------

### from Ed use ageadjust.indirect(count,pop,stdcount,stdpop,stdrate = NULL, conf.level = 0.95)

list_for_adjusting <- group_split(incidence_estimates_standard_age)

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
  name_list <- c(name_list, glue("{names(results)[(i-1)*4+1+j]},{list_for_adjusting[[i]]$database_name[1]},{list_for_adjusting[[i]]$denominator_strata_cohort_name[1]},{list_for_adjusting[[i]]$outcome_cohort_name[1]},"))
   }
}

names(results) <- name_list

observed_crude <- sapply(results, "[",1, USE.NAMES = TRUE) %>% data.frame() 

observed <- observed_crude %>%
  filter(grepl("observed", rownames(observed_crude))) 
observed <- observed %>%
  mutate(rown = rownames(observed)) %>% as_tibble() 
observed[c('stand','database_name','denominator_strata_cohort_name','outcome_cohort_name','type')] <- str_split_fixed(observed$rown, ',',5) 
observed <- observed %>%
  rename("observed" = ".") %>%
  select("observed","database_name","denominator_strata_cohort_name","outcome_cohort_name")

crude <- observed_crude %>%
  filter(grepl("crude", rownames(observed_crude)))
crude <- crude %>%
  mutate(rown = rownames(crude)) %>% as_tibble() 
crude[c('rates','database_name','denominator_strata_cohort_name','outcome_cohort_name','type')] <- str_split_fixed(crude$rown, ',',5) 
crude <- crude %>%
  rename("crude_ir" = ".") %>%
  select("crude_ir","database_name","denominator_strata_cohort_name","outcome_cohort_name")

exp_adj <- sapply(results, "[",1, USE.NAMES = TRUE) %>% data.frame() 

exp <- exp_adj %>%
  filter(grepl("exp", rownames(exp_adj))) 
exp <- exp %>%
  mutate(rown = rownames(exp)) %>% as_tibble() 
exp[c('stand','database_name','denominator_strata_cohort_name','outcome_cohort_name','type')] <- str_split_fixed(exp$rown, ',',5) 
exp <- exp %>%
  rename("exp" = ".") %>%
  select("exp","database_name","denominator_strata_cohort_name","outcome_cohort_name")

adj <- exp_adj %>%
  filter(grepl("adj", rownames(exp_adj)))
adj <- adj %>%
  mutate(rown = rownames(adj)) %>% as_tibble() 
adj[c('rates','database_name','denominator_strata_cohort_name','outcome_cohort_name','type')] <- str_split_fixed(adj$rown, ',',5) 
adj <- adj %>%
  rename("adj_ir" = ".") %>%
  select("adj_ir","database_name","denominator_strata_cohort_name","outcome_cohort_name")

sir_lci <- sapply(results, "[",1, USE.NAMES = TRUE) %>% data.frame() 

sir <- sir_lci %>%
  filter(grepl("sir", rownames(sir_lci))) 
sir <- sir %>%
  mutate(rown = rownames(sir)) %>% as_tibble() 
sir[c('stand','database_name','denominator_strata_cohort_name','outcome_cohort_name','type')] <- str_split_fixed(sir$rown, ',',5) 
sir <- sir %>%
  rename("sir" = ".") %>%
  select("sir","database_name","denominator_strata_cohort_name","outcome_cohort_name")

lci_ir <- sir_lci %>%
  filter(grepl("lci", rownames(sir_lci)))
lci_ir <- lci_ir %>%
  mutate(rown = rownames(lci_ir)) %>% as_tibble() 
lci_ir[c('rates','database_name','denominator_strata_cohort_name','outcome_cohort_name','type')] <- str_split_fixed(lci_ir$rown, ',',5) 
lci_ir <- lci_ir %>%
  rename("lci_ir" = ".") %>%
  select("lci_ir","database_name","denominator_strata_cohort_name","outcome_cohort_name")



lci_uci <- sapply(results, "[",1, USE.NAMES = TRUE) %>% data.frame() 

lci <- lci_uci %>%
  filter(grepl("lci", rownames(lci_uci))) 
lci <- lci %>%
  mutate(rown = rownames(lci)) %>% as_tibble() 
lci[c('stand','database_name','denominator_strata_cohort_name','outcome_cohort_name','type')] <- str_split_fixed(lci$rown, ',',5) 
lci <- lci %>%
  rename("lci" = ".") %>%
  select("lci","database_name","denominator_strata_cohort_name","outcome_cohort_name")

uci_ir <- lci_uci %>%
  filter(grepl("uci", rownames(lci_uci)))
uci_ir <- uci_ir %>%
  mutate(rown = rownames(uci_ir)) %>% as_tibble() 
uci_ir[c('rates','database_name','denominator_strata_cohort_name','outcome_cohort_name','type')] <- str_split_fixed(uci_ir$rown, ',',5) 
uci_ir <- uci_ir %>%
  rename("uci_ir" = ".") %>%
  select("uci_ir","database_name","denominator_strata_cohort_name","outcome_cohort_name")


uci_null <- sapply(results, "[",1, USE.NAMES = TRUE) %>% data.frame() 

uci <- uci_null %>%
  filter(grepl("uci", rownames(uci_null))) 
uci <- uci %>%
  mutate(rown = rownames(uci)) %>% as_tibble() 
uci[c('stand','database_name','denominator_strata_cohort_name','outcome_cohort_name','type')] <- str_split_fixed(uci$rown, ',',5) 
uci <- uci %>%
  rename("uci" = ".") %>%
  select("uci","database_name","denominator_strata_cohort_name","outcome_cohort_name")




tibble_results <- crude %>%
  
  inner_join(adj, join_by ( "database_name" == "database_name",
                            "denominator_strata_cohort_name" == "denominator_strata_cohort_name",
                            "outcome_cohort_name" == "outcome_cohort_name")) %>%
  
  inner_join(lci_ir, join_by ( "database_name" == "database_name",
                               "denominator_strata_cohort_name" == "denominator_strata_cohort_name",
                               "outcome_cohort_name" == "outcome_cohort_name")) %>%
  
  inner_join(uci_ir, join_by ( "database_name" == "database_name",
                               "denominator_strata_cohort_name" == "denominator_strata_cohort_name",
                               "outcome_cohort_name" == "outcome_cohort_name")) 


# Direct Standardisation ---------------------------------------------
## using the population distribution (direct method) of the general population
## age-adjusted rate is a weighted average of the age-specific (crude) rates,
#### where the weights are the proportions of persons in the corresponding age groups of a standard population.
#https://seer.cancer.gov/seerstat/tutorials/aarates/printable.html#:~:text=An%20age-adjusted%20rate%20is%20a%20weighted%20average%20of,the%20corresponding%20age%20groups%20of%20a%20standard%20population.


## age ADJUSTED rates only makes sense if we have only one sex (e.g. prostate cancer, breast cancer)
# or we do not expect sex to have a great impact (assumed in this case)
# need age distribution of general population

# 
# incidence_estimates_age_distribution <- incidence_estimates_general_help %>%
#   select("analysis_interval","outcome_cohort_name",
#          "denominator_age_group","denominator_sex","n_persons", "database_name") %>%
#   filter(analysis_interval == "overall",
#          denominator_sex == "Both",
#          denominator_age_group != "0;150") %>%
#   rename(ref_persons = n_persons) %>% 
#   inner_join(
#     incidence_estimates_help, 
#     join_by(
#       "analysis_interval" == "analysis_interval",
#       "outcome_cohort_name" == "outcome_cohort_name",
#       "denominator_age_group" == "denominator_age_group",
#       "database_name" == "database_name")
#   )
# 
# 
# #### need to know what can be added up in order to get the age adjusted output per group
# # we need to group by database, by outcome, by target cohort
# 
# 
# incidence_estimates_age <- incidence_estimates_age_distribution %>%
#   group_by(denominator_strata_cohort_name, outcome_cohort_name, database_name) %>%
#   mutate(ref_persons_total = sum(ref_persons)) %>%
#   ungroup() %>% 
#   arrange(ref_persons_total) %>%
#   mutate(age_distr = ref_persons/ref_persons_total) %>%
#   mutate(age_stand_IR_100000_pys_distr = incidence_100000_pys * age_distr) %>%
#   group_by(denominator_strata_cohort_name, outcome_cohort_name,database_name) %>%
#   summarise(age_stand_IR_100000_pys = sum(age_stand_IR_100000_pys_distr),
#             check = sum(age_distr)) %>%
#   arrange(database_name,outcome_cohort_name)

### for comparison, merge it with my own results

crude_adj_rates <- tibble_results %>%
  select("database_name","denominator_strata_cohort_name","outcome_cohort_name","crude_ir","adj_ir","lci_ir","uci_ir")  %>%
  mutate(crude_ir = crude_ir * 100000,
         adj_ir = adj_ir * 100000,
         lci_ir = lci_ir * 100000,
         uci_ir =  uci_ir * 100000) 

# compare_incidence_estimates_age  <- crude_adj_rates %>% 
#   inner_join(incidence_estimates_age, 
#              join_by ( "database_name" == "database_name",
#                        "denominator_strata_cohort_name" == "denominator_strata_cohort_name",
#                        "outcome_cohort_name" == "outcome_cohort_name"))  %>%
#   select("database_name","denominator_strata_cohort_name","outcome_cohort_name",
#          "crude_ir","age_stand_IR_100000_pys","adj_ir","lci_ir","uci_ir")
#   

## adjusted results are the same when using ageadjust.direct!!!



################################# same for yearly rates ------------------------------------------------------------------------

############################################################### DIRECT METHOD for adj--------------------------------------------------------------------------------------

#estimates in the general population (overall analysis, all ages, both sexes)
incidence_estimates_IR_general_age_years <- incidence_estimates_general_help %>%
  select("analysis_interval","outcome_cohort_name","n_persons","n_events",
         "denominator_age_group","denominator_sex","incidence_100000_pys","database_name",
         "year_index") %>%#
  filter(analysis_interval == "years",
         denominator_age_group != "0;150" ,
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

################################# same for monthly rates ------------------------------------------------------------------------

############################################################### DIRECT METHOD for adj--------------------------------------------------------------------------------------

#estimates in the general population (overall analysis, all ages, both sexes)
incidence_estimates_IR_general_age_months <- incidence_estimates_general_help %>%
  select("analysis_interval","outcome_cohort_name","n_persons","n_events",
         "denominator_age_group","denominator_sex","incidence_100000_pys","database_name",
         "year_month") %>%#
  filter(analysis_interval == "months",
         denominator_age_group != "0;150" ,
         denominator_sex == "Both") %>%
  rename(ref_IR_100000_pys = incidence_100000_pys,
         ref_persons = n_persons,
         ref_events = n_events)

#standardised estimates in the specific populations
incidence_estimates_standard_age_months <- incidence_estimates_help %>% 
  select ("denominator_strata_cohort_name","analysis_interval","outcome_cohort_name","incidence_100000_pys",
          "denominator_age_group","denominator_sex","person_years","n_events","database_name",
          "incidence_100000_pys_95CI_lower", "incidence_100000_pys_95CI_upper","year_month") %>%
  inner_join(incidence_estimates_IR_general_age_months ,
             join_by (
               "analysis_interval" == "analysis_interval",
               "outcome_cohort_name" == "outcome_cohort_name",
               "denominator_age_group" == "denominator_age_group",
               "denominator_sex" == "denominator_sex",
               "database_name" == "database_name",
               "year_month" == "year_month")) %>%
  select("database_name","denominator_strata_cohort_name","outcome_cohort_name","denominator_age_group",
         "n_events","incidence_100000_pys","ref_events","ref_persons","ref_IR_100000_pys",
         "person_years","incidence_100000_pys_95CI_lower", "incidence_100000_pys_95CI_upper","year_month") %>% 
  group_by(database_name, denominator_strata_cohort_name,outcome_cohort_name,year_month)
# need this group by at the end for the group_split


### from Ed use ageadjust.indirect(count,pop,stdcount,stdpop,stdrate = NULL, conf.level = 0.95)

list_for_adjusting <- group_split(incidence_estimates_standard_age_months)

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
    name_list <- c(name_list, glue("{names(results)[(i-1)*4+1+j]},{list_for_adjusting[[i]]$database_name[1]},{list_for_adjusting[[i]]$denominator_strata_cohort_name[1]},{list_for_adjusting[[i]]$outcome_cohort_name[1]},{list_for_adjusting[[i]]$year_month[1]},"))
  }
}

names(results) <- name_list

observed_crude <- sapply(results, "[",1, USE.NAMES = TRUE) %>% data.frame() 

crude <- observed_crude %>%
  filter(grepl("crude", rownames(observed_crude)))
crude <- crude %>%
  mutate(rown = rownames(crude)) %>% as_tibble() 
crude[c('rates','database_name','denominator_strata_cohort_name','outcome_cohort_name','year_month','type')] <- str_split_fixed(crude$rown, ',',6) 
crude <- crude %>%
  rename("crude_ir" = ".") %>%
  select("crude_ir","database_name","denominator_strata_cohort_name","outcome_cohort_name","year_month")

exp_adj <- sapply(results, "[",1, USE.NAMES = TRUE) %>% data.frame() 

adj <- exp_adj %>%
  filter(grepl("adj", rownames(exp_adj)))
adj <- adj %>%
  mutate(rown = rownames(adj)) %>% as_tibble() 
adj[c('rates','database_name','denominator_strata_cohort_name','outcome_cohort_name','year_month','type')] <- str_split_fixed(adj$rown, ',',6) 
adj <- adj %>%
  rename("adj_ir" = ".") %>%
  select("adj_ir","database_name","denominator_strata_cohort_name","outcome_cohort_name","year_month")

sir_lci <- sapply(results, "[",1, USE.NAMES = TRUE) %>% data.frame() 

lci_ir <- sir_lci %>%
  filter(grepl("lci", rownames(sir_lci)))
lci_ir <- lci_ir %>%
  mutate(rown = rownames(lci_ir)) %>% as_tibble() 
lci_ir[c('rates','database_name','denominator_strata_cohort_name','outcome_cohort_name','year_month','type')] <- str_split_fixed(lci_ir$rown, ',',6) 
lci_ir <- lci_ir %>%
  rename("lci_ir" = ".") %>%
  select("lci_ir","database_name","denominator_strata_cohort_name","outcome_cohort_name","year_month")



lci_uci <- sapply(results, "[",1, USE.NAMES = TRUE) %>% data.frame() 

uci_ir <- lci_uci %>%
  filter(grepl("uci", rownames(lci_uci)))
uci_ir <- uci_ir %>%
  mutate(rown = rownames(uci_ir)) %>% as_tibble() 
uci_ir[c('rates','database_name','denominator_strata_cohort_name','outcome_cohort_name','year_month','type')] <- str_split_fixed(uci_ir$rown, ',',6) 
uci_ir <- uci_ir %>%
  rename("uci_ir" = ".") %>%
  select("uci_ir","database_name","denominator_strata_cohort_name","outcome_cohort_name","year_month")




tibble_results <- crude %>%
  
  inner_join(adj, join_by ( "database_name" == "database_name",
                            "denominator_strata_cohort_name" == "denominator_strata_cohort_name",
                            "outcome_cohort_name" == "outcome_cohort_name",
                            "year_month" == "year_month"
  )) %>%
  
  inner_join(lci_ir, join_by ( "database_name" == "database_name",
                               "denominator_strata_cohort_name" == "denominator_strata_cohort_name",
                               "outcome_cohort_name" == "outcome_cohort_name",
                               "year_month" == "year_month")) %>%
  
  inner_join(uci_ir, join_by ( "database_name" == "database_name",
                               "denominator_strata_cohort_name" == "denominator_strata_cohort_name",
                               "outcome_cohort_name" == "outcome_cohort_name",
                               "year_month" == "year_month")) 



crude_adj_rates_months <- tibble_results %>%
  select("database_name","denominator_strata_cohort_name","outcome_cohort_name","year_month","crude_ir","adj_ir","lci_ir","uci_ir")  %>%
  mutate(crude_ir = crude_ir * 100000,
         adj_ir = adj_ir * 100000,
         lci_ir = lci_ir * 100000,
         uci_ir =  uci_ir * 100000) 



########################################### Plots 1-3  ---------------------------------------------------------------------------------


adj_ir_sir <- function(specific_population){

plot.data <- crude_adj_rates %>%
  mutate(outcome_cohort_name = factor(outcome_cohort_name,
                             levels=c("dysautonomia",
                                      "ibd",
                                      "me_cfs",
                                      "me_cfs_symptoms",
                                      "pots",
                                      "ra",
                                      "t1dm",
                                      "sle",
                                      "misc",
                                      "juvenilearthritis"))) %>%
  filter(denominator_strata_cohort_name == specific_population)


p1 <-   plot.data %>% 
  ggplot(aes(y=database_name, color = database_name)) + 
  geom_errorbarh(aes(xmin = lci_ir, xmax =  uci_ir), height = 0, size = 1) +
  geom_point(aes(x=adj_ir),
             size=2.5) +
  facet_grid(outcome_cohort_name~., scales="free", switch = "y") +
  theme_bw() +
  ylab("") +
  xlab("age adjusted IR (95% CI) [log scale]") +
  scale_y_discrete(position = "left",limits=rev) +
  scale_x_continuous(label=label_comma(accuracy= 1), trans="pseudo_log",
                      breaks = c(10,100,1000,10000)) +
  theme(
    axis.text.y= element_blank(),
    axis.ticks.y= element_blank(),
    legend.position="none")
  


plot.data <- obs_exp_sir %>%
  mutate(outcome_cohort_name = factor(outcome_cohort_name,
                                      levels=c("dysautonomia",
                                               "ibd",
                                               "me_cfs",
                                               "me_cfs_symptoms",
                                               "pots",
                                               "ra",
                                               "t1dm",
                                               "sle",
                                               "misc",
                                               "juvenilearthritis"))) %>%
  filter(denominator_strata_cohort_name == specific_population)
  
p2 <-  plot.data %>% 
  ggplot(aes(y=database_name, fill = database_name, color = database_name)) + 
  geom_errorbarh(aes(xmin = lci, xmax =  uci), height = 0, size = 1) +
  geom_point(aes(x=sir),
             size=2.5) +
  facet_grid(outcome_cohort_name~., scales="free", switch = "y") +
  theme_bw() +
  ylab("") +
  geom_vline(xintercept = 1)+
  xlab("standardised incidence ratio (95% CI) [log scale]") +
  scale_y_discrete(position = "left",limits=rev) +
  scale_x_continuous(label=label_comma(accuracy= 1), trans="pseudo_log",
                     breaks = c(0,0.5,1,2)) +
  theme(
    axis.text.y= element_blank(),
    axis.ticks.y= element_blank(),
    legend.position="none")


  

pg<-cowplot::plot_grid(p1,p2, rel_widths = c(0.5,0.25))
return(cowplot::plot_grid(legend,pg, rel_heights =  c(0.25, 3), ncol = 1))
  
}

#figure 1
adj_ir_sir(specific_population = "infection")

#figure 2
adj_ir_sir(specific_population = "reinfection")

#figure 3
adj_ir_sir(specific_population = "test_negative")



########################################### Plots yearly ---------------------------------------------------------------------------------



plot.data <- crude_adj_rates_years  %>% 
  group_by(database_name, denominator_strata_cohort_name, outcome_cohort_name) %>%
  mutate( number_point = n()) %>% 
  ungroup() %>% 
  filter(number_point >= 1) %>%
  group_by(database_name)

list_for_plots <- group_split(plot.data)

## 3 lines per plot, 1 outcome per plot

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



########################################### Plots monthly ---------------------------------------------------------------------------------



plot.data <- crude_adj_rates_months  %>% 
  group_by(database_name, denominator_strata_cohort_name, outcome_cohort_name) %>%
  mutate( number_point = n()) %>% 
  ungroup() %>% 
  filter(number_point >= 1) %>%
  group_by(database_name)

list_for_plots <- group_split(plot.data)

## 3 lines per plot, 1 outcome per plot

p1 <- plot.new()
p2 <- plot.new()


for (i in 1:length(list_for_plots)){
  
  assign(glue("p{i}"),ggplot(as_tibble(list_for_plots[[i]]), aes( x = factor(year_month), y = adj_ir, color = denominator_strata_cohort_name)) + 
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

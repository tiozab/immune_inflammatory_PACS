## summarise study period from databases

incidence_estimates_help <- rbind(CPRDGOLD[[2]],
                                  IMASIS[[2]],
                                  Pharmetrics[[2]],
                                  IPCI[[2]],
                                  eDOL_CHUM[[2]],
                                  CORIVA[[2]],
                                  CPRDAurum[[2]],
                                  University_of_Oslo[[2]],
                                  Pharmetrics[[2]],
                                  AUSOM[[2]])

summary <- incidence_estimates_help %>% group_by(database_name) %>%
  summarise(max(incidence_end_date))

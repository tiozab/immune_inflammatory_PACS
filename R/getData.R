## read in the results files

source(here::here("R","read_in.R"))
CPRDGOLD <- read_in(database_results_name = "CPRDGOLD")
Pharmetrics <- read_in(database_results_name = "Pharmetrics")
IMASIS <- read_in(database_results_name = "IMASIS")
AUSOM <- read_in(database_results_name = "AUSOM")
eDOL_CHUM <- read_in(database_results_name = "eDOL_CHUM")
IPCI <- read_in(database_results_name = "IPCI")
CORIVA <- read_in(database_results_name = "CORIVA")


#load outcome names
names_conditions <- read_csv(here::here("names_conditions.csv"))

## put the results from the individual data sources into one file

# general population
incidence_estimates_general_help <- rbind(CPRDGOLD[[1]],
                                          Pharmetrics[[1]],
                                          IMASIS[[1]],
                                          AUSOM[[1]],
                                          IPCI[[1]],
                                          eDOL_CHUM[[1]],
                                          CORIVA[[1]])

# specific populations
incidence_estimates_help <- rbind(CPRDGOLD[[2]],
                                  Pharmetrics[[2]],
                                  IMASIS[[2]],
                                  AUSOM[[2]],
                                  IPCI[[2]],
                                  eDOL_CHUM[[2]],
                                  CORIVA[[2]])



## read in the results files

source(here::here("R","read_in.R"))
CPRDGOLD <- read_in(database_results_name = "CPRDGOLD")
Pharmetrics <- read_in(database_results_name = "Pharmetrics")
IMASIS <- read_in(database_results_name = "IMASIS")
AUSOM <- read_in(database_results_name = "AUSOM")
eDOL_CHUM <- read_in(database_results_name = "eDOL_CHUM")
IPCI <- read_in(database_results_name = "IPCI")
CORIVA <- read_in(database_results_name = "CORIVA")
CPRDAurum <- read_in(database_results_name = "CPRDAurum")
University_of_Oslo <- read_in(database_results_name = "University_of_Oslo")

#load outcome names
names_conditions <- read_csv(here::here("names_conditions.csv"))




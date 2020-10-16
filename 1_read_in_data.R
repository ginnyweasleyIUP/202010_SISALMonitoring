## Read in DATA

Bunker <- list()
path_bunker = "/home/ginnyweasley/Dokumente/01_Promotion/06_Daten/02_SISAL/SISAL_Monitoring/2020-09-22_monitoringdb_nk_lcb_bunker_nk3_dr3.xlsx"

Bunker$precip_sample <- readxl::read_xlsx(path = path_bunker, sheet = "precip_sample", skip =1)
Bunker$precip_d18O <- readxl::read_xlsx(path = path_bunker, sheet = "precip_d18O", skip =1)
Bunker$cave_t <- readxl::read_xlsx(path = path_bunker, sheet = "cave_t", skip =1)
Bunker$cave_rh <- readxl::read_xlsx(path = path_bunker, sheet = "cave_rh", skip =1)
Bunker$cave_pco2 <- readxl::read_xlsx(path = path_bunker, sheet = "cave_pco2", skip =1)
Bunker$drip_d18O <- readxl::read_xlsx(path = path_bunker, sheet = "drip_d18O", skip =1)


Cathedral <- list()

path_cathedral = "/home/ginnyweasley/Dokumente/01_Promotion/06_Daten/02_SISAL/SISAL_Monitoring/2020-09-22_monitoringdb_nk_lcb_cathedral_ab3_nk4.xlsx"

Cathedral$precip_sample <- readxl::read_xlsx(path = path_cathedral, sheet = "precip_sample", skip =1)
Cathedral$precip_d18O <- readxl::read_xlsx(path = path_cathedral, sheet = "precip_d18O", skip =1)
Cathedral$cave_t <- readxl::read_xlsx(path = path_cathedral, sheet = "cave_t", skip =1)
Cathedral$cave_rh <- readxl::read_xlsx(path = path_cathedral, sheet = "cave_rh", skip =1)
Cathedral$cave_pco2 <- readxl::read_xlsx(path = path_cathedral, sheet = "cave_pco2", skip =1)
Cathedral$drip_d18O <- readxl::read_xlsx(path = path_cathedral, sheet = "drip_d18O", skip =1)

save(Bunker , file = "Bunker.RData")
save(Cathedral , file = "Cathedral.RData")

source("SISAL_extracting.R")

data <- load_sisal_data(year_start = 50, year_stop = -50, min_period = 50, min_dating = 1, min_d18O = 5)

CAVES <- list()
CAVES$entity242 <- data[[2]] %>% filter(entity_id == 242) 

#conversion to dripwater equivalents:

CAVES$entity242$d18O_dweq <- 1.03092 * (CAVES$entity242$d18O_measurement - ((16.1*1000)/(mean(Bunker$cave_t$cave_t, na.rm = T)+273.15)-24.6)) + 30.92
CAVES$entity242$d18O_SMOW <- 1.03092 * (CAVES$entity242$d18O_measurement) + 30.92

save(CAVES, file = "CAVES.RData")

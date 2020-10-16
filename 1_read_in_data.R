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

Mawmluh <- list()
path_mawmluh = "/home/ginnyweasley/Dokumente/01_Promotion/06_Daten/02_SISAL/SISAL_Monitoring/2020-09-22_monitoringdb_nk_lcb_mawmluh_fl3_nk3.xlsx"
Mawmluh$precip_sample <- readxl::read_xlsx(path = path_mawmluh, sheet = "precip_sample", skip =1)
Mawmluh$precip_d18O <- readxl::read_xlsx(path = path_mawmluh, sheet = "precip_d18O", skip =1)
Mawmluh$cave_t <- readxl::read_xlsx(path = path_mawmluh, sheet = "cave_t", skip =1)
Mawmluh$cave_rh <- readxl::read_xlsx(path = path_mawmluh, sheet = "cave_rh", skip =1)
Mawmluh$cave_pco2 <- readxl::read_xlsx(path = path_mawmluh, sheet = "cave_pco2", skip =1)
Mawmluh$drip_d18O <- readxl::read_xlsx(path = path_mawmluh, sheet = "drip_d18O", skip =1)


South_Glory <- list()
path_south_glory = "/home/ginnyweasley/Dokumente/01_Promotion/06_Daten/02_SISAL/SISAL_Monitoring/2020-09-22_monitoringdb_nk_lcb_south_glory_ab1_nk1.xlsx"
South_Glory$precip_sample <- readxl::read_xlsx(path = path_south_glory, sheet = "precip_sample", skip =1)
South_Glory$precip_d18O <- readxl::read_xlsx(path = path_south_glory, sheet = "precip_d18O", skip =1)
South_Glory$cave_t <- readxl::read_xlsx(path = path_south_glory, sheet = "cave_t", skip =1)
South_Glory$cave_rh <- readxl::read_xlsx(path = path_south_glory, sheet = "cave_rh", skip =1)
South_Glory$cave_pco2 <- readxl::read_xlsx(path = path_south_glory, sheet = "cave_pco2", skip =1)
South_Glory$drip_d18O <- readxl::read_xlsx(path = path_south_glory, sheet = "drip_d18O", skip =1)




save(Bunker , file = "Bunker.RData")
save(Cathedral , file = "Cathedral.RData")
save(Mawmluh, file = "Mawmluh.RData")
save(South_Glory, file = "South_Glory.RData")

source("SISAL_extracting.R")
source("BilinearExtraction_HadCM3.R")

data <- load_sisal_data(year_start = 150, year_stop = -50, min_period = 30, min_dating = 1, min_d18O = 5)

#We need to extract temp data and d18O at cave locations:
#bunker 51.4142,7.4903
#south glory -35.7, 148.5
#mawmluh 25.25489, 91.716408
#cathedral -32.6, 148.5

CAVES <- list()
CAVES$entity242 <- data[[2]] %>% filter(entity_id == 242) 
CAVES$bunker_hadcm3 <- list()
CAVES$bunker_hadcm3$time = seq(from = 1350, to = 1850, by = 1)
CAVES$bunker_hadcm3$temp = HadCM3_LMill_Data(variable = "TEMP", longitude = 7.4903, latitude = 51.4142, year_start = 1350, year_stop = 1850)
CAVES$bunker_hadcm3$isot = HadCM3_LMill_Data(variable = "ISOT", longitude = 7.4903, latitude = 51.4142, year_start = 1350, year_stop = 1850)

CAVES$cathedral_hadcm3 <- list()
CAVES$cathedral_hadcm3$time = seq(from = 1350, to = 1850, by = 1)
CAVES$cathedral_hadcm3$temp = HadCM3_LMill_Data(variable = "TEMP", longitude = 148.5, latitude = -32.6, year_start = 1350, year_stop = 1850)
CAVES$cathedral_hadcm3$isot = HadCM3_LMill_Data(variable = "ISOT", longitude = 148.5, latitude = -32.6, year_start = 1350, year_stop = 1850)

CAVES$south_glory_hadcm3 <- list()
CAVES$south_glory_hadcm3$time = seq(from = 1350, to = 1850, by = 1)
CAVES$south_glory_hadcm3$temp = HadCM3_LMill_Data(variable = "TEMP", longitude = 148.5, latitude = -35.7, year_start = 1350, year_stop = 1850)
CAVES$south_glory_hadcm3$isot = HadCM3_LMill_Data(variable = "ISOT", longitude = 148.5, latitude = -35.7, year_start = 1350, year_stop = 1850)

CAVES$entity451 <- data[[2]] %>% filter(entity_id == 451)
CAVES$mawmluh_hadcm3 <- list()
CAVES$mawmluh_hadcm3$time = seq(from = 1350, to = 1850, by = 1)
CAVES$mawmluh_hadcm3$temp = HadCM3_LMill_Data(variable = "TEMP", longitude = 91.72, latitude = 25.25, year_start = 1350, year_stop = 1850)
CAVES$mawmluh_hadcm3$isot = HadCM3_LMill_Data(variable = "ISOT", longitude = 91.72, latitude = 25.25, year_start = 1350, year_stop = 1850)

#conversion to dripwater equivalents:

CAVES$entity242$d18O_dweq <- 1.03092 * (CAVES$entity242$d18O_measurement - ((16.1*1000)/(mean(Bunker$cave_t$cave_t, na.rm = T)+273.15)-24.6)) + 30.92
CAVES$entity242$d18O_dweq_hadcm3 <- 1.03092 * (CAVES$entity242$d18O_measurement - ((16.1*1000)/(mean(CAVES$bunker_hadcm3$temp, na.rm = T)+273.15)-24.6)) + 30.92
CAVES$entity451$d18O_dweq <- 1.03092 * (CAVES$entity451$d18O_measurement - ((18.34*1000)/(mean(Mawmluh$cave_t$cave_t, na.rm = T)+273.15)-31.954)) + 30.92
CAVES$entity451$d18O_dweq_hadcm3 <- 1.03092 * (CAVES$entity451$d18O_measurement - ((18.34*1000)/(mean(CAVES$mawmluh_hadcm3$temp, na.rm = T)+273.15)-31.954)) + 30.92

#aragonit
#dw_eq[jj] = 1.03092 * (data_rec$d18O_measurement[jj] - ((18.34*1000)/(data_sim[[paste0("TEMP_", run)]][jj]+273.15)-31.954)) + 30.92

save(CAVES, file = "CAVES.RData")

#Entity_Info_SISAL

library(plyr)
library(dplyr)
library(tidyverse)

prefix = ""
#path = "/stacywork/ginnyweasley/02_SISAL/SISAL_v2/"
path = "/home/ginnyweasley/Dokumente/01_Promotion/06_Daten/02_SISAL/SISAL_v2/"
composite_link_entity <- read.csv(paste(path, prefix,'composite_link_entity.csv',sep = ''), header = T,stringsAsFactors = F)
d13C <- read.csv(paste(path, prefix,'d13c.csv',sep='') ,header = T, stringsAsFactors = F)
d13C <- plyr::rename(d13C, c("iso_std" = "iso_std_d13C"))
d18O <- read.csv(paste(path, prefix,'d18o.csv', sep =''),header = T, stringsAsFactors = F)
d18O <- plyr::rename(d18O, c("iso_std" = "iso_std_d18O"))
dating_lamina <- read.csv(paste(path, prefix,'dating_lamina.csv', sep = ''), header = T, stringsAsFactors = F)
dating <- read.csv(paste(path, prefix,'dating.csv',sep = ''), header = T, stringsAsFactors = F)
entity <- read.csv(paste(path, prefix,'entity.csv', sep = ''), header = T, stringsAsFactors = F)
gap <- read.csv(paste(path, prefix,'gap.csv', sep = ''), header = T, stringsAsFactors = F)
hiatus <- read.csv(paste(path, prefix,'hiatus.csv', sep =''), header = T, stringsAsFactors = F)
original_chronology <- read.csv(paste(path, prefix,'original_chronology.csv', sep = ''), header = T, stringsAsFactors = F)
sample <- read.csv(paste(path, prefix,'sample.csv', sep = ''), header = T, stringsAsFactors = F)
site <- read.csv(paste(path, prefix,'site_countries.csv', sep = ''), header = T, stringsAsFactors = F)
  
  
# build SISAL tables
site_tb <- left_join(site, entity, by = 'site_id') %>% mutate_at(vars(site_id, entity_id), as.numeric)
dating_tb <- left_join(dating, entity) %>% group_by(entity_id) %>%mutate(laminar_dated = if_else((entity_id %in% dating_lamina$entity_id), 'yes', 'no')) %>% 
  mutate_at(vars(dating_id, depth_dating, dating_thickness, X14C_correction, corr_age, corr_age_uncert_pos, corr_age_uncert_neg), as.numeric) %>%ungroup()
sample_tb <- plyr::join_all(list(sample,hiatus, gap, original_chronology, d13C, d18O), by = 'sample_id', type = 'full', match = 'all') %>% 
  mutate_at(vars(entity_id, sample_id, sample_thickness, depth_sample, interp_age, interp_age_uncert_pos, interp_age_uncert_neg, d13C_measurement,
                   d13C_precision, d18O_measurement, d18O_precision), as.numeric)
  
  
# filter for 'from base' dated entities
entity_from_base <- site_tb %>% filter(depth_ref == 'from base') %>% distinct(entity_id)
sample_from_base <- sample_tb %>% filter(entity_id %in% entity_from_base$entity_id) %>% 
  select(entity_id,depth_sample) %>% group_by(entity_id) %>% dplyr::summarise(max = max(depth_sample))
  
# transform depths for 'from base' dated entities in dating file
dating_tb_new <- full_join(dating_tb, sample_from_base, by = 'entity_id') %>% group_by(entity_id) %>% 
  mutate(depth_conv = if_else(entity_id %in% entity_from_base$entity_id, max-depth_dating, NA_real_)) %>% 
  mutate(depth_dating = if_else(!is.na(depth_conv), depth_conv, depth_dating)) %>%
  select(-depth_conv) %>% arrange(., depth_dating, .by_group = T)

#transform depths for 'from base' dated entities in sample file
sample_tb_new <- full_join(sample_tb, sample_from_base, by = 'entity_id') %>% group_by(entity_id) %>% 
  mutate(depth_conv = if_else(entity_id %in% entity_from_base$entity_id, max-depth_sample, NA_real_)) %>% 
  mutate(depth_sample = if_else(!is.na(depth_conv), depth_conv, depth_sample)) %>%
  select(-depth_conv) %>% arrange(., depth_sample, .by_group = T)
  
  
## filter dating table
# filter if status is current and gets all hiatuses out
dating_tb_withouthiatus <- dating_tb_new %>% filter(entity_status == 'current') %>% mutate_at(vars(corr_age),as.numeric) %>% 
  filter(date_used == 'yes' & date_type != 'Event; hiatus'& date_type != 'Event; actively forming'& date_type != 'Event; start of laminations'& date_type != 'Event; end of laminations')

only_h <-  dating %>% group_by(entity_id) %>% filter(all(date_type == 'Event; hiatus')) %>% distinct(entity_id)

# entities with more than 3 dates
nr_dates <- dating_tb %>% filter(date_used == 'yes' & date_used != 'Event; hiatus') %>% dplyr::select(entity_id, corr_age) %>% group_by(entity_id) %>%count() %>% filter(n>=3)

# entities containing only U/Th dates, enough dates, sample depths; 523
run <- dating_tb_withouthiatus %>% distinct(entity_id) %>%
  filter(entity_id %in% nr_dates$entity_id) %>%
  filter(!(entity_id %in% only_h$entity_id)) %>%
  arrange(., entity_id)


dating_tb_filtered <- dating_tb_withouthiatus %>% filter(entity_id %in% run$entity_id)
sample_tb_new_filtered <- sample_tb_new %>% filter(entity_id %in% run$entity_id)
site_tb_filtered <- site_tb %>% filter(entity_id %in% run$entity_id)

## Now sort data and get extra information

entities <- sample_tb_new_filtered %>% group_by(entity_id) %>% count() %>% rename(d18O_measurements = n)
entities_dating <- dating_tb_filtered %>% group_by(entity_id) %>% count() %>% rename(datings = n)
entities_period <- sample_tb_new_filtered %>% select(entity_id, interp_age) %>% 
  summarise(min_corr_age = round(min(interp_age, na.rm = T), digits = 2),
            max_corr_age = round(max(interp_age, na.rm = T), digits = 2)) %>% 
  mutate(period = max_corr_age -min_corr_age)

entities_mineralogy <- sample_tb_new_filtered %>% select(entity_id, mineralogy) %>% filter(mineralogy != "NULL") %>% group_by(entity_id) %>% distinct()
entities_min_double <- entities_mineralogy %>% group_by(entity_id) %>% count() %>% filter(n>1)
entities_min_double$mineralogy = "mixed"
entities_min_double <- entities_min_double %>% select(entity_id, mineralogy)
entities_mineralogy_2 <- entities_mineralogy %>% filter(!entity_id %in% entities_min_double$entity_id)
entities_mineralogy_3 <- rbind(entities_mineralogy_2, entities_min_double) %>% arrange(entity_id)

sample_mean <- sample_tb_new_filtered %>% distinct(entity_id, d18O_measurement) %>% summarise(mean_d18O = round(mean(d18O_measurement, na.rm = T), digits = 3))

sites <- site_tb_filtered %>% select(site_id, entity_id, latitude, longitude, elevation, geology, cover_thickness, distance_entrance) %>% distinct()
all <- join_all(list(sites, entities, entities_dating, entities_period, entities_mineralogy_3, sample_mean), by = "entity_id") %>% arrange(entity_id)

  
write.csv(all, file = "SISAL_entity_info_all.csv", row.names = FALSE)



remove(composite_link_entity, d13C, d18O, dating, dating_lamina, dating_tb, dating_tb_filtered, dating_tb_new, dating_tb_withouthiatus,
       entities, entities_dating, entities_min_double, entities_mineralogy, entities_mineralogy_2, entities_mineralogy_3, entities_period,
       entity, entity_from_base, gap, hiatus, nr_dates, only_h, original_chronology, run, sample, sample_from_base, sample_mean, sample_tb,
       sample_tb_new, sample_tb_new_filtered, site, site_tb, site_tb_filtered, sites, prefix)








inat_insecta_sample <- fread("data/downloads/gbif/inat_insecta_sample.csv")
inat_insecta_selection <- inat_insecta_sample[sample(1:50000, 10000)]

some_inat_insecta_obs <- inat_insecta_selection %>%
  mutate(jday = yday(eventDate), 
         jd_wk = 7*floor(jday/7)) %>%
  filter(jd_wk >= minJulianWeek, jd_wk <= maxJulianWeek) %>%
  select(catalogNumber, eventDate, year, jday, jd_wk, recordedBy, decimalLatitude, decimalLongitude) %>%
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude")) %>%
  st_set_crs(4326) %>%
  st_intersection(hex) %>%
  st_set_geometry(NULL) %>%
  group_by(cell, year, jd_wk) %>%
  summarize(nObs = n_distinct(catalogNumber))

#write_csv(inat_insecta_obs, "data/inat_hex_week_insects.csv")

inat_insect_hexfilter <- some_inat_insecta_obs %>%
  filter(cell %in% hex_subset$cell) %>%
  filter(nObs > 1) %>% #will be 50 with full dataset
  ungroup() %>%
  mutate_at(c("year", "cell"), as.numeric) %>%
  left_join(hex_start_end, by = c("year" = "Year", "cell")) %>%
  filter(jd_wk <= end, jd_wk >= start) %>%
  group_by(cell, year) %>%
  mutate(n_wks = n()) %>%
  filter(n_wks >= 6)

# Calculate caterpillars/insect observations per week for inaturalist
# this one also takes forever

cats1 <- fread("data/downloads/inat_caterpillars/2015_01_01-2019_12_31_CENA.csv", sep = ",") %>%
  mutate_at("time_observed_at", as.character) %>%
  mutate_at("observed_on", as.Date)
cats2 <- fread("data/downloads/inat_caterpillars/2020_01_01-2021_09_30_CENA.csv", sep = ",") %>%
  select(-created_at) %>%
  mutate_at("time_observed_at", as.character) %>%
  mutate_at("observed_on", as.Date)
cats3 <- fread("data/downloads/inat_caterpillars/2021_10_01-2023_08_31_CENA.csv", sep = ",") %>%
  mutate_at("time_observed_at", as.character) %>%
  mutate_at("observed_on", as.Date)
cats4 <- fread("data/downloads/inat_caterpillars/2023_09_01-2024_12_31_CENA.csv", sep = ",") %>%
  mutate_at("time_observed_at", as.character) %>%
  mutate_at("observed_on", as.Date)
cats5 <- fread("data/downloads/inat_caterpillars/2025_01_01-2025_12_31_CENA.csv", sep = ",") %>%
  mutate_at("time_observed_at", as.character) %>%
  mutate_at("observed_on", as.Date)

cats_ena <- bind_rows(cats1, cats2, cats3, cats4, cats5)

cats_ena2 = cats_ena[sample(1:nrow(cats_ena), 10000)]

cats_pheno <- cats_ena2 %>%
  select(id, observed_on, user_login, url, latitude, longitude, scientific_name, common_name, taxon_id, taxon_family_name, taxon_genus_name) %>%
  filter(user_login != "caterpillarscount") %>%
  mutate(year = as.numeric(substr(observed_on, 1, 4)),
         jday = yday(observed_on), 
         jd_wk = 7*floor(jday/7)) %>%
  filter(year >= 2015, year <= 2026) %>%
  st_as_sf(coords = c("longitude", "latitude")) %>%
  st_set_crs(4326) %>%
  st_intersection(hex) %>%
  st_set_geometry(NULL) %>%
  group_by(cell, year, jd_wk) %>%
  summarize(nCats = n_distinct(id))

## Peak and centroid dates and anomalies for caterpillars (effort corrected)

inat_combined_pheno <- cats_pheno %>%
  right_join(inat_insect_hexfilter, by = c("cell", "year", "jd_wk")) %>%
  mutate(cat_effort = nCats/nObs) %>%
  group_by(cell, year) %>%
  summarize(peakDate = median(jd_wk[cat_effort == max(cat_effort, na.rm = T)], na.rm = T),
            centroidDate = sum(jd_wk*cat_effort, na.rm = T)/sum(cat_effort, na.rm = T)) %>%
  group_by(cell) %>%
  mutate(meanPeakDate = mean(peakDate, na.rm = T),
         meanCentroidDate = mean(centroidDate, na.rm = T),
         devPeakDate = meanPeakDate - peakDate,
         devCentroidDate = meanCentroidDate - centroidDate)

## Temperature anomalies

hex_temps <- read.csv("data/derived_data/hex_mean_temps.csv", stringsAsFactors = F)
hex_temps_dummy <- bind_rows(hex_temps, hex_temps) %>%
  mutate(year = c(year[1:(length(year)/2)], year[1:(length(year)/2)]+8)) # 2012:2019 duplicated gives dummy data for 2020:2027

cell_pheno <- cell_pheno %>%
  ungroup() %>%
  mutate_at(c("cell"), ~as.numeric(as.character(.)))

temp_dev <- hex_temps_dummy %>%
  right_join(inat_combined_pheno, by = c("cell", "year")) %>%
  group_by(cell) %>%
  mutate(tempDev = mean(mean_temp) - mean_temp) %>%
  left_join(cell_pheno, by = c("cell", "year" = "Year"))

## Figure: peak and centroid date deviations vs temperature deviations, peak & centroid v temp and lat in 2019 iNat and CC diff colors

theme_set(theme_classic(base_size = 17))

# read in 2019 peak, centroid, temp, lat data
pheno_plot <- read.csv("data/InTheMiddle/pheno_2019_cc_inat_plot.csv", stringsAsFactors = F)

peak_dev <- temp_dev %>%
  dplyr::select(cell, year, mean_temp, tempDev, devPeakDate, avgDevPeak) %>%
  pivot_longer(devPeakDate:avgDevPeak, names_to = "metric", values_to = "deviance") %>%
  mutate(dataset = case_when(metric == "devPeakDate" ~ "iNaturalist",
                             TRUE ~ "Caterpillars Count!"))

# iNat peak model
summary(lm(deviance ~ tempDev, data = filter(peak_dev, dataset == "iNaturalist")))

# CC peak model
summary(lm(deviance ~ tempDev, data = filter(peak_dev, dataset == "Caterpillars Count!")))

CC_dev_plot <- ggplot(peak_dev, aes(x = tempDev, y = deviance, col = dataset)) +
  geom_hline(yintercept = 0, lty = 2, col = "darkgray", linewidth = 1) + 
  geom_vline(xintercept = 0, lty = 2, col = "darkgray", linewidth = 1) +
  geom_point(cex = 3) + 
  geom_smooth(method = "lm", se = F, cex = 1) +
  scale_color_manual(values = c("blue3", "palegreen3")) +
  labs(x = "Spring temperature deviation (°C)", y = "Peak date deviation (days)", col = "", title = "C. Peak date deviation") +
  theme(legend.position = c(0.8, 0.9), plot.title = element_text(hjust = -0.25))


centroid_dev <- temp_dev %>%
  dplyr::select(cell, year, mean_temp, tempDev, devCentroidDate, avgDevCentroid) %>%
  pivot_longer(devCentroidDate:avgDevCentroid, names_to = "metric", values_to = "deviance") %>%
  mutate(dataset = case_when(metric == "devCentroidDate" ~ "iNaturalist",
                             TRUE ~ "Caterpillars Count!"))

# iNat centroid model
summary(lm(deviance ~ tempDev, data = filter(centroid_dev, dataset == "iNaturalist")))

# CC centroid model
summary(lm(deviance ~ tempDev, data = filter(centroid_dev, dataset == "Caterpillars Count!")))


CC_centroid_plot <- ggplot(centroid_dev, aes(x = tempDev, y = deviance, col = dataset)) +
  geom_hline(yintercept = 0, lty = 2, col = "darkgray", cex = 1) + 
  geom_vline(xintercept = 0, lty = 2, col = "darkgray", cex = 1) +
  geom_point(cex = 3) + 
  geom_smooth(method = "lm", se = F, cex = 1) +
  scale_color_manual(values = c("blue3", "palegreen3")) +
  labs(x = "Spring temperature deviation (°C)", y = "Centroid date deviation (days)", col = "", title = "D. Centroid date deviation") +
  theme(legend.position = c(0.8, 0.9), plot.title = element_text(hjust = -0.25))

# Plot comparing cc and inaturalist deviations in peak date 
cc_inat_cent <- ggplot(temp_dev, aes(x = devCentroidDate, y = avgDevCentroid)) + 
  geom_hline(yintercept = 0, lty = 2, col = "darkgray", cex = 1) + 
  geom_vline(xintercept = 0, lty = 2, col = "darkgray", cex = 1) +
  geom_point(cex = 3) + 
  labs(x = "iNaturalist", y = "Caterpillars Count!", title = "B. Centroid date deviation") +
  theme(plot.title = element_text(hjust = -0.25))

cor.test(temp_dev$devCentroidDate, temp_dev$avgDevCentroid) # r = -0.354, p = 0.235

# Plot comparing cc and inatuarlist deviations in centroid date
cc_inat_peak <- ggplot(temp_dev, aes(x = devPeakDate, y = avgDevPeak)) + 
  geom_hline(yintercept = 0, lty = 2, col = "darkgray", cex = 1) + 
  geom_vline(xintercept = 0, lty = 2, col = "darkgray", cex = 1) +
  geom_point(cex = 3) + 
  labs(x = "iNaturalist", y = "Caterpillars Count!", title = "A. Peak date deviation") +
  theme(plot.title = element_text(hjust = -0.25))

cor.test(temp_dev$devPeakDate, temp_dev$avgDevPeak) # r = -0.217, p = 0.477

plot_grid(cc_inat_peak, cc_inat_cent,
          CC_dev_plot, CC_centroid_plot, ncol = 2)
ggsave("InTheMiddle/figs/phenometric_deviations.pdf", units = 'in', height = 10, width = 12)


#2019 pheno metrics
cols <- c("Caterpillars Count!" = "skyblue2", "iNaturalist" = "springgreen3")
line_types <- c("Peak date" = 1, "Centroid date" = 3)
shapes <- c("Peak date" = 15, "Centroid date" = 17)

lat_plot <- ggplot(pheno_plot, aes(x = lat)) + 
  geom_point(aes(y = peakDate, col = "iNaturalist", shape = "Peak date"), cex = 2) +
  geom_smooth(aes(y = peakDate, col = "iNaturalist", lty = "Peak date"), method = "lm", se = F) +
  geom_point(aes(y = avgPeakDate, col = "Caterpillars Count!", shape = "Peak date"), cex = 2) +
  geom_smooth(aes(y = avgPeakDate, col = "Caterpillars Count!", lty = "Peak date"), method = "lm", se = F) +
  geom_point(aes(y = centroidDate, col = "iNaturalist", shape = "Centroid date"), cex = 2) +
  geom_smooth(aes(y = centroidDate, col = "iNaturalist", lty = "Centroid date"), method = "lm", se = F) +
  geom_point(aes(y = avgCentroidDate, col = "Caterpillars Count!", shape = "Centroid date"), cex = 2) +
  geom_smooth(aes(y = avgCentroidDate, col = "Caterpillars Count!", lty = "Centroid date"), method = "lm", se = F) +
  scale_color_manual(name = "Dataset", values =  cols) +
  scale_shape_manual(name = "Pheno metric", values = shapes) +
  scale_linetype_manual(name = "Pheno metric", values = line_types) +
  labs(x = "Latitude", y = "Julian day")

# Scatterplot showing peak dates (col = dataset, symbol = metric) vs mean temperature

temp_plot <- ggplot(pheno_plot, aes(x = mean_temp)) + 
  geom_point(aes(y = peakDate, col = "iNaturalist", shape = "Peak date"), cex = 2) +
  geom_smooth(aes(y = peakDate, col = "iNaturalist", lty = "Peak date"), method = "lm", se = F) +
  geom_point(aes(y = avgPeakDate, col = "Caterpillars Count!", shape = "Peak date"), cex = 2) +
  geom_smooth(aes(y = avgPeakDate, col = "Caterpillars Count!", lty = "Peak date"), method = "lm", se = F) +
  geom_point(aes(y = centroidDate, col = "iNaturalist", shape = "Centroid date"), cex = 2) +
  geom_smooth(aes(y = centroidDate, col = "iNaturalist", lty = "Centroid date"), method = "lm", se = F) +
  geom_point(aes(y = avgCentroidDate, col = "Caterpillars Count!", shape = "Centroid date"), cex = 2) +
  geom_smooth(aes(y = avgCentroidDate, col = "Caterpillars Count!", lty = "Centroid date"), method = "lm", se = F) +
  scale_color_manual(name = "Dataset", values =  cols) +
  scale_shape_manual(name = "Pheno metric", values = shapes) +
  scale_linetype_manual(name = "Pheno metric", values = line_types) +
  theme(legend.position = "none") +
  labs(x = "Spring temperature (C)", y = "Julian day")

# Linear models of inat/cc centroid dates with spring temperature
# iNat centroid date
summary(lm(centroidDate ~ mean_temp, data = pheno_plot))

# CC centroid date
summary(lm(avgCentroidDate ~ mean_temp, data = pheno_plot))

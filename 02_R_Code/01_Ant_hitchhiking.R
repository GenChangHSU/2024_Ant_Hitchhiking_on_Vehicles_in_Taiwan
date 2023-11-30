## -----------------------------------------------------------------------------
## Title: Analysis of ant hitchhiking on vehicles in Taiwan
##
## Author: Gen-Chang Hsu
##
## Date: 2023-09-20
##
## Description:
## 1. Summarize cases of ant hitchhiking on vehicles in Taiwan.
## 2. Examine the temporal patterns of ant hitchhiking cases in Taiwan.
## 3. Create a map of ant hitchhiking cases in Taiwan.
## 4. Create a map of the intended destinations for the hitchhiked vehicles.
##
## -----------------------------------------------------------------------------
set.seed(123)


# Libraries --------------------------------------------------------------------
library(readxl)
library(lubridate)
library(magrittr)
library(maps)
library(ggthemes)
library(cowplot)
library(sf)
library(elevatr)
library(raster)
library(ggnewscale)
library(ggsn)
library(tidyverse)


# Import files -----------------------------------------------------------------
ant_hitchhike_new <- read_xlsx("./01_Data_Raw/Ant_Hitchhiking_for_Analysis.xlsx", sheet = 1, na = "NA") %>% 
  mutate(Parking_date = ymd(Parking_date),
         Destination_lon = as.numeric(Destination_lon),
         Destination_lat = as.numeric(Destination_lat))
ant_hitchhike_old <- read_xlsx("./01_Data_Raw/Ant_Hitchhiking_for_Analysis.xlsx", sheet = 3, na = "NA") %>% 
  mutate(Parking_date = ymd(Parking_date),
         Destination_lon = as.numeric(Destination_lon),
         Destination_lat = as.numeric(Destination_lat))


# ggplot theme -----------------------------------------------------------------
my_theme <- 
  theme(# Axis
    axis.text.x = element_text(size = 12, color = "black", margin = margin(t = 3)),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title.x = element_text(size = 15, margin = margin(t = 10)),
    axis.title.y = element_text(size = 15, margin = margin(r = 8)),
    axis.ticks.length.x = unit(0.2, "cm"),
    
    # Plot
    plot.title = element_text(hjust = 0.5, size = 18),
    plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"),
    plot.background = element_rect(colour = "transparent"),
    
    # Panel
    panel.background = element_rect(fill = "transparent"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    
    # Legend
    legend.position = c(1, 1),
    legend.spacing.x = unit(0.2, "cm"),
    legend.key.width = unit(1.5, "cm"),
    legend.key.size = unit(1.2, "line"),
    legend.key = element_blank(),
    legend.text = element_text(size = 10, margin = margin(0, 10, 0, -5)),
    legend.text.align = 0,
    legend.box.just = "center",
    legend.justification = c(0.5, 0.5),
    legend.title.align = 0.5,
    legend.background = element_rect(fill = "transparent"),
    
    # Facet strip
    strip.background = element_rect(fill = "transparent"),
    strip.text = element_text(size = 12, hjust = 0.5)
  )


############################### Code starts here ###############################

# 1. Data summary --------------------------------------------------------------
### Merge the two datasets
ant_hitchhike_all <- ant_hitchhike_new %>%
  mutate(Parking_duration_hr = as.numeric(Parking_duration_hr)) %>% 
  mutate(Parking_duration = case_when(Parking_duration_hr < 12 ~ "Half day",
                                      Parking_duration_hr > 12 & Parking_duration_hr < 24 ~ "A day",
                                      Parking_duration_hr > 24 & Parking_duration_hr < 168 ~ "A week",
                                      Parking_duration_hr > 168 ~ "A month")) %>% 
  relocate(Parking_duration, .after = Parking_time) %>% 
  select(-Parking_duration_hr) %>% 
  bind_rows(ant_hitchhike_old) %>% 
  arrange(Parking_date) %>% 
  mutate(ID = 1:n())

### Write out the cleaned data as the supplementary data
ant_hitchhike_all_supplementary <- ant_hitchhike_all %>% 
  select(-Species_Chinese, -Location, -Destination, -Surrounding_description, -Post_url, -Note) %>% 
  write_csv("./04_Manuscript/Supplementary_Data.csv")
  
### Number of cases
ant_hitchhike_all %>% nrow()

### Number of species
ant_hitchhike_all$Species_English %>% 
  unique() %>% 
  length()

### Number of cases by species
ant_hitchhike_all %>% 
  group_by(Species_English, Species_status) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  arrange(desc(n)) %>% 
  mutate(prop = round(n/sum(n), 2)) %T>% 
  write_csv("./03_Outputs/Tables/Case_Summary.csv")

### Number of native vs. exotic species
ant_hitchhike_all %>% 
  distinct(Species_English, Species_status) %>% 
  group_by(Species_status) %>% 
  summarise(n = n())

### Number of native vs. exotic species cases
ant_hitchhike_all %>% 
  group_by(Species_status) %>% 
  summarise(n = n()) %>% 
  mutate(prop = round(n/sum(n), 3))

### Number of cases on cars vs. scooters
ant_hitchhike_all %>% 
  group_by(Vehicle_type) %>% 
  summarise(n = n())

### Number of cases on vehicles of different colors
ant_hitchhike_all %>% 
  group_by(Vehicle_color) %>% 
  summarise(n = n())

### Number of cases under different weather conditions
ant_hitchhike_all %>% 
  group_by(Weather) %>% 
  summarise(n = n())

### Number of cases with vs. without trees nearby the vehicles
ant_hitchhike_all %>% 
  group_by(Tree_nearby) %>% 
  summarise(n = n())


# 2. Temporal patterns of ant hitchhiking cases --------------------------------
### Parking duration
Parking_duration <- ant_hitchhike_all %>% 
  filter(!Parking_duration == "NA") %>% 
  group_by(Parking_duration) %>% 
  summarise(n = n()) %>% 
  mutate(prop = round(n/sum(n), 3)) %>% 
  mutate(Parking_duration = fct_relevel(Parking_duration, "Half day", "A day", "A week", "A month")) %>% 
  arrange(Parking_duration)

Parking_duration

### Cases by month  
cases_by_month <- ant_hitchhike_all %>% 
  transmute(month = month(Parking_date)) %>% 
  count(month)

cases_by_month

### Cases by season
cases_by_season <- ant_hitchhike_all %>%
  transmute(month = month(Parking_date)) %>% 
  mutate(month = as.factor(month)) %>% 
  .$month %>% 
  fct_collapse(., 
               spring = c("3", "4", "5"),
               summer = c("6", "7", "8"),
               fall = c("9", "10", "11"),
               winter = c("12", "1", "2")) %>% 
  fct_count() %>% 
  rename(season = f, case = n) %>% 
  mutate(season = fct_relevel(season, "winter", after = 3)) %>% 
  arrange(season)
  
cases_by_season

# Chi-square test of cases in each season
chi_test_season <- chisq.test(cases_by_season$case)
chi_test_season

# barplot of cases by season
ggplot(cases_by_season) + 
  geom_bar(aes(x = season, y = case, fill = season), stat = "identity", 
           color = "black", width = 0.7, show.legend = F) +
  labs(x = NULL, y = "Number of cases") + 
  scale_x_discrete(labels = c("Spring", 
                              "Summer", 
                              "Fall",
                              "Winter")) + 
  scale_y_continuous(limits = c(0, 30), expand = c(0, 0)) + 
  scale_fill_manual(values = c("#3CB371", "#FF4500", "#f1a340", "#1E90FF")) +
  my_theme + 
  theme(axis.ticks.length.x = unit(0, "in"),
        axis.text.x = element_text(margin = margin(t = 6))) + 
  annotate(geom = "text", x = 3.5, y = 25, size = 5,
           label = substitute(list(italic(chi)^2 == chisqr, ~italic(p) < 0.001), 
                              list(chisqr = round(chi_test_season$statistic, 2), 
                                   pval = round(chi_test_season$p.value, 3))))

ggsave("./03_Outputs/Figures/Season_Barplot.tiff", width = 5, height = 4, dpi = 600, device = "tiff")  


# 3. Case map ------------------------------------------------------------------
### An inset map of East Asia
inset_map_asia <- ggplot(map_data("world"), aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "grey90", color = "black") + 
  geom_rect(xmin = 119, xmax = 123, ymin = 21, ymax = 26, 
            color = "red", fill = NA, linewidth = 1) +
  geom_text(x = 125, y = 23.5, label = "Taiwan", hjust = 0, size = 5, fontface = "bold") + 
  coord_fixed(ratio = 1.05, xlim = c(100, 140), ylim = c(0, 50)) +
  theme_map() +
  theme(panel.border = element_rect(colour = "black", fill = NA),
        panel.background = element_rect(fill = "white"))

inset_map_asia

### A map of ant hitchhiking cases in Taiwan
# (1) exotic species
exotic_sp <- filter(ant_hitchhike_all, Species_status == "Exotic") %>% 
  rename(Exotic = Species_English)

exotic_rank <- exotic_sp %>% 
  count(Exotic) %>% 
  arrange(desc(n)) %>% 
  pull(Exotic)

exotic_sp <- exotic_sp %>% 
  mutate(Exotic = factor(Exotic, levels = exotic_rank, ordered = T))

# (2) native species
native_sp <- filter(ant_hitchhike_all, Species_status == "Native") %>% 
  rename(Native = Species_English)

native_rank <- native_sp %>% 
  count(Native) %>% 
  arrange(desc(n)) %>% 
  pull(Native)

native_sp <- native_sp %>% 
  mutate(Native = factor(Native, levels = native_rank, ordered = T))

# administrative boundary data of Taiwan from the "geoBoundaries" database 
taiwan_boundary <- st_read("./01_Data_Raw/TW_geoBoundaries/geoBoundaries-TWN-ADM0.shp")

# elevation data of Taiwan from the "Amazon Web Services" database  
taiwan_elevation <- get_elev_raster(locations = taiwan_boundary, z = 9, clip = "locations") %>% 
  as.data.frame(xy = T, na.rm = T) %>%
  `colnames<-`(c("lon", "lat", "elev"))

# the map
map_taiwan <- ggplot() +
  geom_raster(data = taiwan_elevation, aes(x = lon, y = lat, fill = elev), alpha = 0.7, show.legend = F) +
  geom_sf(data = taiwan_boundary, color = "grey10", fill = "transparent", size = 0.2) +
  scale_fill_gradient2(low = "grey", high = "grey70") +
  new_scale_fill() +
  geom_point(data = exotic_sp, aes(x = Location_lon, y = Location_lat, color = Exotic), size = 2) + 
  geom_point(data = native_sp, aes(x = Location_lon, y = Location_lat, fill = Native), size = 2, shape = 21, color = "transparent") + 
  coord_sf(xlim = c(118.3, 125.5), ylim = c(21, 26.6)) +
  scale_color_manual(values = c("#FF0000", "#34210C", "#DFAC77", "#764A1B", "#FF9933", "#FFCC99", "#FF6699"), 
                     labels = c("Dolichoderus thoracicus \n n = 31",
                                "Tapinoma melanocephalum \n n = 5",
                                "Paratrechina longicornis \n n = 5",
                                "Technomyrmex albipes \n n = 4",
                                "Technomyrmex brunneus \n n = 2",
                                "Anoplolepis gracilipes \n n = 1",
                                "Trichomyrmex destructor \n n = 1")) + 
  scale_fill_manual(values = c("#0033FF", "#0099FF"),
                    labels = c("          Polyrhachis dives       \n n = 2",
                               "Nylanderia     \n n = 1")) + 
  guides(color = guide_legend(byrow = F, title.hjust = 0.63, order = 2, 
                              override.aes = list(size = 3)), 
         fill = guide_legend(byrow = F, title.hjust = 0.63, order = 1,
                             override.aes = list(size = 3))) +
  labs(x = "", y = "") + 
  theme_classic() + 
  theme(axis.text = element_text(colour = "black", size = 13),
        axis.line = element_blank(),
        panel.background = element_rect(color = "black"),
        legend.position = c(0.78, 0.5),
        legend.margin = margin(r = 12, b = 10),
        legend.title = element_text(face = "bold", margin = margin(t = 5, b = 0), size = 15),
        legend.title.align = 0.5,
        legend.text = element_text(face = "italic", hjust = 0.5, vjust = -3.5,
                                   margin = margin(b = 12), size = 13),
        legend.spacing.y = unit(0, "in"),
        legend.background = element_rect(color = "white")) +
  scalebar(x.min = 119.45, x.max = 119.45, y.min = 21.1, y.max = 21.4,
           dist = 50, dist_unit = "km", transform = T, model = "WGS84", height = 0.2, st.dist = 0.4, st.size = 3.2) +
  north(x.min = 119.23, x.max = 119.4, y.min = 21.65, y.max = 21.75, symbol = 10, scale = 5) + 
  annotate(geom = "text", x = 118.98, y = 21.8, label = "N", size = 6)
  
map_taiwan

### Merge the inset map and the main map
ggdraw(map_taiwan) +
  draw_plot(inset_map_asia, x = 0.11, y = 0.288, width = 0.23) +
  draw_label(label = "sp.", x = 0.854, y = 0.78, size = 13)

ggsave("./03_Outputs/Figures/Case_Map.tiff", width = 8.5, height = 7, dpi = 600, device = "tiff")


# 4. Destination map -----------------------------------------------------------

















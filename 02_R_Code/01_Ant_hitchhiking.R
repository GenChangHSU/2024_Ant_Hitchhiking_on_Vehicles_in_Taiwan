## -----------------------------------------------------------------------------
## Title: Analysis of ant hitchhiking on vehicles in Taiwan
##
## Author: Gen-Chang Hsu
##
## Date: 2023-06-12
##
## Description:
## 1. Summarize cases of ant hitchhiking on vehicles in Taiwan.
## 2. Examine the temporal patterns of ant hitchhiking cases in Taiwan.
## 3. Create a map of ant hitchhiking cases in Taiwan.
##
## -----------------------------------------------------------------------------
set.seed(123)


# Libraries --------------------------------------------------------------------
library(tidyverse)
library(readxl)
library(lubridate)
library(magrittr)
library(maps)
library(ggthemes)
library(ggmap)
library(ggsn)
library(ggsci)
library(cowplot)
library(cropcircles)

# Import files -----------------------------------------------------------------
ant_hitchhike_new <- read_xlsx("./01_Data_raw/ant_hitchhiking_full.xlsx", sheet = 1) %>% 
  mutate(Parking_date = ymd(Parking_date))
ant_hitchhike_old <- read_xlsx("./01_Data_raw/ant_hitchhiking_full.xlsx", sheet = 3) %>% 
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
ant_hitchhike_all <- ant_hitchhike_new %>% mutate(Parking_duration = case_when(Parking_duration_hr < 12 ~ "Half day",
                                                          Parking_duration_hr > 12 & Parking_duration_hr < 24 ~ "A day",
                                                          Parking_duration_hr > 24 & Parking_duration_hr < 168 ~ "A week",
                                                          Parking_duration_hr > 168 ~ "A month")) %>% 
  relocate(Parking_duration, .after = Parking_time) %>% 
  select(-Parking_duration_hr) %>% 
  bind_rows(ant_hitchhike_old)

### Number of cases
ant_hitchhike_all %>% nrow()

### Number of species
ant_hitchhike_all$Species_English %>% 
  unique() %>% 
  length()

### Number of cases by species
ant_hitchhike_all %>% 
  group_by(Species_English, Species_status) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  mutate(prop = round(n/sum(n), 3)) %T>% 
  write_csv("./03_Outputs/Tables/Case_summary.csv")


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

# barplot of cases by season
ggplot(cases_by_season) + 
  geom_bar(aes(x = season, y = case, fill = season), stat = "identity", 
           color = "black", width = 0.7, show.legend = F) +
  labs(x = NULL, y = "Number of cases") + 
  scale_x_discrete(labels = c("Spring", 
                              "Summer", 
                              "Fall",
                              "Winter")) + 
  scale_y_continuous(limits = c(0, 25), expand = c(0, 0)) + 
  scale_fill_manual(values = c("#3CB371", "#FF4500", "#f1a340", "#1E90FF")) +
  my_theme + 
  theme(axis.ticks.length.x = unit(0, "in"),
        axis.text.x = element_text(margin = margin(t = 6))) + 
  annotate(geom = "text", x = 2.5, y = 23.5, size = 5,
           label = substitute(list(italic(chi)^2 == chisqr, ~italic(p) == pval), 
                              list(chisqr = round(chi_test_season$statistic, 2), 
                                   pval = round(chi_test_season$p.value, 3))))

ggsave("./03_Outputs/Figures/Season_barplot.tiff", width = 5, height = 4, dpi = 600, device = "tiff")  







# 3. Map -----------------------------------------------------------------------
### An inset map of East Asia
inset_map_asia <- ggplot(map_data("world"), aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "grey90", color = "black") + 
  geom_rect(xmin = 119, xmax = 123, ymin = 21, ymax = 26, 
            color = "red", fill = NA, linewidth = 1) +
  geom_text(x = 125, y = 23.5, label = "Taiwan", hjust = 0, size = 4, fontface = "bold") + 
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
  mutate(Exotic = factor(Exotic, level = exotic_rank, ordered = T))

# (2) native species
native_sp <- filter(ant_hitchhike_all, Species_status == "Native") %>% 
  rename(Native = Species_English)

native_rank <- native_sp %>% 
  count(Native) %>% 
  arrange(desc(n)) %>% 
  pull(Native)

native_sp <- native_sp %>% 
  mutate(Native = factor(Native, level = native_rank, ordered = T))

# bounding box of the Taiwan map
taiwan_bbox <- c(left = 118.5, right = 125.1, bottom = 21, top = 26)

# the map
map_taiwan <- get_stamenmap(taiwan_bbox, zoom = 8, maptype = "terrain") %>% 
  ggmap() + 
  geom_point(data = exotic_sp, aes(x = Location_lon, y = Location_lat, color = Exotic), size = 2) + 
  geom_point(data = native_sp, aes(x = Location_lon, y = Location_lat, fill = Native), size = 2, shape = 21, color = "transparent") + 
  labs(x = NULL, y = NULL) + 
  scale_color_manual(values = pal_nejm()(8)[1:6], 
                     labels = c("Dolichoderus thoracicus \n n = 26",
                                "Tapinoma melanocephalum \n n = 5",
                                "Paratrechina longicornis \n n = 4",
                                "Technomyrmex albipes \n n = 4",
                                "Technomyrmex brunneus \n n = 2",
                                "Anoplolepis gracilipes \n n = 1")) + 
  scale_fill_manual(values = pal_nejm()(8)[7:8],
                    labels = c("          Polyrhachis dives       \n n = 2",
                               "Nylanderia     \n n = 1")) + 
  scale_x_continuous(limits = c(118.5, 124.7), breaks = 119:124, labels = paste0(119:124, "° E"), expand = c(0, 0)) + 
  scale_y_continuous(breaks = 21:26, labels = paste0(21:26, "° N"), expand = c(0, 0)) + 
  theme_classic() + 
  theme(axis.text = element_text(colour = "black", size = 11),
        axis.line = element_blank(),
        legend.position = c(0.84, 0.54),
        legend.margin = margin(r = 30, b = 10),
        legend.title = element_text(face = "bold", margin = margin(t = 5, b = 5)),
        legend.title.align = 0.5,
        legend.text = element_text(face = "italic", hjust = 0.5, vjust = -5,
                                   margin = margin(b = 17)),
        legend.spacing.y = unit(0, "in"),
        legend.background = element_rect(fill = "#adc7e0"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = "transparent")) + 
  guides(color = guide_legend(byrow = F, title.hjust = 0.63, order = 2, 
                              override.aes = list(size = 3)), 
         fill = guide_legend(byrow = F, title.hjust = 0.63, order = 1,
                             override.aes = list(size = 3))) +
  scalebar(x.min = 122.8, x.max = 123.8, y.min = 21.3, y.max = 21.6,
           dist = 50, dist_unit = "km", transform = T, model = "WGS84", height = 0.2, st.dist = 0.2, st.size = 2.8) + 
  coord_equal() +
  north(x.min = 124.5, x.max = 124.6, y.min = 21.4, y.max = 21.5, symbol = 10, scale = 5) + 
  annotate(geom = "text", x = 124.35, y = 21.6, label = "N", size = 6)
  
map_taiwan

### Merge the inset map and the main map
ggdraw(map_taiwan) +
  draw_plot(inset_map_asia, x = 0.05, y = 0.305, width = 0.23) +
  draw_label(label = "sp.", x = 0.875, y = 0.7485, size = 9)

ggsave("./03_Outputs/Figures/Cases_map.tiff", width = 8.5, height = 7, dpi = 600, device = "tiff")




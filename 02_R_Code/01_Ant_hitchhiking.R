## -----------------------------------------------------------------------------
## Title: Analysis of ant hitchhiking on vehicles in Taiwan
##
## Author: Gen-Chang Hsu
##
## Date: 2023-01-24
##
## Description:
## 1. Summary of ant hitchhiking on vehicles in Taiwan.
## 2. Analyze the temporal patterns of ant hitchhiking on vehicles in Taiwan.
## 3. A map of reported ant hitchhiking cases in Taiwan.
##
## Notes:
##
## -----------------------------------------------------------------------------
set.seed(123)


# Libraries --------------------------------------------------------------------
library(tidyverse)
library(readxl)
library(lubridate)
library(maps)
library(ggthemes)
library(ggmap)
library(ggsn)
library(ggsci)
library(cowplot)
library(cropcircles)

# Import files -----------------------------------------------------------------
ant_hitchhike <- read_xlsx("./01_Data_raw/ant_hitchhiking_2022.xlsx", sheet = 1,
                           col_types = c("guess", "date", "guess", "guess", "skip",
                                         "guess", "guess", "guess", "skip", "guess",
                                         "guess"))


############################### Code starts here ###############################

# 1. Data summary --------------------------------------------------------------

### Number of cases
ant_hitchhike %>% nrow()

### Number of species
ant_hitchhike$Species %>% 
  unique() %>% 
  length()

### Number of cases by species
ant_hitchhike %>% 
  group_by(Species) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  mutate(prop = round(n/sum(n), 3))

### Number of native vs. invasive species
ant_hitchhike %>% 
  distinct(Species, Status) %>% 
  group_by(Status) %>% 
  summarise(n = n())

### Number of native vs. invasive species cases
ant_hitchhike %>% 
  group_by(Status) %>% 
  summarise(n = n()) %>% 
  mutate(prop = round(n/sum(n), 3))


# 2. Temporal patterns of ant hitchhiking --------------------------------------
### Day/night pattern
ant_hitchhike %>% 
  filter(Parking_duration %in% c("half_to_a_day", "half_day")) %>% 
  group_by(Day_night) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n/sum(n))

### Monthly pattern  
ant_hitchhike %>% 
  transmute(month = month(Date)) %>% 
  count(month)

### Seasonal pattern
case_by_season <- ant_hitchhike %>%
  transmute(month = month(Date)) %>% 
  mutate(month = as.factor(month)) %>% 
  .$month %>% 
  fct_collapse(., 
               spring = c("3", "4", "5"),
               summer = c("6", "7", "8", "9"),
               fall_winter = c("10", "11", "12")) %>% 
  fct_count() %>% 
  rename(season = f, case = n)

chi_test_season <- chisq.test(case_by_season$case, p = c(0.3, 0.4, 0.3))

ggplot(case_by_season) + 
  geom_bar(aes(x = season, y = case, fill = season), stat = "identity", 
           color = "black", width = 0.7, show.legend = F) +
  labs(x = NULL, y = "Number of cases") + 
  scale_x_discrete(labels = c("Spring \n (Mar. - May)", 
                              "Summer \n (June - Sept.)", 
                              "Fall/Winter \n (Oct. - Dec.)")) + 
  scale_y_continuous(limits = c(0, 32), expand = c(0, 0)) + 
  scale_fill_manual(values = c("#3CB371", "#FF4500", "#1E90FF")) +
  theme_classic() + 
  theme(axis.text = element_text(size = 13, color = "black"),
        axis.title = element_text(size = 15),
        axis.ticks.x = element_blank()) +
  annotate(geom = "text", x = 2, y = 29, size = 5,
           label = substitute(list(italic(chi)^2 == chisqr, ~italic(p) == pval), 
                              list(chisqr = round(chi_test_season$statistic, 2), 
                                   pval = round(chi_test_season$p.value, 3))))

ggsave("./03_Outputs/Figures/Season_barplot.tiff", width = 5, height = 4, dpi = 600, device = "tiff")  

### Colonization time
col_time <- ant_hitchhike %>% 
  filter(Parking_duration != "NA") %>% 
  group_by(Parking_duration) %>% 
  summarise(n = n()) %>% 
  mutate(Parking_duration = fct_relevel(Parking_duration, "a_day_to_a_week", after = 2))

col_time_labs <- col_time %>% 
  arrange(desc(Parking_duration)) %>% 
  mutate(pos = cumsum(n) - n/2,
         labs = c("> A month", "A day - A week", "Half day - A day", "< Half day"))


col_time %>% 
  ggplot() + 
  geom_bar(aes(x = "x", y = n, fill = Parking_duration), 
           stat = "identity", position = "stack", color = "black") + 
  geom_text(data = col_time_labs, aes(x = "x", y = pos, label = paste("italic(n) == ", n)), parse = T,
            nudge_x = c(0.18, 0.1, 0.11, 0.1), 
            nudge_y = c(0, 0.75, -0.6, -0.6)) +
  geom_text(data = col_time_labs, aes(x = "x", y = pos, label = labs), 
            nudge_x = c(0.28, 0.1, 0, 0.15), 
            nudge_y = c(0, -0.5, -0.5, 0.5), 
            fontface = "bold.italic", size = 4) + 
  scale_y_continuous(breaks = col_time_labs$pos, labels = NULL) + 
  scale_fill_manual(values = c("#fef0d9", "#fdcc8a", "#fc8d59", "#d7301f"), guide = F) +
  coord_polar(theta = "y", direction = -1, clip = "off") + 
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.text.x = element_text(size = 12, margin = margin(l = 15))) 

ggsave("./03_Outputs/Figures/Col_time_pie.tiff", width = 4, height = 4, dpi = 600, device = "tiff")


# 3. Maps ----------------------------------------------------------------------

### A map of East Asia
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
# Split the data set into invasive and native species
dat_invasive <- filter(ant_hitchhike, Status == "invasive") %>% 
  rename(Invasive = Species)

invasive_rank <- dat_invasive %>% 
  count(Invasive) %>% 
  arrange(desc(n)) %>% 
  pull(Invasive)

dat_invasive <- dat_invasive %>% 
  mutate(Invasive = factor(Invasive, level = invasive_rank, ordered = T))

dat_native <- filter(ant_hitchhike, Status == "native") %>% 
  rename(Native = Species)

native_rank <- dat_native %>% 
  count(Native) %>% 
  arrange(desc(n)) %>% 
  pull(Native)

dat_native <- dat_native %>% 
  mutate(Native = factor(Native, level = native_rank, ordered = T))

# Bounding box of the Taiwan map
taiwan_bbox <- c(left = 118.5, right = 125.1, bottom = 21, top = 26)

# The map
map_taiwan <- get_stamenmap(taiwan_bbox, zoom = 8, maptype = "terrain") %>% 
  ggmap() + 
  geom_point(data = dat_invasive, aes(x = Lon, y = Lat, color = Invasive), size = 2) + 
  geom_point(data = dat_native, aes(x = Lon, y = Lat, fill = Native), size = 2, shape = 21, color = "transparent") + 
  labs(x = NULL, y = NULL) + 
  scale_color_manual(values = pal_nejm()(8)[1:6], 
                     labels = c("Dolichoderus thoracicus \n n = 22",
                                "Tapinoma melanocephalum \n n = 5",
                                "Paratrechina longicornis \n n = 4",
                                "Technomyrmex albipes \n n = 4",
                                "Technomyrmex brunneus \n n = 2",
                                "Anoplolepis gracilipes \n n = 1")) + 
  scale_fill_manual(values = pal_nejm()(8)[7:8],
                    labels = c("     Polyrhachis dives            \n n = 2",
                               "Nylanderia sp. \n n = 1")) + 
  scale_x_continuous(breaks = 118:125, labels = paste0(118:125, "° E"), expand = c(0, 0)) + 
  scale_y_continuous(breaks = 21:26, labels = paste0(21:26, "° N"), expand = c(0, 0)) + 
  theme_classic() + 
  theme(axis.text = element_text(colour = "black", size = 8),
        axis.line = element_blank(),
        legend.position = c(0.8, 0.54),
        legend.margin = margin(r = 50, b = 10),
        legend.title = element_text(face = "bold", margin = margin(t = 5, b = 5)),
        legend.title.align = 0.5,
        legend.text = element_text(face = "italic", hjust = 0.5, vjust = -5,
                                   margin = margin(b = 17)),
        legend.spacing.y = unit(0, "in"),
        legend.background = element_rect(fill = "#adc7e0")) + 
  guides(color = guide_legend(byrow = F, title.hjust = 0.63, order = 1, 
                              override.aes = list(size = 3)), 
         fill = guide_legend(byrow = F, title.hjust = 0.63, order = 2,
                             override.aes = list(size = 3))) +
  scalebar(x.min = 123, x.max = 124, y.min = 21.3, y.max = 21.6,
           dist = 50, dist_unit = "km", transform = T, model = "WGS84", height = 0.2, st.dist = 0.2, st.size = 2.8) + 
  coord_equal() +
  north(x.min = 124.7, x.max = 124.8, y.min = 21.4, y.max = 21.5, symbol = 10, scale = 5) + 
  annotate(geom = "text", x = 124.55, y = 21.6, label = "N", size = 6)
  
map_taiwan

### Merge the inset map and the main map and add ant images
ant_images <- c("https://www.natureloveyou.sg/Minibeast-Bee/Dolichoderus%20thoracicus/DSC00118%20(14).jpg",
                "https://entnemdept.ufl.edu/creatures/urban/ants/ghost_ant03.jpg",
                "https://bugguide.net/images/cache/7QF/RQQ/7QFRQQYRJKQ0X0JQ70TQX0DQ403QZQYRXQBRLQTQ40ARJK1R3KNRLQURYKVRG0NRYKBR7QK0XQJR20JRZQ1R40OQX0AR.jpg",
                "https://canberrapestcontrol.com.au/wp-content/uploads/2016/08/White-Footed-House-Ant.jpg",
                "https://taieol.tw/files/muse_taieol/muse_styles/waterfall_col_3/mcode/a2a0fdff79e9e01012ac65f3affa5b8a.jpg?itok=QQq2fhxX",
                "https://www.natureloveyou.sg/Minibeast-Bee/Anoplolepis%20gracilipes/DSC05141%20(13).jpg",
                "https://www.antwiki.org/wiki/images/3/36/Polyrhachis_dives_worker%2C_Okinawa%2C_Japan%2C_Taku_Shimada.jpg",
                "https://openpsychometrics.org/tests/characters/test-resources/pics/BB/1.jpg")

ant_image_df <- data.frame(x = 0.915, 
                           y = c(0.807, 0.737, 0.667, 0.597, 0.527, 0.457, 0.329, 0.259), 
                           images = ant_images) %>%
  mutate(images_cropped = circle_crop(images))

ggdraw(map_taiwan) + 
  draw_plot(inset_map_asia, x = 0.039, y = 0.2875, width = 0.22) +
  draw_image(ant_image_df$images_cropped[1], x = ant_image_df$x[1], y = ant_image_df$y[1], 
             width = 0.05, hjust = 0.5, vjust = 0.5) + 
  draw_image(ant_image_df$images_cropped[2], x = ant_image_df$x[2], y = ant_image_df$y[2], 
             width = 0.05, hjust = 0.5, vjust = 0.5) + 
  draw_image(ant_image_df$images_cropped[3], x = ant_image_df$x[3], y = ant_image_df$y[3], 
             width = 0.05, hjust = 0.5, vjust = 0.5) + 
  draw_image(ant_image_df$images_cropped[4], x = ant_image_df$x[4], y = ant_image_df$y[4], 
             width = 0.05, hjust = 0.5, vjust = 0.5) + 
  draw_image(ant_image_df$images_cropped[5], x = ant_image_df$x[5], y = ant_image_df$y[5], 
             width = 0.05, hjust = 0.5, vjust = 0.5) + 
  draw_image(ant_image_df$images_cropped[6], x = ant_image_df$x[6], y = ant_image_df$y[6], 
             width = 0.05, hjust = 0.5, vjust = 0.5) + 
  draw_image(ant_image_df$images_cropped[7], x = ant_image_df$x[7], y = ant_image_df$y[7], 
             width = 0.05, hjust = 0.5, vjust = 0.5) + 
  draw_image(ant_image_df$images_cropped[8], x = ant_image_df$x[8], y = ant_image_df$y[8], 
             width = 0.05, hjust = 0.5, vjust = 0.5)

ggsave("./03_Outputs/Figures/Cases_map.tiff", width = 8.5, height = 7, dpi = 600, device = "tiff")





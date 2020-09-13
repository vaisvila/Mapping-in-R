##Tri-river Area
rm(list = ls())
setwd("C:/Users/avaisvi/Google Drive/OK SB Project 19'-23'/Figures")
setwd("~/Google Drive/OK SB Project 19'-23'/Figures")


##install.packages(c("ggsn", "sf", "maptools", "directlabels", "rnaturalearth", "USAboundaries"))
library(ggsn)
library(sf)
library(ggplot2)
library(maptools)
library(tidyverse)
library(directlabels)
library(rnaturalearth)
library(rnaturalearthdata)
library(USAboundaries)
library(ggrepel)
library(ggspatial)
library(tigris)



setwd("C:/Users/avaisvi/Google Drive/OK SB Project 19'-23'/Data/Spatial")
setwd("~/Google Drive/OK SB Project 19'-23'/Data/Spatial/lat_long")
rivers <- read_sf("rivers_OK", "rivers")
lakes <- read_sf("lakes_OK", "owrb_lakes")


ggplot() +
  geom_sf(mapping = aes(), fill= "light blue", rivers)+
  geom_sf(mapping = aes(), fill= "light blue", lakes) +
  theme_bw() 

world <- ne_countries(scale = 'medium', returnclass = 'sf')
usa <- subset(world, admin == "United States of America")
OK <- us_states(states = "Oklahoma")


rivers_state <- read_sf("rivers__state", "rivers") 
  
lakes_state <- read_sf("owrb_lakes", "owrb_lakes")

reservoirs <- c("Robert S Kerr Reservoir", "Webbers Falls Reservoir", "Tenkiller Ferry Lake", "Eufaula Lake")
rivers <- c("Arkansas River from mouth of Canadian River to the mouth of the Verdigris River including Webbers Falls Reservoir", 
            "Canadian River from mouth to Eufaula Reservoir Dam", 
            "Lower Illinois River from headwater of Robert S. Kerr Reservoir to Tenkiller Dam", 
            "Arkansas River from the Arkansas State Line to the mouth of the Canadian River including R.S. Kerr Reservoir")  

lakes_area <- lakes_state %>%
  filter(name %in% reservoirs)

rivers_ACI <- rivers_state %>%
  filter(name %in% rivers)

##mapRange <- c(range(st_coordinates(counties_spec)[,1]),range(st_coordinates(counties_spec)[,2]))

rivers_study_area <- rivers_state %>%
  filter(grepl("Arkansas River|Canadian River from mouth to Eufaula Reservoir Dam|Lower Illinois", name))

reservoirs_study_area <- lakes_state %>%
  filter(grepl('Robert S Kerr|Webbers Falls|Tenkiller|Eufaula|Keystone|Kaw', name))

ok_cities <- us_cities(states = "OK")

tulsa_okc <- ok_cities %>%
  filter(city == c("Tulsa", "Oklahoma City")) %>%
  mutate(lon=map_dbl(geometry, ~st_centroid(.x)[[1]]), # add centroid values for labels
       lat=map_dbl(geometry, ~st_centroid(.x)[[2]]))
  
  ##devtools::install_github("ropensci/USAboundariesData")


primary_roads <- primary_roads("2019", "sf")
primary_roads <- gSimplify(primary_roads, 
          tol = 3, 
          topologyPreserve = TRUE)


I40 <-  primary_roads[primary_roads$FULLNAME == 'I- 40',]
I40_OK <- raster::crop(x = I40, y = OK)
I40_ss <- raster::crop(x = I40, y = lakes_area)


rivers_ACI <- rivers_ACI %>%
  mutate(lon=map_dbl(geometry, ~st_centroid(.x)[[1]]), # add centroid values for labels
         lat=map_dbl(geometry, ~st_centroid(.x)[[2]]))

lakes_study_area <- lakes_area %>%
  mutate(lon=map_dbl(geometry, ~st_centroid(.x)[[1]]), # add centroid values for labels
         lat=map_dbl(geometry, ~st_centroid(.x)[[2]]))

river_labels <- rivers_study_area[c(46, 71, 82),]
reservoir_labels <- reservoirs_study_area[c(9, 10, 8, 11),]


ok_map <- ggplot() +
  geom_sf(mapping = aes(), rivers_study_area, color = "light blue", size = 1) +
  geom_sf(mapping = aes(), reservoirs_study_area, color = "light blue", size = 1) +
  geom_sf(mapping = aes(), OK, fill=NA, lwd=1.3, color = "black") +
  #geom_path(data = I40_OK, aes(x = long, y = lat, group = group)) +
  #geom_text(data=I40_OK, aes(x=, y=lat, label= "I-40", size = 4), nudge_y = 0.1) +
  geom_text(data=tulsa_okc, aes(x=lon, y=lat, label=city, size = 4), nudge_y = 0.1) +
  geom_label(aes(x = -97.55, y = 36.8, label = "Arkansas River"), fontface = "bold", color = "black", size = 4, angle = 0) +
  geom_label(aes(x = -95.9, y = 34.8, label = "Eufaula Reservoir"), fontface = "bold", color = "black", size = 4, angle = 0) +
  geom_label(aes(x = -94.9, y = 35.9, label = "Tenkiller Reservoir"), fontface = "bold", color = "black", size = 4, angle = 0) +
  geom_sf(mapping = aes(), tulsa_okc, color = "black", size = 4) +
  annotation_scale(location = "bl", width_hint = 0.15) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.25, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_minimal) +
  annotate(geom = "rect", xmin = -95.4, xmax = -94.8, ymin = 35.2, ymax = 35.6, alpha = 0.2)+
  coord_sf() +
  theme(axis.line=element_blank(),
        axis.text.y=element_blank(), axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(), legend.position="none",
        panel.background=element_blank(), panel.grid.major=element_line(color = "white"),
        panel.grid.minor=element_line(NULL), plot.background=element_blank(), axis.text.x = element_blank())

ok_map  
tiff("ok_map.tiff", units="in", width=12, height=7, res=1200)
ok_map
dev.off()          

arrowA <- data.frame(x1 = 2.3, x2 = 10.4, y1 = 16.4, y2 = 21.2)
arrowB <- data.frame(x1 = 2.3, x2 = 10.4, y1 = 16.4, y2 = 10)



study_site <- ggplot() +
  geom_sf(mapping = aes(), size = 2, color= "lightblue3", rivers_ACI)+
  geom_sf(mapping = aes(), fill= "light blue", lakes_area) +
  #geom_path(data = I40_ss, aes(x = long, y = lat, group = group)) +
  coord_sf(xlim = c(-95.35, -94.78), ylim = c(35.25, 35.62)) +
  theme(axis.line=element_blank(),
        axis.text.y=element_blank(), axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(), legend.position="none",
        panel.background=element_blank(), panel.grid.major=element_line(color = "white"),
        panel.grid.minor=element_line(NULL), plot.background=element_blank(), 
        axis.text.x = element_blank(), plot.title = element_text(hjust = 0.5, size = 20), 
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  labs(x="Longitude", y="Latitude") +
  annotation_scale(location = "br", width_hint = 0.15) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.25, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_minimal) +
  geom_label(aes(x = -95.17, y = 35.35, label = "Canadian River"), fontface = "bold", 
           color = "black", size = 4, angle = 0) +
  geom_label(aes(x =	-95.08, y = 35.56, label = "lower Illinois River"), fontface = "bold", 
           color = "black", size = 4, angle = 0) +

  geom_point(aes(x = -95.055, y = 35.595), size = 6, color = "black", shape = 18) +
  geom_point(aes(x = -95.1, y = 35.5), size = 6, color = "black", shape = 18) +
  
  geom_point(aes(x = -95.08, y = 35.43), size = 6, color = "black", shape = 18) +
  geom_point(aes(x = -95.355, y = 35.304), size = 6, color = "black", shape = 18) +
  
  geom_point(aes(x = -95, y = 35.413), size = 5, color = "black", shape = 17) +
  geom_point(aes(x = -95.165, y = 35.55), size = 5, color = "black", shape = 17) +
  geom_point(aes(x = -95.091, y = 35.48), size = 5, color = "black", shape = 17) +
  geom_point(aes(x = -94.78, y = 35.345), size = 5, color = "black", shape = 17)+
  
  annotate(geom = "text", x = -95.26, y = 35.54, label = "Webbers Falls Lock and Dam 16", fontface = "bold", color = "black", size = 4) +
  geom_point(aes(x = -95.17, y = 35.553), size = 5, color = "black", shape = 20)+
  geom_segment(aes(x = -95.17, y = 35.553, xend = -95.188, yend = 35.545), lineend = "round") + 
  
  annotate(geom = "text", x = -94.85, y = 35.46, label = "Robert S. Kerr Lock and Dam 15", fontface = "bold", color = "black", size = 4) +
  geom_point(aes(x = -94.78, y = 35.345), size = 5, color = "black", shape = 20)+
  geom_segment(aes(x = -94.78, y = 35.345, xend = -94.78, yend = 35.455), lineend = "round") 

study_site
tiff("study_site_passive.tiff", units="in", width=12, height=7, res=1200)
study_site
dev.off()

study_site_wo_telem <- ggplot() +
  geom_sf(mapping = aes(), size = 2, color= "lightblue3", rivers_ACI)+
  geom_sf(mapping = aes(), fill= "light blue", lakes_area) +
  coord_sf(xlim = c(-95.35, -94.78), ylim = c(35.25, 35.62)) +
  theme(axis.line=element_blank(),
        axis.text.y=element_blank(), axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(), legend.position="none",
        panel.background=element_blank(), panel.grid.major=element_line(color = "white"),
        panel.grid.minor=element_line(NULL), plot.background=element_blank(), 
        axis.text.x = element_blank(), plot.title = element_text(hjust = 0.5, size = 20), 
        panel.border = element_rect(colour = "black", fill=NA, size=0.75)) +
  labs(x="Longitude", y="Latitude") +
  annotation_scale(location = "br", width_hint = 0.15) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.25, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_minimal) +
  geom_label(aes(x = -95.17, y = 35.35, label = "Canadian River"), fontface = "bold", 
             color = "black", size = 4, angle = 0) +
  geom_label(aes(x =	-95.08, y = 35.56, label = "lower Illinois River"), fontface = "bold", 
             color = "black", size = 4, angle = 0) +
 annotate(geom = "text", x = -95.26, y = 35.54, label = "Webbers Falls Lock and Dam 16", fontface = "bold", color = "black", size = 4) +
  geom_point(aes(x = -95.17, y = 35.553), size = 5, color = "black", shape = 20)+
  geom_segment(aes(x = -95.17, y = 35.553, xend = -95.188, yend = 35.545), lineend = "round") + 
  
  annotate(geom = "text", x = -94.85, y = 35.46, label = "Robert S. Kerr Lock and Dam 15", fontface = "bold", color = "black", size = 4) +
  geom_point(aes(x = -94.78, y = 35.345), size = 5, color = "black", shape = 20)+
  geom_segment(aes(x = -94.78, y = 35.345, xend = -94.78, yend = 35.455), lineend = "round")
 
study_site_wo_telem
tiff("study_site_wo_telem.tiff", units="in", width=12, height=7, res=1200)
study_site_wo_telem
dev.off()

reservoirs_deathly <- lakes_area %>%
  filter(grepl('Robert S Kerr|Webbers Falls', name))

reservoirs_regular <- lakes_area %>%
  filter(grepl('Tenkiller|Eufaula', name))

rivers_deathly <- rivers_ACI %>%
  filter(grepl("Arkansas River", name))

rivers_stress <- rivers_ACI %>%
  filter(grepl("Lower Illinois", name))

rivers_regular <- rivers_ACI %>%
  filter(grepl("Canadian River from mouth to Eufaula Reservoir Dam", name))


spring_striper_stress <- ggplot() +
  geom_sf(mapping = aes(color = "green"), size = 2, rivers_deathly)+
  geom_sf(mapping = aes(color = "green"), size = 2, rivers_stress)+
  geom_sf(mapping = aes(color = "light blue"), size = 2, rivers_regular) +
  geom_sf(mapping = aes(color = "green"), fill= "light blue", reservoirs_deathly) +
  geom_sf(mapping = aes(color = "light blue"), fill= "light blue", reservoirs_regular) +
  scale_colour_manual(name = 'Habitat Suitability', 
                      values =c('green', 'light blue'), labels = c('Adequate','Unknown/Inaccessible'))+
  #scale_color_identity(name = "",
                      # breaks = c('light blue', 'green', 'red', 'orange'),
                      # labels = c('Unknown/Inaccessible', 'Adequate', 'Lethal', 'Dangerous'),
                      # guide = "legend") +
  coord_sf(xlim = c(-95.35, -94.78), ylim = c(35.25, 35.62)) +
  theme(axis.line=element_blank(),
        axis.text.y=element_blank(), axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(), panel.grid.major=element_line(color = "white"),
        panel.grid.minor=element_line(NULL), plot.background=element_blank(), 
        axis.text.x = element_blank(), plot.title = element_text(hjust = 0.5, size = 20), 
        panel.border = element_rect(colour = "black", fill=NA, size=0.75)) +
  labs(x="Longitude", y="Latitude") +
  annotation_scale(location = "br", width_hint = 0.15) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.25, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_minimal) +
  geom_label(aes(x = -95.17, y = 35.35, label = "Canadian River"), fontface = "bold", 
             color = "black", size = 4, angle = 0) +
  geom_label(aes(x = -95.2, y = 35.3, label = "?"), fontface = "bold", 
             color = "black", size = 5, angle = 0) +
  geom_label(aes(x =	-95.08, y = 35.56, label = "lower Illinois River"), fontface = "bold", 
             color = "black", size = 4, angle = 0) +
  annotate(geom = "text", x = -95.26, y = 35.54, label = "Webbers Falls Lock and Dam 16", fontface = "bold", color = "black", size = 4) +
  geom_point(aes(x = -95.17, y = 35.553), size = 5, color = "black", shape = 20)+
  geom_segment(aes(x = -95.17, y = 35.553, xend = -95.188, yend = 35.545), lineend = "round") + 
  
  annotate(geom = "text", x = -94.85, y = 35.46, label = "Robert S. Kerr Lock and Dam 15", fontface = "bold", color = "black", size = 4) +
  geom_point(aes(x = -94.78, y = 35.345), size = 5, color = "black", shape = 20)+
  geom_segment(aes(x = -94.78, y = 35.345, xend = -94.78, yend = 35.455), lineend = "round")+
  ggtitle(label = "Spring")
  

spring_striper_stress
tiff("spring_striper_stress.tiff", units="in", width=12, height=7, res=1200)
spring_striper_stress
dev.off() 

summer_striper_stress <- ggplot() +
  geom_sf(mapping = aes(color = "red"), size = 2, rivers_deathly)+
  geom_sf(mapping = aes(color = "red"), fill= "light blue", reservoirs_deathly) +
  geom_sf(mapping = aes(color = "orange"), size = 2, rivers_stress)+
  geom_sf(mapping = aes(color = "light blue"), size = 2, rivers_regular) +
  geom_sf(mapping = aes(color = "light blue"), fill= "light blue", reservoirs_regular) +
  scale_colour_manual(name = 'Habitat Suitability', 
                      values =c('light blue', 'orange', 'red', 'light blue', 'light blue'), 
                      labels = c('Unknown/Inaccessible', 'Dangerous', 'Lethal'))+
  #scale_color_identity(name = "",
  # breaks = c('light blue', 'green', 'red', 'orange'),
  # labels = c('Unknown/Inaccessible', 'Adequate', 'Lethal', 'Dangerous'),
  # guide = "legend") +
  
  coord_sf(xlim = c(-95.35, -94.78), ylim = c(35.25, 35.62)) +
  theme(axis.line=element_blank(),
        axis.text.y=element_blank(), axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(), panel.grid.major=element_line(color = "white"),
        panel.grid.minor=element_line(NULL), plot.background=element_blank(), 
        axis.text.x = element_blank(), plot.title = element_text(hjust = 0.5, size = 20), 
        panel.border = element_rect(colour = "black", fill=NA, size=0.75)) +
  labs(x="Longitude", y="Latitude") +
  annotation_scale(location = "br", width_hint = 0.15) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.25, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_minimal) +
  geom_label(aes(x = -95.17, y = 35.35, label = "Canadian River"), fontface = "bold", 
             color = "black", size = 4, angle = 0) +
  geom_label(aes(x = -95.2, y = 35.3, label = "?"), fontface = "bold", 
             color = "black", size = 5, angle = 0) +
  geom_label(aes(x =	-95.08, y = 35.56, label = "lower Illinois River"), fontface = "bold", 
             color = "black", size = 4, angle = 0) +
  annotate(geom = "text", x = -95.26, y = 35.54, label = "Webbers Falls Lock and Dam 16", fontface = "bold", color = "black", size = 4) +
  geom_point(aes(x = -95.17, y = 35.553), size = 5, color = "black", shape = 20)+
  geom_segment(aes(x = -95.17, y = 35.553, xend = -95.188, yend = 35.545), lineend = "round") + 
  
  annotate(geom = "text", x = -94.85, y = 35.46, label = "Robert S. Kerr Lock and Dam 15", fontface = "bold", color = "black", size = 4) +
  geom_point(aes(x = -94.78, y = 35.345), size = 5, color = "black", shape = 20)+
  geom_segment(aes(x = -94.78, y = 35.345, xend = -94.78, yend = 35.455), lineend = "round")+
  ggtitle(label = "Summer")

summer_striper_stress
tiff("summer_striper_stress.tiff", units="in", width=12, height=7, res=1200)
summer_striper_stress
dev.off() 

autumn_striper_stress <- ggplot() +
  geom_sf(mapping = aes(color = "green"), size = 2, rivers_deathly)+
  geom_sf(mapping = aes(color = "green"), size = 2, rivers_stress)+
  geom_sf(mapping = aes(color = "light blue"), size = 2, rivers_regular) +
  geom_sf(mapping = aes(color = "green"), fill= "light blue", reservoirs_deathly) +
  geom_sf(mapping = aes(color = "light blue"), fill= "light blue", reservoirs_regular) +
  scale_colour_manual(name = 'Habitat Suitability', 
                      values =c('green', 'light blue'), labels = c('Adequate','Unknown/Inaccessible'))+
  
  coord_sf(xlim = c(-95.35, -94.78), ylim = c(35.25, 35.62)) +
  theme(axis.line=element_blank(),
        axis.text.y=element_blank(), axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(), panel.grid.major=element_line(color = "white"),
        panel.grid.minor=element_line(NULL), plot.background=element_blank(), 
        axis.text.x = element_blank(), plot.title = element_text(hjust = 0.5, size = 20), 
        panel.border = element_rect(colour = "black", fill=NA, size=0.75)) +
  labs(x="Longitude", y="Latitude") +
  annotation_scale(location = "br", width_hint = 0.15) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.25, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_minimal) +
  geom_label(aes(x = -95.17, y = 35.35, label = "Canadian River"), fontface = "bold", 
             color = "black", size = 4, angle = 0) +
  geom_label(aes(x = -95.2, y = 35.3, label = "?"), fontface = "bold", 
             color = "black", size = 5, angle = 0) +
  geom_label(aes(x =	-95.08, y = 35.56, label = "lower Illinois River"), fontface = "bold", 
             color = "black", size = 4, angle = 0) +
  annotate(geom = "text", x = -95.26, y = 35.54, label = "Webbers Falls Lock and Dam 16", fontface = "bold", color = "black", size = 4) +
  geom_point(aes(x = -95.17, y = 35.553), size = 5, color = "black", shape = 20)+
  geom_segment(aes(x = -95.17, y = 35.553, xend = -95.188, yend = 35.545), lineend = "round") + 
  
  annotate(geom = "text", x = -94.85, y = 35.46, label = "Robert S. Kerr Lock and Dam 15", fontface = "bold", color = "black", size = 4) +
  geom_point(aes(x = -94.78, y = 35.345), size = 5, color = "black", shape = 20)+
  geom_segment(aes(x = -94.78, y = 35.345, xend = -94.78, yend = 35.455), lineend = "round")+
  ggtitle(label = "Autumn")

autumn_striper_stress
tiff("autumn_striper_stress.tiff", units="in", width=12, height=7, res=1200)
autumn_striper_stress
dev.off()




sb_map <- ggplot() +
  coord_equal(xlim = c(0, 30), ylim = c(0, 30), expand = FALSE) +
  annotation_custom(ggplotGrob(study_site), xmin = 10, xmax = 30, ymin = 0, ymax = 30) +
  annotation_custom(ggplotGrob(inset_map), xmin = -5, xmax = 10, ymin = 12, ymax = 22) +
  theme_void() +
  theme(plot.margin = unit(c(0,0,0,0), "cm"))  +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = arrowA, lineend = "round") +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = arrowB, lineend = "round")

sb_map


tiff("sb_map.tiff", units="in", width=11, height=8, res=1200)
sb_map
dev.off()


library(cowplot)

ggdraw(xlim = c(0, 28), ylim = c(0, 20)) +
  draw_plot(study_site, x = 0, y = 0, width = 20, height = 20) +
  draw_plot(inset_map, x = 20, y = 11.25, width = 8, height = 8) +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = arrowA, 
               arrow = arrow(), lineend = "round") +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = arrowB, 
               arrow = arrow(), lineend = "round")
#theme(panel.grid.major=element_line(colour = NA))
  #geom_dl(aes(label = passive_arrays), method = "last.points", cex = 0.8)
#geom_segment(aes(x = -95.03, y = 35.58, xend = -95.05, yend = 35.59), size = 1.3)






##function for scale bar and north arrow##


create_scale_bar <- function(lon,lat,distance_lon,distance_lat,distance_legend, dist_units = "km"){
  # First rectangle
  bottom_right <- gcDestination(lon = lon, lat = lat, bearing = 90, dist = distance_lon, dist.units = dist_units, model = "WGS84")
  
  topLeft <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distance_lat, dist.units = dist_units, model = "WGS84")
  rectangle <- cbind(lon=c(lon, lon, bottom_right[1,"long"], bottom_right[1,"long"], lon),
                     lat = c(lat, topLeft[1,"lat"], topLeft[1,"lat"],lat, lat))
  rectangle <- data.frame(rectangle, stringsAsFactors = FALSE)
  
  # Second rectangle t right of the first rectangle
  bottom_right2 <- gcDestination(lon = lon, lat = lat, bearing = 90, dist = distance_lon*2, dist.units = dist_units, model = "WGS84")
  rectangle2 <- cbind(lon = c(bottom_right[1,"long"], bottom_right[1,"long"], bottom_right2[1,"long"], bottom_right2[1,"long"], bottom_right[1,"long"]),
                      lat=c(lat, topLeft[1,"lat"], topLeft[1,"lat"], lat, lat))
  rectangle2 <- data.frame(rectangle2, stringsAsFactors = FALSE)
  
  # Now let's deal with the text
  on_top <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distance_legend, dist.units = dist_units, model = "WGS84")
  on_top2 <- on_top3 <- on_top
  on_top2[1,"long"] <- bottom_right[1,"long"]
  on_top3[1,"long"] <- bottom_right2[1,"long"]
  
  legend <- rbind(on_top, on_top2, on_top3)
  legend <- data.frame(cbind(legend, text = c(0, distance_lon, distance_lon*2)), stringsAsFactors = FALSE, row.names = NULL)
  return(list(rectangle = rectangle, rectangle2 = rectangle2, legend = legend))
}

scale_bar <- function(lon, lat, distance_lon, distance_lat, distance_legend, dist_unit = "km", rec_fill = "white", rec_colour = "black", rec2_fill = "black", rec2_colour = "black", legend_colour = "black", legend_size = 3, orientation = TRUE, arrow_length = 500, arrow_distance = 300, arrow_north_size = 6){
  the_scale_bar <- create_scale_bar(lon = lon, lat = lat, distance_lon = distance_lon, distance_lat = distance_lat, distance_legend = distance_legend, dist_unit = dist_unit)
  # First rectangle
  rectangle1 <- geom_polygon(data = the_scale_bar$rectangle, aes(x = lon, y = lat), fill = rec_fill, colour = rec_colour)
  
  # Second rectangle
  rectangle2 <- geom_polygon(data = the_scale_bar$rectangle2, aes(x = lon, y = lat), fill = rec2_fill, colour = rec2_colour)
  
  # Legend
  scale_bar_legend <- annotate("text", label = paste(the_scale_bar$legend[,"text"], dist_unit, sep=""), x = the_scale_bar$legend[,"long"], y = the_scale_bar$legend[,"lat"], size = legend_size, colour = legend_colour)
  
  res <- list(rectangle1, rectangle2, scale_bar_legend)
  
  if(orientation){# Add an arrow pointing North
    coords_arrow <- create_orientation_arrow(scale_bar = the_scale_bar, length = arrow_length, distance = arrow_distance, dist_unit = dist_unit)
    arrow <- list(geom_segment(data = coords_arrow$res, aes(x = x, y = y, xend = xend, yend = yend)), annotate("text", label = "N", x = coords_arrow$coords_n[1,"x"], y = coords_arrow$coords_n[1,"y"], size = arrow_north_size, colour = "black"))
    res <- c(res, arrow)
  }
  return(res)
}

create_orientation_arrow <- function(scale_bar, length, distance = 1, dist_units = "km"){
  lon <- scale_bar$rectangle2[1,1]
  lat <- scale_bar$rectangle2[1,2]
  
  # Bottom point of the arrow
  beg_point <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distance, dist.units = dist_units, model = "WGS84")
  lon <- beg_point[1,"long"]
  lat <- beg_point[1,"lat"]
  
  # Let us create the endpoint
  on_top <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = length, dist.units = dist_units, model = "WGS84")
  
  left_arrow <- gcDestination(lon = on_top[1,"long"], lat = on_top[1,"lat"], bearing = 225, dist = length/5, dist.units = dist_units, model = "WGS84")
  
  right_arrow <- gcDestination(lon = on_top[1,"long"], lat = on_top[1,"lat"], bearing = 135, dist = length/5, dist.units = dist_units, model = "WGS84")
  
  res <- rbind(
    cbind(x = lon, y = lat, xend = on_top[1,"long"], yend = on_top[1,"lat"]),
    cbind(x = left_arrow[1,"long"], y = left_arrow[1,"lat"], xend = on_top[1,"long"], yend = on_top[1,"lat"]),
    cbind(x = right_arrow[1,"long"], y = right_arrow[1,"lat"], xend = on_top[1,"long"], yend = on_top[1,"lat"]))
  
  res <- as.data.frame(res, stringsAsFactors = FALSE)
  
  # Coordinates from which "N" will be plotted
  coords_n <- cbind(x = lon, y = (lat + on_top[1,"lat"])/2)
  
  return(list(res = res, coords_n = coords_n))
}



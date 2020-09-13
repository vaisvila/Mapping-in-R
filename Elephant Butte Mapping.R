##Elephant Butte Map
rm(list = ls())
#install.packages("ggsn")
library(ggsn)
library(sf)
library(ggplot2)
library(maptools)
library(plyr)

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





setwd("C:/Users/alexv1/Google Drive/NM LMB Project 16-19'/Data/ArcGIS")
s1 <- read_sf("Sample Locations", "Sample Locations")
s1 <- st_set_crs(s1, "+proj=longlat +datum=NAD83 +no_defs")
s2 <- read_sf(".", "export_ELBU_map")
s3 <- read_sf(".", "Elephant_Butte")
s4 <- read_sf("new_mexico_bnd_shp", "new_mexico_bnd")

structure(s1)
structure(s2)

s1$'Sample location' <- c("Kettle Top", "McRae", "The Jungles")

library(rgeos)
library(rnaturalearth)
library(tidyverse)
library(ggpubr)

world <- ne_countries(scale = 'medium', returnclass = 'sf')
usa <- subset(world, admin == "United States of America")


site_map <- ggplot() +
  geom_sf(mapping = aes(), fill= "light blue", s2)+
  geom_sf(mapping = aes(color='Sample location'), show.legend = "point", size = 3, s1) +
  coord_sf(xlim = c(-107.24, -107.10), ylim = c(33.14, 33.34)) +
  theme_bw()+ 
  scale_color_manual(values = c("Sample location" = "black")) +
  theme(panel.grid.major=element_line(colour = NA), panel.border = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
        axis.text.x = element_blank(), legend.position = "none", axis.text.y = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank()) +
   labs(x="Longitude", y="Latitude") +
  scale_bar(lon = -107.162, lat = 33.132, 
        distance_lon = 2, distance_lat = 0.5, distance_legend = 0.8, 
        dist_unit = "km", arrow_length = 2, arrow_distance = 1, arrow_north_size = 5)+ 
  annotate("text", x = -107.14, y = 33.17, label = "The Jungles", size = 4)+
  annotate("text", x = -107.125, y = 33.197, label = "McRae Canyon", size = 4)+
  annotate("text", x = -107.13, y = 33.246, label = "Kettle Top", size = 4)

  site_map

inset_map <- ggplot() +
  geom_sf(mapping = aes(), fill = "white", usa) +
  geom_sf(mapping = aes(), fill = "white", s4) +
  geom_sf(mapping = aes(), shape = 2, size = 3, fill = "black", s3) +
  coord_sf(xlim = c(-125, -65), ylim = c(25, 50))+
  theme(axis.line=element_blank(),
        axis.text.y=element_blank(), axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(), legend.position="none",
        panel.background=element_blank(), panel.grid.major=element_line(color = "white"),
        panel.grid.minor=element_line(NULL), plot.background=element_blank(), axis.text.x = element_blank())

inset_map  

arrowA <- data.frame(x1 = 5, x2 = 17.1, y1 = 18.9, y2 = 27)
arrowB <- data.frame(x1 = 5, x2 = 16.5, y1 = 18.9, y2 = 3.7)

tiff("elbu_map.tiff", units="in", width=5, height=5, res=1200)

full_map <- ggplot() +
  coord_equal(xlim = c(0, 30), ylim = c(0, 30), expand = FALSE) +
  annotation_custom(ggplotGrob(site_map), xmin = 10, xmax = 30, ymin = 0, ymax = 30) +
  annotation_custom(ggplotGrob(inset_map), xmin = 0, xmax = 15, ymin = 15, ymax = 25) +
  theme_void() +
  theme(plot.margin = unit(c(0,0,0,0), "cm"))  +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = arrowA, lineend = "round") +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = arrowB, lineend = "round")

  full_map
        
  dev.off()
#,legend.background = element_blank(), legend.margin,
       # legend.spacing, legend.spacing.x, legend.spacing.y, legend.key,
       # legend.key.size, legend.key.height, legend.key.width, ,
       # legend.text.align, legend.title.align, ,
       # legend.direction, legend.justification, legend.box, legend.box.just,
        #legend.box.margin, legend.box.background, legend.box.spacing)

 united_states <- ne_states(country = 'United States of America', returnclass = 'sf')
 
arrowC <- data.frame(x1 = -105.86, x2 = -107.191, y1 = 46.38, y2 = 33.1542)
arrowD <- data.frame(x1 = -91.87, x2 = -107.191, y1 = 34.78, y2 = 33.1542)
arrowE <- data.frame(x1 = -104.67, x2 = -107.191, y1 = 34.91, y2 = 33.1542)

arrows_stocking <- join_all(list(arrowC, arrowD, arrowE), by = c("x1", "x2", "y1", "y2"), type = 'full')

MT <- data.frame(x=-105.86, y=46.38, name = "Miles City, MT")
AR <- data.frame(x=-91.87, y=34.78, name = "F&L Anderson Farm, AR")
NM <- data.frame(x=-104.67, y=34.91, name = "Rock Lake, NM")

points_stocking <- join_all(list(MT, AR, NM), by = c("x", "y", "name"), type = 'full')
  
  stocking_map <- ggplot() +
    geom_sf(mapping = aes(), fill = "white", united_states) +
    geom_sf(mapping = aes(), fill = "white", s4) +
    geom_sf(mapping = aes(), shape = 2, size = 3, fill = "black", s3) +
    coord_sf(xlim = c(-125, -65), ylim = c(25, 50))+
    theme(axis.line=element_blank(),
          axis.text.y=element_blank(), axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(), legend.position="none",
          panel.background=element_blank(), panel.grid.major=element_line(color = "white"),
          panel.grid.minor=element_line(NULL), plot.background=element_blank(), 
          axis.text.x = element_blank(), plot.title = element_text(hjust = 0.5, size = 20)) +
    geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = arrows_stocking, lineend = "round") +
    geom_point(data = points_stocking, aes(x=x, y=y), size = 2) +
    geom_label(data = points_stocking, aes(x=x, y=y, label = name), size=4, nudge_y = 1) +
    ggtitle("2011-2015")
    
  stocking_map  
  

  tiff("stocking_map.tiff", units="in", width=9, height=5, res=1200)
  stocking_map
  dev.off()

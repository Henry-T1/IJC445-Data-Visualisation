###############################################################
###############################################################
# Packages
###############################################################
###############################################################


set_api_key("b012a62cf391274dc5f7901f9b6484359fd703a376910daac2bb9edbee432a73")

library(sf)
library(osmdata)
library(ggspatial)

# Clean Data file
aq_clean <- read.csv("aq_clean.csv")

# Restructuring datetime lost when reading the csv
aq_clean <- aq_clean %>% 
  mutate(datetime_from=ifelse(
    nchar(datetime_from)==10,
    paste0(datetime_from," 00:00:00"),
    datetime_from),
    datetime_from=ymd_hms(datetime_from, tz = "UTC", quiet=TRUE)) %>% 
  na.omit() 


###############################################################
###############################################################
# Plot 1 - Geospatial Bubblemap
###############################################################
###############################################################


###############################################################
#Formatting Data
###############################################################

# Obtaining location means
aq_means <- aq_clean %>% 
  group_by(longitude, latitude, location) %>% 
  summarise(mean_value=mean(no2, na.rm=TRUE),
            n_obs=n())

# Converting means to sf to be plotted
aq_sensor_sf <- st_as_sf(aq_means,
                         coords=c("longitude", "latitude"),
                         crs=4326)  

# Using a labeller for better location names

aq_sensor_sf$label <- c("barnsley_road"="Barnsley Road",
                        "barnsley_gawber"="Barnsley Gawber",
                        "chesterfield_roadside"="Chesterfield Roadside",
                        "ladybower"="Ladybower",
                        "tinsley"="Tinsley",
                        "chesterfield_loundsley"="Chesterfield Loundsley",
                        "devonshire"="Devonshire")[aq_sensor_sf$location]

# Extracting coordinates for later use 

coords <- st_coordinates(aq_sensor_sf)
aq_sensor_sf$X <- coords[, 1]
aq_sensor_sf$Y <- coords[, 2]

###############################################################
# Extracting OSM Layers
###############################################################

# Setting a bounding box
Sheffield_bbx <- c(xmin = -1.86,
                   ymin =  53.117,
                   xmax = -1.03,
                   ymax =  53.6)


# Industrial areas
Sheffield_industrial <- opq(bbox=Sheffield_bbx, timeout=50) %>% 
  add_osm_feature(key="landuse",
                  value="industrial") %>% 
  osmdata_sf()

# Railways
Sheffield_railway <- opq(bbox=Sheffield_bbx, timeout=50) %>% 
  add_osm_feature(key="railway", 
                  value="rail") %>% 
  osmdata_sf()

# Highways
Sheffield_higwhay <- opq(bbox=Sheffield_bbx, timeout=50) %>% 
  add_osm_feature(key="highway",
                  value=c("motorway", "primary",
                          "motorway_link", "primary_link")) %>% 
  osmdata_sf()

# Streets
Sheffield_streets <- opq(bbox=Sheffield_bbx, timeout=50) %>% 
  add_osm_feature(key="highway",
                  value=c("secondary", "tertiary",
                          "secondary_link", "tertiary_link")) %>% 
  osmdata_sf()

# Side streets
Sheffield_side_streets <- opq(bbox=Sheffield_bbx, timeout=50) %>% 
  add_osm_feature(key="highway",
                  value=c("residential", "living street", 
                          "unclassified", "service", "footway")) %>% 
  osmdata_sf()

# Lakes
Sheffield_lakes <- opq(bbox=Sheffield_bbx, timeout=50) %>% 
  add_osm_feature(key="natural", 
                  value="water") %>% 
  osmdata_sf()

# Rivers
Sheffield_rivers <- opq(bbox=Sheffield_bbx, timeout=50) %>% 
  add_osm_feature(key="waterway", 
                  value="river") %>% 
  osmdata_sf()

# Peak district 
Sheffield_greenspace <- opq(bbox = Sheffield_bbx) %>%
  add_osm_feature(key = "natural") %>% 
  osmdata_sf()

###############################################################
# Building the Plot
###############################################################


bubblemap <- ggplot()+
  geom_sf(data=Sheffield_higwhay$osm_lines, inherit.aes=FALSE, colour="grey60", linewidth=0.9, alpha=0.8)+
  geom_sf(data=Sheffield_streets$osm_lines, inherit.aes = FALSE, colour='grey60', linewidth=0.7, alpha=0.7)+
  geom_sf(data=Sheffield_side_streets$osm_lines, inherit.aes=FALSE, colour='grey70', alpha=0.5, linewidth=0.5)+
  geom_sf(data=Sheffield_lakes$osm_polygons, inherit.aes = FALSE, fill="lightblue")+
  geom_sf(data=Sheffield_lakes$osm_multipolygons, inherit.aes = FALSE, fill="lightblue")+
  geom_sf(data=Sheffield_rivers$osm_lines, inherit.aes=FALSE, colour='lightblue', linewidth=0.7)+
  geom_sf(data=Sheffield_greenspace$osm_multipolygons, inherit.aes = FALSE, fill='#d9f0d3', alpha=0.2)+
  geom_sf(data=Sheffield_railway$osm_lines, inherit.aes = FALSE, colour="#666666", linetype="dotdash", linewidth=1)+
  geom_sf(data=Sheffield_industrial$osm_polygons,
          aes(fill="Industrial Area"),colour="#6B7A8F", alpha=0.8)+
  scale_fill_manual(name="Land Use",
                    values=c("Industrial Area"="#6B7A8F"), guide=guide_legend(order=3,
                                                                              title.position="top",
                                                                              label.position="right",
                                                                              keywidth=unit(0.6, "cm")))+
geom_sf_label(data=subset(aq_sensor_sf, label=="Barnsley Gawber"),
                aes(label=label),
                size=4,
                nudge_y=0.01,
                label.size=0,
                fill=alpha("white", 0.75)) +
geom_sf_label(data=subset(aq_sensor_sf, label=="Barnsley Road"),
                aes(label = label),
                size=4,
                nudge_y=0.014,
                label.size=0,
                fill=alpha("white", 0.75)) +
geom_sf_label(data=subset(aq_sensor_sf, label=="Devonshire"),
                aes(label=label),
                size=4,
                nudge_y=0.01,
                label.size=0,
                fill=alpha("white", 0.75)) +
geom_sf_label(data=subset(aq_sensor_sf, label=="Tinsley"),
                aes(label=label),
                size=4,
                nudge_y=0.0135,
                label.size=0,
                fill=alpha("white", 0.75)) +
geom_sf_label(data=subset(aq_sensor_sf, label=="Chesterfield Roadside"),
                aes(label=label),
                size=4,
                nudge_y=-0.01,
                label.size=0,
                fill=alpha("white", 0.75)) +
geom_sf_label(data=subset(aq_sensor_sf, label=="Chesterfield Loundsley"),
                aes(label=label),
                size=4,
                nudge_y=0.01,
                label.size=0,
                fill=alpha("white", 0.75)) +
geom_sf_label(data= subset(aq_sensor_sf, label=="Ladybower"),
                aes(label=label),
                size=4,
                nudge_y=0.0075,
                label.size=0,
                fill=alpha("white", 0.75)) +
  ggnewscale::new_scale_fill()+
  geom_sf(data=aq_sensor_sf,
          aes(size=mean_value, fill=mean_value), colour="grey30", shape=21, stroke=0.8, alpha=0.7)+
  coord_sf(xlim=c(-1.8, -1.3), ylim=c(53.2, 53.6))+
  scale_size_continuous(name= expression("Mean NO"[2]~"Concentration ("*mu*"g/m"^3*")"), range=c(4,14),
                        guide=guide)+
  scale_fill_viridis_c(name=expression("Mean NO"[2]~"Concentration ("*mu*"g/m"^3*")"), option="inferno",
                       guide=guide_colorbar(title.position="top",
                                            label.position="right"))+
  labs(x="Longitude", y="Latitude",  title = expression(
    "Mean NO"[2]~"Concentration at Air QUality Monitoring Sites Across South Yorkshire"))+
  theme_minimal(base_size=14)+
  ggspatial::annotation_north_arrow(location="tl", which_north="true",
                                    style=north_arrow_orienteering,
                                    height=unit(1.2, "cm"),
                                    width=unit(1.2, "cm"))+
  theme(legend.box="vertical",
        legend.box.just="center",
        legend.justification="center",
        legend.spacing.y=unit(0.4, "cm"),
        legend.margin=margin(t=-5, r=0, b=0, l=0),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))+
  theme(panel.grid=element_blank())
 
print(bubblemap)

ggsave("Plots/Geospatial_bubbleplot.png", plot=bubblemap, 
       dpi=300, width=14, height=14, units="in")


###############################################################
###############################################################
# Plot 2 - Geospatial Bubblemap
###############################################################
###############################################################

###############################################################
# Building the Plot
###############################################################

# Reordering based on median 

aq_clean$location <- reorder(aq_clean$location, -aq_clean$no2, FUN=median)

# Building the plot

boxplot <- ggplot(aq_clean, aes(x=location, y=no2, group=location))+
  geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.5)+
  theme_minimal()+
scale_x_discrete(labels=c(
  "ladybower"="Ladybower",
  "chesterfield_loundsley"="Chesterfield Loundsley",
  "barnsley_gawber"="Barnsley Gawber",
  "chesterfield_roadside"="Chesterfield Roadside",
  "devonshire"="Devonshire",
  "tinsley"="Tinsley",
  "barnsley_road"="Barnsley Road"))+
  labs(title="Distribution of Hourly NO"[2]~"Concentration at Air Quality Monitoring Sites Across South Yorkshire",
  x="Location",
  y=  expression("NO"[2]~"Concentration ("*mu*"g/m"^3*")"))+
  coord_cartesian(ylim=c(0,100))+
  theme(plot.title=element_text(size=18))+
  theme(axis.title.x=element_text(size=16, face="bold"),
        axis.title.y=element_text(size=16, face="bold"),
        axis.text.x=element_text(size=16, angle=45, hjust=1),
        axis.text.y=element_text(size=16))

print(boxplot)

ggsave("Plots/Boxplot.png", plot=boxplot, width=10, height=8, dpi=300)


###############################################################
###############################################################
# Plot 3 - Seasonal Line Plot
###############################################################
###############################################################





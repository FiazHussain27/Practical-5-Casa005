library(sf)
library(tidyverse)
library(tmap)
# OSM data

OSM2 <- st_read("C:/UCL Urban Spatial Sciences/wk5/casa005/prac_5/greater-london-251027-free.shp/gis_osm_pois_free_1.shp")%>%
  st_transform(., 27700) %>%
  #select hotels only
  filter(fclass == 'hotel')

# Airbnb data
Airbnb2 <- read_csv("C:/UCL Urban Spatial Sciences/wk5/casa005/prac_5/listings.csv") %>%
  # longitude is considered x value here, latitude is y
  st_as_sf(., coords = c("longitude", "latitude"), 
           crs = 4326) %>%
  st_transform(., 27700)%>%
  #select entire places that are available all year
  filter(room_type == 'Entire home/apt' & availability_365 =='365')

#London Borough data is already in 277000
Londonborough2 <- st_read("C:/UCL Urban Spatial Sciences/wk5/casa005/prac_5/statistical-gis-boundaries-london/statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp")%>%
  st_transform(., 27700)

# load world cities
Worldcities2 <- st_read("C:/UCL Urban Spatial Sciences/wk5/casa005/prac_5/World_Cities/World_Cities.shp") %>%
  st_transform(., 27700)
Worldcities22 <- Worldcities %>%
  filter(CNTRY_NAME=='United Kingdom'&
           Worldcities$CITY_NAME=='Birmingham'|
           Worldcities$CITY_NAME=='London'|
           Worldcities$CITY_NAME=='Edinburgh')

# load UK outline
UK_outline2 <- st_read("C:/UCL Urban Spatial Sciences/wk5/casa005/prac_5/gadm41_GBR_shp/gadm41_GBR_0.shp")%>%
  st_transform(., 27700)

#Join

Hotels <-  st_join(Londonborough2, OSM2)

Airbnbs <-  st_join(Londonborough2, Airbnb2)

head(Hotels)

#group by borough and sum

Hotels_sum <- Hotels %>%
  # we need to list the columns we want to keep in the summarise
  group_by(., GSS_CODE, NAME)%>%
  # for each group count the number of rows and store in a column called accomodation count.
  summarise(`Accomodation count` = n())

Airbnb_sum <- Airbnbs %>%
  group_by(., GSS_CODE, NAME)%>%
  summarise(`Accomodation count` = n())
# Check for any missing hotels 

Hotels_found <-st_contains(Londonborough2, OSM2)

Hotels_found
# find length of each hotel
Accomodation_contained <- Londonborough2%>%
  mutate(hotels_n = lengths(st_contains(., OSM2)))%>%
  mutate(airbnbs_n = lengths(st_contains(., Airbnbs)))
#check if there is no hotel
Accomodation_contained %>%
  filter(NAME=="Bexley")

# set the shape
tm1 <- tm_shape(Accomodation_contained) + 
  # set the map layer
  # try changing this to tm_symbols()
  tm_polygons("hotels_n",
              col = "black", lwd=0.5, lty="dashed",)

# plot the map
tm1

# change borders
tm_no_map_layer <- tm_shape(Accomodation_contained) + tm_borders(col= "darkblue")
tm_no_map_layer
tmap_mode("plot")
# plot each map
tm1 <- tm_shape(Accomodation_contained) + 
  tm_polygons(fill ="hotels_n",
              col = "black", 
              lwd =0.5,
              lty="dashed",
              fill.chart = tm_chart_violin(),
              # above this was the same as before
              fill.scale = tm_scale_intervals(
                values="brewer.bu_pu",
                n=5,
                style="jenks"))

tm1
# stats extraction for two columns comaprision 
stats <- Accomodation_contained %>%
  st_drop_geometry() %>%
  dplyr::select(hotels_n, airbnbs_n) %>%  
  summarise(across(everything(), list(
    min = min,
    max = max,
    mean = mean,
    median = median,
    sd = sd
  )))
library(classInt)
# Get Jenks breaks for 5 classes
breaks <- Accomodation_contained%>%
  st_drop_geometry()%>%
  #need a numeric vector not a dataframe or tibble
  pull(airbnbs_n) %>%             
  classIntervals(., n = 5, style = "jenks")

breaks$brks
# facet map for both columns Airbnbs + Hotels
tm1 <- tm_shape(Accomodation_contained) + 
  tm_polygons(
    fill = c("hotels_n", "airbnbs_n"),
    
    
)
)
  
)

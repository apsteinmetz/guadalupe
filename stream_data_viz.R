# display stream data

# Load required libraries
library(leaflet)
library(tidyverse)
library(sf)
library(elevatr)
library(ggridges)

sites_metadata<- read_csv("data/sites_metadata.csv") |> 
  arrange(longitude) |> 
  # factor order is from west to east
  # mutate(site_number = as_factor(site_number)) |> 
  # remove redunant parts of site names
  mutate(site_name = str_remove(site_name, " nr | at | abv ")) |> 
  mutate(site_name = str_remove(site_name, "Guadalupe Rv"))
  
# add location of camp mystic
camp_mystic <- tibble(
  site_number = "Camp Mystic",
  site_name = "Camp Mystic",
  latitude = 30.0101571,
  longitude = -99.3736444,
  altitude = 1000,
  distance_from_first_site_miles = 0
)

# load stream Rdata from data folder
load("data/guadalupe_stream_data.RData")

stream_data_full <- stream_data |> 
  group_by(site_number) |> 
  left_join(sites_metadata, by = "site_number") |> 
  # limit to sites within 75 miles
  filter(distance_from_first_site_miles < 75) |>
  # include just july 4th to July 6 2025
  filter(datetime >= as.POSIXct("2025-07-03 21:00:00",tz = "US/Central") & datetime <= as.POSIXct("2025-07-06",tz ="US/Central")) |>
  arrange(desc(distance_from_first_site_miles))


# reorder factor levels
stream_data_full$site_name <- factor(stream_data_full$site_name, 
                                      levels = unique(stream_data_full$site_name))

stream_data_full

# plot distance vs altitude
stream_data_full  |>
  ggplot(aes(x = distance_from_first_site_miles, y = altitude)) +
  geom_line() +
  labs(title = "Drop for Guadalupe River Sites",
       x = "Distance From First Site (miles)", 
       y = "Altitude (ft)",
      caption = "Source: USGS") +
#  theme_minimal() + 
  # add labels with site names
  geom_text(aes(label = site_name), hjust = 0.5, vjust = -0.5, size = 3)


 # line plot of stream data
stream_data_full |>
  ggplot(aes(x = datetime, y = gage_height_ft,color = site_name)) +  geom_line() +
  labs(title = "Stream Data for Guadalupe River Sites",
       y = "Gage Height (ft)") +
  scale_color_viridis_d() +
  theme_minimal()

# bar chart showing maximum gage height for each site, sorted by distance from first site


max_levels <- stream_data_full |>
  filter(distance_from_first_site_miles < 75) |>
  group_by(site_name) |>
  summarize(max_gage_height = max(gage_height_ft, na.rm = TRUE),
                    max_discharge_cfs = max(discharge_cfs, na.rm = TRUE)) |> 
  left_join(stream_data_full,by = join_by(site_name,max_discharge_cfs == discharge_cfs)) |>
  group_by(site_name) |>
  filter(datetime == min(datetime, na.rm = TRUE))


max_levels |> 
  ggplot(aes(x = site_name, y = max_gage_height)) +
  geom_col(fill = "steelblue") +
  labs(title = "Maximum Gage Height for Guadalupe River Sites",
       x = "Site Name", 
       y = "Maximum Gage Height (ft)") +
  # annotate with datetime of maximum gage height
  geom_text(aes(label = datetime),nudge_y = 15) +
  coord_flip()

# make a ridge plot of gage height over time by site
stream_data_full |>
  ggplot(aes(x = datetime, y = site_name, fill = site_name, height = gage_height_ft)) +
  geom_ridgeline(scale = .1, alpha = 0.8) +
  labs(title = "Gage Height Distribution Over Time by Site",
       subtitle = "Guadalupe River Sites, July 4-6, 2025",
       x = "Gage Height (ft)", 
       y = "Site Name") +
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(legend.position = "none") +
 # show a  15 minute time scale on the x-axis
  # scale_x_datetime(date_breaks = "15 min", date_labels = "%H:%M") +
  # rotate x-axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






# Create a leaflet map with the USGS sites showing the stream data
leaflet_map <- leaflet(sites_metadata) %>%
  addTiles() %>%  # Add OpenStreetMap tiles
  addMarkers(
    lng = ~longitude, 
    lat = ~latitude,
    popup = ~paste("<b>", site_name, "</b><br>",
                   "Site Number:", site_number, "<br>",
                   "Latitude:", round(latitude, 5), "<br>",
                   "Longitude:", round(longitude, 5)),
    label = ~site_name,
    clusterOptions = markerClusterOptions()
  ) %>%
  setView(lng = mean(sites_metadata$longitude, na.rm = TRUE), 
          lat = mean(sites_metadata$latitude, na.rm = TRUE), 
          zoom = 8)



# Display the map
leaflet_map

# show a satellite map
leaflet_map_satellite <- leaflet(sites_metadata) %>%
  addProviderTiles("Esri.WorldImagery") %>%  # Add Esri satellite tiles
  addMarkers(
    lng = ~longitude, 
    lat = ~latitude,
    popup = ~paste("<b>", site_name, "</b><br>",
                   "Site Number:", site_number, "<br>",
                   "Latitude:", round(latitude, 5), "<br>",
                   "Longitude:", round(longitude, 5)),
    label = ~site_name,
    clusterOptions = markerClusterOptions()
  ) %>%
  setView(lng = mean(sites_metadata$longitude, na.rm = TRUE), 
          lat = mean(sites_metadata$latitude, na.rm = TRUE), 
          zoom = 8)

leaflet_map_satellite


# create a bounding box for the map using sf_sites
sf_sites <- st_as_sf(sites_metadata, coords = c("longitude", "latitude"), crs = 4326)
bbox <- st_bbox(sf_sites) |> 
  # expand the bounding box by 10% for better visibility
  st_as_sfc() |>
  st_buffer(dist = 0.1)

#use the elevatr package to get elevation data
elevation_data <- get_elev_raster(sf_sites, z = 10, clip = "bbox",expand = 0.1)
# Plot the elevation data on the map
leaflet_map_elevation <- leaflet() %>%
  addTiles() %>%  # Add OpenStreetMap tiles
  addRasterImage(elevation_data, colors = terrain.colors(10), opacity = 0.5) %>%
  addMarkers(
    data = sites_metadata,
    lng = ~longitude, 
    lat = ~latitude,
    popup = ~paste("<b>", site_name, "</b><br>",
                   "Site Number:", site_number, "<br>",
                   "Latitude:", round(latitude, 5), "<br>",
                   "Longitude:", round(longitude, 5)),
    label = ~site_name,
    clusterOptions = markerClusterOptions()
  ) %>%
  setView(lng = mean(sites_metadata$longitude, na.rm = TRUE), 
          lat = mean(sites_metadata$latitude, na.rm = TRUE), 
          zoom = 8)

leaflet_map_elevation


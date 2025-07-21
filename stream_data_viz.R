# display stream data

# Load required libraries
library(leaflet)
library(tidyverse)
library(sf)
library(elevatr)
library(ggridges)
library(gridExtra)
library(grid)
library(ggmap)

sites_metadata<- read_csv("data/sites_metadata.csv") |> 
  arrange(longitude) |> 
  # factor order is from west to east
  # mutate(site_number = as_factor(site_number)) |> 
  # remove redunant parts of site names
  mutate(site_name = str_remove(site_name, " nr | at | abv ")) |> 
  mutate(site_name = str_remove(site_name, ", TX")) |> 
  mutate(site_name = str_remove(site_name, "Guadalupe Rv"))
  
# add location of camp mystic
camp_mystic_latlon <- c(latitude = 30.0101571,longitude = -99.3736444,altitude = 1850)

# add camp mystic to sites_metadata
sites_metadata <- sites_metadata |> 
  mutate(distance_from_first_site_miles = distance_from_first_site_miles - distance_from_first_site_miles[2]) |>
  add_row(site_number = "000001",
          site_name = "Camp Mystic",
          map_name = "Camp Mystic",
          latitude = camp_mystic_latlon["latitude"],
          longitude = camp_mystic_latlon["longitude"],
          altitude = 1850,
          distance_from_first_site_miles = 
            # compute haveersine distance from first site
            -geosphere::distHaversine(c(camp_mystic_latlon["longitude"], camp_mystic_latlon["latitude"]),
                                     c(sites_metadata$longitude[2], sites_metadata$latitude[2])) / 1609.34)
# load stream Rdata from data folder
load("data/guadalupe_stream_data.RData")

stream_data_full <- stream_data |> 
  group_by(site_number) |> 
  left_join(sites_metadata, by = "site_number") |> 
  # limit to sites within 75 miles
  filter(distance_from_first_site_miles < 70) |>
  # include just july 4th to July 6 2025
  filter(datetime >= as.POSIXct("2025-07-04 02:00:00",tz = "US/Central") & 
           datetime <= as.POSIXct("2025-07-05",tz ="US/Central")) |>
  arrange(desc(distance_from_first_site_miles)) |> 
  select(site_name,everything())


# reorder factor levels
stream_data_full$site_name <- factor(stream_data_full$site_name, 
                                      levels = unique(stream_data_full$site_name))
# plot distance vs altitude
gg_dist <- sites_metadata  |>
  filter(distance_from_first_site_miles < 70) |>
  filter(site_name != "Camp Mystic") |>
  ggplot(aes(x = distance_from_first_site_miles, y = altitude)) +
  geom_line() +
  labs(title = "Camp Mystic is at a Higher Elevation than (Upstream From) all Stream Gages",
       subtitle = "Elevation of Guadalupe River Sites vs. Distance from Guadalupe River Fork at Hunt",
       x = "Distance From River Fork at Hunt (miles)", 
       y = "Elevation (ft)",
      caption = "Source: USGS") +
  # add a vertical line at 0 distance
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  # set ylim at 2000
  ylim(700, 2000) +
  theme_minimal() + 
  # add labels with site names
  geom_text(aes(label = site_name, hjust = 0.5, vjust = "top"), color = "blue",size = 4)

# add segment between Camp Mystic and Hunt
gg_dist <- gg_dist + geom_segment(data = sites_metadata, 
                       aes(x = distance_from_first_site_miles[site_name == "Camp Mystic"], 
                           y = altitude[site_name == "Camp Mystic"],
                           xend = distance_from_first_site_miles[site_name == "Hunt"], 
                           yend = altitude[site_name == "Hunt"]), 
                       color = "red", 
                       inherit.aes = FALSE) +
  geom_label(data = sites_metadata %>% filter(site_name =="Camp Mystic"),
            aes(x = distance_from_first_site_miles, 
                y = altitude, 
                label = paste(site_name,"(South Fork)")), 
            hjust = 0.1, vjust = -0.5, size = 4, color = "red")


# make plot background a gradient going from dark blue to light blue

gg_dist

 
# line plot of stream data
stream_data_full |>
  ggplot(aes(x = datetime, y = gage_height_ft,color = site_name)) +  geom_line() +
  labs(title = "Stream Data for Guadalupe River Sites",
       y = "Gage Height (ft)") +
  scale_color_viridis_d() +
  theme_minimal()

# bar chart showing maximum gage height for each site, sorted by distance from first site


max_heights <- stream_data_full |>
  filter(distance_from_first_site_miles < 75) |>
  group_by(site_name) |>
  filter(gage_height_ft == max(gage_height_ft, na.rm = TRUE))

max_heights |> 
  ggplot(aes(x = site_name, y = gage_height_ft)) +
  geom_col(fill = "steelblue") +
  labs(title = "Maximum Gage Height for Guadalupe River Sites",
       x = "Site Name", 
       y = "Maximum Gage Height (ft)") +
  # annotate with datetime of maximum gage height
  geom_text(aes(label = datetime),nudge_y = 5) +
  coord_flip()

# ------------------------------------------------------------------------------
# create a ridgeline plot of gage height over time

gg_ridge <- stream_data_full |>
  # take just july 4
  filter(datetime > as.POSIXct("2025-07-04 02:00:00", tz = "US/Central") & 
           datetime < as.POSIXct("2025-07-05", tz = "US/Central")) |>
  ggplot(aes(x = datetime, y = site_name, 
             fill = site_name,
             height = gage_height_ft)) +
   annotation_custom(
     grob = rectGrob(
       gp = gpar(fill = linearGradient(
         colours = c("black", "lightblue"),
         x1 = 0, y1 = 0, x2 = .9, y2 = 0
       ), alpha = 1)
     )
   ) +
 geom_ridgeline(aes(x = datetime, y = site_name, 
                     fill = site_name,
                     height = gage_height_ft), 
                 scale = .1, alpha = 0.8) +
labs(title = "Gage Height at Each Site over Time",
       subtitle = "Guadalupe River Sites, July 4, 2025",
       x = "Time on July 4, 2025", 
       y = "<- Downstream - Upstream ->") +
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(legend.position = "none") +
   # show a  15 minute time scale on the x-axis
  scale_x_datetime(date_breaks = "60 min", date_labels = "%H:%M") +
   # rotate x-axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  geom_vline(xintercept = as.POSIXct("2025-07-04 06:40:00", tz = "US/Central"), 
             linetype = "dashed", color = "orange",linewidth = 1) +
  annotate("text",label = "Dawn",
           x = as.POSIXct("2025-07-04 07:30:00", tz = "US/Central"), 
           y = max(stream_data_full$gage_height_ft, na.rm = TRUE)/10 + 6, 
           vjust = -0.5, hjust = 0.5, size = 5, color = "orange")

gg_ridge
# ------------------------------------------------------------------------------
# create a ridgeline plot of discharge rate over time

gg_ridge_discharge <-stream_data_full |>
  # take just july 4
  filter(datetime > as.POSIXct("2025-07-03", tz = "US/Central") & 
           datetime < as.POSIXct("2025-07-05", tz = "US/Central")) |>
  ggplot(aes(x = datetime, y = site_name, 
             fill = site_name,
             height = discharge_cfs)) +
  geom_ridgeline(scale = .00005, alpha = 0.8) +
  labs(title = "Discharge Rate (CFS) Distribution Over Time by Site",
       subtitle = "Guadalupe River Sites, July 3-4, 2025",
       x = "Time on July 4, 2025", 
       y = "Site Name") +
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(legend.position = "none") +
  # show a  15 minute time scale on the x-axis
  scale_x_datetime(date_breaks = "60 min", date_labels = "%H:%M") +
  # rotate x-axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
gg_ridge_discharge

# create a static map of the sites using ggplot2 and ggmap


sites_metadata_crop <- max_heights |> 
  filter(distance_from_first_site_miles < 20) |>
  # format datetime as just time
  mutate(datetime = format(datetime, "%H:%M")) |>
  select(site_name, latitude, longitude, gage_height_ft, datetime)


# Register Google Maps API key (if needed)
# Get a base map centered around the average coordinates of the sites
gg_map <- get_map(location = c(lon = mean(sites_metadata_crop$longitude, na.rm = TRUE), 
                                lat = mean(sites_metadata_crop$latitude, na.rm = TRUE)), 
                  zoom = 11, maptype = "terrain")

# stadia
maptype = "stamen_terrain_background"
sour = "stadia"
map_tiles <- get_map(location = c(left = min(sites_metadata_crop$longitude, na.rm = TRUE)-.02, 
                                  bottom = min(sites_metadata_crop$latitude, na.rm = TRUE)-.02, 
                                  right = max(sites_metadata_crop$longitude, na.rm = TRUE)+.02, 
                                  top = max(sites_metadata_crop$latitude, na.rm = TRUE)+.02),
                         zoom = 11, maptype = maptype,sour = sour)

create_scale_bar <- function(map_data, scale_miles = 5) {
  # Get map bounds
  map_bounds <- attr(map_data, "bb")
  
  # Calculate positions for scale bar
  x_start <- map_bounds$ll.lon + (map_bounds$ur.lon - map_bounds$ll.lon) * 0.4
  y_pos <- map_bounds$ll.lat + (map_bounds$ur.lat - map_bounds$ll.lat) * 0.1
  
  # Convert miles to degrees (approximate)
  miles_to_deg <- scale_miles / 69
  x_end <- x_start + miles_to_deg
  
  list(
    annotate("segment", x = x_start, y = y_pos, xend = x_end, yend = y_pos, 
             color = "black", linewidth = 1.5),
    annotate("text",x = (x_start + x_end) / 2, y = y_pos, 
              label = paste(scale_miles, "miles"), 
              vjust = -0.5, hjust = 0.5, size = 3, color = "black")
  )
}

# Plot the sites on the map
gg_map <- ggmap(map_tiles) +
  geom_point(data = sites_metadata_crop, 
             aes(x = longitude, y = latitude, color = site_name), 
             size = 3) +
  geom_text(data = sites_metadata_crop, 
            aes(x = longitude, y = latitude, label = paste0(site_name,"\n",gage_height_ft, " ft. at ", datetime," AM")), 
            vjust = -.2, hjust = 0.5, size = 3.5) +
  # add camp mystic location
  geom_point(aes(x = camp_mystic_latlon["longitude"], 
                 y = camp_mystic_latlon["latitude"]), 
             color = "red", size = 4, shape = 17) +
  geom_text(aes(x = camp_mystic_latlon["longitude"], 
                y = camp_mystic_latlon["latitude"], 
                label = "Camp Mystic"),
            vjust = -1, hjust = 0.5, size = 3.5, color = "red") +
  
  
  create_scale_bar(map_tiles, scale_miles = 5) +
  labs(title = "Guadalupe River Sites",
       subtitle = "Locations of Stream Gages and Camp Mystic",
       caption = "Source: USGS,© Stadia Maps © OpenMapTiles © OpenStreetMap",
       x = "Longitude", 
       y = "Latitude") +
  theme_minimal() +
  theme(legend.position = "none")

gg_map

geom_function()
# --------------------------------------------------------------------------------------
# combine gg_dist, gg_ridge and gg_map into a 4x4 panel grid with panel one containing text
text_for_panel <- paste0("Camp Mystic Area USGS Stream Gage Data\nJuly 4, 2025\n",
                         "The Camp is upstream from all USGS sites.\n",
                         "The first downstream site is 6 miles below at Hunt.\n",
                         "At nightfall July 3 the river height at Hunt\n",
                         "was 7 feet and the flow was 8 feet per second.\n",
                         "Hunt stopped reporting around 5 AM on July 4 when the\n",
                         "river height was 38 feet and the flow was 120,000\n",
                         "cubic feet per second. It took 2 hours go from 8 feet to 38 feet at Hunt.\n",
                         "Judging from the other sites, the wave would have hit\n",
                         "the camp around 4 AM. The flood wave moved quickly downstream.\n",
                         "The dam at Sattler was not breached.")
                         

grid_plot <- grid.arrange(
  textGrob(text_for_panel, gp = gpar(fontsize = 14)),
  gg_dist,
  gg_ridge,
  gg_map,
  ncol = 2, nrow = 2
)


library(httr)
library(jsonlite)
library(tidyverse)
library(geosphere)
# Load required libraries
library(leaflet)
library(tidyverse)
library(sf)
library(elevatr)
library(ggridges)
library(gridExtra)
library(grid)
library(ggmap)


# Extract site numbers and names from the text
site_text <- c(
  "USGS 08165300 N Fk Guadalupe Rv nr Hunt, TX",
  "USGS 08165500 Guadalupe Rv at Hunt, TX",
  "USGS 08166140 Guadalupe Rv abv Bear Ck at Kerrville, TX",
  "USGS 08166200 Guadalupe Rv at Kerrville, TX",
  "USGS 08166250 Guadalupe Rv nr Center Point, TX",
  "USGS 08167000 Guadalupe Rv at Comfort, TX",
  "USGS 08167200 Guadalupe Rv at FM 474 nr Bergheim, TX",
  "USGS 08167500 Guadalupe Rv nr Spring Branch, TX",
  "USGS 08167800 Guadalupe Rv at Sattler, TX",
  "USGS 08167900 Guadalupe Rv at Third Crossing nr Sattler, TX",
  "USGS 08168500 Guadalupe Rv abv Comal Rv at New Braunfels, TX",
  "USGS 08169500 Guadalupe Rv at New Braunfels, TX",
  "USGS 08169740 Guadalupe Rv at Hwy 123-BR at Seguin, TX",
  "USGS 08169792 Guadalupe Rv at FM 1117 nr Seguin, TX",
  "USGS 08169845 Guadalupe Rv at CR 143 nr Gonzales, TX",
  "USGS 08173900 Guadalupe Rv at Gonzales, TX",
  "USGS 08174700 Guadalupe Rv at Hwy 183 nr Hochheim, TX",
  "USGS 08175800 Guadalupe Rv at Cuero, TX",
  "USGS 08176500 Guadalupe Rv at Victoria, TX",
  "USGS 08177520 Guadalupe Rv nr Bloomington, TX",
  "USGS 08188800 Guadalupe Rv nr Tivoli, TX",
  "USGS 08188810 Guadalupe Rv at SH 35 nr Tivoli, TX"
)

# Extract site numbers and names
site_numbers <- sub("^USGS ([0-9]+) .*", "\\1", site_text)
site_names <- sub("^USGS [0-9]+ (.*)$", "\\1", site_text)

# Create initial tibble
sites_df <- tibble(
  site_number = site_numbers,
  site_name = site_names,
  latitude = NA_real_,
  longitude = NA_real_
)

print(sites_df)

# Function to get site information from USGS API
get_usgs_site_info <- function(site_number) {

  base_url <- "https://waterservices.usgs.gov/nwis/site/"
  url <- paste0(base_url, "?format=rdb&sites=", site_number, "&siteOutput=expanded")
   response <- GET(url)
    if (status_code(response) == 200) {
      # Parse the response into a data frame
       content_text <- content(response, "text", encoding = "UTF-8")
      
       # Split into lines and find data lines (skip comments starting with #)
       lines <- strsplit(content_text, "\n")[[1]]
      data_lines <- lines[!grepl("^#", lines)]
      
      if (length(data_lines) >= 3) {
        # Split by tabs
        col_names <- strsplit(data_lines[1], "\t")[[1]]
        fields <- strsplit(data_lines[3],"\t")[[1]]
        # pad fields with empty string if not enough fields to match col_names
        if (length(fields) < length(col_names)) {
          fields <- c(fields, rep("", length(col_names) - length(fields)))
        }
        # enframe fields with col_names as names
        fields <- setNames(fields, col_names)
        site_metadata <- enframe(fields) |>
          filter(name %in% c("station_nm", "site_no", "map_nm", "dec_lat_va", "dec_long_va", "alt_va")) %>%
          pivot_wider(names_from = name, values_from = value) |> 
          # simplify names
          rename(site_name = station_nm, site_number = site_no, 
            latitude = dec_lat_va, longitude = dec_long_va, 
            altitude = alt_va,map_name=map_nm) |> 
          # convert latitude and longitude and altitude to numeric
          mutate(
            latitude = as.numeric(latitude),
            longitude = as.numeric(longitude),
            altitude = as.numeric(altitude)
          )
      }
    return(site_metadata)
      } else {
        warning(paste("No data found for site number:", site_number))
        return(tibble(site_name = NA, site_number = site_number, latitude = NA, longitude = NA, map_name = NA, 
          altitude = NA))
      }
}

sites_metadata <- site_numbers |> 
  map(get_usgs_site_info) |> 
  bind_rows()

# add column of distance from first site in miles
sites_metadata <- sites_metadata |> 
  # interpolate missing altitude values based on the previous and next values
  mutate(altitude = zoo::na.approx(altitude, na.rm = FALSE)) |>
  mutate(
    distance_from_first_site_miles = round(
      geosphere::distHaversine(cbind(longitude, latitude), 
        cbind(first(longitude), first(latitude))) / 1609.34, 2
    )
  )
# load the site data into a CSV file in the data folder
write_csv(sites_metadata, "data/sites_metadata.csv")

# -------------------------------------------------------------------------------------------------------------------------------------
# read the site data from the CSV file
sites_metadata <- read_csv("data/sites_metadata.csv")

#' Download USGS Stream Gauge Data
#' 
#' Downloads highest available frequency time series data for USGS stream gauges
#' including stream height (gage height) and discharge rate
#' 
#' @param station_ids Character vector of USGS station IDs (e.g., "08167000")
#' @param start_date Start date in format "YYYY-MM-DD" 
#' @param end_date End date in format "YYYY-MM-DD"
#' @return Data frame with columns: site_number, datetime, discharge_cfs, gage_height_ft, plus quality flags
#' 
download_usgs_stream_data <- function(station_ids, start_date, end_date) {
  
  # Load required libraries
  if (!require(httr)) stop("httr package required")
  if (!require(readr)) stop("readr package required") 
  if (!require(dplyr)) stop("dplyr package required")
  if (!require(lubridate)) stop("lubridate package required")
  
  # Validate inputs
  if (length(station_ids) == 0) stop("At least one station ID required")
  
  # Convert dates to proper format
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  if (start_date > end_date) stop("Start date must be before end date")
  
  # USGS parameter codes
  # 00060 = Discharge, cubic feet per second
  # 00065 = Gage height, feet
  parameter_codes <- "00060,00065"
  
  # Create the API URL
  base_url <- "https://waterservices.usgs.gov/nwis/iv/"
  
  # Combine station IDs into comma-separated string
  sites_param <- paste(station_ids, collapse = ",")
  
  # Build query parameters
  query_params <- list(
    format = "rdb",
    sites = sites_param,
    parameterCd = parameter_codes,
    startDT = format(start_date, "%Y-%m-%d"),
    endDT = format(end_date, "%Y-%m-%d"),
    siteStatus = "all"
  )
  
  cat("Downloading data for", length(station_ids), "stations from", 
      format(start_date), "to", format(end_date), "...\n")
  
  # Make the API request
  response <- httr::GET(base_url, query = query_params)
  
  # Check if request was successful
  if (httr::status_code(response) != 200) {
    stop("API request failed with status code: ", httr::status_code(response))
  }
  
  # Get the content as text
  content_text <- httr::content(response, "text", encoding = "UTF-8")
  
  # Split into lines
  lines <- strsplit(content_text, "\n")[[1]]
  
  # Remove comment lines (lines starting with #)
  data_lines <- lines[!grepl("^#", lines)]
  
  # Remove empty lines
  data_lines <- data_lines[data_lines != ""]
  
  if (length(data_lines) < 2) {
    warning("No data returned for the specified parameters")
    return(data.frame())
  }
  
  # The first line contains column headers, second line contains data types
  # Skip the data types line and read from the third line onwards
  header_line <- data_lines[1]
  data_content <- data_lines[3:length(data_lines)]
  
  # Parse the header to get column names
  headers <- strsplit(header_line, "\t")[[1]]
  
  # Parse the data
  if (length(data_content) == 0) {
    warning("No data rows found")
    return(data.frame())
  }
  
  # Split each data line by tabs
  data_split <- strsplit(data_content, "\t")
  
  # Convert to matrix then data frame
  max_cols <- max(sapply(data_split, length))
  data_matrix <- matrix(NA, nrow = length(data_split), ncol = max_cols)
  
  for (i in seq_along(data_split)) {
    row_data <- data_split[[i]]
    data_matrix[i, 1:length(row_data)] <- row_data
  }
  
  # Create data frame with proper column names
  df <- as.data.frame(data_matrix, stringsAsFactors = FALSE)
  
  # Set column names (truncate if we have more columns than headers)
  if (ncol(df) <= length(headers)) {
    names(df) <- headers[1:ncol(df)]
  } else {
    names(df) <- c(headers, paste0("col_", (length(headers)+1):ncol(df)))
  }
  
  # Clean and standardize the data frame
  # Find the essential columns we need
  site_col <- which(grepl("site_no", names(df), ignore.case = TRUE))[1]
  datetime_col <- which(grepl("datetime", names(df), ignore.case = TRUE))[1]
  
  # Find discharge and gage height columns (they often have parameter codes in names)
  discharge_cols <- which(grepl("00060|discharge", names(df), ignore.case = TRUE))
  gage_height_cols <- which(grepl("00065|gage.*height", names(df), ignore.case = TRUE))
  
  if (is.na(site_col) || is.na(datetime_col)) {
    stop("Could not identify required columns in the data")
  }
  
  # Select and rename columns
  result_df <- df %>%
    select(
      site_number = all_of(site_col),
      datetime = all_of(datetime_col),
      everything()
    ) %>%
    # Convert datetime
    mutate(
      datetime = ymd_hm(datetime, tz = "UTC")
    )
  
  # Handle discharge columns
  if (length(discharge_cols) > 0) {
    discharge_col <- discharge_cols[1]  # Take first discharge column
    discharge_flag_col <- discharge_cols[2] # Take second as flag if available
    
    result_df <- result_df %>%
      mutate(
        discharge_cfs = as.numeric(.data[[names(df)[discharge_col]]])
      )
    
    if (length(discharge_cols) > 1) {
      result_df <- result_df %>%
        mutate(
          discharge_flag = .data[[names(df)[discharge_flag_col]]]
        )
    }
  } else {
    result_df$discharge_cfs <- NA_real_
    result_df$discharge_flag <- NA_character_
  }
  
  # Handle gage height columns  
  if (length(gage_height_cols) > 0) {
    gage_col <- gage_height_cols[1]  # Take first gage height column
    gage_flag_col <- gage_height_cols[2] # Take second as flag if available
    
    result_df <- result_df %>%
      mutate(
        gage_height_ft = as.numeric(.data[[names(df)[gage_col]]])
      )
    
    if (length(gage_height_cols) > 1) {
      result_df <- result_df %>%
        mutate(
          gage_height_flag = .data[[names(df)[gage_flag_col]]]
        )
    }
  } else {
    result_df$gage_height_ft <- NA_real_
    result_df$gage_height_flag <- NA_character_
  }
  
  # Select final columns and clean up
  final_df <- result_df %>%
    select(
      site_number,
      datetime,
      discharge_cfs,
      discharge_flag,
      gage_height_ft,
      gage_height_flag
    ) %>%
    # Remove rows where both discharge and gage height are missing
    filter(!(is.na(discharge_cfs) & is.na(gage_height_ft))) %>%
    # Arrange by site and datetime
    arrange(site_number, datetime)
  
  cat("Downloaded", nrow(final_df), "records for", 
      length(unique(final_df$site_number)), "stations\n")
  
  return(final_df)
}

stream_data <- download_usgs_stream_data(
  station_ids = sites_metadata$site_number,
  start_date = "2025-07-03", 
  end_date = "2025-07-07"
)


# save stream data as an R data file in the data folder
save(stream_data, file = "data/guadalupe_stream_data.RData")

# display stream data

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
  filter(datetime >= as.POSIXct("2025-07-03 21:00:00",tz = "US/Central") & datetime <= as.POSIXct("2025-07-06",tz ="US/Central")) |>
  arrange(desc(distance_from_first_site_miles))


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


max_levels <- stream_data_full |>
  filter(distance_from_first_site_miles < 75) |>
  group_by(site_name) |>
  summarize(max_gage_height_ft = max(gage_height_ft, na.rm = TRUE),
            max_discharge_cfs = max(discharge_cfs, na.rm = TRUE)) |> 
  left_join(stream_data_full,by = join_by(site_name,max_gage_height_ft == gage_height_ft)) |> 
  group_by(site_name) |>
  filter(datetime == min(datetime, na.rm = TRUE))


max_levels |> 
  ggplot(aes(x = site_name, y = max_gage_height_ft)) +
  geom_col(fill = "steelblue") +
  labs(title = "Maximum Gage Height for Guadalupe River Sites",
       x = "Site Name", 
       y = "Maximum Gage Height (ft)") +
  # annotate with datetime of maximum gage height
  geom_text(aes(label = datetime),nudge_y = 15) +
  coord_flip()

gg_ridge <-stream_data_full |>
  # take just july 3-4
  filter(datetime >= as.POSIXct("2025-07-03", tz = "US/Central") & 
           datetime < as.POSIXct("2025-07-05", tz = "US/Central")) |>
  ggplot(aes(x = datetime, y = site_name, 
             fill = site_name,
             height = gage_height_ft)) +
  geom_ridgeline(scale = .1, alpha = 0.8) +
  labs(title = "Gage Height Distribution Over Time by Site",
       subtitle = "Guadalupe River Sites, July 3-4, 2025",
       x = "Time on July 3-4, 2025", 
       y = "Site Name") +
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(legend.position = "none") +
  # show a  15 minute time scale on the x-axis
  scale_x_datetime(date_breaks = "60 min", date_labels = "%H:%M") +
  # rotate x-axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  # add a gradient nightime background to the gg_ridge plot
  annotation_custom(
    grob = rectGrob(
      gp = gpar(fill = linearGradient(colours = c("black", "lightblue"), 
                                      x1 = 0, y1 = 0, x2 = .6, y2 = 0), 
                col = NA)
    ),
    xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
  ) +
  geom_vline(xintercept = as.POSIXct("2025-07-04 06:40:00", tz = "US/Central"), 
             linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.POSIXct("2025-07-04 00:00:00", tz = "US/Central"), 
             linetype = "dashed", color = "white") +
  annotate("text", 
           x = as.POSIXct("2025-07-04 07:30:00", tz = "US/Central"), 
           y = max(stream_data_full$gage_height_ft, na.rm = TRUE)/10 + 5, 
           label = "Dawn",
           vjust = -0.5, hjust = 0.5, size = 4, color = "black") +
  annotate("text", 
           x = as.POSIXct("2025-07-04 01:30:00", tz = "US/Central"), 
           y = max(stream_data_full$gage_height_ft, na.rm = TRUE)/10 + 6, 
           label = "Midnight",
           vjust = -0.5, hjust = 0.5, size = 4, color = "white") +
  geom_ridgeline(aes(x = datetime, y = site_name, 
                     fill = site_name,
                     height = gage_height_ft), 
                 scale = .1, alpha = 0.8)
gg_ridge

gg_ridge_discharge <-stream_data_full |>
  # take just july 4
  filter(datetime >= as.POSIXct("2025-07-03", tz = "US/Central") & 
           datetime < as.POSIXct("2025-07-05", tz = "US/Central")) |>
  ggplot(aes(x = datetime, y = site_name, 
             fill = site_name,
             height = discharge_cfs)) +
  geom_ridgeline(scale = .00005, alpha = 0.8) +
  labs(title = "Discharge Rate (CFS) Distribution Over Time by Site",
       subtitle = "Guadalupe River Sites, July 3-4, 2025",
       x = "Time on July 3-4, 2025", 
       y = "Site Name") +
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(legend.position = "none") +
  # show a  15 minute time scale on the x-axis
  scale_x_datetime(date_breaks = "60 min", date_labels = "%H:%M") +
  # rotate x-axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# create a static map of the sites using ggplot2 and ggmap


sites_metadata_crop <- sites_metadata |>
  filter(distance_from_first_site_miles < 20)
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
            aes(x = longitude, y = latitude, label = site_name), 
            vjust = -1, hjust = 0.5, size = 3.5) +
  create_scale_bar(map_tiles, scale_miles = 5) +
  labs(title = "Guadalupe River Sites",
       subtitle = "Locations of Stream Gages and Camp Mystic",
       caption = "Source: USGS,© Stadia Maps © OpenMapTiles © OpenStreetMap",
       x = "Longitude", 
       y = "Latitude") +
  theme_minimal() +
  theme(legend.position = "none")

gg_map

# --------------------------------------------------------------------------------------
# combine gg_dist, gg_ridge and gg_map into a 4x4 panel grid with panel one containing text
text_for_panel <- paste0("Camp Mystic Area USGS Stream Gage Data\nJuly 3-4, 2025\n",
                         "The Camp is upstream from all USGS sites.\n",
                         "The first downstream site is 6 miles below at Hunt.\n",
                         "On July 2 the river height at Hunt height was 7 feet and the flow was\n",
                         "8 feet per second. Hunt stopped reporting around Midnight.\n",
                         "on July 3-4 when the river height was 38 feet\n",
                         "and the flow was 120 thousand cubic feet per second.\n",
                         "It took 2 hours go from 7 feet to 38 feet at Hunt.\n",
                         "Judging from this data, the wave would have the camp\n",
                         "around 11pm on the 3rd. The flood wave moved quickly downstream.")


grid_plot <- grid.arrange(
  textGrob(text_for_panel, gp = gpar(fontsize = 14)),
  gg_dist,
  gg_ridge,
  gg_map,
  ncol = 2, nrow = 2
)


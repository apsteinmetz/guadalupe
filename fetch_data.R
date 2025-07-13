library(httr)
library(jsonlite)
library(tidyverse)
library(geosphere)


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

print("Initial tibble created:")
print(sites_df)

# Function to get site information from USGS API
get_usgs_site_info <- function(site_number) {
# site_number <- site_numbers[10]

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
  
  
# Test with first site
test_result <- get_usgs_site_info(site_numbers[10])
print(test_result)

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


# Display the results
head(stream_data, 20)
# save streatm data as an R data file in the data folder
save(stream_data, file = "data/guadalupe_stream_data.RData")

  
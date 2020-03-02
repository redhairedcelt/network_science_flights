library(data.table)
library(dplyr)
library(magrittr)
library(igraph)
library(networkD3)

# Set working directory path (CHANGE WHEN WORKING LOCALLY)
# path <- "C:/Users/alexj/OneDrive/Desktop/GW/Spring '20/DATS 6450 (Network)/Project/"
path <- ""

tryCatch(setwd(path), error=function(x) stop("STOP: Update path variable to final directory"))

# initialize data frame
df <- data.table()

# loop through yearly files and append to overall dataframe
for (d in c("Air_Data_2018.csv", 'Air_Data_1998.csv')){
  year <- as.numeric(gsub('[^[:digit:]]', '', d))
  data <- fread(d)
  data$V21 <- NULL
  data$Year <- year
  df <- rbind(df, data)
  rm(data)
}

# define our grouping function
group_flights <- function(data, airline, cols, year){
  
  a <- data[SEATS > 0 & UNIQUE_CARRIER %in% airline & Year == year, 
            .(flights = sum(DEPARTURES_PERFORMED), passengers = sum(PASSENGERS)),
            cols]
  a %<>% arrange(desc(flights))
  
  a %<>% filter(ORIGIN != DEST)
  
  return(setDT(a))
  
}

# read the lat/lon for each airport
origin <- fread('node_locations.csv')
names(origin) <- c("ORIGIN", 'origin_lat', 'origin_lon')

dest <- fread('node_locations.csv')
names(dest) <- c("DEST", 'dest_lat', 'dest_lon')

# define the grouping columns
cols <- c("ORIGIN", "ORIGIN_CITY_NAME", "DEST", "DEST_CITY_NAME", "UNIQUE_CARRIER")

# loop through our two years
for(yr in c(1998, 2018)){
  
  # define variable name
  var_df <- paste0('all_flights_', yr)
  
  # create grouped data frame
  assign(var_df, group_flights(df, unique(df$UNIQUE_CARRIER), cols, yr))
  
  # join grouped data frame with lat/long for dest/origin
  assign(var_df, get(var_df) %>% left_join(origin) %>% left_join(dest) %>% setDT())

  # loop through carriers
  for(al in c("AA", "DL", "UA", "WN")){
    
    # create variable name
    var_df <- paste0(al, '_', yr)
    
    # create grouped data frame
    assign(var_df, group_flights(df, al, cols, yr))
    
    # join grouped data frame with lat/lon for dest/origin
    assign(var_df, get(var_df) %>% left_join(origin) %>% left_join(dest) %>% setDT())
      }
}

rm(origin, dest)

# write all files to working directory
for (file in ls(pattern = "*8")){
  fwrite(get(file), paste0(file, '.csv'))
}


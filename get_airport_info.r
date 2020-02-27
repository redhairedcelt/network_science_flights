# This function will install any packages you dont have installed already.
loadPkg = function(x) { if 
  (!require(x,character.only=T, quietly =T)) 
{ install.packages(x,dep=T,repos="http://cran.us.r-project.org"); 
    if(!require(x,character.only=T)) stop("Package not found") } }

loadPkg('dplyr')
library(dplyr)
loadPkg('readr')
library(readr)


loadPkg('ggmap')
library(ggmap)
loadPkg('ggplot2')
library(ggplot2)


df <- read_csv('data/Air_Data_2018.csv')
df$index <- rownames(df)

## Using ggmap's geocoding service, which I can enable with my Google API
# Hiding my key since this repo is public.  Writing to my renv.

#register_google(key = , write = TRUE)

# Now get all the nodes as a set, make a new df, and add lon and lat columns
origins <- select(df, ORIGIN, index)
colnames(origins) <- c('iata_code','index')

dests <- (select(df, DEST, index))
colnames(dests) <- c('iata_code','index')

nodes = unique(c(origins$iata_code, dests$iata_code))
node_df <- as.data.frame(nodes)
node_df$nodes <- as.character(node_df$nodes)
node_df$lon <- NA
node_df$lat <- NA

# This block of code will actually pass the nodes to Google's API for reverse geolocation
# Dont run it too often, only so many API calls
for(i in 1:nrow(node_df[1:3,]))
{
  print(node_df$nodes[i])
 # need to add '+Airport' to resolve ambiguities
  result <- geocode(paste((node_df$nodes[i]),'+Airport'), output = 'all')
  #node_df$lon[i] <- as.numeric(result$lon)
  #node_df$lat[i] <- as.numeric(result$lat)
  node_df$state[i] <- result[1][1]$results[[1]]$address_components[[6]]$short_name
  node_df$county[i] <- result[1][1]$results[[1]]$address_components[[7]]$short_name
}

node_df$iata_code <- node_df$nodes
node_df <- select(node_df, c(iata_code, lat, lon))
#write_csv(node_df, 'data/node_locations.csv')
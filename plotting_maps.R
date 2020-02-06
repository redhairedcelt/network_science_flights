# This function will install any packages you dont have installed already.
loadPkg = function(x) { if 
  (!require(x,character.only=T, quietly =T)) 
{ install.packages(x,dep=T,repos="http://cran.us.r-project.org"); 
    if(!require(x,character.only=T)) stop("Package not found") } }

loadPkg('dplyr')
library(dplyr)
loadPkg('readr')
library(readr)

loadPkg("igraph") 
library(igraph) # Load the igraph package
loadPkg("networkD3") 
library(networkD3)
loadPkg("threejs") 
library(threejs)

loadPkg('maps')
library(maps)
loadPkg('geosphere')
library(geosphere)
loadPkg('ggmap')
library(ggmap)

df <- read_csv('data/Air_Data_2018.csv')
df$index <- rownames(df)

## Using ggmap's geocoding service, which I can enable with my Google API
# Hiding my key since this repo is public.  Writing to my renv.

#register_google(key = , write = TRUE)

# Now get all the nodes as a set, make a new df, and add lon and lat columns
origins <- (select(df, ORIGIN, index))
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
#for(i in 1:nrow(node_df))
#{
#  print(node_df$nodes[i])
  # need to add '+Airport' to resolve ambiguities
#  result <- geocode(paste((node_df$nodes[i]),'+Airport'))
#  node_df$lon[i] <- as.numeric(result$lon)
#  node_df$lat[i] <- as.numeric(result$lat)
#}

#node_df$iata_code <- node_df$nodes
#node_df <- select(node_df, c(iata_code, lat, lon))
#write_csv(node_df, 'data/node_locations.csv')

node_df <- read_csv('data/node_locations.csv')

## Merge locs to the edges
origins_loc <- merge(origins, node_df, by = 'iata_code', full.x = T)
colnames(origins_loc) <- c('origin_iata', 'index', 'origin_lat', 'origin_lon')

dests_loc <- merge(dests, node_df, by = 'iata_code', full.x = T)
colnames(dests_loc) <- c('dest_iata', 'index', 'dest_lat', 'dest_lon')

edge_locs <- merge(origins_loc, dests_loc, by = 'index')
edge_locs_no_na <- na.omit(edge_locs)

edge_locs_clean <- edge_locs_no_na %>% 
  group_by(origin_iata, origin_lon, origin_lat, 
           dest_iata, dest_lon, dest_lat) %>% 
  summarise(total = n())

origin_counts <- edge_locs_no_na %>% 
  group_by(origin_iata) %>% 
  summarise(total = n())

# Plot on a map.  Once I get the geo coords in, we can start messing around with visuals. 
# I'd like to do a time lapse to show the flights over days/months with different colors
# for different attributes, such as airline or how early/late each flight is.
df_map <- edge_locs_clean[order(-edge_locs_clean$total),][1:100,]

map("state", col="grey20", fill=TRUE, bg="black", lwd=0.1)

points(x=df_map$origin_lon, y=df_map$origin_lat, pch=19, col="green")
points(x=df_map$dest_lon, y=df_map$dest_lat, pch=19, col="red")

col.1 <- adjustcolor("orange red", alpha=0.2)
col.2 <- adjustcolor("orange", alpha=0.2)
edge.pal <- colorRampPalette(c(col.1, col.2), alpha = TRUE)
edge.col <- edge.pal(100)

for(i in 1:nrow(df_map))  {
  
  arc <- gcIntermediate( c(df_map$origin_lon[i], df_map$origin_lat[i]), 
                         c(df_map$dest_lon[i], df_map$dest_lat[i]), 
                         n=1000, addStartEnd=TRUE )
  edge.ind <- round(100*df_map$total / max(df_map$total))
  
  lines(arc, col=edge.col[edge.ind], lwd=edge.ind/30)
}



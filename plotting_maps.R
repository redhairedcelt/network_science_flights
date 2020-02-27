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
loadPkg('ggplot2')
library(ggplot2)

node_df <- read_csv('data/node_locations.csv')
# the number of nas in the node_df:
print(nrow(node_df)-nrow(na.omit(node_df)))

df <- read_csv('data/Air_Data_2018.csv')
# Now get all the nodes as a set, make a new df, and add lon and lat columns
df$index <- rownames(df)

origins <- (select(df, ORIGIN, index))
colnames(origins) <- c('iata_code','index')
dests <- (select(df, DEST, index))
colnames(dests) <- c('iata_code','index')

# Merge locs to origins and destinations
origins_loc <- merge(origins, node_df, by = 'iata_code', full.x = T)
colnames(origins_loc) <- c('origin_iata', 'index', 'origin_lat', 'origin_lon')
dests_loc <- merge(dests, node_df, by = 'iata_code', full.x = T)
colnames(dests_loc) <- c('dest_iata', 'index', 'dest_lat', 'dest_lon')
# merge the origins and destinations together
edge_locs <- merge(origins_loc, dests_loc, by = 'index')
# merge the edge_locs with geocoords back to the original df
df_locs <- merge(edge_locs, df, by='index')
# drop any nas
print(nrow(edge_locs))
print(nrow(na.omit(edge_locs)))
nas <- filter(edge_locs, is.na(origin_lat) | is.na(dest_lat))
length_nas <- nrow(nas)
print(length_nas)

missing_airports_origin <- (select(nas, origin_iata)) 
missing_airports_dest <- (select(nas, origin_iata))
missing_airports <- unique(rbind(missing_airports_origin, missing_airports_dest))

edge_locs[is.na(edge_locs)]

subset(df, is.na()) 

edge_locs_no_na <- na.omit(edge_locs)

edge_locs_clean <- edge_locs_no_na %>% 
  group_by(origin_iata, origin_lon, origin_lat, 
           dest_iata, dest_lon, dest_lat) %>% 
  summarise(total = n())



# Plot on a map.  Once I get the geo coords in, we can start messing around with visuals. 
# I'd like to do a time lapse to show the flights over days/months with different colors
# for different attributes, such as airline or how early/late each flight is.

df_full <- edge_locs_clean[order(-edge_locs_clean$total),]

df_map <- edge_locs_clean[order(-edge_locs_clean$total),]




map("state", col="grey20", fill=TRUE, bg="black", lwd=0.1)
points(x=df_map$origin_lon, y=df_map$origin_lat, pch=19, cex=.2, col='blue')
points(x=df_map$dest_lon, y=df_map$dest_lat, pch=19, cex=.2, col='blue')

col.1 <- adjustcolor('grey', alpha=0.2)
col.2 <- adjustcolor('blue', alpha=0.2)
edge.pal <- colorRampPalette(c(col.1, col.2), alpha = TRUE)
edge.col <- edge.pal(100)

for(i in 1:nrow(df_map))  {
  
  arc <- gcIntermediate( c(df_map$origin_lon[i], df_map$origin_lat[i]), 
                         c(df_map$dest_lon[i], df_map$dest_lat[i]), 
                         n=1000, addStartEnd = F )
  edge.ind <- round(100*df_map$total / max(df_map$total))
  
  lines(arc, col=edge.col[edge.ind], lwd=edge.ind/500)
}


plot_map = function(network, map_color){

# must drop nas
  edge_locs_no_na <- na.omit(network)
# group by the origins and dests, summarize total as atotal  
  edge_locs_clean <- edge_locs_no_na %>% 
    group_by(origin_iata, origin_lon, origin_lat, 
             dest_iata, dest_lon, dest_lat) %>% 
    summarise(total = n())  

  df_map <- edge_locs_clean[order(-edge_locs_clean$total),]

  map("state", col="grey20", fill=TRUE, bg="black", lwd=0.1)
  points(x=df_map$origin_lon, y=df_map$origin_lat, pch=19, cex=.2, col=map_color)
  points(x=df_map$dest_lon, y=df_map$dest_lat, pch=19, cex=.2, col=map_color)
  
  col.1 <- adjustcolor('white', alpha=0.2)
  col.2 <- adjustcolor(map_color, alpha=0.2)
  edge.pal <- colorRampPalette(c(col.1, col.2), alpha = TRUE)
  edge.col <- edge.pal(100)
  
  for(i in 1:nrow(df_map))  {
    
    arc <- gcIntermediate( c(df_map$origin_lon[i], df_map$origin_lat[i]), 
                           c(df_map$dest_lon[i], df_map$dest_lat[i]), 
                           n=1000, addStartEnd = F )
    edge.ind <- round(100*df_map$total / max(df_map$total))
    
    lines(arc, col=edge.col[edge.ind], lwd=edge.ind/500)
  }}


#airlines <- unique(df_full$UNIQUE_CARRIER_NAME)

delta <- subset(df_locs, UNIQUE_CARRIER_NAME == 'Delta Air Lines Inc.')
united <- subset(df_locs, UNIQUE_CARRIER_NAME == 'Delta Air Lines Inc.')
southwest <- subset(df_locs, UNIQUE_CARRIER_NAME == 'Delta Air Lines Inc.')

plot_map(df_map, 'blue')
plot_map(united, 'red')

delta <- subset(df_full, UNIQUE_CARRIER_NAME == 'Delta Air Lines Inc.')
united <- subset(df_full, UNIQUE_CARRIER_NAME == 'Delta Air Lines Inc.')
southwest <- subset(df_full, UNIQUE_CARRIER_NAME == 'Delta Air Lines Inc.')

#airlines <- unique(df_full$UNIQUE_CARRIER_NAME)

#df <- subset(df_full, UNIQUE_CARRIER_NAME == 'Delta Air Lines Inc.')


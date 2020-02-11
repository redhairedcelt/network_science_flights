library(data.table)
library(dplyr)
library(magrittr)
library(igraph)
library(networkD3)
library(maps)
library(threejs)
library(geosphere)
library(ggmap)


setwd("C:/Users/alexj/OneDrive/Desktop/GW/Spring '20/DATS 6450 (Network)/network_science_flights/")

data_path = "data/Air_Data_2018.csv" # UPDATE PATH

data <- fread(data_path, header = TRUE)
data$V21 <- NULL
data$YEAR <- 2018

node_df_dest <- fread('data/node_locations.csv')  # UPDATE PATH
names(node_df_dest) <- c("DEST", 'dest_lat', 'dest_lon')

node_df_origin <- fread('data/node_locations.csv')
names(node_df_origin) <- c("ORIGIN", 'origin_lat', 'origin_lon')

data %<>% left_join(node_df_dest,   by = 'DEST')
data %<>% left_join(node_df_origin, by = 'ORIGIN')

setDT(data)

cols <- c("ORIGIN", "ORIGIN_CITY_NAME", "DEST", "DEST_CITY_NAME", "dest_lat", "dest_lon", "origin_lat", 
          "origin_lon")

group_flights <- function(data, airline){
        
        a <- data[SEATS > 0 & UNIQUE_CARRIER == airline, 
                    .(flights = sum(DEPARTURES_PERFORMED), passengers = sum(PASSENGERS)),
                    cols]
         a %<>% arrange(desc(flights))
        
        return(setDT(a))
        
}

plot_map <- function(df){
        
        map("state", col="grey40", fill=TRUE, bg="black", lwd=0.1)
        
        points(x=df$origin_lon, y=df$origin_lat, pch=19, col="red", cex = 0.5)
        points(x=df$dest_lon,   y=df$dest_lat,   pch=19, col="red", cex = 0.5)
        
        col.1 <- adjustcolor("orange", alpha=0.2)
        col.2 <- adjustcolor("orange", alpha=0.2)
        edge.pal <- colorRampPalette(c(col.1, col.2), alpha = TRUE)
        edge.col <- edge.pal(100)
        
        for(i in 1:nrow(df))  {
                
                arc <- gcIntermediate( c(df$origin_lon[i], df$origin_lat[i]), 
                                       c(df$dest_lon[i], df$dest_lat[i]), 
                                       n=1000, addStartEnd=TRUE )
                edge.ind <- round(100*df$flights / max(df$flights))
                
                lines(arc, col=edge.col[edge.ind], lwd=edge.ind/250)
        }
        
}

UA_data <- group_flights(data, 'UA')
WN_data <- group_flights(data, 'WN')
DL_data <- group_flights(data, 'DL')
AA_data <- group_flights(data, 'AA')

all_data <- data[SEATS > 0, 
                      .(flights = sum(DEPARTURES_PERFORMED), passengers = sum(PASSENGERS)),
                      cols] %>% arrange(desc(flights))

AA_graph     <- graph_from_edgelist(AA_data %>% select(ORIGIN, DEST) %>%               as.matrix())
AA_graph_250 <- graph_from_edgelist(AA_data %>% select(ORIGIN, DEST) %>% head(250) %>% as.matrix())

DL_graph     <- graph_from_edgelist(DL_data %>% select(ORIGIN, DEST) %>%               as.matrix())
DL_graph_250 <- graph_from_edgelist(DL_data %>% select(ORIGIN, DEST) %>% head(250) %>% as.matrix())

UA_graph     <- graph_from_edgelist(UA_data %>% select(ORIGIN, DEST) %>%               as.matrix())
UA_graph_250 <- graph_from_edgelist(UA_data %>% select(ORIGIN, DEST) %>% head(250) %>% as.matrix())

WN_graph     <- graph_from_edgelist(WN_data %>% select(ORIGIN, DEST) %>%               as.matrix())
WN_graph_250 <- graph_from_edgelist(WN_data %>% select(ORIGIN, DEST) %>% head(250) %>% as.matrix())

all_graph     <- graph_from_edgelist(all_data %>% select(ORIGIN, DEST) %>%               as.matrix())
all_graph_250 <- graph_from_edgelist(all_data %>% select(ORIGIN, DEST) %>% head(250) %>% as.matrix())


plot(AA_graph_250,  main = 'Top 250 Air Routes for AA', layout = layout_with_kk)
plot(DL_graph_250,  main = 'Top 250 Air Routes for DL', layout = layout_with_kk)
plot(UA_graph_250,  main = 'Top 250 Air Routes for UA', layout = layout_with_kk)
plot(WN_graph_250,  main = 'Top 250 Air Routes for WN', layout = layout_with_kk)         
plot(all_graph_250, main = 'Top 250 Air Routes',        layout = layout_with_kk)         

# 500 pts to keep the graphs from taking too long to render
plot_map( AA_data[1:500,])
plot_map( DL_data[1:500,])
plot_map( UA_data[1:500,])
plot_map( WN_data[1:500,])
plot_map(all_data[1:500,])

hist(degree(AA_graph),  main = 'Degree of AA',          breaks = 10)
hist(degree(DL_graph),  main = 'Degree of DL',          breaks = 10)
hist(degree(UA_graph),  main = 'Degree of UA',          breaks = 10)
hist(degree(WN_graph),  main = 'Degree of WN',          breaks = 10)
hist(degree(all_graph), main = 'Degree of All Flights', breaks = 10)

# HOW YOU WOULD DO THE ABOVE IN A LOOP

# for (airline in c('WN', 'AA')){ # SOUTHWEST, DETLA, UNITED, AMERICAN
#         
#         assign(paste0(airline, "_data"), group_flights(data, airline))
#         
#         assign(paste0(airline, '_graph'), graph_from_edgelist(
#                 get(paste0(airline, '_data')) %>% select(ORIGIN, DEST) %>% as.matrix()))
#         
#         assign(paste0(airline, '_graph_250'), graph_from_edgelist(
#                 get(paste0(airline, '_data')) %>% select(ORIGIN, DEST) %>% head(250) %>% as.matrix()))
#         
# 
#         plot(get(paste0(airline, '_graph')), main = paste0('Top 250 Air Routes for ', airline))
# 
#         plot_map(get(paste0(airline, '_data[1:500,]')))
#         
#         hist(degree(get(paste0(airline, '_graph'))), main = paste0('Degree of ', airline))
# }

# #############################   SCRATCH   #############################
# 
# a <- data[SEATS > 0, .(flights = sum(DEPARTURES_PERFORMED), passengers = sum(PASSENGERS)),
#          cols]
# a %<>% arrange(desc(flights)) %>% setDT()
# a %>% head(100) %>% View()
# 
# # travel_graph <- graph_from_edgelist(a %>% select(ORIGIN, DEST) %>% as.matrix())
# g <- graph_from_edgelist(a %>% select(ORIGIN, DEST) %>% head(250) %>% as.matrix())
# 
# plot(g,
#      edge.arrow.size = 0.25,       
#      vertex.color = "light blue",  
#      vertex.frame.color = "white", 
#      vertex.size = 10,             
#      vertex.label.cex = 0.75,      
#      vertex.label.color = "black", 
#      edge.curved = 0.00)    
# 
# d3_graph <- igraph_to_networkD3(g) 
# 
# # add in value of links (required to get links and arrows to work)
# d3_graph$links$value <- 1
# 
# # create networkD3 object
# forceNetwork(d3_graph$links,     # links dataframe
#              d3_graph$nodes,     # nodes dataframes
#              Source = "source",  # edge origin
#              Target = "target",  # edge destination
#              Value = "value",    # link weight
#              NodeID = "name",    # labels
#              Group = "name",     # color formatting
#              zoom = TRUE,        # allow zoom when clicking node
#              arrows = TRUE,      # show edge directional labels
#              opacityNoHover = 1) # show names on graph

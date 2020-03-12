# load packages
library(dplyr)
library(igraph)

#setwd('/Users/carolinesklaver/Desktop/Network_DS/ProjectEDA')


# ________________________________________________________________________________________
# FUNCTION FOR METRICS
# function that can take in any df and pump out metrics as df

data = read.csv('Air_Data_2018.csv', header = TRUE)
data_98 = read.csv('Air_Data_1998.csv', header = TRUE)
southwest_2018 = data[data$UNIQUE_CARRIER_NAME == 'Southwest Airlines Co.',]
american_2018 = data[data$UNIQUE_CARRIER_NAME == 'American Airlines Inc.',] 
delta_2018 = data[data$UNIQUE_CARRIER_NAME == 'Delta Air Lines Inc.',]  
united_2018 = data[data$UNIQUE_CARRIER_NAME == 'United Air Lines Inc.',] 

get_metrics <- function(dataframe) {

  # Make df into matrix
  # Can change to city instead of city code if we want
  g <- graph_from_edgelist(dataframe %>% select(ORIGIN, DEST) %>% as.matrix())
  
  # CENTRALITY
  # An example of a local centrality measure is the degree centrality, 
  # which counts the number of links held by each node and points at individuals 
  # who can quickly connect with the wider network
  degree.cent <- centr_degree(g, mode = "all")
  Degree_Centrality = mean(degree.cent$res)
  
  # It calculates the shortest paths between all nodes, 
  # then assigns each node a score based on its sum of shortest paths 
  # and is useful for finding the individuals who are best placed to influence 
  # the entire network most quickly.
  Closeness_Centrality <- mean(closeness(g, mode="all"))
  
  #BETWEENESS
  # vertex betweness
  Betweenness = mean(betweenness(g))
  
  #DEGREE
  # gives degree of each node, we can do in and out separate if we want
  Degree = mean(degree(g, mode='all'))
  
  ##DISTANCE
  # The diameter of a graph is the length of the longest geodesic.
  Diameter <- diameter(g, directed = TRUE)
  
  metrics_df = data.frame(Degree,Degree_Centrality,Betweenness,Closeness_Centrality,Diameter)
  #get number of vertices, edges, and p(edge) for random networks
  numb_edges <- length(E(g))
  numb_vertices <-length(V(g))
  p_edges <- (numb_edges/(numb_vertices*(numb_vertices-1)))
  
  # generate randmom networks using the parameters from the given network
  # intialize random dataframe
  rand_g_metrics <- data.frame()
  for(i in 1:1000)  {
    g <- sample_gnp(n=numb_vertices, p=p_edges, directed=TRUE)
    degree.cent <- centr_degree(g, mode = "all")
    rand_Degree_Centrality = mean(degree.cent$res)
    rand_Closeness_Centrality <- mean(closeness(g, mode="all"))
    rand_Betweenness = mean(betweenness(g))
    rand_Degree <- mean(degree(g, mode='all'))
    rand_Diameter <- diameter(g, directed = TRUE)
    rand_metric = data.frame(rand_Degree,rand_Degree_Centrality,rand_Betweenness,
                             rand_Closeness_Centrality,rand_Diameter)
    rand_g_metrics <- rbind(rand_g_metrics, rand_metric)

  }
  rand_g_metrics <- colMeans(rand_g_metrics)
  rand_g_metrics_df <- data.frame(rand_g_metrics)
  metrics_df <- merge(metrics_df, t(rand_g_metrics))
  return(metrics_df)
  
}


metrics_2018 = get_metrics(data)
metrics_1998 = get_metrics(data_98)
metrics_sw_2018 = get_metrics(southwest_2018)
metrics_american_2018 = get_metrics(american_2018)
metrics_delta_2018 = get_metrics(delta_2018)
metrics_united_2018 = get_metrics(united_2018)



summary_metrics <- data.frame()
metric_dfs <- list(
                metrics_1998,
                
                metrics_american_2018,
                metrics_delta_2018,
                metrics_united_2018)

row_names <- c(
                   'metrics_1998',
                   
                   'metrics_american_2018',
                   'metrics_delta_2018',
                   'metrics_united_2018')

col_names <- colnames(metrics_2018)

# for averaged metrics output
for (df in metric_dfs) {
  summary_metrics <- rbind(summary_metrics, df)
}
row.names(summary_metrics) <- row_names
colnames(summary_metrics) <- col_names

#write.csv(summary_metrics, 'summary_metrics.csv')

# for non averaged metrics output
#for (df in metric_dfs) {
#  summary_metrics <- rbind(summary_metrics, colMeans(df))
#  }
#row.names(summary_metrics) <- dfs_names
#colnames(summary_metrics) <- col_names

library(ggplot2)
library(maps)
library(ggmap)

plot_network = function (df_map, point_color = 'blue', line_color = point_color,
                         map_title = 'Top Routes', background_fill = 'grey',
                         map_state_lines = 'black', map_fill = 'black' ) 
{
  
  df_map <- subset(df_map, ORIGIN!=DEST)
  df_map <- na.omit(df_map)
  df_map <-subset(df_map, origin_lat!=dest_lat)
  
  # scaling function.  Could use some work.
  range01 <- function(x){(x+1-min(x))/(max(x)-min(x))}
  #range01 <- function(x){(log(x)-min(log(x)))/(max(log(x))-min(log(x)))}
  # need to plot state lines
  states_map <- map_data("state")  
  
  # Make a color pallate between two colors.  Grey is default first color.
  col.1 <- adjustcolor('grey', alpha=0.4)
  col.2 <- adjustcolor(line_color, alpha=.8)
  edge.pal <- colorRampPalette(c(col.1, col.2), alpha = TRUE)
  edge.col <- edge.pal(10)
  # add a column for color indexing scaled by number of flights on each leg.
  # make sure any 0 are set to minimum of 1, because R starts at 1.  Argh.
  df_map$color_ind <- round(range01(df_map$flights)*10)
  df_map$color_ind[df_map$color_ind==0] <- 1
  
  # add and scale total number of flights
  df_map$total_flts_scaled <- range01(df_map$flights)
  
  # find counts of flights leaving by iata code, add to df_map as airport counts
  airport_counts_df <- as.data.frame(df_map %>% group_by(ORIGIN) %>% 
                                       summarise(counts = n()))
  df_map <- merge(df_map, airport_counts_df, by='ORIGIN')
  # scale and add back to df
  df_map$airpot_counts_scaled <- range01(df_map$counts)
  
  # actual plotting
  gg <- ggplot()
  gg <- gg + geom_map(data=states_map, map=states_map, aes(map_id=region),
                      color=map_state_lines, fill=map_fill, size=0.25) +
    expand_limits(x=states_map$long, y=states_map$lat)
  gg <- gg + labs(x=NULL, y=NULL, title=map_title) + 
    theme_void() + # Empty theme without axis lines and texts
    theme(panel.background = element_rect(fill=background_fill, colour=background_fill),
          plot.background = element_rect(fill=background_fill, color=background_fill))
  #coord_map("albers", lat0=39, lat1=49) +
  #coord_map("albers", lat0=39, lat1=49) +
  gg <- gg +
    # The geom points are plotted scaled 0 to 1.  The factor can be adjusted 
    geom_point(data=df_map, aes(x=origin_lon, y=origin_lat), 
               #col=point_color, 
               shape = 21, colour = "black", fill = point_color,
               size=(df_map$airpot_counts_scaled)*4) +
    geom_curve(data = df_map, 
               aes(x=origin_lon, y=origin_lat, xend=dest_lon, yend=dest_lat), 
               # color is selected by index from the predefined color pallete above
               # needs to be rounded to an int between 1 and 10
               col = edge.col[df_map$color_ind], 
               # size is scaled 0 to 1
               size = (df_map$total_flts_scaled),
               curvature = 0.3, angle = 90, ncp = 5)
  gg
}

dataframe <- united_2018
g <- graph_from_edgelist(dataframe %>% select(ORIGIN, DEST) %>% as.matrix())
#get number of vertices, edges, and p(edge) for random networks
numb_edges <- length(E(g))
numb_vertices <-length(V(g))
p_edges <- (numb_edges/(numb_vertices*(numb_vertices-1)))
g_rand <- sample_gnp(n=numb_vertices, p=p_edges, directed=TRUE)
plot(g_rand)

# translate to a df
g_rand_df <- as_long_data_frame(g_rand)
head(g_rand_df)
# read the lat/lon for each airport
origin <- fread('cont_us_nodes.csv')
names(origin) <- c("ORIGIN", 'origin_lat', 'origin_lon')

dest <- fread('cont_us_nodes.csv')
names(dest) <- c("DEST", 'dest_lat', 'dest_lon')

# define the grouping columns
cols <- c("ORIGIN", "ORIGIN_CITY_NAME", "DEST", "DEST_CITY_NAME", "UNIQUE_CARRIER")



plot_network(df_map=DL_1998, map_title="Delta in 1998", point_color='blue')





# load packages
library(dplyr)
library(igraph)

#setwd('/Users/carolinesklaver/Desktop/Network_DS/ProjectEDA')


# ________________________________________________________________________________________
# FUNCTION FOR METRICS
# function that can take in any df and pump out metrics as df

# read in pipeline data
all_flights_2018 = read.csv('all_flights_2018.csv')

# select origin, dest, flights, make flights = to weight
test = all_flights_2018 %>% select(ORIGIN, DEST, flights) %>% rename(from = ORIGIN, to = DEST, weight = flights)

test <- filter(test, weight>0)
# make into igraph object
test_graph_wgt <- graph.data.frame(test, directed = TRUE)

plot(betweenness(test_graph_wgt))
mean(betweenness(test_graph_wgt))

mean(betweenness(g))

plot(edge_betweenness(test_graph_wgt))
mean(edge_betweenness(test_graph_wgt))


# make into igraph object
test_graph_wgt <- graph.data.frame(test, directed = TRUE)
# check that it is weighted
is_weighted(test_graph_wgt)
print(V(test_graph_wgt))
plot(degree(test_graph_wgt))
print(diameter(test_graph_wgt))
E(test_graph_wgt)


g <- graph_from_edgelist(dataframe %>% select(ORIGIN, DEST) %>% as.matrix())
diameter(g)
farthest_vertices(g)

mean(betweenness(test_graph_wgt))

edge_betweenness(g)

farthest_vertices(g)
farthest_vertices(test_graph_wgt)

dist_mat <- distances(g, v=V(g), to=V(g))
write.csv(dist_mat, 'dist_mat.csv')

dist_mat <- distances(test_graph_wgt, v=V(test_graph_wgt), to=V(test_graph_wgt))
write.csv(dist_mat, 'dist_mat_weighted.csv')

data_98 = read.csv('Air_Data_1998.csv', header = TRUE)
data = read.csv('Air_Data_2018.csv', header = TRUE)
southwest_2018 = data[data$UNIQUE_CARRIER_NAME == 'Southwest Airlines Co.',]
american_2018 = data[data$UNIQUE_CARRIER_NAME == 'American Airlines Inc.',] 
delta_2018 = data[data$UNIQUE_CARRIER_NAME == 'Delta Air Lines Inc.',]  
united_2018 = data[data$UNIQUE_CARRIER_NAME == 'United Air Lines Inc.',] 

get_metrics <- function(dataframe) {

  # Make df into matrix
  # Can change to city instead of city code if we want
  g <- graph_from_edgelist(dataframe %>% select(ORIGIN, DEST) %>% as.matrix())
  
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

#troubleshoot sw
g <- graph_from_edgelist(southwest_2018 %>% select(ORIGIN, DEST) %>% as.matrix())
numb_edges <- length(E(g))
numb_vertices <-length(V(g))
p_edges <- (numb_edges/(numb_vertices*(numb_vertices-1)))

print(p_edges)
print(numb_vertices)

summary_metrics <- data.frame()
metric_dfs <- list(
                metrics_1998,
                metrics_2018,
                metrics_american_2018,
                metrics_delta_2018,
                metrics_united_2018)

row_names <- c(    'metrics_2018',
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

summary_metrics <- round(summary_metrics, digits=5)
write.csv(summary_metrics, 'summary_metrics.csv')

# for non averaged metrics output
#for (df in metric_dfs) {
#  summary_metrics <- rbind(summary_metrics, colMeans(df))
#  }
#row.names(summary_metrics) <- dfs_names
#colnames(summary_metrics) <- col_names

V(AA_1998)

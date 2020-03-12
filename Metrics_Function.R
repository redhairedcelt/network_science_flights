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
  Degree_Centrality = degree.cent$res
  
  
  # It calculates the shortest paths between all nodes, 
  # then assigns each node a score based on its sum of shortest paths 
  # and is useful for finding the individuals who are best placed to influence 
  # the entire network most quickly.
  Closeness_Centrality <- closeness(g, mode="all")
  
  
  #BETWEENESS
  # vertex betweness
  Betweenness = betweenness(g)
  
  
  #DEGREE
  # gives degree of each node, we can do in and out separate if we want
  Degree = degree(g, mode='all')
  
  
  metrics_df = data.frame(Degree,Degree_Centrality, Betweenness,Closeness_Centrality)
  
  return(metrics_df)
  
}

metrics_2018 = get_metrics(data)
metrics_1998 = get_metrics(data_98)
metrics_sw_2018 = get_metrics(southwest_2018)
metrics_american_2018 = get_metrics(american_2018)
metrics_delta_2018 = get_metrics(delta_2018)
metrics_united_2018 = get_metrics(united_2018)


head(metrics_1998[order(-metrics_1998$Closeness_Centrality),])
head(metrics_2018[order(-metrics_2018$Closeness_Centrality),])
head(metrics_sw_2018[order(-metrics_sw_2018$Closeness_Centrality),])
head(metrics_american_2018[order(-metrics_american_2018$Closeness_Centrality),])
head(metrics_delta_2018[order(-metrics_delta_2018$Closeness_Centrality),])
head(metrics_united_2018[order(-metrics_united_2018$Closeness_Centrality),])


# generate random networks and compare

#specify df you want as a graph
df <- data
g <- graph_from_edgelist(data %>% select(ORIGIN, DEST) %>% as.matrix())

#get number of vertices, edges, and p(edge)
numb_edges <- length(E(g))
numb_vertices <-length(V(g))
p_edges <- (numb_edges/(numb_vertices*(numb_vertices-1)))

#iterate through 1000 random gnp graphs and save the degree
deg_list <- c()
for(i in 1:1000)  {
  g <- sample_gnp(n=numb_vertices, p=p_edges, directed=TRUE)
  deg <- degree(g)
  deg_list <- c(deg_list, deg)
}
deg_list_gnp <- deg_list

#iterate through 1000 random gnp graphs and save the degree
deg_list <- c()
for(i in 1:1000)  {
  g <- sample_gnm(n=numb_vertices, m=numb_edges, directed=TRUE)
  deg <- degree(g)
  deg_list <- c(deg_list, deg)
}
deg_list_gmp <- deg_list

#make histograms of the two random model distributions 
#and the original network's degree distribution
hist(deg_list_gnp, main='Histogram of Gnp Model', 
     xlab='Degree', col='purple')
hist(deg_list_gmp, main='Histogram of Gnm Model',
     xlab='Degree', col='green')
hist(degree(g), main='Histogram of Original Air Traffic Network',
     xlab='Degree', col='orange')


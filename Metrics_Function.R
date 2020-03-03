# load packages
library(dplyr)
library(igraph)

setwd('/Users/carolinesklaver/Desktop/Network_DS/ProjectEDA')


# ________________________________________________________________________________________
# FUNCTION FOR METRICS
# function that can take in any df and pump out metrics as df

data = read.csv('Air_Data_2018.csv', header = TRUE)
data_98 = read.csv('Air_Data_1998.csv', header = TRUE)

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

metrics_1998 = get_metrics(data)

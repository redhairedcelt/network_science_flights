# load packages
library(dplyr)
library(igraph)



# load dataset 2018
setwd('/Users/carolinesklaver/Desktop/Network_DS/ProjectEDA')
data = read.csv('data/Air_Data_2018.csv', header = TRUE)


# Metrics
g <- graph_from_edgelist(data %>% select(ORIGIN, DEST) %>% as.matrix())

# in and out degree
od = degree(g, mode= 'out')
id = degree(g, mode='in')

# mean distance of verticies
mdis = mean_distance(g, directed = T)
mdis

# diameter of the network
dia = diameter(g, directed = T, weights = NA)
dia

# there are a few different functions to measure centrality
cd = centr_degree(g, mode= 'in', normalized=T)
cd
 
# get the average degree
mdeg = mean(degree(g))
mdeg

# vertex and edge betweenness (shortest paths)
btwn = betweenness(g)
btwn


clust = clusters(g)
clust

# we can get mean min max ect. of degree
degree = sort(degree(g))
degree

dd = degree.distribution(g)
mean(dd)

# can adjust the order of size of the neighborhood
ns = neighborhood.size(g)
mean(ns)





# making graph symmetrical ?
# g.sym = as.undirected(g, mode='collapse', 
#         edge.attr.comb=list(weight='sum', 'ignore'))




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

df <- read_csv('data/ontime_rpt_201901.csv')

str(df)
summary(df)
head(df)

# How many airports are there?


## Lets try to build a network from edges
edges <- select(df, DEST, ORIGIN)
head(edges)
flight_graph <- graph_from_data_frame(edges, directed=T)
# This takes a pretty long time to run...
#plot(flight_graph)

# Here's all the airports we have flights to or from in the data
V(flight_graph)

# Let's look at the degree of some airports
plot(degree(flight_graph))
max(degree(flight_graph))
hist(degree(flight_graph))
# Really right-tailed distribution.  Scale-free?

# Whole network kept freezing my computer, so lets just plot the first 10,000 edges
sample_edges <- graph_from_data_frame(edges[1:10000,], directed=T)
plot(sample_edges)

# Plot on a map.  Once I get the geo coords in, we can start messing around with visuals. 
# I'd like to do a time lapse to show the flights over days/months with different colors
# for different attributes, such as airline or how early/late each flight is.
map("state", col="grey20", fill=TRUE, bg="black", lwd=0.1)






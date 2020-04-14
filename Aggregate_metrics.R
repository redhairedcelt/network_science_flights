library(dplyr)
library(igraph)
library(tnet)

# set your own directory
setwd('/Users/carolinesklaver/Desktop/Network_DS/ProjectEDA')

# read in pipeline data
all_flights_2018 = read.csv('all_flights_2018.csv')
all_flights_1998 = read.csv('all_flights_1998.csv')

# cheeky way to remove islands
# but just pull from the right data
remove_islands = function(data){
  data = data[grep("TT", data$ORIGIN_CITY_NAME, invert = TRUE) ,]
  data = data[grep("HI", data$ORIGIN_CITY_NAME, invert = TRUE) ,]
  data = data[grep("AK", data$ORIGIN_CITY_NAME, invert = TRUE) ,]
  data = data[grep("TT", data$DEST_CITY_NAME, invert = TRUE) ,]
  data = data[grep("HI", data$DEST_CITY_NAME, invert = TRUE) ,]
  data = data[grep("AK", data$DEST_CITY_NAME, invert = TRUE) ,]
  return(data)
}

# run remove islands function
all_2018 = remove_islands(all_flights_2018)
all_1998 = remove_islands(all_flights_1998)

# subset airlines
all_american_2018 = all_2018[all_2018$UNIQUE_CARRIER == 'AA',]
all_american_1998 = all_1998[all_1998$UNIQUE_CARRIER == 'AA',]
all_southwest_2018 = all_2018[all_2018$UNIQUE_CARRIER == 'WN',]
all_southwest_1998 = all_1998[all_1998$UNIQUE_CARRIER == 'WN',]
all_delta_2018 = all_2018[all_2018$UNIQUE_CARRIER == 'DL',]
all_delta_1998 = all_2018[all_1998$UNIQUE_CARRIER == 'DL',]
all_united_2018 = all_2018[all_2018$UNIQUE_CARRIER == 'UA',]
all_united_1998 = all_1998[all_1998$UNIQUE_CARRIER == 'UA',]

airports <- c("LAX", "SFO", "ORD", "JFK", "ATL", "IAH", "DFW", "SAN", "SEA", "HNL", "ANC", "MSP", "DTW", "MIA", "IAD", "PHL", "EWR", "BOS", "ELP")

# all routes through covid airports 
all_covid = all_2018 %>% filter(ORIGIN %in% airports | DEST %in% airports)

# origin in covid airport
all_covid_origin <- all_2018 %>% filter(ORIGIN %in% airports)

# only the 19 airports
only_covid_19 = all_2018 %>% filter(ORIGIN %in% airports & DEST %in% airports)

# final data to use is all_2018 and all_1998

# --------------------- TEST to play around with different metrics -------------------------------------

# select origin, dest, flights, make flights = to weight
test = all_2018 %>% select(ORIGIN, DEST, flights) %>% rename(from = ORIGIN, to = DEST, weight = flights)
test98 = all_1998 %>% select(ORIGIN, DEST, flights) %>% rename(from = ORIGIN, to = DEST, weight = flights)


# remove weights less than 0
test <- filter(test, weight>0)
test98 <- filter(test98, weight>0)


# make into igraph object
test_wgt_graph <- graph.data.frame(test, directed = TRUE)
test98_wgt_graph <- graph.data.frame(test98, directed = TRUE)


# make sure it is weighted
is_weighted(test_wgt_graph)

# look at some metrics
# STRENGTH = summing up the edge weights of the adjacent edges for each vertex.
tail(sort(strength(test_wgt_graph)))

# mean degree centrality
mean(centr_degree(test_wgt_graph, mode = "all")$res)

# closeness centrality
# throws error
mean(closeness(test_wgt_graph, mode="all"))

# diameter
mean(diameter(test_wgt_graph))

# same as degree centrality
mean(degree(test_wgt_graph))

# betweenness
mean(betweenness(test_wgt_graph))

# average path length in a graph, by calculating the shortest paths between all pairs of vertices 
# (both ways for directed graphs). This function does not consider edge weights.
mean_d = mean_distance(test_wgt_graph, directed = TRUE)
mean_d

# distance table histogram
par(mfrow=c(1,2))
air_dist_hist <- distance_table(test_wgt_graph)
barplot(air_dist_hist$res, names.arg = seq_along(air_dist_hist$res), 
        main = 'Flight Distances 2018', ylim=c(0,350000), xlim=c(0,9))

air_dist_hist98 <- distance_table(test98_wgt_graph)
barplot(air_dist_hist98$res, names.arg = seq_along(air_dist_hist98$res), 
        main = 'Flight Distances 1998', ylim=c(0,350000), xlim=c(0,9))


# ----------------------------Made into FUNCTION --------------------------------------------------------

get_metrics <- function(dataframe) {
  
  # subset data
  df = dataframe %>% select(ORIGIN, DEST, flights) %>% rename(from = ORIGIN, to = DEST, weight = flights)
  
  # remove weight = 0
  df <- filter(df, weight>0)
  
  # create graph object
  g <- graph.data.frame(df, directed = TRUE)

  
  # CENTRALITY
  degree_centrality = mean(centr_degree(g, mode = "all")$res)
  
  # DIAMETER
  diameter = mean(diameter(g, directed = TRUE))
  
  # MEAN DISTANCE
  mean_distance = mean_distance(g, directed=TRUE)
  
  #BETWEENESS
  betweenness = mean(betweenness(g))

  
  metrics_df = data.frame(degree_centrality,diameter, mean_distance, betweenness)
  return(metrics_df)
}

# run the function
metrics_2018 = get_metrics(all_2018)
metrics_1998 = get_metrics(all_1998)
metrics_all_covid = get_metrics(all_covid)
metrics_only_covid_19 = get_metrics(only_covid_19)

# make empty summary table df
summary_metrics <- data.frame()

# list of dataframes
metric_dfs <- list(metrics_2018,metrics_1998,metrics_all_covid, metrics_only_covid_19 )

# get row col names
row_names <- c('metrics_2018','metrics_1998', 'metrics_all_covid', 'metrics_only_covid_19')
col_names <- colnames(metrics_2018)

# binding each year df
for (df in metric_dfs) {
  summary_metrics <- rbind(summary_metrics, df)
}
# add row and col names
row.names(summary_metrics) <- row_names
colnames(summary_metrics) <- col_names


# look at summary
summary_metrics


metrics_2018 = get_metrics(all_2018)
metrics_1998 = get_metrics(all_1998)
metrics_american_2018 = get_metrics(all_american_2018)
metrics_american_1998 = get_metrics(all_american_1998)
metrics_southwest_2018 = get_metrics(all_southwest_2018)
metrics_southwest_1998 = get_metrics(all_southwest_1998)
metrics_delta_2018 = get_metrics(all_delta_2018)
metrics_delta_1998 = get_metrics(all_delta_1998)
metrics_united_2018 = get_metrics(all_united_2018)
metrics_united_1998 = get_metrics(all_united_1998)


summary_metrics_airlines = data.frame()

# list of dataframes
metric_airline_dfs <- list(metrics_2018,
                           metrics_1998,
                           metrics_american_2018,
                           metrics_american_1998,
                           metrics_southwest_2018,
                           metrics_southwest_1998,
                           metrics_delta_2018,
                           metrics_delta_1998,
                           metrics_united_2018,
                           metrics_united_1998)

# get row col names
row_names <- c('Total network 2018','Total network 1998',
               'american_2018',
               'american_1998',
               'southwest_2018',
               'southwest_1998',
               'delta_2018',
               'delta_1998',
               'united_2018',
              'united_1998')
col_names <- colnames(metrics_2018)

# binding each year df
for (df in metric_airline_dfs) {
  summary_metrics_airlines <- rbind(summary_metrics_airlines, df)
}
# add row and col names
row.names(summary_metrics_airlines) <- row_names
colnames(summary_metrics_airlines) <- col_names



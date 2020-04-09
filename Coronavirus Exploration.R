setwd("C:/Users/alexj/OneDrive/Desktop/GW/Spring '20/")
setwd("DATS 6450 (Network)/Project/")

# https://www.dhs.gov/protecting-air-travelers-and-american-public

library(readr)
library(magrittr)
library(data.table)
library(dplyr)
library(igraph)

# load origina data and continental airports
Air_Data_2018 <- read_csv("Air_Data_2018.csv", col_types = cols(ORIGIN='c', DEST='c'))

#cont_us_ports <- read_csv("cont_us_nodes.csv") %>% select(iata_code) %>% as.matrix()
                      
# specify ports specifically accepting international passengers    
airports <- c("BOS", "ORD", "DFW", "DTW", "HNL", "ATL", "JFK", "LAX", "MIA", "EWR", 
              "SFO", "SEA", "IAD")

setDT(Air_Data_2018)

# filter to February only
feb_data <- Air_Data_2018[MONTH == 2]; rm(Air_Data_2018)

# see how many total unique routes happened in February
feb_pairs   <- feb_data %>% 
               select(ORIGIN, DEST)   %>% 
               filter(ORIGIN != DEST) %>% 
               unique()

# see how many unique routes started in one of the 13 covid-designated airport
covid_pairs <- feb_data %>% 
               select(ORIGIN, DEST) %>% 
               filter(ORIGIN != DEST & ORIGIN %in% airports) %>% 
               unique()

print(paste0(nrow(covid_pairs), " out of ", nrow(feb_pairs), " unique routes originate at covid-designated airports."))

rm(feb_pairs, covid_pairs)

# define grouping function

group_flights <- function(x, cols, airline='None'){
  
  data <- setDT(x)
  
  if (airline == 'None'){
  a <- data[SEATS > 0 & PASSENGERS > 0, 
            .(flights = sum(DEPARTURES_PERFORMED), passengers = sum(PASSENGERS)),
            cols]
  }
  else {
    a <- data[SEATS > 0 & UNIQUE_CARRIER %in% airline & PASSENGERS > 0, 
              .(flights = sum(DEPARTURES_PERFORMED), passengers = sum(PASSENGERS)),
              cols]
    }
  
  a %<>% arrange(desc(passengers))
  
  a %<>% filter(ORIGIN != DEST)
  
  return(setDT(a))
}


cols <- c("ORIGIN", "ORIGIN_CITY_NAME", "DEST", "DEST_CITY_NAME", "UNIQUE_CARRIER")

# group february data
feb_grouped <- group_flights(feb_data, cols)

# filter for routes through specific designated airports
feb_grouped_covid <- feb_grouped %>% filter(ORIGIN %in% airports | DEST %in% airports)

# filter for flight starting at the designated airports
feb_grouped_covid_origin <- feb_grouped_covid %>% filter(ORIGIN %in% airports)


####### Exploratory Data Analysis #####

# percentage of all domestic persons/flights traveling that went through one of 16 airports
n_p <-  sum(feb_grouped_covid$passengers)        / sum(feb_grouped$passengers)
n_p2 <- sum(feb_grouped_covid_origin$passengers) / sum(feb_grouped$passengers)
n_f <-  sum(feb_grouped_covid$flights)           / sum(feb_grouped$flights)
n_f2 <- sum(feb_grouped_covid_origin$flights)    / sum(feb_grouped$flights)


print(paste0(round(n_p*100, 2), "% of all passengers traveled through one of the covid-designated airports."))
print(paste0(round(n_p2*100, 2), "% of all passengers flew out of one of the covid-designated airports."))
print(paste0(round(n_f*100, 2), "% of all flights originated or ended at one of the covid-designated airports."))
print(paste0(round(n_f2*100, 2), "% of all flights originated at one of the covid-designated airports."))

# filter to places with at least daily flights (more than 28 in february)

feb_grouped       %<>% filter(flights >= 28)
feb_grouped_covid %<>% filter(flights >= 28)

n_p <- sum(feb_grouped_covid$passengers)/ sum(feb_grouped$passengers)
n_f <- sum(feb_grouped_covid$flights)   / sum(feb_grouped$flights)

print(paste0(round(n_p*100, 2), "% of all passengers on daily routes traveled through one of the covid-designated airports"))
print(paste0(round(n_f*100, 2), "% of all daily flights originated or ended at one of the covid-designated airports"))

rm(feb_grouped_covid, feb_grouped_covid_origin)

# let's find the places shuttling the most people per day
setDT(feb_grouped)

# daily departures
daily_pass <- feb_grouped[, .(daily_passengers = sum(passengers)/28), ORIGIN][order(-daily_passengers)]

daily_pass_covid <- daily_pass[ORIGIN %in% airports]

daily_flights <- feb_grouped[, .(daily_flights = sum(flights)/28), ORIGIN][order(-daily_flights)]

daily_flights_covid <- daily_flights[ORIGIN %in% airports]


# daily departing passengers
covid_airport_rank <- which(daily_flights$ORIGIN %in% airports)
covid_airport <- as.data.frame(as.data.frame(daily_flights)[covid_airport_rank,])
names(covid_airport) <- c("ORIGIN", "Daily Flights")

covid_airport_rank <- cbind(covid_airport_rank, covid_airport)
names(covid_airport_rank)[1] <- "Rank by Number of Daily Departures"
covid_airport_rank_flights <- covid_airport_rank

### passengers

covid_airport_rank <- which(daily_pass$ORIGIN %in% airports)
covid_airport <- as.data.frame(as.data.frame(daily_pass)[covid_airport_rank,])
names(covid_airport) <- c("ORIGIN", "Daily Flights")

covid_airport_rank <- cbind(covid_airport_rank, covid_airport)
names(covid_airport_rank)[1] <- "Rank by Number of Daily Departing Passengers"
covid_airport_rank_pass <- covid_airport_rank

covid_airport_rank_flights

covid_airport_rank_pass

rm(covid_airport_rank, covid_airport, daily_flights, daily_flights_covid, daily_pass, daily_pass_covid)

# let's see how many airports are one flight away from the covid-designated airports

feb_grouped_covid <- feb_grouped[ORIGIN %in% airports]

# Filter to only coronavirus-specific airports for graph
covid_graph <- graph_from_edgelist(feb_grouped_covid %>% filter(ORIGIN %in% airports) %>% select(ORIGIN, DEST) %>% as.matrix())

plot(covid_graph, layout=layout_with_kk(covid_graph),
     edge.arrow.size = 0.25,
     vertex.color = "light blue",
     vertex.frame.color = "white",
     vertex.size = 10,
     vertex.label.cex = 0.75,
     vertex.label.color = "black",
     edge.curved = 0.00)



origin <- fread('cont_us_nodes.csv')
names(origin) <- c("ORIGIN", 'origin_lat', 'origin_lon')

dest <- fread('cont_us_nodes.csv')
names(dest) <- c("DEST", 'dest_lat', 'dest_lon')

# define the grouping columns
cols <- c("ORIGIN", "ORIGIN_CITY_NAME", "DEST", "DEST_CITY_NAME", "UNIQUE_CARRIER")


feb_grouped_covid %<>% left_join(origin) %>% left_join(dest) %>% setDT()

plot_network = function (df_map, point_color = 'blue', line_color = point_color,
                         map_title = 'Top Routes', background_fill = 'grey',
                         map_state_lines = 'black', map_fill = 'black' ) 
{
  library(ggplot2)
  library(maps)
  library(ggmap)
  
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
               shape = 21, colour = 'Black', fill = point_color,
               size=(df_map$airpot_counts_scaled)*4) +
    geom_curve(data = df_map, 
               aes(x=origin_lon, y=origin_lat, xend=dest_lon, yend=dest_lat), 
               # color is selected by index from the predefined color pallete above
               # needs to be rounded to an int between 1 and 10
               col = edge.col[df_map$color_ind], 
               # size is scaled 0 to 1
               size = (df_map$total_flts_scaled),
               curvature = 0.3, angle = 90, ncp = 5) +
    geom_point(data=df_map, aes(x=origin_lon, y=origin_lat), 
               #col=point_color, 
               shape = 21, colour = "Red", fill = point_color,
               size=(df_map$airpot_counts_scaled)*4) 
  gg
}

plot_network(feb_grouped_covid, map_title = "Direct Routes from COVID-Designated International Arrival Airports to other Domestic Airports with at least 1 Daily Flight (Feb 2018)", point_color = "Red", map_fill = 'white', line_color = 'black')

# Plot the network using D3
library(networkD3)
d3_graph <- igraph_to_networkD3(covid_graph)

d3_graph$links$value <- 1

forceNetwork(d3_graph$links, d3_graph$nodes,
             Source = "source", Target = "target", Value = "value",
             NodeID = "name", Group = "name", zoom = TRUE, arrows = TRUE,
             opacityNoHover = 1)


# patricks attempt at a large network plot
w_df <- feb_grouped_covid %>% select(ORIGIN, DEST, flights) %>% rename(from = ORIGIN, to = DEST, weight = flights)
g_w <- graph.data.frame(w_df, directed = TRUE)

# set colors using vertex attributes
g_w <- set_vertex_attr(g_w, 'Screening', index = V(g_w)[airports], value='red')
g_w <- set_vertex_attr(g_w, 'Screening', index = -V(g_w)[airports], value='yellow')
V(g_w)$color <- V(g_w)$Screening

# Set node size based on degree size:
V(g_w)$size <- log(degree(g_w)) #disparity was too great without a transformation

# The labels are currently node IDs.
V(g_w)$label.color <- "black"
V(g_w)$label.cex <- .4
V(g_w)$label.dist <- 0
# Set edge width based on weight:
E(g_w)$width <- .5

#change arrow size and edge color:
E(g_w)$arrow.size <- .01
E(g_w)$edge.color <- "gray80"

l <- layout_with_lgl(g_w)
# really th big change here is the 'asp' which is default 1.  
# with 0, it drops aspect making layout pretty much null but a better plot for bigger networks
plot(g_w, layout=layout_with_fr,  asp = 0, curved=T, 
     main="Screening Sites for Coronavirus and Connected Airports")

assortativity <- assortativity(g_w, airports)

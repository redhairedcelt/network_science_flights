library(data.table)
library(dplyr)
library(magrittr)
library(igraph)
library(networkD3)

# Set working directory path (CHANGE WHEN WORKING LOCALLY)
# path <- "C:/Users/alexj/OneDrive/Desktop/GW/Spring '20/DATS 6450 (Network)/Project/"
#path <- ""

#tryCatch(setwd(path), error=function(x) stop("STOP: Update path variable to final directory"))

# initialize data frame
df <- data.table()

# loop through yearly files and append to overall dataframe
for (d in c("Air_Data_2018.csv", 'Air_Data_1998.csv')){
  year <- as.numeric(gsub('[^[:digit:]]', '', d))
  data <- fread(d)
  data$V21 <- NULL
  data$Year <- year
  df <- rbind(df, data)
  rm(data)
}

# define our grouping function
group_flights <- function(data, airline, cols, year){
  
  a <- data[SEATS > 0 & UNIQUE_CARRIER %in% airline & Year == year, 
            .(flights = sum(DEPARTURES_PERFORMED), passengers = sum(PASSENGERS)),
            cols]
  a %<>% arrange(desc(flights))
  
  a %<>% filter(ORIGIN != DEST)
  
  return(setDT(a))
  
}

# read the lat/lon for each airport
origin <- fread('cont_us_nodes.csv')
names(origin) <- c("ORIGIN", 'origin_lat', 'origin_lon')

dest <- fread('cont_us_nodes.csv')
names(dest) <- c("DEST", 'dest_lat', 'dest_lon')

# define the grouping columns
cols <- c("ORIGIN", "ORIGIN_CITY_NAME", "DEST", "DEST_CITY_NAME", "UNIQUE_CARRIER")

# loop through our two years
for(yr in c(1998, 2018)){
  
  # define variable name
  var_df <- paste0('all_flights_', yr)
  
  # create grouped data frame
  assign(var_df, group_flights(df, unique(df$UNIQUE_CARRIER), cols, yr))
  
  # join grouped data frame with lat/long for dest/origin
  assign(var_df, get(var_df) %>% left_join(origin) %>% left_join(dest) %>% setDT())

  # loop through carriers
  for(al in c("AA", "DL", "UA", "WN")){
    
    # create variable name
    var_df <- paste0(al, '_', yr)
    
    # create grouped data frame
    assign(var_df, group_flights(df, al, cols, yr))
    
    # join grouped data frame with lat/lon for dest/origin
    assign(var_df, get(var_df) %>% left_join(origin) %>% left_join(dest) %>% setDT())
      }
}

rm(origin, dest)

# write all files to working directory
for (file in ls(pattern = "*8")){
  fwrite(get(file), paste0(file, '.csv'))
}


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


for(yr in c(1998, 2018)){
  for(al in c("AA", "DL", "UA", "WN")){
    # create variable name
    var_df <- paste0(al, '_', yr)
    print(var_df)
    plot_title <- paste0('Plot of ', al, ' in ', yr)
    
  }}
  
plot_network(df_map=DL_1998, map_title="Delta in 1998", point_color='blue')
plot_network(df_map=DL_2018, map_title='Delta in 2018', point_color='blue')

plot_network(df_map=AA_1998, map_title="American in 1998", point_color='red')
plot_network(df_map=AA_2018, map_title='American in 2018', point_color='red')

plot_network(df_map=WN_1998, map_title="Southwest in 1998", point_color='orange')
plot_network(df_map=WN_2018, map_title='Southwest in 2018', point_color='orange')

plot_network(df_map=UA_1998, map_title="United in 1998", point_color='gold3')
plot_network(df_map=UA_2018, map_title='United in 2018', point_color='gold3')


w_df <- WN_2018 %>% select(ORIGIN, DEST, flights) %>% rename(from = ORIGIN, to = DEST, weight = flights)
#w_df <- w_df[1:100]
g_w <- graph.data.frame(w_df, directed = TRUE)
plot(g_w)


library(threejs)

g_d3 <- igraph_to_networkD3(g_w)
# then we canmake a simple plot
simpleNetwork(g_d3$links)
# force network gives us more options.
forceNetwork(Links = g_d3$links, Nodes = g_d3$nodes, Source = "source",
             Target = "target", NodeID = "name", Group = "name", bounded = TRUE,
             opacityNoHover = TRUE, opacity = .6)


# Set node size based on degree size:
V(g_w)$size <- log(degree(g_w)) #disparity was too great without a transformation

# The labels are currently node IDs.
V(g_w)$label.color <- "black"
# Set edge width based on weight:
E(g_w)$width <- 2

#change arrow size and edge color:
E(g_w)$arrow.size <- .2
E(g_w)$edge.color <- "gray80"

plot(g_w)



l <- layout_with_lgl(g_w,
  maxiter = 500,
  maxdelta = vcount(g_w),
  area = vcount(g_w)^6,
  coolexp = 10,
  root = NULL
)
plot(g_w, layout=l)

assortativity <- assortativity(g_w, airports)

tkplot(g_w)

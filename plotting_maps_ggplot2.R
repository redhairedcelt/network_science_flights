# This function will install any packages you dont have installed already.
loadPkg = function(x) { if 
  (!require(x,character.only=T, quietly =T)) 
{ install.packages(x,dep=T,repos="http://cran.us.r-project.org"); 
    if(!require(x,character.only=T)) stop("Package not found") } }

loadPkg('dplyr')
library(dplyr)
loadPkg('readr')
library(readr)


loadPkg('ggplot2')
library(ggplot2)
loadPkg('maps')
library(maps)
loadPkg('ggmap')
library(ggmap)

df_full <- read_csv('sample_for_plots.csv')
df_full <- df_full[order(-df_full$total),]
df_full <- subset(df_full, origin_iata!=dest_iata)
df_full <- subset(df_full, origin_lat!=dest_lat)
df_full <- subset(df_full, origin_lon!=dest_lon)

## plot_network function requires a df with iata_codes for origin and departure
# as well as the lat and long for each airport.  It will remove selp loops.

plot_network = function (df_map, point_color = 'blue', line_color = point_color,
                        map_title = 'Top Routes', background_fill = 'transparent',
                        map_state_lines = 'black', map_fill = 'white' ) 
{
  df_map <- subset(df_map, origin_iata!=dest_iata)
  
  # scaling function.  Could use some work.
  range01 <- function(x){(x-min(x))/(max(x)-min(x))}
  # need to plot state lines
  states_map <- map_data("state")  
  
  # Make a color pallate between two colors.  Grey is default first color.
  col.1 <- adjustcolor('grey', alpha=0.4)
  col.2 <- adjustcolor(line_color, alpha=.8)
  edge.pal <- colorRampPalette(c(col.1, col.2), alpha = TRUE)
  edge.col <- edge.pal(10)
  # add a column for color indexing scaled by number of flights on each leg.
  # make sure any 0 are set to minimum of 1, because R starts at 1.  Argh.
  df_map$color_ind <- round(range01(df_map$total)*10)
  df_map$color_ind[df_map$color_ind==0] <- 1
  
  # add and scale total number of flights
  df_map$total_flts_scaled <- range01(df_map$total)
  
  # find counts of flights leaving by iata code, add to df_map as airport counts
  airport_counts_df <- as.data.frame(df_map %>% group_by(origin_iata) %>% 
    summarise(counts = n()))
  df_map <- merge(df_map, airport_counts_df, by='origin_iata')
  # scale and add back to df
  df_map$airpot_counts_scaled <- range01(df_map$counts)
  
  # actual plotting
  gg <- ggplot()
  gg <- gg + geom_map(data=states_map, map=states_map, aes(map_id=region),
                      color=map_state_lines, fill=map_fill, size=0.25) +
            expand_limits(x=states_map$long, y=states_map$lat)
  gg <- gg + labs(x=NULL, y=NULL, title=map_title) 
    #theme(panel.background = element_rect(fill = background_fill),
    #      plot.background = element_rect(fill = background_fill, color = NA))
    #coord_map("albers", lat0=39, lat1=49) +
  gg <- gg +
    # The geom points are plotted scaled 0 to 1.  The factor can be adjusted 
    geom_point(data=df_map, aes(x=origin_lon, y=origin_lat), col=point_color, 
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

plot_network(df_map=sample_n(df_full, 2000), point_color='red', line_color = 'maroon')
plot_network(df_map=sample_n(df_full, 2000), point_color='blue', line_color='black')
plot_network(df_map=sample_n(df_full, 2000), point_color='orange',
             map_fill = 'black', map_state_lines = 'grey')
plot_network(df_map=sample_n(df_full, 2000), point_color='purple', 
             map_fill = 'grey', map_state_lines = 'white')
plot_network(df_map=sample_n(df_full, 2000), point_color='orange',
             map_fill = 'black', map_state_lines = 'black')


plot_network(df_map=df_full, point_color='purple', line_color='purple')

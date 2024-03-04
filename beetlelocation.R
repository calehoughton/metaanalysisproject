library(ggplot2)
library(maps)

world_map <- map_data("world")


latitude <- c(51.05,33.44,62.27,52.62,38.96,31.04,23.69,37.59,38,53.04,53.76, -35.28,-36.48,50.03,34.13,37.59,32.07)
longitude <- c(3.71,-112.07, 12.34,1.29,35.24,34.85,120.96,-1.07,-82,33.26,-98.81,149.12,140.97,19.56,58.8,14.01,-81.08)


coordinates_df <- data.frame(latitude, longitude)


ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), 
               fill = "white", color = "black") +
  geom_point(data = coordinates_df, aes(x = longitude, y = latitude), 
             color = "red", size = 2) +
  coord_quickmap() +  # Equal aspect ratio projection
  theme_void()  # Removes unnecessary elements like axes and legends

library(tidyverse)
library(sf)
library(LaCroixColoR)
library(showtext)
library(rgdal)

font_add_google("Lato", bold.wt = 400)
showtext_auto()

state_plane <- st_read("https://opendata.arcgis.com/datasets/23178a639bdc4d658816b3ea8ee6c3ae_0.geojson") %>%
  st_transform(2163)



midwest_state_plane <- state_plane %>%
  filter(str_detect( as.character(ZONENAME), "Minn|Wis|Dak|Iowa|Mich")) %>%
  mutate(state = str_sub(ZONE, 1, 2))

states <- midwest_state_plane %>%
  group_by(state) %>%
  count()

pal <- c("#2b908f", "#7cd9a9", "#f45b5b", "#7798BF", "#aaeeee", "#ff0066", "#eeaaee", "#55BF3B", "#DF5353", "#7798BF", "#aaeeee", "#ed8953", "#ed5384", "#539bed", "#a9b7c7")

ggplot(midwest_state_plane) +
  geom_sf(color = "#FEDF38", aes(fill = ZONENAME), alpha = 0.9, show.legend = FALSE, linetype = "dashed", size = 0.5) +
  geom_sf(data = states, fill = NA, color = "white", size = 0.5) +
  theme_minimal() +
  scale_fill_manual(values = pal) +
  theme(axis.text = element_blank(),
        panel.grid.major = element_line("transparent"),
        text = element_text(family = "Lato", color = "#5c6570", size = 50)) +
  labs(title = "State Plane Zones")


ggsave("images/state_plane.png", height = 10, width = 10)

utm_zones <- st_read("https://opendata.arcgis.com/datasets/b294795270aa4fb3bd25286bf09edc51_0.geojson") %>%
  st_transform(2163)


midwest_utm <- utm_zones %>%
  st_intersection(midwest_state_plane) %>%
  filter(ZONE != 44) %>%
  group_by(ZONE) %>%
  count()


unique(midwest_utm$ZONE)


ggplot(midwest_utm) +
  geom_sf(color = "#FEDF38", aes(fill = as.character(ZONE)), alpha = 0.9, show.legend = FALSE, linetype = "dashed", size = 0.5) +
  geom_sf(data = states, fill = NA, color = "white", size = 0.5) +
  scale_fill_manual(values = sample(pal, 6)) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        panel.grid.major = element_line("transparent"),
        text = element_text(family = "Lato", color = "#5c6570", size = 50)) +
  labs(title = "Universal Transverse Mercator (UTM) Zones")

ggsave("images/utm.png", height = 10, width = 10)


##############

library(magick)


stateplane <- image_read("images/state_plane.png")
utm <- image_read("images/utm.png")

image_append(c(stateplane, utm))

img <- image_crop(image_append(c(stateplane, utm)), "1200x350+10+250")

img

img_scale <- image_scale(image_append(c(stateplane, utm)), "900x")

img_scale

img_draw <- image_draw(img_scale)

abline(v = 440, col = '#5c6570', lwd = '0.5', lty = "solid")

dev.off()

img_draw

img_bck <- image_transparent(img_draw, 'white')

img_bck

img_small <- image_crop(img_bck, "1150x285+0+70")

img_small

image_write(img_small, "images/projections.png")

########################

states <- tigris::states() %>%
  st_as_sf() %>%
  filter(! NAME %in% c("Puerto Rico", "Guam", "American Samoa", "Commonwealth of the Northern Mariana Islands", "United States Virgin Islands"))

states_mercator <- states %>%
  st_transform(3857)


ggplot(states_mercator) +
  geom_sf_() +
  theme_minimal()

########### POPULATION LINES

library(sp)
library(rgdal)
library(reshape)
library(ggplot2)
library(maptools)
library(raster)
library(rgeos) #create a range standardisation function
range01 <- function(x){(x-min(x))/(max(x)-min(x))}



input<-raster("data/gpw_v4_population_count_rev11_2020_2pt5_min.tif") # the latest data come as a Tiff so you will need to tweak.



proj4string(input) = CRS("+init=epsg:4326")

midwest <- st_transform(midwest_state_plane, projection(input)) %>%
  filter(str_detect(ZONE, "MN"))

pop_masked = mask(input, midwest)

pop_df <- as(pop_masked, 'SpatialGridDataFrame')


values<-melt(pop_df)
names(values)<- c("pop", "x", "y")

#Rescale the values. This is to ensure that you can see the variation in the data
values$pop_st<-range01(values$pop)*0.20
values$x_st<-range01(values$x)
values$y_st<-range01(values$y)

#Switch off various ggplot things

xquiet<- scale_x_continuous("", breaks=NULL)
yquiet<-scale_y_continuous("", breaks=NULL)
quiet<-list(xquiet, yquiet)

#Add 180 to the latitude to remove negatives in southern hemisphere, then order them.
values$ord<-values$y+180
values_s<- values[order(-values$ord),]

#Create an empty plot called p
p<-ggplot()

#This loops through each line of latitude and produced a filled polygon that will mask out the lines beneath and then plots the paths on top.The p object becomes a big ggplot2 plot.
for (i in unique(values_s$ord))
{
  p<-p+geom_polygon(data=values_s[values_s$ord==i,],aes(x_st, pop_st+y_st,group=y_st), size=0.2, fill="#F9E7E0", col="#F9E7E0")+ geom_path(data=values_s[values_s$ord==i,],aes(x_st, pop_st+y_st,group=y_st),size=0.3, lineend="round", color = "#B75D69")
}

font_add_google("Quicksand", bold.wt = 400)
showtext_auto()



p +theme(panel.background = element_rect(fill='#F9E7E0',colour='#F9E7E0'),
         plot.background = element_rect(fill = "#F9E7E0"))+quiet +
  annotate("text", y = 0.25, x = 0.64, label = "TWIN CITIES", family = "Quicksand", color = "#B75D69", size = 6) +
  annotate("text", y = 0.56, x = 0.74, label = "DULUTH", family = "Quicksand", color = "#B75D69", size = 6) +
  annotate("rect", ymin = .34, ymax = .35, xmin = .29, xmax = .35, fill = "#F9E7E0", color = "#F9E7E0") +
  annotate("text", y = .345, x = .32, label = "ST CLOUD", family = "Quicksand", color = "#B75D69", size = 6)

ggsave("images/mn_poplines_text.png")


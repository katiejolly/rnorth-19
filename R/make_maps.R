library(tidyverse)
library(sf)
library(LaCroixColoR)
library(showtext)

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



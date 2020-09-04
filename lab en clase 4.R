
install.packages("rgeos")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")

library(sf)
library(rnaturalearth)
library(tidyverse)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge)
library(rnaturalearthdata)
library(rgeos)
library(rnaturalearth)
library(classInt)


w <- ne_countries(scale = "medium", returnclass = "sf")
plot(st_geometry(w))

st_crs(w)


DE = st_geometry(ne_countries(country = "germany", returnclass = "sf"))
DE.eqc = st_transform(DE, "+proj=eqc +lat_ts=51.14 +lon_0=90w")


mean(st_bbox(DE)[c("ymin", "ymax")])


par(mfrow = c(1, 2))
plot(DE, axes = TRUE)
plot(DE.eqc, axes = TRUE)

sq = rbind(c(-89,0), c(-89,1), c(-91,1), c(-91,0), c(-89,0))
pol = st_sfc(st_polygon(list(sq)), crs = 4326)
(pol.o = st_transform(pol, "+proj=ortho +lat_0=0 +lon_0=0"))[[1]]


#> POLYGON ((-6377166 0, -6376194 111314, -6377166 0))
st_is_valid(pol.o, NA_on_exception=FALSE)


library(classInt)

r = rnorm(100)
(cI <- classIntervals(r))

cI$brks


(ls = st_sfc(st_linestring(rbind(c(-179.5, 52), c(179.5, 52))), crs = 4326))

system.file("gpkg/nc.gpkg", package="sf") %>% read_sf -> nc
plot(nc)

# 9.1.3


library(sf)
nc %>% select(SID74, SID79) %>% plot(key.pos = 4)

plot(nc["SID74"], logz = TRUE, pal = viridis::viridis,
     breaks = c(0,.5,1,1.5,2,2.5), at = c(0,.5,1,1.5,2,2.5),
     key.width = lcm(1.3), key.length = 1)


plot(nc["SID74"], pal = viridis::viridis, reset = FALSE, graticule = TRUE, axes = TRUE)
plot(st_point(c(-81.498,36.434)), col = 'red', pch = 1, cex = 4, lwd = 3, add = TRUE)
layout(matrix(1))


tif = system.file("tif/L7_ETMs.tif", package = "stars")
library(stars)
x = read_stars(tif)
par(mfrow = c(1, 2))
plot(x[,1:10,1:10,1], text_values=TRUE, key.pos = NULL, col = viridis::viridis(10), 
     reset = FALSE)
plot(x[,1:10,1:10,1:3], rgb=1:3, interpolate = TRUE, reset = FALSE)


plot(x[,1:10,1:10,1:3], rgb=1:3, interpolate = FALSE, reset = FALSE)


par(mfrow = c(1, 3))
xs = adrop(x[,1:10,1:10,1])
attr(attr(xs, "dimensions"), "raster")$affine = c(1, 3)
plot(xs, col = viridis::viridis(10), key.pos = NULL, reset = FALSE)
plot(st_as_sf(xs, as_points = FALSE, merge = FALSE), pal = viridis::viridis, 
     key.pos = NULL, reset = FALSE)
plot(st_as_sf(xs, as_points = TRUE, merge = FALSE), key.pos = NULL, 
     pal = viridis::viridis, pch = 16, reset = FALSE)


# 9.3


library(grid)
system.file("gpkg/nc.gpkg", package="sf") %>% read_sf -> nc
st_viewport(nc) %>% pushViewport
st_geometry(nc) %>% lapply(st_as_grob) %>% lapply(grid.draw) -> x

# 10.1
library(tidyverse)
library(sf)
system.file("gpkg/nc.gpkg", package="sf") %>% read_sf() %>% 
  st_transform(32119) -> nc.32119


ggplot() + geom_sf(data = nc.32119)

ggplot(nc.32119) + geom_sf()

ggplot() + geom_sf(data = nc.32119) + theme_void() +
  theme(panel.grid.major = element_line(color = "white"))



ggplot() + geom_sf(data = nc.32119) + aes(fill = BIR74) +
  scale_fill_gradientn(colors = viridis::viridis(20))


# 10.1.2


ggplot() + geom_sf(data = nc.32119) + geom_sf(data = st_centroid(nc.32119))


# 10.1.3

nc <- sf::st_read(system.file("gpkg/nc.gpkg", package = "sf"), quiet = TRUE)
nc_3857 <- sf::st_transform(nc, "+init=epsg:3857")
ggplot() + 
  geom_sf(data = nc_3857[1:3, ], aes(fill = AREA)) + 
  geom_sf_label(data = nc_3857[1:3, ], aes(label = NAME))

# 10.2

library(stars)
library(ggplot2)
library(viridis)
#> Loading required package: viridisLite
system.file("tif/L7_ETMs.tif", package = "stars") %>% read_stars() -> x
g = ggplot() + 
  coord_equal() + 
  scale_fill_viridis() + 
  theme_void() +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0))
g + geom_stars(data = x) + 
  facet_wrap(~band)

g + geom_stars(data = x, downsample = c(10,10,1)) + 
  facet_wrap(~band)


library(spacetime)
data(air) # this loads several datasets in .GlobalEnv
d = st_dimensions(station = st_as_sfc(stations), time = dates)
aq = st_as_stars(list(PM10 = air), dimensions = d)
# ggplot() + geom_stars(data = aq[,,3000])
aq.sf = st_as_sf(aq[,,3000], long=TRUE)
ggplot() + 
  geom_sf(data = st_as_sf(DE_NUTS1)) + 
  geom_sf(data = aq.sf, mapping = aes(col = PM10)) + 
  ggtitle(aq.sf$time[1])


##########################################################
#Chapter 8 Making maps with R
#install.packages("tmap")
library(sf)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2)


# Add fill layer to nz shape
tm_shape(nz) +
  tm_fill() 
# Add border layer to nz shape
tm_shape(nz) +
  tm_borders() 
# Add fill and border layers to nz shape
tm_shape(nz) +
  tm_fill() +
  tm_borders()


map_nz = tm_shape(nz) + tm_polygons()
class(map_nz)


map_nz1 = map_nz +
  tm_shape(nz_elev) + tm_raster(alpha = 0.7)


nz_water = st_union(nz) %>% st_buffer(22200) %>% 
  st_cast(to = "LINESTRING")

map_nz2 = map_nz1 +
  tm_shape(nz_water) + tm_lines()

map_nz3 = map_nz2 +
  tm_shape(nz_height) + tm_dots()

tmap_arrange(map_nz1, map_nz2, map_nz3)


ma1 = tm_shape(nz) + tm_fill(col = "red")
ma2 = tm_shape(nz) + tm_fill(col = "red", alpha = 0.3)
ma3 = tm_shape(nz) + tm_borders(col = "blue")
ma4 = tm_shape(nz) + tm_borders(lwd = 3)
ma5 = tm_shape(nz) + tm_borders(lty = 2)
ma6 = tm_shape(nz) + tm_fill(col = "red", alpha = 0.3) +
  tm_borders(col = "blue", lwd = 3, lty = 2)
tmap_arrange(ma1, ma2, ma3, ma4, ma5, ma6)


plot(st_geometry(nz), col = nz$Land_area)  # works
tm_shape(nz) + tm_fill(col = nz$Land_area)   # fails

tm_shape(nz) + tm_fill(col = "Land_area")



legend_title = expression("Area (km"^2*")")
map_nza = tm_shape(nz) +
  tm_fill(col = "Land_area", title = legend_title) + tm_borders()


#8.2.4

tm_shape(nz) + tm_polygons(col = "Median_income")
breaks = c(0, 3, 4, 5) * 10000
tm_shape(nz) + tm_polygons(col = "Median_income", breaks = breaks)
tm_shape(nz) + tm_polygons(col = "Median_income", n = 10)
tm_shape(nz) + tm_polygons(col = "Median_income", palette = "BuGn")

tm_shape(nz) + tm_polygons("Population", palette = "Blues")
tm_shape(nz) + tm_polygons("Population", palette = "YlOrBr")

#8.2.5 Layouts

map_nz + 
  tm_compass(type = "8star", position = c("left", "top")) +
  tm_scale_bar(breaks = c(0, 100, 200), text.size = 1)

map_nz + tm_layout(title = "New Zealand")
map_nz + tm_layout(scale = 5)
map_nz + tm_layout(bg.color = "lightblue")
map_nz + tm_layout(frame = FALSE)

map_nza + tm_style("bw")
map_nza + tm_style("classic")
map_nza + tm_style("cobalt")
map_nza + tm_style("col_blind")

#8.2.6

urb_1970_2030 = urban_agglomerations %>% 
  filter(year %in% c(1970, 1990, 2010, 2030))

tm_shape(world) +
  tm_polygons() +
  tm_shape(urb_1970_2030) +
  tm_symbols(col = "black", border.col = "white", size = "population_millions") +
  tm_facets(by = "year", nrow = 2, free.coords = FALSE)



#8.2.7
nz_region = st_bbox(c(xmin = 1340000, xmax = 1450000,
                      ymin = 5130000, ymax = 5210000),
                    crs = st_crs(nz_height)) %>% 
  st_as_sfc()


nz_height_map = tm_shape(nz_elev, bbox = nz_region) +
  tm_raster(style = "cont", palette = "YlGn", legend.show = TRUE) +
  tm_shape(nz_height) + tm_symbols(shape = 2, col = "red", size = 1) +
  tm_scale_bar(position = c("left", "bottom"))

nz_map = tm_shape(nz) + tm_polygons() +
  tm_shape(nz_height) + tm_symbols(shape = 2, col = "red", size = 0.1) + 
  tm_shape(nz_region) + tm_borders(lwd = 3) 


library(grid)
nz_height_map
print(nz_map, vp = viewport(0.8, 0.27, width = 0.5, height = 0.5))




us_states_map = tm_shape(us_states, projection = 2163) + tm_polygons() + 
  tm_layout(frame = FALSE)


hawaii_map = tm_shape(hawaii) + tm_polygons() + 
  tm_layout(title = "Hawaii", frame = FALSE, bg.color = NA, 
            title.position = c("LEFT", "BOTTOM"))
alaska_map = tm_shape(alaska) + tm_polygons() + 
  tm_layout(title = "Alaska", frame = FALSE, bg.color = NA)


us_states_map
print(hawaii_map, vp = grid::viewport(0.35, 0.1, width = 0.2, height = 0.1))
print(alaska_map, vp = grid::viewport(0.15, 0.15, width = 0.3, height = 0.3))


#8.3 Animated maps
#install.packages("gganimate")

library(gganimate)
library(ggplot2)
library(tmap)

urb_anim = tm_shape(world) + tm_polygons() + 
  tm_shape(urban_agglomerations) + tm_dots(size = "population_millions") +
  tm_facets(along = "year", free.coords = FALSE)


tmap_animation(urb_anim, filename = "urb_anim.gif", delay = 25)


#8.4 Interactive maps

tmap_mode("view")
map_nz


map_nz + tm_basemap(server = "OpenTopoMap")

world_coffee = left_join(world, coffee_data, by = "name_long")
facets = c("coffee_production_2016", "coffee_production_2017")
tm_shape(world_coffee) + tm_polygons(facets) + 
  tm_facets(nrow = 1, sync = TRUE)


tmap_mode("plot")

mapview::mapview(nz)



trails %>%
  st_transform(st_crs(franconia)) %>%
  st_intersection(franconia[franconia$district == "Oberfranken", ]) %>%
  st_collection_extract("LINE") %>%
  mapview(color = "red", lwd = 3, layer.name = "trails") +
  mapview(franconia, zcol = "district", burst = TRUE) +
  breweries



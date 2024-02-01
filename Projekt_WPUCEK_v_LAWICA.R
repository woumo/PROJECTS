#### reading in libraries & setting working directory ####
library(dplyr) #v
library(ggplot2) #v
library(metR) #v
library(tidyverse) #v
library(lubridate) #v
library(oce) #v
library(ocedata) #v
library(sf) #v
library(ncdf4) #v
library(RNetCDF) #v
library(raster) #v
library(rgdax)
library(viridis)
library(oceanmap)
library(CFtime)
library(eurocordexr)
library(spData)
library(openair) #v
library(RColorBrewer)
library(rasterVis) #v
library(lattice)
library(grid)
library(latticeExtra)
library(sf)
library(terra)
library(dplyr)
library(spData)
library(tmap)
library(leaflet)
library(broom)
library(ggspatial)

setwd("G:/My Drive/4WIZ_MAROSZ/projekt")

#### reading the NetCDF file and working with raster data####
# FOR SUMMER (15.06.2020 - 15.08.2020), I couldn't do 1st to 1st because the data was too big
# FOR WINTER (15.12.2020 - 15.03.2021)

#reading and viewing the .nc file
surr.nc <- open.nc("dane/lato/snapshots/20231208_1200_io_sosb.nc") #summer
#wurr.nc <- open.nc("dane/zima/snapshots/20231208_1300_io_sosb.nc")
wurr.nc <- open.nc("dane/zima/snapshots1/20231226_1400_io_sosb.nc")#winter
#reading the files in
#read.nc(surr.nc) #summer
#print.nc(surr.nc)
#read.nc(wurr.nc) #winter
#print.nc(wurr.nc)

#adding Lawica Slupska shapefile
lawica <- read_sf(dsn = "dane/lawica/lawica.shp", layer = "lawica")
obrys_l <- plot(lawica$geometry, col = "transparent", border = "black")
# adding merged polygons because others dont work when i add them to a plot
granice <- read_sf(dsn = "dane/granice/granice.shp", layer = "granice")
obrys_g <- plot(granice$geometry, col = "transparent", border = "black") 
#adding the area border
area <- read_sf(dsn = "dane/area/area.shp", layer = "area")
obrys_a <- plot(area$geometry, col = "transparent", border = "black")

#creating rasters
#summer
sla_dir_s = raster("dane/lato/snapshots/20231208_1200_io_sosb.nc",
                   level = 180, 
                   varname = "current_dir-m_ug_pm3d_1_05nm_um_assim_sst_v0-sb1k") # or current_speed-m_ug_pm3d_1_05nm_um_assim_sst_v0-sb1k
sla_speed_s = raster("dane/lato/snapshots/20231208_1200_io_sosb.nc",
                     level = 180, 
                     varname = "current_speed-m_ug_pm3d_1_05nm_um_assim_sst_v0-sb1k")
#wimter
sla_dir_w = raster("dane/zima/snapshots1/20231226_1400_io_sosb.nc",
                   level = 180, 
                   varname = "current_dir-m_ug_pm3d_1_05nm_um_assim_sst_v0-sb1k") # or current_speed-m_ug_pm3d_1_05nm_um_assim_sst_v0-sb1k

sla_speed_w = raster("dane/zima/snapshots1/20231226_1400_io_sosb.nc",
                     level = 180, 
                     varname = "current_speed-m_ug_pm3d_1_05nm_um_assim_sst_v0-sb1k")

#clipping the rasters to the area i want it to be (around Lawica Slupska)
#summer rasters
summerDir.area <- raster::crop(sla_dir_s, area)
summerSpeed.area <- raster::crop(sla_speed_s, area)
#winter rasters
winterDir.area <- raster::crop(sla_dir_w, area)
winterSpeed.area <- raster::crop(sla_speed_w, area)

#saving the clipped rasters
#summer
writeRaster(summerDir.area, filename = "summerDirArea.tiff", format = "GTiff", overwrite = TRUE)
writeRaster(summerSpeed.area, filename = "summerSpeedArea.tiff", format = "GTiff", overwrite = TRUE)
#winter
writeRaster(winterDir.area, filename = "winterDirArea.tiff", format = "GTiff",overwrite = TRUE)
writeRaster(winterSpeed.area, filename = "winterSpeedArea.tiff", format = "GTiff", overwrite = TRUE)

#setting projection for clipped rasters
#summer
proj4string(summerDir.area) = CRS("+init=EPSG:25834")
proj4string(summerSpeed.area) = CRS("+init=EPSG:25834")
#winter
proj4string(winterDir.area) = CRS("+init=EPSG:25834")
proj4string(winterSpeed.area) = CRS("+init=EPSG:25834")

#### data creation for plots ####
#clipped rasters to data frames
summerDir.df = raster::as.data.frame(summerDir.area, xy = TRUE)
summerSpeed.df = raster::as.data.frame(summerSpeed.area, xy = TRUE)
#winter
winterDir.df = raster::as.data.frame(winterDir.area, xy = TRUE)
winterSpeed.df = raster::as.data.frame(winterSpeed.area, xy = TRUE)

colnames(summerDir.df) <- c("x", "y", "Dir")
colnames(summerSpeed.df) <- c("x", "y","Speed")
colnames(winterDir.df) <- c("x", "y", "Dir")
colnames(winterSpeed.df) <- c("x", "y","Speed")

summerDir.df$id <- seq.int(nrow(summerDir.df))
summerDir.df <- summerDir.df[, c("id", "x", "y", "Dir")]

winterDir.df$id <- seq.int(nrow(winterDir.df))
winterDir.df <- winterDir.df[, c("id", "x", "y", "Dir")]

summerSpeed.df$id <- seq.int(nrow(summerSpeed.df))
summerSpeed.df <- summerSpeed.df[, c("id", "x", "y", "Speed")]

winterSpeed.df$id <- seq.int(nrow(winterSpeed.df))
winterSpeed.df <- winterSpeed.df[, c("id", "x", "y", "Speed")]

#dropping the NA values
#summer
summerDir.df <- summerDir.df %>% drop_na()
summerSpeed.df <- summerSpeed.df %>% drop_na()
#winter
winterDir.df <- winterDir.df %>% drop_na()
winterSpeed.df <- winterSpeed.df %>% drop_na()

#chamging the speed values
summerSpeed.df$VEL = summerSpeed.df$Speed * 1000
winterSpeed.df$VEL = winterSpeed.df$Speed * 1000

#### making a plot showing difference in currents' speed values ####
my_style <- theme(text = element_text(size = 16),
                  panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))

speedPlot = ggplot() + 
  geom_line(data = summerSpeed.df, aes(x = id, y = Speed, color = "Letni")) +
  geom_line(data = winterSpeed.df, aes(x = id, y = Speed, color = "Zimowy")) +
  labs(
    title = "Prędkości prądów powierzchniowych na obszarze Ławicy Słupskiej",
    colour = "Sezon"
  ) +
  theme(legend.position = c(.09, .90)) + 
  scale_fill_discrete(labels = c("Lato", "Zima")) +
  scale_color_manual(values = c('Red','Blue')) + 
  theme(plot.title = element_text(size = 22)) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 18)) +
  xlab("ID") +
  ylab('Prędkość [m s-1]') +
  my_style

print(speedPlot)

# saving the plot
ggsave(filename = paste(speedPlot,"p2.jpg", sep = "_"), units = "px", width = 4000, height = 3200, dpi = 300)

#merging directions and speed into one dataframe
summer <- merge(summerDir.df,summerSpeed.df,by = c("id"))
winter <- merge(winterDir.df,winterSpeed.df,by = c("id"))

#deleting the merged columns IN THIS ORDER!
summer <- summer[, -5]
summer <- summer[, -5]
winter <- winter[, -5]
winter <- winter[, -5]

#changing column names to something shorter (for shapefile)
colnames(summer) <- c("id", "x", "y", "Dir", "Speed")
colnames(winter) <- c("id" ,"x", "y", "Dir", "Speed")

#creating shapefile

#summer
summer.sf <- st_as_sf(summer, coords = c("x", "y"), crs = 25834)
#st_write(summer.sf, driver = "ESRI shapefile", "summer1.sph", layer = NULL)
#winter
winter.sf <- st_as_sf(winter, coords = c("x", "y"), crs = 25834)
#st_write(winter.sf, driver = "ESRI shapefile", "winter1.sph", layer = NULL)

# #adding id column and reordering

#summer
summer$id <- seq.int(nrow(summer))
summer <- summer[, c("id", "x", "y", "Dir", "Speed")]
head(summer)
#winter
winter$id <- seq.int(nrow(winter))
winter <- winter[, c("id", "x", "y", "Dir", "Speed")]
head(winter)

#### making the wind roses  ####
#summer windrose
summer$NDIR = summer$Dir + 180      #changing the negative direction values into positive ones
#or is it perhaps something more like this? im not sure if i should only change the negative values to positive ones? the version above gives a more accurate windrose
# summer$NDIR = summer$Dir
# summer <- summer %>% mutate(ifelse(NDIR < 0, NDIR + 180, NDIR))
# head(summer)
# changing the default title into the one I want
my.statistic.summer <- list("fun" = length,"unit" = "%","scale" = "all", "lab" = "Częstość kierunków prądów powierzchniowych w lecie 2020 (%)" , "fun2" = function(x) signif(mean(x, na.rm = TRUE), 3), "lab2" = "średnia prędkość pp", "labcalm" = function(x) round(x, 1))
#generating the windrose for summer
roseSummer = windRose(
  summer,
  ws = "Speed",
  wd = "NDIR",
  ws.int = 0.05,
  angle = 30,
  type = "default",
  bias.corr = TRUE,
  cols = c("green", "blue", "orange", "red", "purple"),
  width = 1,
  auto.text = TRUE,
  breaks = 5,
  offset = 5,
  key.footer = "(m/s)",
  key.position = "right",
  key = TRUE,
  dig.lab = 1,
  statistic = my.statistic.summer,
  annotate = TRUE,
  angle.scale = 45,
  alpha = 1,
  plot = TRUE,
  paddle = FALSE)

#winter windrose
winter$NDIR = winter$Dir + 180      #changing the negative direction values into positive ones
# changing the default title into the one I want
my.statistic.winter <- list("fun" = length,"unit" = "%","scale" = "all", "lab" = "Częstość kierunków prądów powierzchniowych w zimie 2020 (%)" , "fun2" = function(x) signif(mean(x, na.rm = TRUE), 3), "lab2" = "średnia prędkość pp", "labcalm" = function(x) round(x, 1))
#generating the windrose for summer
roseWinter = windRose(
  winter,
  ws = "Speed",
  wd = "NDIR",
  ws.int = 0.05,
  angle = 30,
  type = "default",
  bias.corr = TRUE,
  cols = c("green", "blue", "orange", "red", "purple"),
  width = 1,
  auto.text = TRUE,
  breaks = 6,
  offset = 5,
  key.footer = "(m/s)",
  key.position = "right",
  key = TRUE,
  dig.lab = 1,
  statistic = my.statistic.winter,
  annotate = TRUE,
  angle.scale = 45,
  alpha = 1,
  plot = TRUE,
  paddle = FALSE)

#### plotting the maps with vectors#### 
#summer vector map
summer.df.split = summer %>% 
  st_as_sf(coords = c("x", "y")) %>%
  st_set_crs(25834)

summer.grid = summer.df.split %>% 
  st_make_grid(n = c(70,60)) %>%
  st_sf()

summer.gridded = summer.grid %>% 
  dplyr::mutate(id = 1:n(), contained = lapply(st_contains(st_sf(geometry),summer.df.split),identity),
         obs = sapply(contained, length),
         u = sapply(contained, function(x) {median(summer.df.split[x,]$NDIR, na.rm = TRUE)}),
         v = sapply(contained, function(x) {median(summer.df.split[x,]$Speed, na.rm = TRUE)})) 

summer.gridded = summer.gridded %>% dplyr::select(obs, u, v) %>% na.omit()

## obtain the centroid coordinates from the grid as table
coordinates = summer.gridded %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as_tibble()# %>% 
  #rename(x = X, y = Y)

## remove the geometry from the simple feature of gridded drifter dataset
st_geometry(summer.gridded) = NULL

## stitch together the extracted coordinates and drifter information int a single table for SE monsoon season
summer.stitch = coordinates %>% 
  bind_cols(summer.gridded) #%>% 
  #mutate(season = "SE")

## interpolate the U component
u.summ = interpBarnes(x = summer.stitch$X, y = summer.stitch$Y, z = summer.stitch$u)

## obtain dimension that determine the width (ncol) and length (nrow) for tranforming wide into long format table
dimension = data.frame(lon = u.summ$xg, u.summ$zg) %>% dim()

## make a U component data table from interpolated matrix
u.tb = data.frame(lon = u.summ$xg, 
                  u.summ$zg) %>% 
  gather(key = "lata", value = "u", 2:dimension[2]) %>% 
  mutate(lat = rep(u.summ$yg, each = dimension[1])) %>% 
  dplyr::select(lon,lat, u) %>% as.tibble()

## interpolate the V component
v.summ = interpBarnes(x = summer.stitch$X, 
                    y = summer.stitch$Y, 
                    z = summer.stitch$v)

## make the V component data table from interpolated matrix
v.tb = data.frame(lon = v.summ$xg, v.summ$zg) %>% 
  gather(key = "lata", value = "v", 2:dimension[2]) %>% 
  mutate(lat = rep(v.summ$yg, each = dimension[1])) %>% 
  dplyr::select(lon,lat, v) %>% 
  as.tibble()

## stitch now the V component into the U data table and compute the velocity
uv.summ = u.tb %>% 
  bind_cols(v.tb %>% dplyr::select(v)) %>% 
  mutate(vel = sqrt(u^2 + v^2))

uv.summ$Speed = uv.summ$vel/1000

#PLOTT!! for summer ..............................................................................................................................
summerMap <- ggplot() +
  geom_raster(data = uv.summ, aes(x = lon, y = lat, fill = Speed), interpolate = TRUE) +
  geom_segment(data = uv.summ, aes(x = lon, xend = lon + u/1.2, y = lat, yend = lat + v/1.2), 
               arrow = arrow(angle = 20, length = unit(.2, "cm"), type = "open")) +
  labs(
    title = "Mapa prędkości i kierunków prądów powierzchniowych\nw lecie 2020 roku na wybranym obszarze"
  ) +
  #geom_polygon(data = countries$g, aes(x = longitude, y = latitude), colour = "black", fill = NA) +
  geom_sf(data = granice, fill = "transparent", col = "black") +
  #coord_sf(ylim = c(6000000,6150000), xlim = c(180000, 280000)) +
  scale_fill_gradient(name = "Prędkość\n(m/s)", colours = oceColorsVelocity(120), limits = c(0,0.35), breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35)) +
  #limits = c(0,1.6), breaks = seq(0.1,1.6,.3)) +
  theme_bw() +
  theme(legend.position = "right",
        legend.key.height = unit(2, "cm"),
        legend.background = element_blank(),
        axis.text = element_text(size = 12, colour = 1)) +
  labs(x = "", y = "")

summerMap +
  ggspatial::annotation_scale(
    location = "br",
    bar_cols = c("black", "white")
  ) +
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    )
  )

ggsave(filename = paste(summerMap,"TAK14S.jpg"), units = "px", width = 3000, height = 2000, dpi = 300)
#PLOTT!! for summer ..............................................................................................................................

#winter vector map
winter.df.split = winter %>% 
  st_as_sf(coords = c("x", "y")) %>%
  st_set_crs(25834)

winter.grid = winter.df.split %>% 
  st_make_grid(n = c(70,60)) %>%
  st_sf()

winter.gridded = winter.grid %>% 
  dplyr::mutate(id = 1:n(), contained = lapply(st_contains(st_sf(geometry),winter.df.split),identity),
                obs = sapply(contained, length),
                u = sapply(contained, function(x) {median(winter.df.split[x,]$NDIR, na.rm = TRUE)}),
                v = sapply(contained, function(x) {median(winter.df.split[x,]$VEL, na.rm = TRUE)})) 

winter.gridded = winter.gridded %>% dplyr::select(obs, u, v) %>% na.omit()

## obtain the centroid coordinates from the grid as table
coordinates = winter.gridded %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as_tibble()# %>% 
#rename(x = X, y = Y)

## remove the geometry from the simple feature of gridded drifter dataset
st_geometry(winter.gridded) = NULL

## stitch together the extracted coordinates and drifter information int a single table for SE monsoon season
winter.stitch = coordinates %>% 
  bind_cols(winter.gridded) #%>% 
#mutate(season = "SE")

## interpolate the U component
u.win = interpBarnes(x = winter.stitch$X, y = winter.stitch$Y, z = winter.stitch$u)

## obtain dimension that determine the width (ncol) and length (nrow) for tranforming wide into long format table
dimension = data.frame(lon = u.win$xg, u.win$zg) %>% dim()

## make a U component data table from interpolated matrix
u.tb.win = data.frame(lon = u.win$xg, 
                  u.win$zg) %>% 
  gather(key = "lata", value = "u", 2:dimension[2]) %>% 
  mutate(lat = rep(u.win$yg, each = dimension[1])) %>% 
  dplyr::select(lon,lat, u) %>% as.tibble()

## interpolate the V component
v.win = interpBarnes(x = winter.stitch$X, 
                      y = winter.stitch$Y, 
                      z = winter.stitch$v)

## make the V component data table from interpolated matrix
v.tb.win = data.frame(lon = v.win$xg, v.win$zg) %>% 
  gather(key = "lata", value = "v", 2:dimension[2]) %>% 
  mutate(lat = rep(v.win$yg, each = dimension[1])) %>% 
  dplyr::select(lon,lat, v) %>% 
  as.tibble()

## stitch now the V component into the U data table and compute the velocity
uv.win = u.tb.win %>% 
  bind_cols(v.tb.win %>% dplyr::select(v)) %>% 
  mutate(vel = sqrt(u^2 + v^2))

uv.win$Speed = uv.win$vel/1000

#PLOTT!! for winter .......................................................................................................................................
winterMap <- ggplot() +
  geom_raster(data = uv.win, aes(x = lon, y = lat, fill = Speed), interpolate = TRUE) +
  geom_segment(data = uv.win, aes(x = lon, xend = lon + u/1.2, y = lat, yend = lat + v/1.2), 
               arrow = arrow(angle = 20, length = unit(.2, "cm"), type = "open")) +
  geom_sf(data = granice, fill = "transparent", col = "black") +
  #coord_sf(ylim = c(6000000,6150000), xlim = c(180000, 280000)) +
  labs(
    title = "Mapa prędkości i kierunków prądów powierzchniowych\nw zimie 2020 roku na wybranym obszarze"
  ) +
  scale_fill_gradient(name = "Prędkość\n(m/s)", colours = oceColorsVelocity(120), limits = c(0, 0.35), breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35)) + 
# limits = c(0,1.6), breaks = seq(0.1,1.6,.3))#+
theme_bw() +
theme(legend.position = "right",
      legend.key.height = unit(1.4, "cm"),
      legend.background = element_blank(),
      axis.text = element_text(size = 12, colour = 1)) +
labs(x = "", y = "")

winterMap +
  ggspatial::annotation_scale(
  location = "br",
  bar_cols = c("black", "white")
) +
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "black"
    )
  )

ggsave(filename = paste(winterMap,"TAK14W.jpg", sep = "_"), units = "px", width = 3000, height = 2000, dpi = 300)
#PLOTT!! for winter .......................................................................................................................................

  


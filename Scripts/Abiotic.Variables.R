# Extracting Abiotic variables for BLD plots
# We will extract macroclimate, microclimate, and topographic variables

# load package
#devtools::install_github("matthewkling/topoclimate.pred")
library(topoclimate.pred)
library(sf)

# read in the DEM for Holden
dem = raster("./Raw.Data/USGS_13_n42w082_20230911.tif")
names(dem) = "elevation"
# project the DEM
dem.proj = projectRaster(dem, crs = "+proj=longlat +datum=NAD83 +no_defs")

# crop to focus on Holden, specifically the natural areas
# read in the natural areas shape file
natareas = st_read("./Raw.Data/HA_Natural_Areas/")
# projected CRS = WGS 84/Pseudo-Mercator

# align the crs between dem and natareas
natareas.trans = natareas %>% 
  st_transform(st_crs(dem.proj))

# get bounding box of natareas
bb = st_bbox(natareas.trans)
bb_ext = extent(bb$xmin, bb$xmax, bb$ymin, bb$ymax)

dem.crop = crop(dem.proj,bb_ext)

# terrain map of natural areas
na.terrain = hillShade(terrain(dem.crop,"slope"), terrain(dem.crop,"aspect"))
plot(na.terrain,col = colorRampPalette(c("black","white"))(50),legend = F)

# get variables
# load functions first
clim <- bioclimate(dem.crop, include_inputs = TRUE)

#### Extract data for HOlden BLD plots ####

BLD.plots = read.csv("./Raw.Data/Holden_BLD_plots.csv")

#subset to just the info we need
BLD.plot.local = BLD.plots %>% 
  select(plot_id,latitude,longitude)

# convert to sf object, WGS84
points_sf <- sf::st_as_sf(BLD.plot.local, coords = c("longitude", "latitude"), crs = 4326)

# align the crs between other plots and BLD.plots
BLD.trans = points_sf %>% 
  st_transform(st_crs(natareas.trans))

# coerce into spatial dataframe
BLD_sp <- as(BLD.trans, "Spatial")

# Extract low_temp, moisture, northness, eastness, windward_exposure, mTPI,
# slope, aspect, macro_bio1, macro_bio12, macro_bio5, macro_bio6
BLD.plot.clim = raster::extract(clim, BLD_sp)
BLD.plot.clim = as.data.frame(BLD.plot.clim)

# extract elevation
BLD.plot.elev = as.data.frame(raster::extract(dem.crop,BLD_sp))
BLD.all.data = cbind(BLD.plot.clim,BLD.plot.elev)

# get the data for only Holden plots
coords <- st_coordinates(BLD.trans)
BLD.plot.clim.local = cbind(BLD.all.data,coords)
colnames(BLD.plot.clim.local)[14] = "elevation"

all = left_join(BLD.plot.clim.local,BLD.plots, by = join_by(X == longitude, Y == latitude))

write.csv(all, file = "./Formatted.Data/BLD.plot.variables.csv")




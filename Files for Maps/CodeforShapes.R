##Source and prepare 2011 data zone and intermediate geography shapes
library(rgdal)
library(tools)
library(broom)
library(maptools)
library(leaflet)
#Create a temporary directory to download data into
tmpfl <- tempfile(fileext = ".zip")
#download
download.file("http://sedsh127.sedsh.gov.uk/Atom_data/ScotGov/ZippedShapefiles/SG_DataZoneBdry_2011.zip",destfile = tmpfl)
##create path to temporary directory and then unzip in there
temp_dr <- normalizePath(tempdir(), mustWork = TRUE)
unzip(tmpfl, exdir = temp_dr)
# Get shapefile location
shpfl_path <- file_path_sans_ext(temp_dr)
# Read shapefile
shps_dzs_11 <- readOGR(dsn = shpfl_path, layer = "SG_DataZone_Bdry_2011")
##try out thinnedSpatialPolygons, which makes the object take up less space
#might want to mess about with tolerance i.e make it higher
shps_dzs_11 <- thinnedSpatialPoly(shps_dzs_11, tolerance = 0.1)
###need to convert the coordinate projections
proj4string(shps_dzs_11)
shps_dzs_11 <- spTransform(shps_dzs_11, CRS("+proj=longlat +datum=WGS84"))


saveRDS(shps_dzs_11, "C:/Users/cassidy.nicholas/OneDrive - IS/CommunityPlanningOutcomesProfiler/Files for Maps/DZ11.rds")

#test
leaflet(shps_dzs_11) %>%
  addPolygons()
##IGZs================================================
#Create a temporary directory to download data into
tmpfl <- tempfile(fileext = ".zip")
#download
download.file("http://sedsh127.sedsh.gov.uk/Atom_data/ScotGov/ZippedShapefiles/SG_IntermediateZoneBdry_2011.zip",destfile = tmpfl)
##create path to temporary directory and then unzip in there
temp_dr <- normalizePath(tempdir(), mustWork = TRUE)
unzip(tmpfl, exdir = temp_dr)
# Get shapefile location
shpfl_path <- file_path_sans_ext(temp_dr)
# Read shapefile
shps_ig_11 <- readOGR(dsn = shpfl_path, layer = "SG_IntermediateZone_Bdry_2011")
##try out thinnedSpatialPolygons, which makes the object take up less space
#might want to mess about with tolerance i.e make it higher
shps_ig_11 <- thinnedSpatialPoly(shps_ig_11, tolerance = 0.5, minarea = 0.1)
###need to convert the coordinate projections
proj4string(shps_ig_11)
shps_ig_11 <- spTransform(shps_ig_11, CRS("+proj=longlat +datum=WGS84"))
#test
leaflet(shps_ig_11) %>%
  addPolygons()
saveRDS(shps_ig_11, "C:/Users/cassidy.nicholas/OneDrive - IS/CommunityPlanningOutcomesProfiler/Files for Maps/IG11.rds")

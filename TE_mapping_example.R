
pacman::p_load(rio,terra,fraser,star,rnaturalearth,leaflet,
               sf,RPostgreSQL,janitor,ggmap,tidyverse)

#import world bank data
wb.dat <- import("data/AllWorldBank_IBRDIDA.csv") %>% 
  clean_names() %>% 
  filter(!is.na(latitude)|!is.na(longitude)|longitude<180) %>% 
  filter(grepl("coast*|marine|fish*",development_objective))
#convert to points
wb.pt <- st_as_sf(wb.dat,coords = c( "longitude","latitude"),
                  crs = "WGS84") %>% 
  mutate(type=ifelse(grepl("climate|adaptation",development_objective),"climate","development"))


#load blank coast
crs.val <-  "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84"
coast <- st_as_sf(ne_coastline(),crs = crs.val)
coast.buffer <- coast %>% # buffer coast so we can crop other layers to around coastline 
  st_buffer(dist = 0.5) 
  st_set_crs(crs.val)
plot(st_geometry(coast))
# 
# plot(ne_coastline())

#Create an empty raster grid with the properties of the desired raster map. 
r <- terra::rast(resolution=c(1,1),        # cell size: here 10m x 10m as units in km
                 ext=ext(coast),             # extent/size of raster: set to fishing polygon area
                 crs= crs.val) # coordinate system
# plot(r)
coast.raster <- terra::rasterize(terra::vect(coast),r)
plot(coast.raster)
# plot(st_geometry(wb.pt),add=T)
# plot(st_geometry(eez),add=T)
#print(coast.raster)

# world EEZ
eez <- st_read("data/World_EEZ_v11_20191118/eez_boundaries_v11.shp")
plot(st_geometry(eez))

# load relative inequality dataset
trash <- "_relative_wealth_index.csv"
ineq.list <- list.files("data/relative-wealth-index-april-2021",full.names = T)
imp.func <- function (x) {
  import(x) %>% 
  dplyr::mutate(ctry=substr(basename(x),1,3))
  }

ineq.dat <- lapply(ineq.list,imp.func)
length(ineq.dat)
rineq <- do.call(rbind.data.frame,ineq.dat) 

rineq.sf <-  st_as_sf(rineq,
                      coords=c("longitude","latitude"), 
                      crs=crs.val, agr = NA_agr_, remove = FALSE)

eez.buf <- st_buffer(eez,dist=1) %>% 
  st_set_crs(crs.val)
plot(st_geometry(eez.buf))
head(rineq)
rineq.coast <- rineq.sf %>% 
  st_crop(st_buffer(eez,dist=1))


rineq.raster <- terra::rasterize(terra::vect(rineq.sf),r, fun=mean,
                                 field="rwi")
plot(rineq.crop)
rineq.crop <- coast.raster*rineq.raster
# rineq.coast <- st_is_within_distance(rineq.sf, 
#                                      st_transform(coast,crs=st_crs(rineq.sf)), 
#                                      dist=1)
# rineq.coast <- st_intersects(rineq.sf,coast.raster,sparse = T)

# FSI inequality
fsi <- import("data/Global_fsi_ineq_impact.rds")
head(fsi)

#country
ctry <- ne_countries(scale="small", returnclass = c("sf")) %>% 
  st_set_crs(crs.val)
head(ctry)
unique(fsi$country[fsi$country%in%ctry$name])
plot(st_geometry(ctry))

fsi.ctry <- ctry %>% 
  left_join(fsi,by=c("name"="country")) %>% 
  st_transform(st_crs(eez))
summary(fsi.ctry$light_pollution)
plot(fsi.ctry["sea_level_rise"])
plot(eez)

test <- st_buffer(fsi.ctry,dist=1)
test <- st_join(eez, test, join=st_intersects)
plot(test["sea_level_rise"])
summary(test)
# FIPS (can't find dataframe)
db <- DBI::dbConnect(drv=RPostgreSQL::PostgreSQL(),host=url,db='new_fip_db',
                user="guest",password="guest",port=5432)
DBI::dbListTables(db)
fips <- DBI::dbGetQuery(db,"SELECT * FROM fips")
head(fips)


# Load MPA Atlas data
mpa.merged <- st_read("R:/Gill/research/mpa-data-compilation/spatial/boundaries/MPAs/final/", "MPA_zone_bound_merge.shp")
mpa.atlas <- st_read(paste0(dir.nam,last(sort(grep(nam,list.files(dir.nam), value=T)))))

leaflet() %>%
  addTiles() %>%
  addScaleBar() %>% 
  addRasterImage(raster::raster(coast.raster), opacity = 0.8) %>% 
  addCircleMarkers(data = wb.pt,
                  # label = ~as.character(type),
                   popup=~as.character(type),
                   color = ~as.character(type)) 






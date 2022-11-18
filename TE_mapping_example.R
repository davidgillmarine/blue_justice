# loading packages
pacman::p_load(rio,terra,fraser,star,rnaturalearth,leaflet,
               sf,RPostgreSQL,janitor,ggmap,tidyverse)

#--import world bank data
wb.dat <- import("data/AllWorldBank_IBRDIDA.csv") %>% 
  clean_names() %>% 
  filter(!is.na(latitude)|!is.na(longitude)|longitude<180) %>% 
  filter(grepl("coast*|marine|fish*",development_objective))
#convert to points
wb.pt <- st_as_sf(wb.dat,coords = c( "longitude","latitude"),
                  crs = "WGS84") %>% 
  mutate(type=ifelse(grepl("climate|adaptation",development_objective),"climate","development"))


#load blank coast
crs.val <-  "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84 +proj=longlat +datum=WGS84" # ?need to resolve
coast <- st_as_sf(ne_coastline(),crs = crs.val)
# coast.buffer <- coast %>% # buffer coast so we can crop other layers to around coastline 
#   st_buffer(dist = 0.5)
#  st_set_crs(crs.val)
plot(st_geometry(coast))
# plot(st_geometry(coast.buffer))
# 
# plot(ne_coastline())

#Create an empty raster grid with the properties of the desired raster map. 
r <- terra::rast(resolution=c(1,1),        # cell size: 1 degree forn now
                 ext=ext(coast),             # extent/size of raster
                 crs= crs.val) # coordinate system
coast.raster <- terra::rasterize(terra::vect(coast),r) # populate raster
plot(coast.raster)
# plot(st_geometry(wb.pt),add=T)
# plot(st_geometry(eez),add=T)
#print(coast.raster)

# world EEZ (do we use EEZ as the unit of analysis?)
eez <- st_read("data/World_EEZ_v11_20191118/eez_boundaries_v11.shp") 
  st_set_crs(crs.val)
plot(st_geometry(eez))

# load relative inequality dataset
trash <- "_relative_wealth_index.csv" # unwanted string values
# loop for importing and merging excel files, creating field with country codes (ISO3)
ineq.list <- list.files("data/relative-wealth-index-april-2021",full.names = T)
imp.func <- function (x) {
  import(x) %>% 
  dplyr::mutate(ctry=substr(basename(x),1,3))
  }
ineq.dat <- lapply(ineq.list,imp.func)
rineq <- do.call(rbind.data.frame,ineq.dat) 
#convert to spatial object
rineq.sf <-  st_as_sf(rineq,
                      coords=c("longitude","latitude"), 
                      crs=crs.val, agr = NA_agr_, remove = FALSE)

# convert ineq sf to raster
# eez buffer to capture coastal inequality values
eez.buf <- st_buffer(eez,dist=1) %>%
  st_set_crs(crs.val)
plot(st_geometry(eez.buf))

# crop to eez buffer and convert to raster (this takes a while)
rineq.coast <- rineq.sf %>% 
  st_crop(eez.buf)
rineq.raster <- terra::rasterize(terra::vect(rineq.sf),r, fun=mean,
                                 field="rwi",crs=crs.val)
rineq.crop <- coast.raster*rineq.raster #populate raster with inequality values
plot(rineq.crop)


# rineq.coast <- st_is_within_distance(rineq.sf, 
#                                      st_transform(coast,crs=st_crs(rineq.sf)), 
#                                      dist=1)
# rineq.coast <- st_intersects(rineq.sf,coast.raster,sparse = T)


# FSI composite indicators (? add ISO3 country code to fsi dataframe to join with ctry)
range01 <- function(x){(x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T))} # Function MinMax Scaler
fsi <- import("data/Global_fsi_ineq_impact.rds") %>% 
  mutate(dev.drv=direct_human_impact+organic_pollution+nutrient_pollution+inorganic_pollution+shipping,
         dev.drv=range01(dev.drv),  
         climate.drv=sea_surface_temperature+sea_level_rise+ocean_acidification,
         climate.drv=range01(climate.drv),
         cum.drv=dev.drv+climate.drv) 
names(fsi)
summary(fsi$climate.drv)

#country boundaries
ctry <- ne_countries(scale="small", returnclass = c("sf")) %>% 
  st_set_crs(crs.val)
# head(ctry)
# unique(fsi$country[fsi$country%in%ctry$name])
# plot(st_geometry(ctry))
#join FSI data (?)
fsi.ctry <- ctry %>% 
  left_join(fsi,by=c("name"="country")) %>% 
  st_set_crs(crs.val)

plot(fsi.ctry["dev.drv"])
plot(fsi.ctry["climate.drv"])

#plot(eez)

# fsi.ctry.buff <- st_buffer(fsi.ctry,dist=1) %>% 
#   st_set_crs(crs.val)
# test <- st_join(eez, test, join=st_intersects)
# plot(test["sea_level_rise"])
# summary(test)

# Load MPA Atlas data
mpatlas.data <- import("data/mpa_atlas/20200720_atlas.extract.csv") %>% 
  janitor::clean_names() %>% 
  # creating a 3 level score based on restrictions
  mutate(strict=case_when(
    no_take=="None" ~ 1,
    no_take=="Part" ~ 1.5,
     no_take=="All" ~ 2,
    TRUE ~ NA_real_)) %>%
  dplyr::rename(mpa_id=atlas_id)
# read in shp and join with data table
mpatlas.shp <- st_read("data/mpa_atlas/mpatlas_export_20190124_poly.shp") %>% 
  janitor::clean_names()  %>% 
  left_join(mpatlas.data, by="mpa_id") 
plot(st_geometry(mpatlas.shp["strict"]))

# ? find a way to merge/dissolve features and sum strict values
# mpatlas.merge <- mpatlas.shp %>% 
#   st_transform(crs.val) 
# mpatlas.merge1 <- mpatlas.merge %>% 
#   group_by(group = paste(st_intersects(mpatlas.merge, mpatlas.merge, sparse = T))) %>% 
#   summarise(score = sum(strict))

#summary(mpatlas.merge$mpa_id)

# FIPS (??can't find dataframe)
# db <- DBI::dbConnect(drv=RPostgreSQL::PostgreSQL(),host=url,db='new_fip_db',
#                 user="guest",password="guest",port=5432)
# DBI::dbListTables(db)
# fips <- DBI::dbGetQuery(db,"SELECT * FROM fips")
# head(fips)

#-- Plot preliminary maps 
# Climate
pal <- colorNumeric(
  palette = "RdBu",
  domain = fsi.ctry$climate.drv,
  reverse = TRUE)

leaflet() %>%
  addPolygons(data = fsi.ctry, 
              color = ~pal(climate.drv),
              opacity = 0.4,
              weight = 0)
# Devlopment
pal <- colorNumeric(
  palette = "RdBu",
  domain = fsi.ctry$dev.drv,
  reverse = TRUE)

leaflet() %>%
  addPolygons(data = fsi.ctry, 
              color = ~pal(dev.drv),
              opacity = 0.4,
              weight = 0)

# Cumulative impacts
pal <- colorNumeric(
  palette = "RdBu",
  domain = fsi.ctry$cum.drv,
  reverse = TRUE)

# cumulative
leaflet() %>%
  # addTiles() %>%
  addPolygons(data = fsi.ctry, 
              color = ~pal(cum.drv),
              opacity = 0.4,
              weight = 0)


# cumulative + inequality
leaflet() %>%
  # addTiles() %>%
  addScaleBar() %>% 
  addRasterImage(raster::raster(rineq.crop)) %>% 
  addPolygons(data = fsi.ctry, 
              color = ~pal(cum.drv),
              opacity = 0.4,
              weight = 0)

# cumulative + inequality + world bank
factpal <- colorFactor(topo.colors(2), wb.pt$type)

leaflet() %>%
  # addTiles() %>%
  addScaleBar() %>% 
  addRasterImage(raster::raster(rineq.crop)) %>% 
addPolygons(data = fsi.ctry, 
              #              color = ~as.character(climate.drv))
              color = ~pal(cum.drv),
              opacity = 0.4,
              weight = 0) %>% 
addCircleMarkers(data = wb.pt,
                popup=~as.character(type),
                color = ~factpal(type),
                opacity = 0.5,
                stroke=0.1) 
  addLegend("bottomright", pal = factpal, values = ~type,
            title = "World Bank projects",
            opacity = 1)





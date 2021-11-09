rm (list = ls())

start_time <- Sys.time()

library(sf)
library(raster)
library(dplyr)
library(sp)
library(exactextractr)
library(lwgeom)
library(units)
library(Lslide)
library(rgeos)
library(rmapshaper)
library(stringr)
library(geosphere)
library(Rfast)
options(scipen = 999)



# st_erase = function(x, y) st_difference(x, st_union(st_combine(y)))
# read data ---------------------------------------------------------------

pop_raster<- raster("01_input/01_world_pop/irq_ppp_2020_UNadj_constrained.tif") # %>% projectRaster(crs = 3891)

pop_raster %>% crs()


water_body <- st_read("01_input/02_shapefile/irq_water_bodies.shp") %>% as_Spatial() %>% 
  gBuffer(byid=TRUE, width=0) %>% 
  st_as_sf()

non_liveable_area<-   st_read("01_input/02_shapefile/NonLiveableAreas.shp")  %>% st_transform(32638) %>% as_Spatial() %>% 
  gBuffer(byid=TRUE, width=0) %>% 
  st_as_sf()


iraq_boundary <- st_read("01_input/04_admin_boundary_without_pop/irq_admbnda_adm0_cso_itos_20190603.shp")  %>% 
  st_transform(crs = 32638)

iraq_boundary <- iraq_boundary %>% as_Spatial()

admin_3 <- st_read("01_input/03_admin_boundary_with_pop/admin3.shp") #%>% st_transform(crs = 32638)

admin_3 <- admin_3 %>% dplyr::select(ends_with("_EN")) %>% st_transform(32638)




# create hexagon  ---------------------------------------------------------

# 50394 - 10 KM
# 251970 - 2km
# 503,940 =1 km
# 1,007,880 = 500m

HexPts <-spsample(iraq_boundary, "hexagonal",n=503940, offset=c(0,0))
grid <- HexPoints2SpatialPolygons(HexPts) 
grid <- grid %>% st_as_sf() %>% st_transform(crs= 32638) 


###################################



# Identify non_liveable_area ----------------------------------------------

liveable <- c( "village","shelter",  "building",
               "hamlet", "town","county","locality","city",               
               "region","suburb")


# for ted, delete villages

non_liveable_area <- non_liveable_area %>% filter(!fclass_133 %in% liveable)


non_liveable_area_dis <- non_liveable_area %>%  st_dissolve() 
waterbody_dis <- water_body %>% st_as_sf() %>% st_dissolve()

grid_remove_non_liveable <- rmapshaper::ms_erase(grid,non_liveable_area_dis)
grid_final <- rmapshaper::ms_erase(grid_remove_non_liveable,waterbody_dis)

grid_final$area_km <- st_area(grid_final) %>% set_units( km^2)

grid_wgs <-  grid_final %>% st_transform(crs = 4326)

grid_wgs <- grid_wgs %>% mutate(
  row_id = row.names(grid_wgs)
)

# extract population value -----------------------------------------------------------

pop_frm_zone<- exact_extract(pop_raster, grid_wgs, function(values,coverage_fractions)
  sum(values*coverage_fractions,na.rm = T))


Zone_with_data <- data.frame(row_id = row.names(grid_wgs),
                             pop = pop_frm_zone %>% as.integer() )


# Zone_with_data$row_id <- paste0("ID",Zone_with_data$row_id)

zone_with_population<-grid_wgs %>% left_join(Zone_with_data)

 
zone_with_population_with_info <- zone_with_population %>% mutate(
  cluster_character = case_when(pop == 0 ~ "no_population_in_the_cluster",
                                is.na(pop) ~ "population_NA",
                                1/as.numeric(area_km )< .5 ~ "more_than_half_of_the_area_falls_under_unliveable_zone",
                                pop < 200 ~ "cluster_population_is_less_than_200",
                                pop %in% c(200:500) ~ "need_accessibility_check_manually",
                                T~ "OK_for_sampling"), 
  draft_sampling_sts =  case_when(pop == 0 ~ "NOT_OK",
                                  is.na(pop) ~ "NOT_OK",
                                  1/as.numeric(area_km )< .3  ~ "NOT_OK",
                                  pop < 200 ~ "need_Manuel_check",
                                  pop %in% c(200:500) ~ "need_Manuel_check",
                                  T~ "OK")
)

zone_with_population_with_info$cluster_character %>% table()

zone_with_population_with_info  <- zone_with_population_with_info %>% st_transform(32638)

hex_centriod <- st_centroid(zone_with_population_with_info)



# add admin boubdary ------------------------------------------------------


hex_point_admin_info <- st_intersection(hex_centriod,admin_3)


hex_point_admin_need_info <- hex_point_admin_info %>% dplyr::select(row_id, ends_with("_EN")) %>% as.data.frame() %>% dplyr::select(-geometry)

zone_with_population_with_info <- zone_with_population_with_info %>% st_transform(crs = 4326)

final_zone <- zone_with_population_with_info %>% left_join(hex_point_admin_need_info)

final_zone <- final_zone %>% st_transform(crs=32638)



final_zone <- final_zone %>% st_transform(crs= 4326)
final_zone <- final_zone %>% filter(!is.na(ADM1_EN))
final_zone <- final_zone %>% dplyr::select(row_id,starts_with("ADM"),
                                    pop,cluster_character,draft_sampling_sts) 

# write shapefile ---------------------------------------------------------

st_write(final_zone,paste0("01_input/05_cluster/irq_one_km_hex.shp"))

end_time <- Sys.time()
end_time - start_time

rm(list = ls())

# library -----------------------------------------------------------------


library(sf)
library(dplyr)
library(rgdal)
library(raster)
library(exactextractr)




# read --------------------------------------------------------------------

admin_zero <-st_read("01_input/04_admin_boundary_without_pop/irq_admbnda_adm0_cso_itos_20190603.shp")
admin1_boundary <- st_read("01_input/04_admin_boundary_without_pop/irq_admbnda_adm1_cso_20190603.shp")
admin2_boundary <- st_read("01_input/04_admin_boundary_without_pop/irq_admbnda_adm2_cso_20190603.shp")
admin3_boundary <- st_read("01_input/04_admin_boundary_without_pop/irq_admbnda_adm3_cso_20190603.shp")

raster <- raster::raster("01_input/01_world_pop/irq_ppp_2020_UNadj_constrained.tif")




# admin 1 -----------------------------------------------------------------

admin1_boundary <- admin1_boundary %>% mutate(
  row_id = row.names(admin1_boundary)
)


adm1_zone<- exact_extract(raster, admin1_boundary, function(values,coverage_fractions)
  sum(values*coverage_fractions,na.rm = T))


adm1_data <- data.frame(row_id = row.names(admin1_boundary),
                        pop_all = adm1_zone %>% as.integer() )


# Zone_with_data$row_id <- paste0("ID",Zone_with_data$row_id)

adm1_with_population<-admin1_boundary %>% left_join(adm1_data)

st_write(adm1_with_population, "01_input/03_admin_boundary_with_pop/admin1.shp")



# admin 2 -----------------------------------------------------------------

admin2_boundary <- admin2_boundary %>% mutate(
  row_id = row.names(admin2_boundary)
)


adm2_zone<- exact_extract(raster, admin2_boundary, function(values,coverage_fractions)
  sum(values*coverage_fractions,na.rm = T))


adm2_data <- data.frame(row_id = row.names(admin2_boundary),
                        pop_all = adm2_zone %>% as.integer() )


# Zone_with_data$row_id <- paste0("ID",Zone_with_data$row_id)

adm2_with_population<-admin2_boundary %>% left_join(adm2_data)

adm2_with_population$pop_all %>% sum()


st_write(adm2_with_population, "01_input/03_admin_boundary_with_pop/admin2.shp")



# admin 3 -----------------------------------------------------------------

admin3_boundary <- admin3_boundary %>% mutate(
  row_id = row.names(admin3_boundary)
)


adm3_zone<- exact_extract(raster, admin3_boundary, function(values,coverage_fractions)
  sum(values*coverage_fractions,na.rm = T))


adm3_data <- data.frame(row_id = row.names(admin3_boundary),
                        pop_all = adm3_zone %>% as.integer() )


# Zone_with_data$row_id <- paste0("ID",Zone_with_data$row_id)

adm3_with_population<-admin3_boundary %>% left_join(adm3_data)

adm3_with_population$pop_all %>% sum()


st_write(adm3_with_population, "01_input/03_admin_boundary_with_pop/admin3.shp")


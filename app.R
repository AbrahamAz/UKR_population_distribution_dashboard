rm(list = ls())

# library -----------------------------------------------------------------


library(shiny)
library(shinydashboard)
library(leaflet)
library(sf)
library(dplyr)
library(shinythemes)
library(shinyWidgets)
library(DT)
library(rgdal)
library(zip)
library(raster)
library(leaflet.extras)
library(exactextractr)
library(stringr)
library(htmltools)



# read_data ---------------------------------------------------------------


admin_zero <-st_read("01_input/04_admin_boundary_without_pop/ukr_admbnda_adm0_sspe_20230201.shp")
cluster_sf <- st_read("01_input/05_cluster/UKR_hex1km2_pop_osm.shp")%>% 
  rename(population=SUM_round) %>% 
  mutate(zone_sts="OK") 



admin1_boundary <- st_read("01_input/03_admin_boundary_with_pop/UKR_ADM1_pop2020_eng.shp")
admin2_boundary <- st_read("01_input/03_admin_boundary_with_pop/UKR_ADM2_pop2020_eng.shp")
admin3_boundary <- st_read("01_input/03_admin_boundary_with_pop/UKR_ADM3_pop2020_eng.shp")

conflict_area <- st_read("01_input/02_shapefile/liveuamap_23022023.shp")

conflict <- st_zm(conflict_area, drop = T, what = "ZM")

# leaflet_map -------------------------------------------------------------

base_map <- leaflet::leaflet() %>% leaflet::addProviderTiles(providers$CartoDB.Positron) %>% 
  leaflet::addPolygons(data = admin_zero,color = "#EE5859",fillColor = "transparent") %>% 
  leaflet::addPolygons(data = admin1_boundary,color = "#58585A",
                     # label = ~htmlEscape(ADM1_EN),
                     # labelOptions = labelOptions(noHide = T, textOnly = TRUE,textsize = "15px"),
                     popup = paste("Governorate:", admin1_boundary$ADM1_EN, "<br>",
                                   "Total population:", admin1_boundary$SUM_round),
                     weight = 2,dashArray = "12",fillColor = "transparent") %>% 
  leaflet::addPolylines(data = conflict,
                       color = "#111111",
                       # label = ~htmlEscape(Label),
                       # labelOptions = labelOptions(noHide = T,
                       #                             direction = 'center',
                       #                             textOnly = T,
                       #                             style = list(
                       #                               "font-family" = "Arial Narrow",
                       #                               "font-size" = "10px",
                       #                               "font-style" = "italic")),
                       # popup = paste("Occupied Areas: ",conflict_area$Name),
                       weight = 3,dashArray = "9",
                       fillColor = "#990000"
  )
  


base_map

customized_cluster <- cluster_sf %>% filter(zone_sts != "NOT_OK")



# read population raster --------------------------------------------------

worldpop <- raster("01_input/01_world_pop/ukr_ppp_2020.tif") # %>% projectRaster(crs = crs(admin_zero))


base_map_for_pop <- leaflet::leaflet() %>% 
  leaflet::addProviderTiles(providers$CartoDB.PositronNoLabels) %>% 
  leaflet::addPolygons(data = admin_zero,color = "#EE5859",fillColor = "transparent") %>%  
  # leaflet::addRasterImage(worldpop) %>%
  leaflet::addPolygons(data = admin1_boundary,color = "#D2CBB8",
                       label = ~htmlEscape(ADM1_EN),
                       labelOptions = labelOptions(noHide = T,
                                                   direction = 'center',
                                                   textOnly = T,
                                                   style = list(
                                                     "font-family" = "serif",
                                                     "font-size" = "15px",
                                                     "font-weight" =  "bold"
                                                   )),
                       popup = paste("Governorate:", admin1_boundary$ADM1_EN, "<br>",
                                     "Total population:", admin1_boundary$SUM_round),
                       weight = 3,fillColor = "transparent") %>%

  leaflet::addPolygons(data = admin2_boundary,
                       color = "#58585A",
                       label = ~htmlEscape(ADM2_EN),
                       labelOptions = labelOptions(noHide = T,
                                                   direction = 'center',
                                                   textOnly = T,
                                                   style = list(
                                                     "font-family" = "serif",
                                                     "font-size" = "12px"
                                                   )),

                       popup = paste("Governorate:", admin2_boundary$ADM1_EN, "<br>",
                                     "District:", admin2_boundary$ADM2_EN, "<br>",
                                     "Total population:", admin2_boundary$SUM_round),

                       weight = 1,fillColor = "transparent",group = "District") %>%

  leaflet::addPolygons(data = admin3_boundary,
                       color = "#F69E61",
                       label = ~htmlEscape(ADM3_EN),
                       labelOptions = labelOptions(noHide = T,
                                                   direction = 'center',
                                                   textOnly = T,
                                                   style = list(
                                                     "font-family" = "Arial Narrow",
                                                     "font-size" = "10px",
                                                     "font-style" = "italic"
                                                   )),
                       popup = paste("Governorate:", admin3_boundary$ADM1_EN, "<br>",
                                     "District:", admin3_boundary$ADM2_EN, "<br>",
                                     "Sub-District:", admin3_boundary$ADM3_EN, "<br>",
                                     "Total population:", admin3_boundary$SUM_round),

                       weight = 1,dashArray = "9",
                       fillColor = "transparent",
                       group = "Sub-district") %>%
  leaflet::addPolylines(data = conflict,
                        color = "#990000",
                        # label = ~htmlEscape(Label),
                        # labelOptions = labelOptions(noHide = T,
                        #                             direction = 'center',
                        #                             textOnly = T,
                        #                             style = list(
                        #                               "font-family" = "Arial Narrow",
                        #                               "font-style" = "italic")),
                        weight = 3,dashArray = "9",
                        fillColor = "transparent") %>%
  addLayersControl(
    overlayGroups = c("District", "Sub-district"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  # hideGroup(c("Sub-district")) %>% 
  groupOptions("District", zoomLevels = 9:13) %>%
  groupOptions("Sub-district", zoomLevels = 10:20) %>%
  
  addDrawToolbar(position = "topleft",
                 polylineOptions = F,
                 polygonOptions = T,
                 rectangleOptions = T,
                 markerOptions = F,circleOptions = F,
                 circleMarkerOptions = F,singleFeature = T
                ) %>% setView(lat = 48.312805,lng = 33.361488,zoom = 6) 


# base_map_for_pop
# ui ---------------------------------------------------------------------

ui <- 
  fluidPage(
    
    # Styling -----------------------------------------------------------------
    
    tags$head(
      HTML('<meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no" />'), includeCSS("style.css")
    ),
    #   
    
    navbarPage(
      
      windowTitle = "UKRAINE POPULATION DISTRIBUTION",
      HTML('<a style="padding-left:10px;" class = "navbar-brand" href = "https://www.reach-initiative.org" target="_blank"><img src = "reach_logo.png" height = "50"></a><span class="navbar-text" style="font-size: 16px; color: #FFFFFF"><strong>UKRAINE POPULATION DISTRIBUTION</strong></span>'),
      
      
      tabPanel("Populated Area",
               
               
               column( width =3,
                       br(),
                       h4(strong("Background:"),tags$br(),
                          p(style="text-align: justify;","This app has been created to smoothen the host community sampling process all over Ukraine. WolrdPop population data has been used to identify the populated area over Ukraine. Note that WorldPop used remote sensing techniques to find the population distribution. Hence the population figure and their distribution may not be error-free. The detailed methodology they have followed can be found",em(tags$a(href="https://www.worldpop.org/geodata/summary?id=49994", "here")),"."
                          )),
                       
                       hr(),
                       
                       h4(strong("Methodology:"),tags$br(),
                          p(style="text-align: justify;", "The project was developed for Ukraine, divided into hexagons covering an area of one square kilometer for the whole country. Then the non-livable areas were classified by using OSM data, based on the location of governmental buildings, airports, schools, colleges, waterbodies, forests, and other relevant geospatial information. After that, for each hexagon, were calculated the coverage of non-livable areas in percentage and the number of population, by using the raster dataset of population published by WorldPop (2020). Hexagons where the number of population was less than 50, or where the coverage of non-livable area was greater than or equal to 90% added to the number of population between 50 and 100 were disregarded. The detailed R script is available on request."
                          )),
                       
                       hr(),
                       
                       
                       
                       h4(strong("Uses:"),#tags$br(),
                          p("Although this app was initially focusing on finding the populated area to sample host community assessment but this app can-")), 
                       # br(),
                       
                       p(tags$ol(
                         tags$li(em("Calculate population by Sub-districts")), 
                         tags$li(em("Calculate population from customized shapefile")), 
                         tags$li(em("Explore population distribution over Ukraine"))
                       )),
                       
                       
                       # tags$br(),
                       
                       p("In case of availability of other sources of population figures, this app data should be compared before calculating sample size as the population is based on remote sensing technology, which might not be error-free. However, the app data can be used to prepare the sample points (kmls). The usefulness of this app are listed below:"
                       ),
                       
                       # tags$br(),
                       
                       p(tags$ol(
                         tags$li(em("Find out the populated area.")), 
                         tags$li(em("The app data can be used to prepare the sample points (kml).")), 
                         tags$li(em("Compare the worldPop population data with other sources.")),
                         tags$li(em("In case of the unavailability of other valid population data sources, the app data can be used in any assessment stage."))
                       )),
                       
                       
                       hr(),
                       
                       h4(strong("Data Source:"),tags$br(),
                          p("Admin Boundaries:", em(tags$a(href="https://data.humdata.org/dataset/cod-ab-ukr", "OCHA")),tags$br(),
                            "Population:" , em(tags$a(href="https://data.humdata.org/dataset/worldpop-population-counts-for-ukraine", "WorldPop,2020")))
                       ),
                       
                       hr(),
                       h4(strong("Contact:"),tags$br(),
                          p("Abraham Azar",br(),
                            "Data Specialist",br(),
                            "Email:", tags$a(href="mailto:abraham.azar@impact-initiatives.org","abraham.azar@impact-initiatives.org"))
                       ),
                       p("Bruna Rutynar",br(),
                         "GIS Officer",br(),
                         "Email:", tags$a(href="mailto:bruna.rutyna@reach-initiative.org","bruna.rutyna@reach-initiative.org")),
                       hr(),
                       h4(strong("Credits:"),tags$br(),
                          p("Md. Mehedi Hasan Khan",br(),
                            "GIS Specialist"),
                       ),
                       
                       
                       
               ),
               
               mainPanel(
                 
                 br(),
                 h5(strong("This tab will give you the population data by each sub-district. Here in this tab, you have four parameters-")),
                 tags$ol(p(
                   tags$li(strong("Select governorate:"), em("Here, you should select governorate(s) of interest")), 
                   tags$li(strong("Select district:"), em("Once you select the governorate, 
                                    you will have the district name of the selected governorate. 
                                    From the list you should choose the district of interest")), 
                   tags$li(strong("Select sub-district:"), em("Once you select the district, you will have the district's sub-district name. 
                                                               From the list, you should choose the sub-district of interest")), 
                   tags$li(strong("Minimum allowable density:"), em("This parameter can help you to control the cluster. 
                              For example, If you do not want to consider the hexagon (cluster) with a population density below a specific number,
                              then you can just put the number in this parameter and the app will remove all the clusters with a population density 
                              below the number. The default is 93, which is Iraq's population density. This means initially (unless you do not change),
                              all the clusters with a population below 93 will be removed. 
                              This parameter is necessary to make the sample point considering operational and financial perspectives."))
                 )),
                 
                 
                 hr(),
                 
                 ##########################
                 tags$div(pickerInput("select_governarate",
                                      label = "Select governorate:",
                                      choices = admin1_boundary$ADM1_EN %>% unique() %>% dput(),
                                      selected = admin1_boundary$ADM1_EN %>% unique() %>% dput(),
                                      multiple = T,
                                      options = pickerOptions(title = "Select", actionsBox = TRUE, liveSearch = TRUE)
                 ),style="display:inline-block"),
                 
                 tags$div(pickerInput("select_district",
                                      label = "Select district:",
                                      choices = NULL,
                                      selected = NULL,
                                      multiple = T,
                                      options = pickerOptions(title = "Select", actionsBox = TRUE, liveSearch = TRUE)
                 ),style="display:inline-block"),
                 
                 tags$div(pickerInput("select_sub_district",
                                      label = "Select sub district:",
                                      choices = NULL,
                                      selected = NULL,
                                      multiple = T,
                                      options = pickerOptions(title = "Select", actionsBox = TRUE, liveSearch = TRUE)
                 ),style="display:inline-block"),
                 
                 
                 
                 tags$div(numericInput("minimum_allowable_density",
                                       label = "Minimum allowable density",
                                       value = 93,
                                       min = 10,
                                       max = 500,
                                       step = NA,
                                       width = NULL
                 ),style="display:inline-block"),
                 
                 hr(),
                 
                 
                 actionButton("run", "Show result"), 
                 downloadButton("download_summary", label = "Download sub district population (as csv)", class = NULL),
                 downloadButton("download_csv", label = "Download cluster data (as csv)", class = NULL),
                 downloadButton("download_shp", label = "Download cluster data (as shapefile)", class = NULL),hr(),
                 
                 DTOutput("summary_table"),hr(),
                 
                 h5(em(strong("Clusters Location:"))), 
                 leafletOutput("map")
                 
               )
      ),
      
      
      
      
      tabPanel("Calculate Population from Input Shapefile",
               
               mainPanel(br(),width = 12,
                         
                         h5(strong("This tab is for calculating the population by an input shapefile.")),
                         
                         hr(),
                         
                         
                         tags$div(fileInput(inputId = "shp",
                                            label = "Upload shapefile (projection system should be WGS84)",
                                            multiple = TRUE,
                                            accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj')),style="display:inline-block"),
                         hr(),
                         
                         tags$div(actionButton("run_for_pop", "Calculate Population"), style="display:inline-block"),
                         downloadButton("download_table_cus_shp", label = "Download data with population (csv)", class = NULL),
                         downloadButton("download_cus_shp", label = "Download data with population(shapefile)", class = NULL),
                         
                         hr(),
                         
                         DTOutput("summary_table_cus_shp"), br(),
                         
                         h4(em(strong("Map::Input location"))),hr(),
                         
                          leafletOutput("cus_map"),
                         # div(class = "outer", tags$style(type = "text/css", ".outer {position: absolute;}"),
                         #     leafletOutput("cus_map", width = "100%", height = "100%"))
                         
                         
               )# end mainPanel 
               
      ), # END TAB 2
      
      tabPanel("Explore Population!",
               
               
               
               mainPanel( width = 12,
                          
                          
                         br(),
                         
                         h5(strong("In this tab, you can calculate the population by a customized polygon. You can draw a polygon over the map as you wish by using the top left button.")),
                         
                         br(),
                         
                         tags$div(actionButton("a", "Calculate Population"), style="display:inline-block"),
                         downloadButton("download_shp2", label = "Download data with population(shapefile)", class = NULL),
                         
                         br(),br(),
                         h4(textOutput("population")),
                         hr(),
                        div(class = "outer", tags$style(type = "text/css", ".outer {position: fixed; top: 200px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                         leafletOutput("base_map_for_pop",width = "100%", height = "100%"))
                         
                         # leafletOutput("P_MAP"),
               )
      ) # END TAB 3
      
      
    )  ########## navarpage
    
    
  ) ## fludpage


# server ------------------------------------------------------------------


# Define server logic required to draw a histogram
server <- function(input, output,session){
  
  ################### Filter governorate ###############################################3
  minimum_allowable_density_for_cluster <-  reactive({input$minimum_allowable_density})
  
  governorate_names <- reactive({input$select_governarate})
  
  customized_cluster_filter <- reactive({customized_cluster %>%
      filter(ADM1_EN %in% governorate_names())})
  
  ####################### available district name in the selected governorate ############
  
  available_district <- reactive({customized_cluster_filter()$ADM2_EN %>% unique()})
  
  observe({
    updatePickerInput(session, "select_district", choices = available_district())
  })
  
  
  ########################################## filter district #####################################
  
  district_names <- reactive({input$select_district})
  customized_cluster_filter_district <- reactive({customized_cluster_filter()%>%
      filter(ADM2_EN %in% district_names())})
  
  
  ############################### available_sub_district ##############################
  available_subdistrict <- reactive({customized_cluster_filter_district()$ADM3_EN %>% unique()})
  
  observe({
    updatePickerInput(session, "select_sub_district", choices = available_subdistrict())
  })
  
  
  ############################### Filter sub district #######################################
  
  subdistrict_names <- reactive({input$select_sub_district})
  customized_cluster_final_filter_2 <- reactive({customized_cluster_filter_district() %>% 
      filter(ADM3_EN %in% subdistrict_names())})
  
  
  ###################################### extent ############################################
  customized_cluster_final_filter  <- eventReactive(input$run,{customized_cluster_final_filter_2() %>% filter(population >= minimum_allowable_density_for_cluster())})
  st_cord <- eventReactive(input$run,{customized_cluster_final_filter() %>% st_coordinates() %>% as.data.frame()})
  
  ############################################ MAP #########################################
  
  output$map <-  renderLeaflet({
    
    base_map %>%  leaflet::addPolygons(data = customized_cluster_final_filter(),
                                       fillOpacity = 0.5,
                                       smoothFactor = 0.5,
                                       color = ~colorQuantile("YlOrRd", customized_cluster_final_filter()$population)(customized_cluster_final_filter()$population),
                                       stroke = F,
                                       popup = paste("Row_id:", customized_cluster_final_filter()$row_id, "<br>",
                                                     "Governorate:", customized_cluster_final_filter()$ADM1_EN, "<br>",
                                                     "District:", customized_cluster_final_filter()$ADM2_EN, "<br>",
                                                     "Sub district:", customized_cluster_final_filter()$ADM3_EN, "<br>",
                                                     "Cluster Character for sampling:", customized_cluster_final_filter()$zone_sts, "<br>",
                                                     "population:", customized_cluster_final_filter()$population)
                                       
    )  %>% fitBounds(lng1 = min(st_cord()$X), 
                     lat1 = min(st_cord()$Y), 
                     lng2 = max(st_cord()$X), 
                     lat2 = max(st_cord()$Y))
    
  })
  
  
  ############################################################## Tab 2 map ########################### 
  output$base_map_for_pop <- renderLeaflet({base_map_for_pop})  
  
  
  polygn <- eventReactive(input$a,{
    feat <- input$base_map_for_pop_draw_new_feature
    coords <- unlist(feat$geometry$coordinates)
    coords <- matrix(coords, ncol = 2, byrow = T)
    polygn <- st_sf(st_sfc(st_polygon(list(coords))), crs = st_crs(4326))
    
  })
  
  
  
  ###################################### raster calculation ####################################################
  
  
  clip_pop_raster <-   eventReactive(input$a,{
    
    exact_extract(worldpop, polygn(), function(values,coverage_fractions)
      round(sum(values*coverage_fractions,na.rm = T)))
    
  })
  
  pop_text <- eventReactive(input$a,{paste0("Population within selected area:",clip_pop_raster())})
  
  output$population <- renderText({
    
    pop_text()
    
  })
  
  ################### add new ##################
  
  polygn2 <- eventReactive(input$a,{
    polygn() %>% dplyr::mutate(
    population = clip_pop_raster()
    )})
  
  getGeoContent3 <- eventReactive(input$a,{polygn2() %>% as_Spatial()})
  
  output$download_shp2 <- downloadHandler(
    filename = 'population_polygon.zip',
    content = function(file) {
      if (length(Sys.glob("population_polygon.*"))>0){
        file.remove(Sys.glob("population_polygon.*"))
      }
      writeOGR(getGeoContent3(), dsn="population_polygon.shp", layer="population_polygon", driver="ESRI Shapefile")
      zip(zipfile='population_polygon.zip', files=Sys.glob("population_polygon.*"))
      file.copy("population_polygon.zip", file)
      if (length(Sys.glob("population_polygon.*"))>0){
        file.remove(Sys.glob("population_polygon.*"))
      }
    }
  )
  
  ###################end new ###################
  
  ######################################## Download csv and shapefile ##########################################
  
  cluster_data <- reactive({customized_cluster_final_filter() %>% as.data.frame() %>% dplyr::select(-geometry)})
  
  getGeoContent <- reactive({customized_cluster_final_filter() %>% as_Spatial()})
  
  
  output$download_csv <- downloadHandler(
    filename = function() {
      paste('Cluster_data_', Sys.Date(), '.csv',sep = '')
    },
    content = function(con) {
      write.csv(cluster_data(), con)
    }
  )
  
  output$download_shp <- downloadHandler(
    filename = 'cluster.zip',
    content = function(file) {
      if (length(Sys.glob("cluster.*"))>0){
        file.remove(Sys.glob("cluster.*"))
      }
      writeOGR(getGeoContent(), dsn="cluster.shp", layer="cluster", driver="ESRI Shapefile")
      zip(zipfile='cluster.zip', files=Sys.glob("cluster.*"))
      file.copy("cluster.zip", file)
      if (length(Sys.glob("cluster.*"))>0){
        file.remove(Sys.glob("cluster.*"))
      }
    }
  )
  
  ################################################# Pivot table ########################################
  
  admin_3_name <- eventReactive(input$run,{cluster_data()$ADM3_EN %>% unique()})
  
  
  
  df1 <- eventReactive(input$run,{
    admin3_boundary %>% as.data.frame() %>% dplyr::filter(ADM3_EN %in% admin_3_name()) %>% 
      dplyr::select(ADM1_EN,ADM2_EN,ADM3_EN,pop_all) %>% rename(
        total_pop = pop_all
      )
  })
  df2 <- eventReactive(input$run,{
    cluster_data() %>% group_by(ADM3_EN) %>% summarise(
      pop_in_cluster = sum(population,na.rm = T)
    )
  })
  
  pv_tb <- eventReactive(input$run,{df1() %>% left_join(df2())})
  
  output$summary_table <- renderDT({
    datatable(pv_tb(),
              caption = htmltools::tags$caption(
                style = 'caption-side: top; text-align: center;',
                'Table 1: ', htmltools::em('Population by sub district')
                
              ))
    
    
  })
  
  ######################### download summary table ####################
  output$download_summary <- downloadHandler(
    filename = function() {
      paste('population_by_sub_district_', Sys.Date(), '.csv',sep = '')
    },
    content = function(con) {
      write.csv(pv_tb(), con)
    }
  )
  
  
  ###################################### upload customized shapefile ################################
  
  
  
  # Read-in shapefile function
  Read_Shapefile <- function(shp_path) {
    infiles <- shp_path$datapath # get the location of files
    dir <- unique(dirname(infiles)) # get the directory
    outfiles <- file.path(dir, shp_path$name) # create new path name
    name <- strsplit(shp_path$name[1], "\\.")[[1]][1] # strip name 
    purrr::walk2(infiles, outfiles, ~file.rename(.x, .y)) # rename files
    x <- st_read(file.path(dir, paste0(name, ".shp"))) # read-in shapefile
    return(x)
  }
  
  # Read-shapefile once user submits files
  user_shp <-  eventReactive(input$run_for_pop, {
    user_shp <- Read_Shapefile(input$shp)
  })  
  
  
  
  
  
  
  output$cus_map <-  renderLeaflet({
    
    base_map %>% leaflet::addPolygons(data = user_shp(),
                                      fillOpacity = 0.5,
                                      smoothFactor = 0.5)
    
  })
  
  ################################# start::calculate population ##############################################
  
  
  clip_pop_raster_cus_shp <-   eventReactive(input$run_for_pop,{
    
    user_shp_2 <- user_shp() %>% mutate(
      row_id = row.names(user_shp())
    )
    
    
    
    pop_cus_shp<- exact_extract(worldpop, user_shp_2, function(values,coverage_fractions)
      sum(values*coverage_fractions,na.rm = T))
    
    
    pop_cus_shp2 <- data.frame(row_id = row.names(user_shp_2),
                               population = pop_cus_shp %>% as.integer() )
    
    
    # Zone_with_data$row_id <- paste0("ID",Zone_with_data$row_id)
    
    clip_pop_raster_cus_shp<-user_shp_2 %>% left_join(pop_cus_shp2) %>% dplyr::select(row_id,population,everything())
    
  })
  
  
  output$summary_table_cus_shp <- renderDT({
    datatable(clip_pop_raster_cus_shp(),
              caption = htmltools::tags$caption(
                style = 'caption-side: top; text-align: center;',
                'Table 1: ', htmltools::em('Population by customized shape')
                
              ))
    
  })
  
  ################################# End::calculate population ################################################
  
  
  data_cus_shp_df <- eventReactive(input$run_for_pop,{
    
    data_cus_shp_df <- clip_pop_raster_cus_shp() %>% as.data.frame() %>% dplyr::select(-"geometry")
    
  })
  
  ############################################# write #######################################################
  
  output$download_table_cus_shp <- downloadHandler(
    filename = function() {
      paste('population_by_input_area', str_replace_all(Sys.Date(),"-","_"),".csv",sep = "")
    },
    content = function(con) {
      write.csv(data_cus_shp_df(), con)
    }
  )
  
  
  getGeoContent2 <- reactive({clip_pop_raster_cus_shp() %>% as_Spatial()})
  
  
  
  output$download_cus_shp <- downloadHandler(
    filename = 'input_with_pop.zip',
    content = function(file) {
      if (length(Sys.glob("input_with_pop.*"))>0){
        file.remove(Sys.glob("input_with_pop.*"))
      }
      writeOGR(getGeoContent2(), dsn="input_with_pop.shp", layer="input_with_pop", driver="ESRI Shapefile")
      zip(zipfile='input_with_pop', files=Sys.glob("input_with_pop.*"))
      file.copy("input_with_pop", file)
      if (length(Sys.glob("input_with_popr.*"))>0){
        file.remove(Sys.glob("input_with_popr.*"))
      }
    }
  )
  
  
  
  
  #####################################################################################################
  
  
  
} ### end server




# Run the application 
shinyApp(ui = ui, server = server)

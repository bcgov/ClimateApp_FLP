library(shiny)
library(RColorBrewer)
library(DT)
library(rgdal)
library(leaflet)
library(htmlwidgets)
library(leaflet.opacity)
library(sf)

# setwd("C:\\Users\\CMAHONY\\OneDrive - Government of BC\\Projects\\2020_BGCProjections\\BGCproj-App-RDKB")

# ------------------------------------------
# Load the input data
# ------------------------------------------

studyArea <- "LakesTSA"

modelMetadata <- read.csv("data/ModelList.csv")

dat.ref <- st_transform(st_read(paste("data/Current_",studyArea,".gpkg", sep=""), layer="Normal61"), st_crs("+init=epsg:4326"))
dat.cur <- st_transform(st_read(paste("data/Current_",studyArea,".gpkg", sep=""), layer="Current91"), st_crs("+init=epsg:4326"))
cols <- read.csv("data/bec_colourstyles.csv")

studyArea.hist <- read.csv(paste("data/",studyArea,".ref.100pts.mean.csv", sep=""))
studyArea.proj <- read.csv(paste("data/",studyArea,".proj.100pts.mean.csv", sep=""))
studyArea.9119 <- read.csv(paste("data/",studyArea,".9119.100pts.mean.csv", sep=""))
studyArea.ts <- read.csv(paste("data/",studyArea,".ts.100pts.mean.csv", sep=""))
bbox <- as.vector(unlist(read.csv(paste("data/",studyArea,".bbox.csv", sep=""))))

variables <- names(studyArea.proj)[-c(1:6)]
variable.names <- read.csv("data/Variables_ClimateBC.csv")
variables.select <- variables[c(grep("_wt|_sp|_sm|_at", variables), 225:247)]
variables.select <- variables.select[-grep("RH|Rad|MAR", variables.select)]

variable.types <- rep(NA, length(variables))
variable.types[grep("PPT|DD|PAS|NFFD|Eref|FFP|CMD|MAP|MSP|AHM|SHM|Rad|MAR", variables)] <- "ratio"
variable.types[grep("Tmax|Tmin|Tave|MAT|MWMT|MCMT|TD|EMT|EXT|bFFP|eFFP", variables)] <- "interval"
variable.types[grep("RH", variables)] <- "pct"

Ystr <- strsplit(as.character(studyArea.proj[,1]), "_")
scenario <- matrix(unlist(Ystr), ncol=3, byrow=TRUE)
scenario[,3] <- substr(scenario[,3],1,4)
scenario[grep("Ensemble", scenario[,1]),1] <- "ensembleVote"

proj.years <- c("2025","2055","2085")
rcps <- c("rcp45","rcp85")
gcms <- unique(scenario[,1])
gcms <- gcms[-which(modelMetadata$Runs[match(gcms,modelMetadata$Model)]==1)]
mods <- c("ENS", as.character(modelMetadata$Code[match(gcms,modelMetadata$Model)])[-1])


## calculate change in each climate variable

studyArea.change <- studyArea.proj[-c(1:6)]
studyArea.change[,] <- NA

  for(variable in variables){
    i <- which(variables==variable)
    studyArea.change[,i] <- if(variable.types[i]%in%c("interval", "pct")) studyArea.proj[,i+6] - as.vector(unlist(studyArea.hist[i+3])) else studyArea.proj[,i+6]/as.vector(unlist(studyArea.hist[i+3]))
  }

studyArea.change.9119 <- studyArea.9119[-c(1:3)]
studyArea.change.9119[,] <- NA

  for(variable in variables){
    i <- which(variables==variable)
    studyArea.change.9119[i] <- if(variable.types[i]%in%c("interval", "pct")) studyArea.9119[i+3] - studyArea.hist[i+3] else studyArea.9119[i+3]/studyArea.hist[i+3]
  }


#BGC zone color scheme
BGCcolors.BC <- read.csv("data/BGCzone_Colorscheme.csv")
BGCcolors <- read.csv("data/WNAv11_Zone_Colours.csv")
BGCcolors$colour <- as.character(BGCcolors$colour)
BGCcolors$colour[match(BGCcolors.BC$zone, BGCcolors$classification)] <- as.character(BGCcolors.BC$HEX)
ColScheme.zone <- factor(BGCcolors$colour, levels=BGCcolors$colour)
zones <- factor(BGCcolors$classification, levels=BGCcolors$classification)


# ------------------------------------------
# Define UI 
# ------------------------------------------
ui <- fluidPage(
  navbarPage(title = paste("Biogeoclimatic Projections for the", studyArea), theme = "bcgov.css", 
             tabPanel("BGC projections", 
                      fluidRow(
                        column(2,
                          helpText(""),
                          
                          tags$head(tags$script('$(document).on("shiny:connected", function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            $(window).resize(function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            ')),
                          
                          radioButtons("maptype",
                                       label = "Choose a projection type",
                                       choices = list("Reference (1961-1990)" = 1, "Recent (1991-2019)" = 2, "Future" = 3),
                                       selected = 1),
                          
                          radioButtons("colscheme",
                                       label = "Choose a color scheme",
                                       choices = list("BGC zone" = 1, "BGC subzone/variant" = 2),
                                       selected = 1),
                          
                          sliderInput("transparency", label = "Layer Transparency", min = 0, 
                                      max = 1, value = 1),
                          
                          radioButtons("proj.year",
                                       label = "Choose a future period",
                                       choices = list("2011-2040" = 1, "2041-2070" = 2, "2071-2100" = 3),
                                       selected = 1),
                          
                          radioButtons("rcp",
                                       label = "Choose an emissions scenario",
                                       choices = list("RCP4.5" = 1, "RCP8.5" = 2),
                                       selected = 1),
                          
                          selectInput("gcm", 
                                      label = "Choose the global climate model",
                                      choices = as.list(gcms),
                                      selected = "ensembleVote"),
                          
                          img(src = "refBEC.png", height = 669*0.45, width = 661*0.45)
                        ),    
                        
                        column(6, 
                          
                          leafletOutput(outputId = "map", height="86vh")
                          
                        ),

                        column(4, 
                               column(6, 
                                      selectInput("var1", 
                                                  label = "Choose the primary variable",
                                                  choices = as.list(variables.select),
                                                  selected = "Tave_sm")
                               ),
                               column(6, 
                                      selectInput("var2", 
                                                  label = "Choose the secondary variable",
                                                  choices = as.list(variables.select),
                                                  selected = "PPT_sm"),
                               ),
                               
                               column(12, 
                                      plotOutput(outputId = "timeSeries", height="35vh"),
                               ),
                               
                                      column(12, 
                                             plotOutput(outputId = "scatterPlot", height="35vh")
                                      ),
                        )
                      ),
                      column(width = 12,
                             style = "background-color:#003366; border-top:2px solid #fcba19;",
                             
                             tags$footer(class="footer",
                                         tags$div(class="container", style="display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
                                                  tags$ul(style="display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home", "Home", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;"))
                                                  )
                                         )
                             )
                      )
             ),
             tabPanel("Model Info",
                      DT::dataTableOutput("table"),
                      column(width = 12,
                             style = "background-color:#003366; border-top:2px solid #fcba19;",
                             
                             tags$footer(class="footer",
                                         tags$div(class="container", style="display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
                                                  tags$ul(style="display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home", "Home", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;"))
                                                  )
                                         )
                             )
                      )
                      )
             )
)

# ------------------------------------------
# Define server logic 
# ------------------------------------------
server <- function(input, output) {
  
  
  output$map <- renderLeaflet({
    
    dat.proj <- st_transform(st_read(paste("data/Future_",studyArea,".gpkg", sep=""), layer=paste(input$gcm,rcps[as.numeric(input$rcp)],proj.years[as.numeric(input$proj.year)],sep = "_")), st_crs("+init=epsg:4326"))

    if(input$maptype==1) dat <- dat.ref
    if(input$maptype==2) dat <- dat.cur
    if(input$maptype==3) dat <- dat.proj
    
    if(input$colscheme==1){
      BGC <- dat$BGC
      zone <- rep(NA, length(BGC))
      for(i in zones){ zone[grep(i,BGC)] <- i }
      BGC <- factor(zone)
      col <- data.frame(classification=BGC, colour = BGCcolors[match(BGC,BGCcolors$classification), 2])
    } else {
      BGC <- dat$BGC
      col <- merge(data.frame(becvar=as.character(levels(BGC))),cols)
      col$colour <- rgb(col$r/255,col$g/255,col$b/255)
    }
    
    leaflet() %>% 
      addTiles() %>% 
      addProviderTiles("Esri.WorldImagery", group = "Satellite view") %>%
      addProviderTiles("Esri.WorldTerrain", group = "Terrain only") %>%
      addProviderTiles("Esri.WorldTopoMap", group = "Base map") %>% 
      fitBounds(lng1 = bbox[1], lat1 = bbox[2], lng2 = bbox[3], lat2 = bbox[4]) %>%
      addLayersControl(
        baseGroups = c("Base map", "Terrain only", "Satellite view"),
        options = layersControlOptions(collapsed = FALSE)
      )%>% 
    addPolygons(data=dat, stroke = FALSE, smoothFactor = 0.2, fillOpacity = input$transparency,
                  # color = ~col$colour,popup = paste("1961-1990:",dat.ref$BGC, "<br>", "1991-2019:",dat.cur$BGC, "<br>", "Projected:",dat.proj$BGC),
                color = ~col$colour,popup = dat$BGC
                # highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE) #highlight doesn't work for some reason
      )
  },
  )
  
  # ### Attempt to have map transparency as a reactive event, rather than redrawing the map (and resetting the extent)
  # output$map <- renderLeaflet({
  #   
  #   leaflet() %>% 
  #     addTiles() %>% 
  #     fitBounds(lng1 = bbox[1], lat1 = bbox[2], lng2 = bbox[3], lat2 = bbox[4]) %>%
  #     addProviderTiles("Esri.WorldImagery", group = "Satellite view") %>%
  #     addProviderTiles("Esri.WorldTerrain", group = "Terrain only") %>%
  #     addProviderTiles("Esri.WorldTopoMap", group = "Base map") %>% 
  #     addLayersControl(
  #       baseGroups = c("Base map", "Terrain only", "Satellite view"),
  #       options = layersControlOptions(collapsed = FALSE)
  #     )
  # }
  # )
  # 
  # observe({
  #   dat.proj <- st_transform(st_read(paste("data/Future_",studyArea,".gpkg", sep=""), layer=paste(input$gcm,rcps[as.numeric(input$rcp)],proj.years[as.numeric(input$proj.year)],sep = "_")), st_crs("+init=epsg:4326"))
  #   
  #   if(input$maptype==1) dat <- dat.ref
  #   if(input$maptype==2) dat <- dat.cur
  #   if(input$maptype==3) dat <- dat.proj
  #   
  #   if(input$colscheme==1){
  #     BGC <- dat$BGC
  #     zone <- rep(NA, length(BGC))
  #     for(i in zones){ zone[grep(i,BGC)] <- i }
  #     BGC <- factor(zone)
  #     col <- data.frame(classification=BGC, colour = BGCcolors[match(BGC,BGCcolors$classification), 2])
  #   } else {
  #     BGC <- dat$BGC
  #     col <- merge(data.frame(becvar=as.character(levels(BGC))),cols)
  #     col$colour <- rgb(col$r/255,col$g/255,col$b/255)
  #   }
  #   
  #   leafletProxy("map") %>%
  #     
  #     
  #     addPolygons(data=dat, stroke = FALSE, smoothFactor = 0.2, fillOpacity = input$transparency,
  #                 color = ~col$colour,popup = paste("1961-1990:",dat.ref$BGC, "<br>", "1991-2019:",dat.cur$BGC, "<br>", "Projected:",dat.proj$BGC),
  #                 # color = ~col$colour,popup = dat$BGC
  #                 # highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE) #highlight doesn't work for some reason
  #     )
  # })
  
  # ## Alternative map code with layer control (slower load but faster switching between historical and projected)
  # output$map <- renderLeaflet({
  #   
  #   dat.proj <- st_transform(st_read(paste("data/Future_",studyArea,".gpkg", sep=""), layer=paste(input$gcm,rcps[as.numeric(input$rcp)],proj.years[as.numeric(input$proj.year)],sep = "_")), st_crs("+init=epsg:4326"))
  #   
  #   if(input$maptype==1) dat <- dat.ref
  #   if(input$maptype==2) dat <- dat.cur
  #   if(input$maptype==3) dat <- dat.proj
  #   
  #   if(input$colscheme==1){
  #     BGC.ref <- dat.ref$BGC
  #     zone <- rep(NA, length(BGC.ref))
  #     for(i in zones){ zone[grep(i,BGC.ref)] <- i }
  #     BGC.ref <- factor(zone)
  #     col.ref <- data.frame(classification=BGC.ref, colour = BGCcolors[match(BGC.ref,BGCcolors$classification), 2])
  #   } else {
  #     BGC.ref <- dat.ref$BGC
  #     col.ref <- merge(data.frame(becvar=as.character(levels(BGC.ref))),cols)
  #     col.ref$colour <- rgb(col.ref$r/255,col.ref$g/255,col.ref$b/255)
  #   }
  #   
  #   if(input$colscheme==1){
  #     BGC.cur <- dat.cur$BGC
  #     zone <- rep(NA, length(BGC.cur))
  #     for(i in zones){ zone[grep(i,BGC.cur)] <- i }
  #     BGC.cur <- factor(zone)
  #     col.cur <- data.frame(classification=BGC.cur, colour = BGCcolors[match(BGC.cur,BGCcolors$classification), 2])
  #   } else {
  #     BGC.cur <- dat$BGC
  #     col.cur <- merge(data.frame(becvar=as.character(levels(BGC))),cols)
  #     col.cur$colour <- rgb(col.cur$r/255,col.cur$g/255,col.cur$b/255)
  #   }
  #   
  #   if(input$colscheme==1){
  #     BGC.proj <- dat.proj$BGC
  #     zone <- rep(NA, length(BGC.proj))
  #     for(i in zones){ zone[grep(i,BGC.proj)] <- i }
  #     BGC.proj <- factor(zone)
  #     col.proj <- data.frame(classification=BGC.proj, colour = BGCcolors[match(BGC.proj,BGCcolors$classification), 2])
  #   } else {
  #     BGC.proj <- dat.proj$BGC
  #     col.proj <- merge(data.frame(becvar=as.character(levels(BGC.proj))),cols)
  #     col.proj$colour <- rgb(col.proj$r/255,col.proj$g/255,col.proj$b/255)
  #   }
  #   
  #   leaflet() %>% 
  #     addTiles() %>% 
  #     addProviderTiles("Esri.WorldImagery", group = "Satellite view") %>%
  #     addProviderTiles("Esri.WorldTerrain", group = "Terrain only") %>%
  #     addProviderTiles("Esri.WorldTopoMap", group = "Base map") %>% 
  #     addPolygons(data=dat.ref, stroke = FALSE, smoothFactor = 0.2, fillOpacity = input$transparency,
  #                 # color = ~col$colour,popup = paste("1961-1990:",dat.ref$BGC, "<br>", "1991-2020:",dat.cur$BGC, "<br>", "Projected:",dat.proj$BGC),
  #                 color = ~col.ref$colour,popup = dat.ref$BGC, group = "Reference (1961-1990)"
  #                 # highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE) #highlight doesn't work for some reason
  #     )%>%
  #     addPolygons(data=dat.cur, stroke = FALSE, smoothFactor = 0.2, fillOpacity = input$transparency,
  #                 # color = ~col$colour,popup = paste("1961-1990:",dat.ref$BGC, "<br>", "1991-2020:",dat.cur$BGC, "<br>", "Projected:",dat.proj$BGC),
  #                 color = ~col.cur$colour,popup = dat.cur$BGC, group = "Recent (1991-2019)"
  #                 # highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE) #highlight doesn't work for some reason
  #     )%>%
  #     addPolygons(data=dat.proj, stroke = FALSE, smoothFactor = 0.2, fillOpacity = input$transparency,
  #                 # color = ~col$colour,popup = paste("1961-1990:",dat.ref$BGC, "<br>", "1991-2020:",dat.cur$BGC, "<br>", "Projected:",dat.proj$BGC),
  #                 color = ~col.proj$colour,popup = dat.proj$BGC, group = "Projected future"
  #                 # highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE) #highlight doesn't work for some reason
  #     )%>%
  #     addLayersControl(
  #       baseGroups = c("Base map", "Terrain only", "Satellite view"),
  #       overlayGroups = c("Reference (1961-1990)", "Recent (1991-2019)", "Projected future"),
  #       options = layersControlOptions(collapsed = FALSE)
  #     ) 
  # },
  # )
  
  
  # leaflet() %>%
  #   addTiles() %>%
  #   addProviderTiles("Esri.WorldImagery", group = "Satellite view") %>%
  #   addProviderTiles("Esri.WorldTerrain", group = "Terrain only") %>%
  #   addProviderTiles("Esri.WorldTopoMap", group = "Base map") %>%
  #   addLayersControl(
  #     baseGroups = c("Base map", "Terrain only", "Satellite view"),
  #     options = layersControlOptions(collapsed = FALSE)
  #   )%>%
  #   addPolygons(data=dat, stroke = FALSE, smoothFactor = 0.2, color="red",  fillOpacity = 0.4, opacity = 1, layerId=dat$BGC
  #   )%>%
  #   addOpacitySlider(layerId = dat$BGC)
  
  output$scatterPlot <- renderPlot({
    
    proj.year <-  proj.years[as.numeric(input$proj.year)]
    rcp <- rcps[as.numeric(input$rcp)]
    var1 <- input$var1
    var2 <- input$var2
    variable.type1 <- variable.types[which(variables==var1)]
    variable.type2 <- variable.types[which(variables==var2)]
    
    s <- which(scenario[,1]%in%gcms & scenario[,2]== rcp & scenario[,3]== proj.year )
    x <- studyArea.change[s, which(variables==var1)]
    y <- studyArea.change[s, which(variables==var2)]
    x.all <- studyArea.change[scenario[,1]%in%gcms, which(variables==var1)]
    y.all <- studyArea.change[scenario[,1]%in%gcms, which(variables==var2)]
    
    # xlim=if(variable.type1=="ratio") range(x) else if(min(x)<0) range(x) else c(0, max(x))
    # ylim=if(variable.type2=="ratio") range(y) else if(min(y)<0) range(y) else c(0, max(y))
    xlim=range(x.all)*c(if(min(x.all)<0) 1.1 else 0.9, if(max(x.all)>0) 1.1 else 0.9)
    ylim=range(y.all)*c(if(min(y.all)<0) 1.1 else 0.9, if(max(y.all)>0) 1.1 else 0.9)
    
    par(mar=c(3,4,0,.2), mgp=c(1.25, 0.25,0), cex=1.1)
    plot(x,y,col="white", tck=0, xaxt="n", yaxt="n", xlim=xlim, ylim=ylim, ylab="",
         xlab=paste("Change in", variable.names$Variable[which(variable.names$Code==var1)]), 
    )
    par(mgp=c(2.5,0.25, 0))
    title(ylab=paste("Change in", variable.names$Variable[which(variable.names$Code==var2)]))
    lines(if(variable.type1=="ratio") c(1,1) else c(0,0), c(-99,99), lty=2, col="gray")
    lines(c(-99,99), if(variable.type2=="ratio") c(1,1) else c(0,0), lty=2, col="gray")
    
    x1 <- studyArea.change.9119[which(variables==var1)]
    y1 <- studyArea.change.9119[which(variables==var2)]
    points(x1,y1, pch=16, col="gray", cex=5)
    text(x1,y1, "1991-2019", cex=1.5, font=2, pos=4, col="gray", offset=1.1)  
    
    colors = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)][-1]
    set.seed(2)
    ColScheme <- c(brewer.pal(n=12, "Paired"),sample(colors,length(unique(scenario[,1]))-12))
    ColScheme[11] <- "blue"
    ColScheme <- ColScheme[which(unique(scenario[,1])%in%gcms)]

    points(x.all,y.all, pch=21, bg="darkgray", cex=2)
    
        for(gcm in gcms){
      i=which(gcms==gcm)
      points(x[i],y[i], pch=21, bg=ColScheme[i], cex=4)
      text(x[i],y[i], mods[i], cex=0.7, font=2)
    }
    
    # scenario.select <- nearPoints(studyArea.proj[s,], input$scatterPlotSelection, xvar = var1, yvar = var2, maxpoints = 1)
    # Ystr <- strsplit(as.character(scenario.select), "_")
    # gcm.select <- scenario.select[1]
    # i=which(gcms==gcm.select)
    # points(x[i],y[i], pch=21, bg=ColScheme[i], cex=8)
    # text(x[i],y[i], mods[i], cex=2, font=2)
    # 
    if(variable.type1=="ratio"){
      axis(1, at=seq(-99,99,0.1), labels=paste(round(seq(-99,99,0.1)*100-100), "%", sep=""), tck=0)
    } else axis(1, at=seq(round(range(x.all)[1]-1), round(range(x.all)[2]+1), round(diff(range(x.all))/7, if(diff(range(x.all))<3.5) 1 else 0)), labels=seq(round(range(x.all)[1]-1),round(range(x.all)[2]+1), round(diff(range(x.all))/7, if(diff(range(x.all))<3.5) 1 else 0)), tck=0)
    if(variable.type2=="ratio"){
      axis(2, at=seq(-99,99,0.1), labels=paste(round(seq(-99,99,0.1)*100-100), "%", sep=""), las=2, tck=0)
    } else axis(2, at=seq(round(range(y.all)[1]-1), round(range(y.all)[2]+1), round(diff(range(y.all))/7, if(diff(range(y.all))<3.5) 1 else 0)), labels=seq(round(range(y.all)[1]-1),round(range(y.all)[2]+1), round(diff(range(y.all))/7, if(diff(range(y.all))<3.5) 1 else 0)), las=2, tck=0)
    
    # legend("bottomleft", legend = c("2001-2019", "2011-2019"), title = "Observed change", pch=c(16, 1), pt.cex=2, col="red", bty="n")
    
  },
  # height=reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*0.4,0))
  )
  
  output$timeSeries <- renderPlot({
    
    proj.year <-  proj.years[as.numeric(input$proj.year)]
    rcp <- rcps[as.numeric(input$rcp)]
    var <- input$var1
    variable.type <- variable.types[which(variables==var)]
 
    s <- which(scenario[,1]%in%gcms & scenario[,2]== rcp & scenario[,3]== proj.year )
    x <- studyArea.proj[s, which(names(studyArea.proj)==var)]
    x.ref <- studyArea.hist[which(names(studyArea.hist)==var)]
    ts <- studyArea.ts[which(studyArea.ts$Year>1949), which(names(studyArea.ts)==var)]
    # ts.ref <- studyArea.6190[which(studyArea.6190$id1==studyArea), which(names(studyArea.6190)==var)]
    
    colors = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)][-1]
    set.seed(2)
    ColScheme <- c(brewer.pal(n=12, "Paired"),sample(colors,length(unique(scenario[,1]))-12))
    ColScheme[11] <- "blue"
    ColScheme <- ColScheme[which(unique(scenario[,1])%in%gcms)]

    # # adjust time series to match reference normal (excluded bcause i didn't include the ts.ref data). is just cosmetic and fudgy
    # if(variable.type=="ratio"){
    #   ts <- ts*x.ref/ts.ref
    # } else ts <- ts+x.ref-ts.ref
    
    r <- range(c(x, ts))
    
    par(mar=c(3,4,1,0.2), mgp=c(2,0.25,0), cex=1.1)
    plot(1950:2105, rep(NA, length(1950:2105)), ylim=r, xaxt="n", xaxs="i", xlab="", tck=0, las=2,
         ylab=paste(variable.names$Variable[which(variable.names$Code==var)]))
    
    proj.year.start <- 1981+30*which(proj.years==proj.year)
    proj.year.end <- 2010+30*which(proj.years==proj.year)
    
    rect(proj.year.start, min(x), proj.year.end, max(x), col="gray", border=F)
    lines(c(0,9999), rep(x.ref,2), col="grey", lwd=2, lty=2)
    lines(c(1961,1990), rep(x.ref,2), col=1, lwd=4)
    lines(1950:2019, ts, col="red", lwd=2)
    
    axis(1, at=seq(1960, 2100, 20), labels=seq(1960, 2100, 20), tck=0)
    
    
    gcms.sort <- gcms[order(x)]
    x.sort <- x[order(x)]
    for(gcm in gcms.sort){
      i <- which(gcms.sort==gcm)
      lines(c(proj.year.start, proj.year.end), rep(x.sort[i],2), col=ColScheme[which(gcms==gcm)], lwd=2)
      position <- if(proj.year==proj.years[2]) rep(0:2, times=100) else rep(0:4, times=100)
      side <- if(proj.year==proj.years[1]) rep(4,200) else {if(proj.year==proj.years[2]) rep(c(2,4), times=100) else rep(2,200)}
      lines(if(side[i]==4) c(proj.year.end, proj.year.end+position[i]*11) else c(proj.year.start, proj.year.start-position[i]*11) , rep(x.sort[i],2), col=ColScheme[which(gcms==gcm)], lty=2)
      # points(if(side[i]==4) {proj.year.end+position[i]*8} else {proj.year.start-position[i]*8}, x.sort[i], pch=21, bg=ColScheme[which(gcms==gcm)], cex=if(gcm==gcms[1]) 5.5 else 4)
      text(if(side[i]==4) proj.year.end+position[i]*11 else proj.year.start-position[i]*11, x.sort[i], mods[which(gcms==gcm)], col=ColScheme[which(gcms==gcm)], pos=side[i], font=2, cex=0.9, offset=0.1)
    }
    
  },
  # height=reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*0.4,0))
  )
  
  output$table <- DT::renderDataTable({
    DT::datatable(modelMetadata, 
                  options = list(pageLength = dim(modelMetadata)[1]), 
                  rownames= FALSE, 
                  caption = 'Model Metadata. Global model statistics are quoted from Forster et al. (2013, Journal of Geophysical Research): TCR is transient climate response (temperature change in response to a 1%/yr increase in CO2 at time, at point of doubling CO2), ECS is equilibrium climate sensitivity (temperature change in response to an instant doubling of CO2), and deltaT is global mean surface temperature change since preindustrial for RCP4.5 in the 2090s. All values are in degrees Celsius.'
    )
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)



# proj.year <- 2055
# rcp <- "rcp45"
# gcm <- "CESM1-CAM5"
# var1 <- "Tave_sm"
# var2 <- "PPT_sm"

# install.packages(c('plyr','shiny','dplyr','forcats','leaflet','ukpolice','opencage','ggplot2','highcharter','plotly','leaflet.extras','geojsonio'))

library(plyr)
library(shiny)
library(dplyr)
library(forcats)
library(leaflet)
library(ukpolice)
library(opencage)
library(ggplot2)
library(highcharter)
library(plotly)
library(leaflet.extras)
library(geojsonio)



# import data
states <- geojsonio::geojson_read("https://rstudio.github.io/leaflet/json/us-states.geojson", what = "sp")
homicide_data <- read.csv("https://raw.githubusercontent.com/washingtonpost/data-homicides/master/homicide-data.csv")
fatal_police_shootings <- read.csv("https://raw.githubusercontent.com/washingtonpost/data-police-shootings/master/fatal-police-shootings-data.csv")
killed <- read.csv("Officers Feloniously Killed.csv")
uspopulation <- read.csv("USpopulation.csv")
usmap <- states

# USER INTERFACE #

# Tab 1: Main Dashboard
intro_tab <- tabPanel(
  'Main Dashboard',
  
  h2('Data on killings of civilians and law enforcement officers in the United States'),
  
  # Grid Layout
  fluidRow(
    
    column(5,
           h4('Number of officers feloniously killed and number of civilian killed by police between 2015 and 2021'),
           plotlyOutput('plot_sc', width = "75%" , height = 355),
           h4('Choropleth map of civilians shot and killed by police between 2015 and 2021'),
           leafletOutput("shootmap",width = "90%",height = 300),
    ),
    column(1,
           br()
    ),       
    column(6,
           plotlyOutput('plot_b'),

    )
  ),
  br(),
  br(),
  p('Data source: ',
    a(href = 'https://s3-us-gov-west-1.amazonaws.com/cg-d4b776d0-d898-4153-90c8-8336f86bdfec/LEOKA/downloads/LEOKA-Felonious-2021.zip', 'Officers Feloniously Killed'),
    ",",
    a(href = 'https://github.com/washingtonpost/data-police-shootings', 'Fatal shooting in the United States by a police officer(The Washington Post’s database)')),
  p('Data source: ',
    a(href = 'https://github.com/washingtonpost/data-homicides', 'Criminal homicides over the past decade in 50 of the largest American cities by The Washington Post'),
    ",",
    a(href = "https://www2.census.gov/programs-surveys/decennial/2020/data/apportionment/population-change-data-table.pdf","State population data by U.S. Census Bureau")),
  
  
)


# Tab 2.1: Maps about police
sidebar_content <- sidebarPanel(
  h4('Select the type of dataset'),
  selectInput(
    'type',
    label = 'Type',
    choices = c('Ratio of civilians shot and killed by police to total population'='police_shooting',
                'Number of civilians shot and killed by police' = 'shooting2',
                'Law enforcement officers died as a result of felonious incidents'='police_killed'
                ),
    selected = 'police_shooting'
  )
)

# main 
main_content <- mainPanel(
  leafletOutput("map",height = 800),
)
  
  

maps_tab <- tabPanel(
  'Map about police killing and being killed',
  htmlOutput("tit"),
  sidebarLayout(
    sidebar_content, main_content
  )
)

#Tab2.2 homicide 
sidebar_content2 <- sidebarPanel(
  h4("Criminal homicides in 50 of the largest American cities"),
  selectInput(
    'city',
    label = 'City',
    choices = c(sort(unique(homicide_data$city))),
    selected = sort(unique(homicide_data$city))[1]
  ),
  
  selectInput(
    'year',
    label = 'Year',
    choices = c('2010'='X2017',
                '2011'='X2011',
                '2012'='X2012',
                '2013'='X2013',
                '2014'='X2014',
                '2015'='X2015',
                '2016'='X2016',
                'All'),
    selected = 'X2010'
  )
)

map_content <- mainPanel(
 htmlOutput("homicide_title"),
 leafletOutput("homicidemap",height = 800)
)

map_tab2 <- tabPanel(
  "Location of criminal homicides in the largest American cities",
  sidebarLayout(
    sidebar_content2, map_content
  )
)


#Tab3 Officers Feloniously Killed
sidebar_3 <- sidebarPanel(
  h4("Officers feloniously killed by states"),
  selectInput(
    'states',
    label = 'States',
    choices = c(sort(unique(killed$State))),
    selected = sort(unique(killed$State))[30]
  ),
)

tab3 <- tabPanel(
    "Bar chart",
  sidebarLayout(
    sidebar_3,  mainPanel(plotlyOutput("plot_3_1",width = "90%"))
  )
)





ui <- navbarPage(
  'Crime and shootings in the United States',
  intro_tab,
  tab3,
  navbarMenu("Maps",
             maps_tab,
             map_tab2)
)




# SHINY SERVER #

# Merge two fatal_police_shootings data and police killed data
fatal_police_shootings$date <- as.Date(fatal_police_shootings$date)
police_shootings_2015_2021 <- fatal_police_shootings %>% filter(between(date, as.Date("2015-01-01"),as.Date("2021-12-31")))

fff <- as.data.frame(table(police_shootings_2015_2021$state))  
ss <- as.data.frame(table(homicide_data$city))


names(fff) <- c("state","police_shoot")
fff$police_killed <- NA
fff$State <-NA

for (i in 1:as.integer(lengths(fff[1]))) {
  for (j in 1:as.integer(lengths(killed[1]))){
    if (fff[i,1] == killed[j,2]){
      fff[i,3] <- sum(killed[j,6:12])
      fff[i,4] <- killed[j,1]
    }
  }
}
fff <- fff %>% filter(!is.na(police_killed))

names(ss) <- c("city","homicides")
ss$police_shoot <- NA


server <- function(input, output, session) {
  # Delete Puerto Rico
  states@data <- states@data[-52,]
  # combine police_shoot with geographic information data
  for (i in 1:as.integer(lengths(fff[1]))) {
    for (j in 1:as.integer(lengths(states@data[1]))){
      if (fff[i,4] == states@data[j,2]){
        states@data[j,3] <- fff[i,2]
      }
    }
  }
  names(states@data)[3] <- "police_shoot"
  
  
  # the title of the map
  output$tit <- renderUI(
    {
      if (input$type == "police_killed"){
        h3("Number of officers feloniously killed by states between 2015 and 2021")
      }
      else{
        if (input$type == "police_shooting"){
          h3("Ratio of deaths by police shootings to total population by state between 2015 and 2021")
        }
        else{
          h3("Number of deaths by police shootings by state between 2015 and 2021")
        }
      }
    }
    
  )
  
  output$homicide_title <- renderUI(
    {
      for (i in sort(unique(homicide_data$city))){
        if (input$states == i){
          h3(paste("Location of homicides in",input$states)) 
        }
      }
    }
    
  )
  
  # Choropleth map of civilians shot and killed by police in each states
  output$shootmap <- renderLeaflet({
    pal <- colorBin("Reds", domain = states$police_shoot, bins = c(0, 100, 300, 500, 1000, Inf))
    
    labels <- sprintf(
      "<strong>%s</strong><br/> <strong>%g</strong> people shot and killed by police",
      states$name, states$police_shoot
    ) %>% lapply(htmltools::HTML)
    
    leaflet(states) %>%
      setView(-98, 40, 3.5) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light")) %>%
      addPolygons(
        fillColor = ~pal(police_shoot),
        color = "white",
        weight = 1.5,
        fillOpacity = 1,
        highlightOptions = highlightOptions(
          weight = 4,
          color = "#555555",
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      addLegend(pal = pal, values = ~density, opacity = 0.7, title = NULL,
                position = "bottomleft") %>%
      setMapWidgetStyle(list(background= "white"))
  })
  
  
  # bar chat of number of homicide in different cities
  output$plot_b <- renderPlotly({
    p <- ggplot(data= (ss %>% mutate(name = fct_reorder(ss$city, ss$homicides))) ,aes_string(x='homicides',
                                 y='name')) +
      geom_bar(stat='identity', width=0.8) +
      labs(x='Number of homicides', y=" ") +
      theme(panel.background = element_blank(),axis.ticks = element_blank()) +
      ggtitle('Number of criminal homicides in 50 of the largest American cities between 2010 and 2016') +
      scale_x_continuous(labels = function(x) format(x,big.mark = ","))
    ggplotly(p,height = 720,tooltip = c("homicides"))
  })
  
  names(fff) <- c("State",'civilian_killed_by_police',"officers_feloniously_killed")
  
  # scatter plot of Number of civilian killed by police and Number of officers feloniously killed
  output$plot_sc <- renderPlotly({
    p <- ggplot(data= fff ,aes(label=State)) +
      geom_point(aes(civilian_killed_by_police,officers_feloniously_killed),size = 2,color="#9999ff",alpha=0.5)+
      labs(x='Number of civilian killed by police', y="Number of officers feloniously killed") +
      theme(panel.background = element_blank())
    ggplotly(p)
  })
  
  


  
  # bar chart fo Officers feloniously killed in each states by year
  output$plot_3_1 <- renderPlotly({
    
    if (input$states != ""){
      state_killed <- filter(killed, State == input$states)
      
      linedata <- names(state_killed)
      years <- names(state_killed)
      for (i in 1:as.integer(length(state_killed[1,]))){
        linedata[i] <- as.integer(state_killed[1,i])
        years[i] <- gsub("X","",years[i])
      } 
      
      state_killed <- data.frame(years[3:12],linedata[3:12])
      names(state_killed) <- c("Year","Number_of_officers_killed")
    
      p <- ggplot(data= (state_killed %>% mutate(name = fct_reorder(state_killed$Number_of_officers_killed, state_killed$Year))) ,aes_string(x='Year',
                                                                                               y='Number_of_officers_killed')) +
      geom_bar(stat='identity', width=0.8, fill = "#66ccff") +
      labs(x='Year', y="Number of officers feloniously killed") +
      ggtitle(paste0("Number of law enforcement officers in ",input$states," died in the line of duty as a result of felonious incidents by year"))+
      theme(panel.background = element_blank(),axis.ticks = element_blank()) 
    ggplotly(p,tooltip = c("Number_of_officers_killed"))
   }
  })
    
    
  
  
  
  
 
  
  names(usmap@data)[3] <- "thedata"
  usmap@data <- usmap@data[-52,]
  usmapdata <- usmap@data
  output$map <- renderLeaflet({
    
    
      temp <- uspopulation
      # Combining police deaths data and geographic information
      if (input$type == "police_killed"){
        usmap@data <- usmapdata
        
        for (i in 1:as.integer(lengths(usmap@data[1]))) {
          for (j in 1:as.integer(lengths(killed[1]))){
            if (usmap@data[i,2] == killed[j,1]){
              usmap@data[i,3] <- sum(killed[j,6:12])
            }
          }
        }
        
        pal <- colorBin("OrRd", domain = usmap$thedata, bins = c(0,5,10,15,20,25,30,35,40,45))
        maplabel <- sprintf(
          "<strong>%s</strong><br/> %g law enforcement officers died as a result of felonious incidents",
          usmap$name, usmap$thedata
        ) %>% lapply(htmltools::HTML)
        
        
        
      }else{
        # Combining police shooting data and geographic information
        usmap@data <- usmapdata
        usmap@data$shootings <- NA
        usmap@data$population <- NA
        
        shooting_states = as.data.frame(table(police_shootings_2015_2021$state))
        
        for (i in 1:as.integer(lengths(uspopulation[1]))) {
          for (j in 1:as.integer(lengths(shooting_states[1]))){
            if (shooting_states[j,1] == uspopulation[i,2]){
              temp[i,3] <- shooting_states[j,2]/uspopulation[i,3] * 100
              temp[i,4] <- shooting_states[j,2]
              temp[i,5] <- uspopulation[i,3]
            }
          }
        }
        
        for (i in 1:as.integer(lengths(temp[1]))) {
          for (j in 1:as.integer(lengths(usmap@data[1]))){
            if (usmap@data[j,2] == temp[i,1]){
              usmap@data[j,3] <- temp[i,3]
              usmap@data[j,4] <- temp[i,4]
              usmap@data[j,5] <- temp[i,5] %/% 1000
            }
          }
        }
        
        
        if (input$type == "police_shooting"){
          
          
          maplabel <- sprintf(
            "<strong>%s</strong><br/> Approximately <strong>%g %% </strong>of the population has died as a result of police shootings between 2015 and 2021 <br/> <strong>%g </strong>civilians shot and killed by police between 2015 and 2021<br/> Population: about <strong>%g </strong>thousands",
            usmap$name, usmap$thedata,usmap$shootings,usmap$population
          ) %>% lapply(htmltools::HTML)
          
          pal <- colorBin("OrRd", domain = usmap$thedata, bins = c(0.0001,0.001,0.003,0.005,0.007))
        }
        
        else{
          usmap@data$thedata <- usmap@data$shootings
          
          maplabel <- sprintf(
            "<strong>%s</strong><br/> <strong>%g </strong>civilians shot and killed by police between 2015 and 2021",
            usmap$name, usmap$thedata
          ) %>% lapply(htmltools::HTML)
          
          pal <- colorBin("OrRd", domain = usmap$thedata, bins = c(0,50,100,500,1000,1500))
        }
        
        
      }
      
      
      leaflet(usmap) %>%
        setView(-100, 50, 4) %>%
        addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
        addPolygons(
          fillColor = ~pal(thedata),
          color = "black",
          weight = 1.3,
          fillOpacity = 1,
          highlightOptions = highlightOptions(
            weight = 4,
            color = "#9999FF",
            bringToFront = TRUE),
          label = maplabel,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto")) %>%
        addLegend(pal = pal, values = ~density, opacity = 0.7, title = NULL,
                  position = "bottomleft")
      
      
    
      
    })
  
   # show positions of homicides 
   output$homicidemap <- renderLeaflet({
     if (input$city != ""){
       
     
     homicide_city <- filter(homicide_data, city == input$city)
     
     
     homicide_city$reported_date <- as.Date(as.character(homicide_city$reported_date), format = '%Y%m%d')
     
    
     if (input$year == "All"){
       homicide_year <- homicide_city
     }else{
       homicide_year <- homicide_city %>% filter(between(reported_date, as.Date(paste0(substr(input$year, 2,5),"-01-01")),as.Date(paste0(substr(input$year, 2,5),"-12-31"))))
     }
     
    
     leaflet(filter(homicide_year, city == input$city)) %>%
      addProviderTiles(providers$CartoDB.Voyager) %>%
      addAwesomeMarkers(~lon, ~lat,
                       icon=~awesomeIcons(icon=case_when(disposition == 'Closed by arrest' ~ 'ios-close',
                                                         TRUE ~ 'flag'),
                                          library='ion',
                                          markerColor=case_when(disposition == 'Closed by arrest' ~ 'red',
                                                                TRUE ~ 'blue'),
                                          iconColor=case_when(disposition == 'Closed by arrest' ~ '#000000',
                                                              TRUE ~ '#ffffff')),
                       label=~disposition,
                       clusterOptions = markerClusterOptions())
     }
  })
    
  
  
}


# RUN SHINY #
shinyApp(ui, server)


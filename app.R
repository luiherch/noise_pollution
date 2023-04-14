#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(ggridges)
library(viridis)
library(tidyr)
library(hrbrthemes)
library(leaflet)
library(rsconnect)
load('data/rdata/data_df.RData')
load('data/rdata/stations.RData')
icons <- awesomeIcons(
  icon = 'microphone',
  iconColor = 'black',
  library = 'fa',
  markerColor = 'rgb(248,60,0)'
)

df<- na.omit(df)


# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "united"),
  navbarPage('Noise Pollution',
    tabPanel('Introduction',
  includeHTML("data/html/texto.html"),
  leafletOutput('map'),
  br(),
  hr()
), tabPanel('Analysis',
            theme = bslib::bs_theme(bootswatch = "united"),
            h2('Data visualization'),
            p('In the following panel, the monthly time series of every measurement station can be visualized. As measure stations measure by different time hours, 
    it can be seen the different noise intensities throughout each time of the day (Day, Evening & Night). The slider allows having a fine grain on
    the period that wants to be visualized.'),
            sidebarLayout(
              sidebarPanel(
                
                selectInput(
                  inputId = "select_station",
                  label = "Measure station", 
                  choices = df$Names,
                  selected = 2
                ),
                
                sliderInput(
                  "slider_date_range",
                  "Date range",
                  timeFormat = "%b-%y",
                  min = as.Date('2020-01-01'),
                  max = Sys.Date(),
                  value =  c(as.Date('2021-05-01'), Sys.Date())),
                
                radioButtons("radio_time_period", label = "Noise time period",
                             choices = list("Day" = 'Ld', "Evening" = 'Le', "Night" = 'Ln'), 
                             selected = 'Ld')
              ),
              mainPanel(
                plotOutput('time_series')
              )
            ),
            p('Focusing on ',em('Paseo de recoletos'), ' it can be seen how there is much more noise 
    in the day when it is winter than in the evening and night. However, for the remaining months, 
    it is right the opposite. This may be caused because when it is winter, it is very cold in the 
    evening and at night, so people try to meet during the day. In the other months where
    the weather is much better, it seems like people try to meet more during the evening and the night, as 
    temperature decreases along the day and it is much more comfortable to meet in those moments. This premise
    would require a further study to see the real impact of temperature on environmental noise, as maybe if the 
    temperature is lower, people tend to use cars and eventually noise actually increases.'),
            p('In the next plot the noise of every place in different time hours can be compared. The evolution along stations
    can be seen with the play button.'),
            sidebarLayout(
              mainPanel(
                plotOutput('ridges')
              ),
              sidebarPanel(
                selectInput(
                  inputId = "select_station_2",
                  label = "Measure station", 
                  choices = df$Names,
                  selected = 2
                ),
                sliderInput(
                  "slider_date_range_2",
                  "Date range",
                  timeFormat = "%b-%y",
                  min = as.Date('2020-01-01'),
                  max = as.Date('2022-04-01'),
                  step=31*3,
                  value =  c(as.Date('2020-01-01'), as.Date('2020-04-01')),
                  animate = animationOptions(interval = 1500, loop = T))
              )
            ),
            p('For example, focusing on ', em('Vallecas'), ' it seems that noise levels of day and evening are
    somehow correlated, as the values are always very similar regarding the station. This would also
    require a further study to discover which are the underlying reasons for this phenomenon.'),
            p('In the next lollipop plot, several statistics can be seen. The most interesting one is the median, as it can
    be seen which are the zones that are usually less noisy. Max an min shows a general idea
    of which is the maximum amount of noisiness every zone has reached and also  the minimum threshold. 
    The measure used for this plot is LAeq24.'),
            sidebarLayout(
              sidebarPanel(
                radioButtons("radio_lollipop", label = "Summary",
                             choices = list("Median" = 'median',"Max" = 'max',  "Min" =  "min"), 
                             selected = 'median')
              ),
              mainPanel(
                plotOutput('lollipop', width = "100%")
              )
            ),
            p('This plot shows that ', em('Casa de campo'), ' is the most silent place in Madrid. This seems reasonable, as
    it is the largest public park in the city and situated in the west, far from diary activity. Moreover, 
    the minimum value is really close to the median, indicating that it is usually as quiet as possible apart
    from probably common nature sounds.'),
            p('In contrast, ',em('Gregorio Marañón'), 'is one of the noisiest areas. The reason could be because
    it is located near Castellana, which always contains very high volumes of traffic. Moreover, it is one
    of the largest hospitals in Madrid. Hence, it is a very visited place not only by patients but also by 
    ambulances which are known to be one of the noisiest vehicles.'),
            h2('Conclusions'),
            p('Environmental noise is an invisible topic that threatens the life quality of everybody. Being aware of this 
    issue is the first step for towards building a noise-free lifestyle society.'),
            p(em('Gregorio Marañón'),' is one of the noisiest zones of Madrid even though there is a hospital there. This is counterproductive, as hospitals
    are full of patients who need rest and noise is not only not letting them rest properly, but also compromising
    their health state. In contrast, green zones tend to be quieter than the rest.'),
            p('There also seems to be a correlation between the station and the time of the day the noise occurs. Temperature may 
    be the reason for this.'),
            p('Each time more people is aware of noise pollution and there are already many improvement proposals for reducing the amount of noise in urban places such as:'),
            tags$ul(
              tags$li('Installing low-noise asphalt on roads and using quiet tyres on public transport vehicles.'),
              tags$li('Setting up acoustic absorbing barriers/panels along the roads.'),
              tags$li('Encouring the implementation of electric vehicles and bicycles.'),
              tags$li('Creating more green zones')
            )),
  tabPanel('About',
           includeHTML("data/html/texto2.html"))
))


# Define server logic required to draw a histogram
server <- function(input, output) {
  
    output$map <- renderLeaflet({
      stations %>%
        leaflet(stations) %>% addProviderTiles(providers$CartoDB.Positron) %>%
        addAwesomeMarkers(~LONGITUD_ED50, ~LATITUD_ED50, popup = paste0("<strong>",stations$Names,"</strong>","<br>",stations$Dirección), icon=icons)
    })
    
    output$time_series<- renderPlot({
      df %>% filter(Names==input$select_station) %>%
        filter(Fecha >= input$slider_date_range[1] & Fecha <= input$slider_date_range[2]) %>%
        ggplot(aes(x=Fecha, y=get(input$radio_time_period))) +
          geom_line( color="orangered") + 
          geom_point() +
          xlab("") +
          ylab(input$radio_time_period)+
          ggtitle('Noise levels (dBA)')+
          theme_light()+
          theme(axis.text.x=element_text(angle=60, hjust=1))
    })
    
    output$ridges <- renderPlot({
      df %>%
        select(Fecha, Names, Ld, Le, Ln) %>%
        gather('Daytime','Measure',-Fecha, -Names) %>%
        filter(Names==input$select_station_2) %>% 
        filter(Fecha >= input$slider_date_range_2[1] & Fecha <= input$slider_date_range_2[2]) %>%
        ggplot(aes(x = Measure, y = Daytime, fill = ..x..)) +
          geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
          scale_fill_viridis(name = "Measure", option = "C") +
          labs(title = 'dBA densities') +
          theme_light() +
          theme(
            legend.position="none",
            panel.spacing = unit(0.1, "lines"),
            strip.text.x = element_text(size = 8)
          )
    })
    
    output$lollipop <- renderPlot({
      col_sel = "#FF7B1A"
      if(input$radio_lollipop == "max"){
        col_sel = "red"
      } else if(input$radio_lollipop == "min"){
        col_sel = "blue"
      }
      df %>% 
        na.omit()%>%
        select(Names, LAeq24)%>%
        group_by(Names) %>%
        summarise(median = median(LAeq24), mean=mean(LAeq24), max=max(LAeq24), min=min(LAeq24),n = n())%>%
        ggplot( aes(x=Names, y=get(input$radio_lollipop))) +
        geom_segment( aes(x=Names ,xend=Names, y=0, yend=get(input$radio_lollipop)), color="grey") +
        geom_point(size=4, color=col_sel) +
        coord_flip() +
        theme_light()+
        theme(
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.position="none"
        ) +
        ylab("LAeq24") +
        xlab("Stations")+
        ggtitle(paste(input$radio_lollipop, ' values for LAeq24'))+
        ylim(0,80)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

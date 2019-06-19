#Installing Packages
list.of.packages <- c("data.table","date","gdata","lubridate","stringr","shiny","ggplot2","rlang",
                      "leaflet","tidyverse","plotly","sqldf","ggthemes","stringr")
#new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#if(length(new.packages)) install.packages(new.packages,dependencies = TRUE)

#Loading Packages
invisible(lapply(list.of.packages, library, character.only = TRUE))

#Loading Dataset
df <- fread("traffic-collision-data-from-2010-to-present.csv")

#Converting Character to Date
df$"Date Reported" <- as.Date(df$"Date Reported")

#Filtering 2018 Data
df2 <- df[year(as.Date(df$"Date Reported", format = "%m/%d/%Y"))==2018]

#Function to Extract the hour of the day
str_hr <- function(x) substr(str_pad(toString(x), 4, 'left', '0'), 1, 2)

#Creating a new column for the hour of the Day
df2$Time_hr <- lapply(df2$`Time Occurred`,str_hr)
df2$Time_hr <- as.character(df2$Time_hr)
bar_plot_df <- sqldf("select Time_hr as Hour_of_the_Day, count(Time_hr) as Count_of_Traffic_Collisions  from df2
                     group by Hour_of_the_Day")

#Extracting Latitudes and Longitudes from Location
df2 <- df2 %>% separate(Location, c('Latitude', 'Longitude'), sep=",")
df2$Latitude <- as.numeric(gsub("[A-Za-z':{} ]","",df2$Latitude))
df2$Longitude <- as.numeric(gsub("[A-Za-z':{} ]","",df2$Longitude))

#Creating new DataFrames with Area and Coordinates
df3 <- df2[,c("Area Name","Latitude","Longitude")]
colnames(df3)[colnames(df3)=="Area Name"] <- "AreaName"
df_map <- sqldf("select distinct AreaName,avg(latitude) as Latitude,avg(longitude) as Longitude from df3 group by areaname")

#Restructuring DataFrame for HeatMap
df4 <- df2[,c("Area Name","Time_hr")]
df4$Time_hr <- as.numeric(df4$Time_hr)
colnames(df4)[colnames(df4)=="Area Name"] <- "AreaName"
df5 <- sqldf('select AreaName,Time_hr,Count(Time_hr) as CountOfCollisions from df4
             group by AreaName,Time_hr')

#UI
ui <- fluidPage(
  titlePanel(strong(h2("Traffic Collision Analysis - City of Los Angeles"),style = "font-family: 'Arial', 'Helvetica', 'sans-serif'")),
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("BarChart",br(),h4(strong("Frequency of collisions by the Time of the Day"),align="center"), plotOutput("BarChart")),
                tabPanel("Location", br(),h4(strong("Location of collisions in Los Angeles"),align="center"), leafletOutput("Maps")),
                tabPanel("Heat Map", br(), h4(strong("HeatMap Showing Location and Time of the collision"),align="center") ,plotOutput("HeatMap"))
    )
  )
)


#SERVER
server <- function(input, output) {
  
  
  output$BarChart <- renderPlot({ggplot(data=bar_plot_df, aes(x=Hour_of_the_Day, y=Count_of_Traffic_Collisions)) +
      geom_bar(stat="identity", fill="steelblue")+
      geom_text(aes(label=Count_of_Traffic_Collisions), vjust=1.6, color="white", size=3.5)+
      theme_minimal()+
      xlab("Time of the Collision (Hour)") + ylab("Total Number of Collisions") +
      labs(fill = "Number of Collisions")
  })
  
  output$Maps <- renderLeaflet({leaflet(df_map) %>% setView(lng=-118.5,lat=34, zoom = 9.5) %>%
      addTiles() %>%
      addMarkers(lng=~Longitude, lat=~Latitude, popup=~AreaName)
  })
  
  output$HeatMap <- renderPlot({ggplot(data = df5, aes(x = Time_hr, y = AreaName)) +
      geom_tile(aes(fill = CountOfCollisions)) +
      scale_fill_gradient(low = "white",high = "red") +
      theme_minimal()+
      xlab("Time of the Collision (Hour)") + ylab("Area Name") +
      labs(fill = "Number of Collisions") +
      theme(legend.position="right")
  })
}


#R Shiny Application
shinyApp(ui = ui, server = server)


library(tidyverse)
library(knitr)
library(fs)
library(graphics)
#this library needed to read in excel
library(readxl)
#these libraries will be needed to plot map
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(shiny)

#loading in data
construction <- read_excel("construction.xlsx", skip = 1)

#This theme will get rid of the gridlines but keep the legend
ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)



#chosing the state data map
states <- map_data("state")
#selecting texas as the state to use
texas <- states %>%
  subset(region == "texas")

#Putting in county lines. First I need all county lines and then I will subset for Texas to get Texas county lines
counties <- map_data("county")
tx_county <- counties %>%
  subset(region == "texas") %>%
  #going to filter for the Houston counties
  subset(subregion %in% c("harris", "galveston", "brazoria", "fort bend", "waller", "montgomery", "chambers"))

#looking at texas map
tx_base <- texas %>%
  ggplot(mapping = aes(x = long, y = lat, group = group)) + coord_fixed(1.3) + geom_polygon(color = "black", fill = "gray")

#This was before, I used gg2 as my Houston map, so I'm commenting this out

#gg1 <- tx_base + theme_nothing() +
#putting in county lines
#geom_polygon(data = tx_county, fill = NA, color = "white") + 
#geom_polygon(color = "black", fill = NA)
#this puts the state border on top


#plotting the points of new construction on the whole state map
#gg1 + geom_point(data = construction, aes(x = Longitude, y = Latitude, group = NA), color = "yellow", size = 1)

#trying to get Houston plot
hou_base <- tx_county %>%
  ggplot(mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + geom_polygon(color = "black", fill = "gray")

#repeating steps from earlier but Houston counties instead of whole state of Texas. Trying to get Houston plot continued
gg2 <- hou_base + ditch_the_axes + 
  geom_polygon(data = tx_county, fill = NA, color = "white") + geom_polygon(color = "black", fill = NA)

gg2 + geom_point(data = construction, aes(x = Longitude, y = Latitude, group = NA), color = "black", size = 1, alpha = .5) +
  geom_point(data = construction, aes(x = Longitude, y = Latitude, 
                                      color = Category, group = NA), size = .8, alpha = .5) + scale_color_manual(values = c("red", "blue", "yellow"))

submark <- function(st) {
  
  filter(st == construction$State)
  return(construction$Submarket)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Upcoming Houston Apartment Complexes"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     sidebarPanel(
       htmlOutput("section_selector"),
       htmlOutput("submarket_selector")
       ),
     
       
      
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   ))


# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$section_selector <- renderUI({
      # generate bins based on input$bins from ui.R
      selectInput(inputId = "section",
                  label = "Section",
                  choices = as.character(unique(construction$Section)),
                  selected = "Southwest Houston")
    
   })
   output$county_selector = renderUI({
     data_available = construction[construction$Section == input$section, "Submarket"]
     
     selectInput(inputID = "submarket", 
                 label = "Submarket",
                 choices = unique(data_available), 
                 selected = unique(data_available)[1])
     
     ouput$plot1 = renderPlot({})
   })
}

# Run the application 
shinyApp(ui = ui, server = server)


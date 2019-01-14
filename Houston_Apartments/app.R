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
library(shinythemes)
library(janitor)
library(kableExtra)
library(plyr)
library(dplyr)


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

class_df <- read_excel("Market Summary by Class - Houston 201811.xlsx", skip = 1) %>%
  clean_names()

class1_df <- class_df %>% 
  select(submarket, class, rental_rate_sf_mo) %>%
  filter(class %in% c("Class A", "Class B", "Class C", "Class D")) %>%
  spread(class, rental_rate_sf_mo)

class1_df <- rename(class1_df, c("submarket" = "Submarket"))

class_table <- kable(class1_df, caption = "Square Foot Monthly Rental Rate by Class") %>% kable_styling(full_width = F) %>%
  row_spec(2, bold = T)

construction_merge <- construction %>%
  select(Submarket, Section)

class_complete <- left_join(class1_df, construction_merge, by = "Submarket")

#putting in sections for those with NA
working_class <- class_complete %>%
  filter(!Section %in% c("Northwest Houston", "Northeast Houston", "Southwest Houston", "Southeast Houston", "Central Houston"))

class1_df <- class1_df %>%
  mutate(Section = case_when(Submarket %in% c("Sharpstown/ Westwood", "Woodlake/ Westheimer", "Westpark/ Bissonnet", "Westchase", "Braeswood/ Fondren SW", "Almeda/ South Main", "Galleria/ Uptown", "Energy Corridor/ CityCentre/ Briar Forest", "Alief", "Sugar Land/ Stafford/ Sienna", "Richmond/ Rosenberg" ) ~
                               "Southwest Houston",
                             
                             Submarket %in% c("Alvin/ Angleton/ Lake Jackson", "Hwy 288 South/ Pearland West", "U of H/ I-45 South", "Beltway 8 / I-45 South", "Pasadena/ Deer Park/ La Porte", "Friendswood/ Pearland East", "Clear Lake/ Webster/ League City", "Baytown", "Dickinson/ Galveston") ~
                               "Southeast Houston",
                             
                             Submarket %in% c("Montrose/ Museum/ Midtown", "Highland Village/ Upper Kirby/ West U", "Med Center/ Braes Bayou", "Heights/ Washington Ave", "Downtown") ~ "Central Houston",
                             
                             Submarket %in% c("Inwood/ Hwy 249", "Brookhollow/ Northwest Crossing", "Memorial/ Spring Branch", "Willowbrook/ Champions/ Ella", "Jersey Village/ Cypress", "Bear Creek/ Copperfield/ Fairfield", "Katy/ Cinco Ranch/ Waterside", "Tomball/ Spring", "Woodlands/ Conroe South", "Conroe North/ Montgomery") ~ 
                               "Northwest Houston",
                             
                             Submarket %in% c("Northeast Houston/ Crosby", "Greenspoint/ Northborough/ Aldine", "FM 1960 East/ IAH Airport", "I-10 East/ Woodforest/ Channelview", "I-69 North", "Northline", "Lake Houston/ Kingwood") ~ 
                               "Northeast Houston",
                             TRUE ~ "NA"))

#class1_df[is.na(class1_df)] <- 0

averaging_number <- class1_df %>%
  group_by(Section) %>%
  tally()

detach(package:plyr)

averaging <- class1_df %>%
  select(`Class A`, `Class B`, `Class C`, `Class D`, Section, Submarket) %>%
  group_by(Section) %>%
  arrange(Section) %>%
  group_by(Section) %>%
  summarise("Class A" = mean(`Class A`, na.rm = TRUE),
            "Class B" = mean(`Class B`, na.rm = TRUE),
            "Class C" = mean(`Class C`, na.rm = TRUE),
            "Class D" = mean(`Class D`, na.rm = TRUE)) %>%
  filter(!Section %in% "NA") 

library(plyr)
averaging <- rename(averaging, c("Section" = "Submarket"))
detach(package:plyr)

#preparing Northwest Houston
nw_averaging <- averaging %>%
  filter(Submarket == "Northwest Houston")
nw_class <- class1_df %>%
  filter(Section == "Northwest Houston") %>%
  select(Submarket, `Class A`, `Class B`, `Class C`, `Class D`)
#binding northwest
nw <- rbind(nw_averaging, nw_class)

#preparing Northeast Houston
ne_averaging <- averaging %>%
  filter(Submarket == "Northeast Houston")
ne_class <- class1_df %>%
  filter(Section == "Northeast Houston") %>%
  select(Submarket, `Class A`, `Class B`, `Class C`, `Class D`)
#binding northeast
ne <- rbind(ne_averaging, ne_class)

#preparing Southwest Houston
sw_averaging <- averaging %>%
  filter(Submarket == "Southwest Houston")
sw_class <- class1_df %>%
  filter(Section == "Southwest Houston") %>%
  select(Submarket, `Class A`, `Class B`, `Class C`, `Class D`)
#binding southwest
sw <- rbind(sw_averaging, sw_class)

#preparing Southeast Houston
se_averaging <- averaging %>%
  filter(Submarket == "Southeast Houston")
se_class <- class1_df %>%
  filter(Section == "Southeast Houston") %>%
  select(Submarket, `Class A`, `Class B`, `Class C`, `Class D`)
#binding northwest
se <- rbind(se_averaging, se_class)

#preparing Central Houston
ce_averaging <- averaging %>%
  filter(Submarket == "Central Houston")
ce_class <- class1_df %>%
  filter(Section == "Central Houston") %>%
  select(Submarket, `Class A`, `Class B`, `Class C`, `Class D`)
#binding northwest
ce <- rbind(ce_averaging, ce_class)

#preparing all markets
all <- class1_df %>%
  filter(Submarket == "All") %>%
  select(Submarket, `Class A`, `Class B`, `Class C`, `Class D`)

#making full table for kable
full_table <- rbind(all, sw, nw, ce, ne, se)
#setting 0 equal to NA
full_table[full_table == 0] <- NA


#making kable
finished_table <- kable(full_table, caption = "Monthly Rental Rate per Square Foot by Apartment Class", digits = round(3)) %>%
  kable_styling(full_width = F) %>%
  group_rows("Total Houston Market Average", 1, 1) %>%
  group_rows("Southwest Houston", 2, 13) %>%
  group_rows("Northwest Houston", 14, 24) %>%
  group_rows("Central Houston", 25, 30) %>%
  group_rows("Northeast Houston", 31, 38) %>%
  group_rows("Southeast Houston", 39, 48) %>%
  row_spec(1, bold = T) %>%
  row_spec(2, bold = T) %>%
  row_spec(14, bold = T) %>%
  row_spec(25, bold = T) %>%
  row_spec(31, bold = T) %>%
  row_spec(39, bold = T)

#making kables for each Section of Houston
#central Houston
'Central Houston' <- kable(ce, caption = "Monthly Rental Rate per Square Foot by Apartment Class", digits = round(3)) %>%
  kable_styling(full_width = F) %>%
  group_rows("Central Houston", 1,6) %>%
  row_spec(1, bold = T)


#northeast houston
'Northeast Houston' <- kable(ne, caption = "Monthly Rental Rate per Square Foot by Apartment Class", 
                  digits = round(3)) %>%
  kable_styling(full_width = F) %>%
  group_rows("Northeast Houston", 1,8) %>%
  row_spec(1, bold = T)

#northwest houston
'Northwest Houston' <- kable(nw, caption = "Monthly Rental Rate per Square Foot by Apartment Class",
                  digits = round(3)) %>%
  kable_styling(full_width = F) %>%
  group_rows("Northwest Houston", 1,11) %>%
  row_spec(1, bold = T)

# southeast houston
'Southeast Houston' <- kable(se, caption = "Monthly Rental Rate per Square Foot by Apartment Class",
                  digits = round(3)) %>%
  kable_styling(full_width = F) %>%
  group_rows("Southeast Houston", 1,10) %>%
  row_spec(1, bold = T)

#southwest houston
'Southwest Houston' <- kable(sw, caption = "Monthly Rental Rate per Square Foot by Apartment Class",
                  digits = round(3)) %>%
  kable_styling(full_width = F) %>%
  group_rows("Southwest Houston", 1,12) %>%
  row_spec(1, bold = T)


if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup")

ggmap::register_google(key = "AIzaSyDOt3F5FU_4XtBOLLnOXpB2B9O7iXxtA5Q")

mylocation <- c(lat = 29.749263, long =  -95.401658)


mymap <- get_map(location = mylocation,
                 source = "google",
                 maptype = "roadmap",
                 zoom = 9,
                 color = "color")


HouMap <- ggmap(mymap)

HouMap + geom_point(data = construction, aes(x = Longitude, y = Latitude, 
                                             color = Category, group = NA), size = .8, alpha = .7) + scale_color_manual(values = c("red", "blue", "deepskyblue")) + 
  theme(legend.text = element_text(size = 12)) + theme(legend.title = element_text(size = 15, face = "bold")) + ditch_the_axes


library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cyborg"),
   
   # Application title
   titlePanel("Upcoming Houston Apartment Complexes"),
   
   # Sidebar with a slider input for number of bins 
   column(12,
          h5("Market Area"),
          sidebarPanel(
       htmlOutput("section_selector"),
       htmlOutput("submarket_selector")
          )
     ),
     
     
       #table dimensions may be too big for sidebar
      
      
      # Show a plot of the generated distribution
      column(12,
         tabsetPanel(type = "tabs",
                     tabPanel("Summary",verbatimTextOutput("summary"), h1("Plot"),
                              p("The data used was acquired from ApartmentData.com. The map shows the 7 counties that make up the city of Houston, Texas (Harris, Galveston, Brazoria, Fort Bend, Waller, Montgomery, and Chambers County). The Houston Apartment Market is split up into 5 different Sections (Northwest, Northeast, Southeast, Southwest, and Central Houston). These sections are split up into even further sections called \"Submarkets\". This shiny app will allow you to navigate through different Sections and Submarkets of Houston. The points plotted out will give you information about the Upcoming Apartment Market in Houston. Red markers represent \"Proposed\" apartment sites. These sites have signed a contractual agreement to build at the designated area, but have yet to break ground and start construction. Blue markers represent \"Recently Opened\" apartment sites. These apartment sites have opened up and have began renting out units to tenants in the last 2 months. Yellow markers represent apartment sites that are currently \"Under Construction\" and have yet to be completed. Some of these sites are renting out units to tenants during construction, but have yet to do a Grand Opening for the Apartment Complex."),
                              h1("Table"), 
                              p("The table also uses data acquired from ApartmentData.com. The table shows the monthly rental rate per square foot across all markets and submarkets for different classes of apartments. Apartment Complexes are given a class ranking ranging from \"A\" through \"D\". Apartments with a \"Class A\" ranking are relatively luxurious and newly built. \"Class D\" apartments represent the opposite side of the spectrum and are the least luxurious and are more poorly put together. An average monthly rental rate per square foot in dollar amount is given for the entire Houston market, represented by the \"All\" row. From there, the average monthly rental rate per square foot is given in dollar amounts for each Section of Houston followed by the Subsections that make up that Section. Some of the lower income Subsections don't contain higher classed apartment complexes, just as some higher income Subsections don't contain lower classed apartment complexes." )),
                     tabPanel("Plot", column(8, plotOutput("plot1")),
                              column(4, tableOutput("market"))),
                     tabPanel("Table", htmlOutput("table"))
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
   
   output$submarket_selector <- renderUI({
     
     data_available <- construction[construction$Section == input$section, "Submarket"]
     
     selectInput(inputId = "submarket", 
                 label = "Submarket",
                 choices = unique(data_available), 
                 selected = unique(data_available)[1])
   })

   
   output$summary <- renderPrint({
   text = "hey there"
   })
     
     output$plot1 <- renderPlot({
       #input$section %>%
         #input$submarket %>%
       df <- construction %>%
         filter(Submarket == input$submarket)
       HouMap + geom_point(data = df, aes(x = Longitude, y = Latitude, 
                                                    color = Category, group = NA), size = 2.5, alpha = .7) + scale_color_manual(values = c("red", "blue", "deepskyblue")) + 
         theme(legend.text = element_text(size = 20)) + theme(legend.title = element_text(size = 23, face = "bold")) + ditch_the_axes
       
     }, height = 600, width = 800
     )
     
     output$table <- renderText({
       kable(full_table, caption = "Monthly Rental Rate per Square Foot by Apartment Class", digits = round(3)) %>%
         kable_styling(full_width = F) %>%
         group_rows("Total Houston Market Average", 1, 1) %>%
         group_rows("Southwest Houston", 2, 13) %>%
         group_rows("Northwest Houston", 14, 24) %>%
         group_rows("Central Houston", 25, 30) %>%
         group_rows("Northeast Houston", 31, 38) %>%
         group_rows("Southeast Houston", 39, 48) %>%
         row_spec(1, bold = T) %>%
         row_spec(2, bold = T) %>%
         row_spec(14, bold = T) %>%
         row_spec(25, bold = T) %>%
         row_spec(31, bold = T) %>%
         row_spec(39, bold = T)
     })
     
     
     hmark <- function(x) {
       
       #input$section <- renderText({ans()})
       
       #x <- input$section
       
       if(x == 'Central Houston') {
         result <- kable(ce, caption = "Monthly Rental Rate per Square Foot by Apartment Class", digits = round(3)) %>%
           kable_styling(full_width = F) %>%
           group_rows("Central Houston", 1,6) %>%
           row_spec(1, bold = T)}
       
       else if (x == 'Northeast Houston') {
         result <- kable(ne, caption = "Monthly Rental Rate per Square Foot by Apartment Class", 
                         digits = round(3)) %>%
           kable_styling(full_width = F) %>%
           group_rows("Northeast Houston", 1,8) %>%
           row_spec(1, bold = T)}
       
       else if (x == 'Northwest Houston') {
         result <- kable(nw, caption = "Monthly Rental Rate per Square Foot by Apartment Class",
                         digits = round(3)) %>%
           kable_styling(full_width = F) %>%
           group_rows("Northwest Houston", 1,11) %>%
           row_spec(1, bold = T)}
       
       else if (x == 'Southeast Houston') {
         result <- kable(se, caption = "Monthly Rental Rate per Square Foot by Apartment Class",
                         digits = round(3)) %>%
           kable_styling(full_width = F) %>%
           group_rows("Southeast Houston", 1,10) %>%
           row_spec(1, bold = T)}
       
       else if (x == 'Southwest Houston') {
         result <- kable(sw, caption = "Monthly Rental Rate per Square Foot by Apartment Class",
                         digits = round(3)) %>%
           kable_styling(full_width = F) %>%
           group_rows("Southwest Houston", 1,12) %>%
           row_spec(1, bold = T)
       }
       
       return(result)
       
     }
     
     
     
     
     #Probably need to use function and return at the end
     
     #might have to put function before the renderText and output
     output$market <- renderText(
       {hmark(input$section)
       # hmark <- function(x) {
       #   
       #   #input$section <- renderText({ans()})
       #   
       #   #x <- input$section
       #   
       # if(x == 'Central Houston') {
       #   result <- kable(ce, caption = "Monthly Rental Rate per Square Foot by Apartment Class", digits = round(3)) %>%
       #                kable_styling(full_width = F) %>%
       #                group_rows("Central Houston", 1,6) %>%
       #                row_spec(1, bold = T)}
       # 
       # else if (x == 'Northeast Houston') {
       #   result <- kable(ne, caption = "Monthly Rental Rate per Square Foot by Apartment Class", 
       #                   digits = round(3)) %>%
       #     kable_styling(full_width = F) %>%
       #     group_rows("Northeast Houston", 1,8) %>%
       #     row_spec(1, bold = T)}
       #   
       #   else if (x == 'Northwest Houston') {
       #     result <- kable(nw, caption = "Monthly Rental Rate per Square Foot by Apartment Class",
       #                     digits = round(3)) %>%
       #       kable_styling(full_width = F) %>%
       #       group_rows("Northwest Houston", 1,11) %>%
       #       row_spec(1, bold = T)}
       #   
       #   else if (x == 'Southeast Houston') {
       #     result <- kable(se, caption = "Monthly Rental Rate per Square Foot by Apartment Class",
       #                       digits = round(3)) %>%
       #       kable_styling(full_width = F) %>%
       #       group_rows("Southeast Houston", 1,10) %>%
       #       row_spec(1, bold = T)}
       #   
       #   else if (x == 'Southwest Houston') {
       #     result <- kable(sw, caption = "Monthly Rental Rate per Square Foot by Apartment Class",
       #           digits = round(3)) %>%
       #       kable_styling(full_width = F) %>%
       #       group_rows("Southwest Houston", 1,12) %>%
       #       row_spec(1, bold = T)
       #   }
       #   
       #   return(result)
       #   
       # }
         
      
     })
     
   }



# Run the application 
shinyApp(ui = ui, server = server)


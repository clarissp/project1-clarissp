#Project 1 
#Using Shinydashboard

library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
pdf(NULL)

#Create variable for intimate partner violence data downloaded from NYC Open Data 
partner <- read.csv("IntimatePartnerViolence.csv")
partner.load <- partner %>%
  mutate(boro = as.character("Comm_Dist_Boro")
         )

#Creates the header of the shiny dashboard 
header <- dashboardHeader(title = "NYC Intimate Partner Violence Dashboard",
                          #Creates the content for the notifications menu
                          dropdownMenu(type = "notifications",
                                       notificationItem(text = "Data updated on September 6, 2018", 
                                                        icon = icon("life-ring")
                                                        ),
                                       notificationItem(text = "BXJIA indicates a Bronx joing Area of Interest",
                                                        icon = icon("users")
                                                        ),
                                       notificationItem(text = "MNJIA indicates a Manhattan Join Area of Interest",
                                                        icon = icon("users")
                                                        ),
                                       notificationItem(text = "QNJIA indicates a Queens Join Area of Interest",
                                                        icon = icon("users")
                                       ),
                                       notificationItem(text = "SIJIA indicates a Staten Island Join Area of Interest",
                                                        icon = icon("users")
                                       )
                          ),
                          dropdownMenu(type = "tasks", badgeStatus = "success",
                                       taskItem(value = 70, color = "green",
                                                "Downloading Data")
                          ),
                          #Creates the content for the messages menu 
                          dropdownMenu(type = "messages",
                                       messageItem(
                                         from = "NYC Open Data",
                                         message = "This data is collected by The Mayor's Office to Combat Domestic Violence",
                                         icon = icon("life-ring"),
                                         time = "3:55"
                                         ),
                                       messageItem(
                                         from = "New User",
                                         message = "Is it possible to get notified when the data is updated?",
                                         icon = icon("question"),
                                         time = "4:00"
                                       )
                          )
)

#Creates the sidebar of the shiny dashboard 
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("Plot", icon = icon("bar-chart"), tabName = "plot"),
    menuItem("Table", icon = icon("table"), tabName = "table", badgeLabel = "new", badgeColor = "green"),
    #Creates borough select input
    selectInput("boro",
                "Boroughs:",
                choices = sort(unique(partner.load$Comm_Dist_.Boro)),
                multiple = TRUE,
                selectize = TRUE,
                selected = c("Queens", "Manhattan")),
    #Creates Community District slider input
    sliderInput("district",
                "Community Districts:",
                min = min(partner.load$Comm_District, na.rm = T),
                max = max(partner.load$Comm_District, na.rm = T),
                value = c(min(partner.load$Comm_District, na.rm = T), max(partner.load$Comm_District, na.rm = T)),
                step = 1)
  )
)

#Need to add body section 
body <- dashboardBody()

ui <- dashboardPage(header, sidebar, body, skin = "green")

# Define server logic
server <- function(input, output) {
  partnerInput <- reactive({
    partner <- partner.load %>%
      # Allows for the slider input to be reactive
      filter(Comm_District >= input$district[1] & Comm_District <= input$district[2])
    # Allows for the select input to be reactive
    if (length(input$boro) > 0 ) {
      partner <- subset(partner, Comm_Dist_.Boro %in% input$boro)
    }
    
    return(partner)
  })
  
  #Bar plot code
  output$barplot <- renderPlotly({
    partner <- partnerInput()
    ggplot(data = partner, aes(x = Comm_Dist_.Boro, y =IPV_DIR)) +
      geom_bar(stat="identity")
  })
  

}

# Run the application 
shinyApp(ui = ui, server = server)

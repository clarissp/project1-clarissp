#Project 1 
#Using Shinydashboard

library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(ggplot2)
pdf(NULL)

#Create variable for intimate partner violence data downloaded from NYC Open Data 
partner <- read.csv("IntimatePartnerViolence.csv")
partner.load <- partner %>%
  mutate(boro = trimws(as.character(Comm_Dist_.Boro),"both")
         )

#Creates the header of the shiny dashboard 
header <- dashboardHeader(title = "NYC Intimate Partner Violence Dashboard",
                          #Creates the content for the notifications menu
                          dropdownMenu(type = "notifications",
                                       notificationItem(text = "Data updated on September 6, 2018", 
                                                        icon = icon("calendar")
                                                        ),
                                       notificationItem(text = "Joint Interest Areas removed from data", 
                                                        icon = icon("scissors")
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
                                         message = HTML("This data is collected by The Mayor's <br> Office to Combat Domestic Violence"),
                                         icon = icon("institution"),
                                         time = "3:55"
                                         ),
                                       messageItem(
                                         from = "New User",
                                         message = HTML("Is it possible to get notified when <br> the data is updated?"),
                                         icon = icon("question"),
                                         time = "4:00"
                                       )
                          )
)

#Creates the sidebar of the shiny dashboard 
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    #Creates pages in the sidebar
    menuItem("Felony Assault Incidents", icon = icon("balance-scale"), tabName = "assault"),
    menuItem("Rape Incidents", icon = icon("balance-scale"), tabName = "rape"),
    menuItem("Complete Dataset", icon = icon("database"), tabName = "table"),
    #Creates borough select input
    selectInput("boro",
                "Boroughs:",
                choices = sort(unique(partner.load$boro)),
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

#Creates the body of the shiny dashboard
body <- dashboardBody(tabItems(
  #Creates the felony assault page
  tabItem("assault",
          fluidRow(
            valueBoxOutput("incidents"),
            valueBoxOutput("assault")
          ),
          fluidRow(
            box(
              selectInput("districtassault",
                        "X-Axis:",
                        choices = c("Community Distrct" = "Comm_District",
                                    "Borough" = "boro"),
                        selected = c("Comm_District")
                        )
            ),
            tabBox(width = 12,
                   tabPanel("Bar Plot", plotlyOutput("assaultbarplot")),
                   tabPanel("Box Plot", plotlyOutput("assaultboxplot"))
            )
          )
  ),
  tabItem("rape",
          fluidRow(
            valueBoxOutput("incidents"),
            valueBoxOutput("rape")
          ),
          fluidRow(
            box(
              selectInput("districtrape",
                          "X-Axis:",
                          choices = c("Community Distrct" = "Comm_District",
                                      "Borough" = "boro"),
                          selected = c("boro")
              )
            ),
            tabBox(width = 12,
                 tabPanel("Bar Plot", plotlyOutput("rapebarplot")),
                 tabPanel("Box Plot", plotlyOutput("rapeboxplot"))
            )
          )
  ),
  tabItem("table",
          fluidPage(
            box(title = "Complete Intimate Partner Violence Dataset", DT::dataTableOutput("table"), width = 12))
  )
)
)

ui <- dashboardPage(header, sidebar, body, skin = "black")

# Define server logic
server <- function(input, output, session = session) {
  partnerInput <- reactive({
    partner <- partner.load %>%
      # Allows for the slider input to be reactive
      filter(Comm_District >= input$district[1] & Comm_District <= input$district[2])
    # Allows for the select input to be reactive
    if (length(input$boro) > 0 ) {
      partner <- subset(partner, boro %in% input$boro)
    }
    if (length(input$districtassault) > 0 ) {
      partner <- subset(partner, Comm_District %in% input$districtassault)
    }
    if (length(input$districtrape) > 0 ) {
      partner <- subset(partner, boro %in% input$districtrape)
    }
    
    return(partner)
  })
  
  #Bar plot for felony assault that involved a family member by borough 
  output$assaultbarplot <- renderPlotly({
    partner <- partnerInput()
    ggplot(data = partner, aes(x = boro, y =IPV_Fel_Assault)) +
      geom_bar(stat="identity")
  })
  
  #Box plot for felony assault that involved a family member by borough 
  output$assaultboxplot <- renderPlotly({
    partner <- partnerInput()
    ggplot(data = partner) +
      geom_boxplot(mapping = aes(x= boro, y = IPV_Fel_Assault))
  })
  
  #Bar plot for rape that involved a family member by borough 
  output$rapebarplot <- renderPlotly({
    partner <- partnerInput()
    ggplot(data = partner, aes(x = boro, y =IPV_Fel_Rape)) +
      geom_bar(stat="identity")
  })
  
  #Box plot for rape that involved a family member by borough 
  output$rapeboxplot <- renderPlotly({
    partner <- partnerInput()
    ggplot(data = partner) +
      geom_boxplot(mapping = aes(x= boro, y = IPV_Rape))
  })
  
  #Data table of whole dataset
  output$table <- DT::renderDataTable({
    subset(partnerInput(), select = c(boro, Comm_District, IPV_Fel_Assault, DV_Fel_Assault, IPV_Rape, DV_Rape))
  })
  
  #Value box of average incidents 
  output$incidents <- renderValueBox({
    partner <- partnerInput()
    incidents <- round(mean(partner$IPV_DIR, na.rm = T), 2)
    
    valueBox(subtitle = "Average Number of Incidents Reported", value = num, icon = icon("gavel"), color = "purple")
  })
  
  #Value box of average felony assaults
  output$assault <- renderValueBox({
    partner <- partnerInput()
    assault <- round(mean(partner$IPV_Fel_Assault, na.rm = T), 2)
    
    valueBox(subtitle = "Average Number of Felony Assault Incidents Reported", value = num, icon = icon("gavel"), color = "purple")
  })
  
  #Value box of average rapes
  output$rape <- renderValueBox({
    partner <- partnerInput()
    rape <- round(mean(partner$IPV_Rape, na.rm = T), 2)
    
    valueBox(subtitle = "Average Number of Rape Incidents Reported", value = num, icon = icon("gavel"), color = "purple")
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

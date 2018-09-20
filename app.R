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
                                       notificationItem(text = "MNBJA indicates a Manhattan Join Area of Interest",
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
                                       taskItem(value = 110, color = "green",
                                                "Midichlorians")
                          ),
                          dropdownMenu(type = "messages",
                                       messageItem(
                                         from = "Princess Leia",
                                         message = HTML("Help Me Obi-Wan Kenobi! <br> You're my only hope."),
                                         icon = icon("exclamation-circle"))
                          )
)

# Run the application 
shinyApp(ui = ui, server = server)

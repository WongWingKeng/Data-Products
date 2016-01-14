## Loading required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(markdown)
library(dplyr) ## for data grouping
library(scales) ## for percent function in pie chart
library(plotly)

## using shiny dashboard
## define dashboard header
header <- dashboardHeader(
        title = "UK Road Safety - Digital Breath Test Data Analysis - 2014",
        titleWidth = 550
)

sidebar <- dashboardSidebar(
        tags$head(tags$script(HTML('Shiny.addCustomMessageHandler("jsCode",function(message) {eval(message.value);});'))),
        
        sidebarMenu(id = "menu",
                    
                menuItem("Welcome Note", tabName = "welcome", icon = icon("car")),

                menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                
                conditionalPanel( #dashboard conditional panel 
                        condition = "input.menu == 'dashboard'", ##display when dashboard menu chosen

                        conditionalPanel( #conditional subpanel 1
                                condition = "input.plot_type == 'bar'",    
                                radioButtons("bargroupOptions", "Group by: ",
                                             c("None"="None",
                                               "Reason"="Reason",
                                               "Month"="Month",
                                               "WeekType"="WeekType",
                                               "TimeBand"="TimeBand",
                                               "AgeBand"="AgeBand",
                                               "Gender"="Gender",
                                               "Quarter"="Quarter"),
                                             selected="None"
                                )#end of radiobutton
                        ), #Closing conditional subpanel 1     

                        conditionalPanel( #conditional subpanel 2
                                condition = "input.plot_type == 'line'",    
                                radioButtons("linegroupOptions", "Group by: ",
                                             c("None"="None",
                                               "Reason"="Reason",
                                               "WeekType"="WeekType",
                                               "AgeBand"="AgeBand",
                                               "Gender"="Gender"),
                                             selected="None"
                                )#end of radiobutton
                        )#, #Closing conditional subpanel 2     

                ),##Closing dashboard conditional panel

                menuItem("About/Documentaion", tabName = "about", icon= icon("info-circle")),

                menuItem("Help/User Guide", tabName ="help", icon=icon("question-circle"))
        ) # Closing sidebar menu
)

body <- dashboardBody(
        
        tabItems(
                tabItem(tabName = "dashboard",    
                fluidRow(
                        column(width = 8,
                               box(width = NULL, solidHeader = TRUE,
                                   uiOutput("plotArea")
                               )#closing column
                        ),#closing plotarea

                        column(width = 4, #Outcome values
                             box(width = NULL, status = "warning",
                                 radioButtons("outcome", "Choose Y-axis (outcome) feature:",
                                    choices=c("Total Breath Alcohol Level(microg/100ml)" = "Alcohol_Level",
                                             "Total No. of Test Performed"="Test_Peformed",
                                             "Total No. of Alcohol Detected Case"="Alcohol_Detected" )
                                 ),#end of radiobutton
                               p(
                                   class = "text-muted",
                                   paste("Note: One of these values will always be used as basis for Analysis."
                               )# closing p
                             )#closing box
                        ), #closing outcome columns
                       
                        box(width = NULL, status = "warning",
                            
                            conditionalPanel( #conditional panel 1
                                condition = "input.plot_type == 'bar'",    
                                selectInput("barCXList", "Choose X-axis feature:",
                                        c("Reason"="Reason",
                                          "Month"="Month",
                                          "WeekType"="WeekType",
                                          "TimeBand"="TimeBand",
                                          "AgeBand"="AgeBand",
                                          "Gender"="Gender",
                                          "Quarter"="Quarter")
                                        )#Closing selectInput 1
                            ),#Closing conditional Panel 1
                            
                            conditionalPanel( #conditional panel 2
                                condition = "input.plot_type == 'line'",
                                selectInput("lineCXList", "Choose X-axis feature:",
                                            c("TimeBand"="TimeBand",
                                              "Month"="Month",
                                              "Quarter"="Quarter")
                                )#Closing selectInput 2
                            ), #Closing conditional Panel 2

                            conditionalPanel( #conditional panel 3
                                condition = "input.plot_type == 'pie'",
                                selectInput("pieCXList", "Choose X-axis feature:",
                                            c("Reason"="Reason",
                                              "Month"="Month",
                                              "WeekType"="WeekType",
                                              "TimeBand"="TimeBand",
                                              "AgeBand"="AgeBand",
                                              "Gender"="Gender",
                                              "Quarter"="Quarter")
                                )#Closing selectInput 3
                            ) #Closing conditional Panel 3
                        
                        ), # Closing box    

                        box(width = NULL, status = "warning",  ##Plot type selection
                            radioButtons(
                                    "plot_type", "Choose Plot Type: ", #inline=TRUE,
                                    c("Bar chart"="bar", 
                                      "Line chart" = "line",
                                      "Pie chart" = "pie")
                                   ) ##closing radio button     
                        )#closing box

                )#closing fluidRow
        ),
        
        fluidRow(
          column(width = 12,
                 #div(style = 'overflow-x: scroll', 
                 tabBox(width = NULL, 
                   tabPanel("Visual Plot's Dynamic Dataset Summary", 
                            verbatimTextOutput("summary")
                           ),
                   tabPanel("Complete Digital Breath Test Raw Dataset Summary",
                            verbatimTextOutput("raw")
                            )
                 )
                 #)
          ) #closing column
        ),# closing fluidRow

        fluidRow(
                column(width = 12,
                       div(style = 'overflow-x: scroll', 
                           DT::dataTableOutput("BreathTestTable"))
                       ) #closing column
                )# closing fluidRow
         ), #closing dashboard tabitem 
        
        tabItem(
                tabName = "about",
                includeHTML("About.html")
         ),# closing about tabitem
        
        tabItem(
                tabName = "help",
                ## allow to scroll right if page not fully visible
                div(style = 'overflow-x: scroll', 
                    includeHTML("Help.html")
                   )
        ),# closing help tabitem
        
        tabItem( ##Welcome note 
                tabName = "welcome",
                includeHTML("Welcome.html")
        )# closing welcome note tabitem
        
     ) # closing all tabitems   
        
) #closing dashboard body


## display dashboard
dashboardPage(
        header,
        skin = "blue",
        sidebar,
        body
) #Closing dashboard page



## Load all prequired libraries and data preparation
source("BackEndProcessing.R")

## Function to determine the right render object to return.
choose_plotAreaType <- function(inputType){
        
        if (is.null(inputType)) return()
        
        switch(inputType,
                 "bar" = return(plotlyOutput("barplot", height = "500px", width = "auto")),
                 "line" = return(plotlyOutput("lineplot", height = "500px", width = "auto")),
                 "pie" = return(plotOutput("pieplot", height = "500", width = "auto"))
        )
        
        
}


## Define Shiny server logic required to draw the and display the table
shinyServer(function(input, output, session) {
        
         ### Function to initiate dataset filtering
         dataTableSubset<- reactive({
                 
                switch(input$plot_type,
                       "bar" = {xaxis<-input$barCXList 
                       groupby<-input$bargroupOptions},
                       "line" = {xaxis<-input$lineCXList 
                       groupby<-input$linegroupOptions},
                       "pie" = {xaxis<-input$pieCXList 
                       groupby<-"None"}
                )
                 
                switch(input$outcome,
                       "Alcohol_Level"=yaxis<-"Breath.Alcohol.Level",
                       "Test_Peformed"=yaxis<-"Test.Performed",
                       "Alcohol_Detected"=yaxis<-"Alchohol.Detected"
                )
                
                dtSubset(breathDT, xaxis, yaxis, groupby)
                 
         }) #Closing dataTableSubset 
         
         ### dynamic generate plot UI area, choose between normal plot or plot_ly    
         output$plotArea = renderUI({
                  choose_plotAreaType(input$plot_type)
         }) 

         ## render plot_ly barchart for server generated barplot output UI object
         output$barplot<- renderPlotly({
                 plotBarC(dataTableSubset()) 
         })
         
         ## render plot_ly line chart for server generated lineplot output UI object
         output$lineplot<- renderPlotly({
                 plotLineC(dataTableSubset())  
         })
         
         ## render ggplot chart for server generated pieplot output UI object
         output$pieplot<- renderPlot({
                    plotPieC(dataTableSubset())
         }, width="auto",height="auto")
         
 
         # Generate a summary of the dataset
         output$summary <- renderPrint({
           datasets<-dataTableSubset()
           summary(as.data.frame(datasets[1]))
         })     
         
         output$raw <- renderPrint({
           summary(breathDT)
         })     
         
         ## Drawing the data table browser area
         ## set to use server side processing and filtering on top row
         output$BreathTestTable = DT::renderDataTable(
                 breathDT,
                 server=TRUE,  
                 extensions = 'FixedColumns',
                 selection='none',
                 filter = list(position = 'top', clear = FALSE),
                 caption = htmltools::tags$caption(
                         style = 'text-align: left; color: black; 
                         font-weight: bold; font-size: 15px',
                         '*Table : ', htmltools::em("Digital Breath Test 2014 - Data Browser")),
                 
                 options = list(
                         searching = TRUE, ##Enable search field
                         
                         autoWidth = TRUE, ##Columns width
                         columnDefs = list(list(width = '150px', targets = c(1)),
                                           list(className = 'dt-center', targets = c(2:10)))  
                         
                 ) ##end of DT options
         ) ##end of DT 
         
                  
}) ## End of ShinyServer 
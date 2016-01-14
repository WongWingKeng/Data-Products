suppressMessages(library(data.table))
suppressMessages(library(DT))

## Reading subset data
miniDT<-data.frame(fread("Sample_DigitalBreathTestData2014.csv",stringsAsFactors=T))

DTObject <- DT::datatable(
        miniDT,
        extensions = 'FixedColumns',
        selection='none',
        filter = list(position = 'top', clear = FALSE),
        caption = htmltools::tags$caption(
                style = 'text-align: left; color: black; 
                font-weight: bold; font-size: 15px',
                '*Table : ', htmltools::em("Digital Breath Test 2014 - Data Browser")),
        options = list(
                pageLength=5,
                lengthMenu = c(5),
                searching = TRUE, ##Enable search field
                autoWidth = TRUE, ##Columns width
                columnDefs = list(list(width = '150px', targets = c(1)),
                                  list(className = 'dt-center', targets = c(2:10))
                )  
        ) ##end of DT options
) ##end of DT 

## Save DTObject widget to html and attach to slidify
if (!dir.exists("./assets/obj")) {dir.create("./assets/obj")} ##create sub-directory as needed
savepath<-paste(getwd(),"/assets/obj/DTtable.html",sep="") ## set the storage path
DT::saveWidget(DTObject, savepath) ## store the DT table widget

## return output back to slidify
cat('<iframe width="100%" height="270" src="assets/obj/DTtable.html"> </iframe>')

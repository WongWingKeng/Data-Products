library(shiny)
library(shinydashboard)
library(DT)
library(markdown)
library(dplyr) ## for data grouping
library(scales) ## for percent function in pie chart
library(plotly)
library(data.table)


##--------------------------------------------------------------------------
## Data Loading and preparation

#Using fread to speed up reading of large table
breathDT<-data.frame(fread("DigitalBreathTestData2014.csv",stringsAsFactors=T))


##Remove year variable since all rows are from the same year
breathDT$Year<-NULL ## Remove YEAR column since all data are from 2014

##Add a quater variable
breathDT$Quarter<-breathDT$Month
levels(breathDT$Quarter)<-list( 
        Q1 =c("Jan","Feb","Mar"), 
        Q2 =c("Apr","May","Jun"), 
        Q3 =c("Jul","Aug","Sep"), 
        Q4 =c("Oct","Nov","Dec")
)

##Add a total test count variable
breathDT$Test.Performed<-1

##Add a Alcohol.Detected count field
breathDT$Alchohol.Detected<-breathDT$BreathAlcoholLevel.microg.100ml. >0

##Rename variables
colnames(breathDT)[colnames(breathDT)=="BreathAlcoholLevel.microg.100ml."] <- "Breath.Alcohol.Level"

##--------------------------------------------------------------------------


### Function to generate the required dataset, xaxis, yaxis and groupby objects
dtSubset<-function(dt, xaxis, yaxis, grp){
        
        
        targetColumns<-as.character()
        targetColumns[1]<-xaxis
        targetColumns[2]<-yaxis
        
        
        groupby<-as.character()
        groupby[1]<-xaxis
        
        if(grp!=xaxis && grp!="None") {targetColumns[3]<-grp
                        groupby[2]<-grp}

        groupby<-lapply(groupby,as.symbol)
        
        dtTmp<-subset(dt, select=targetColumns)
        dtTmp<- dtTmp %>% group_by_(.dots=groupby) %>% summarise_each(funs(sum))
        
        ObjList<-list(dtTmp, xaxis, yaxis, grp) ## Compile all into a list to return
        return(ObjList)
}

##--------------------------------------------------------------------------

### Function to create bar charts
plotBarC<-function(pObjList){ 
        
        df<-as.data.frame(pObjList[1])

        if (as.character(pObjList[4])=="None") { colorgrp<-"'NULL'" } else { colorgrp<-pObjList[4] }
        
        ### Do xaxis sorting
        ## Pre-defined the appropriate ordering
        mon_ord<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
        timeband_ord<-c("12am-4am","4am-8am","8am-12pm","12pm-4pm","4pm-8pm","8pm-12pm","Unknown")
        ageband_ord<-c("16-19","20-24","25-29","30-39","40-49","50-59","60-69","70-98","Other","Unknown")
        quarter_ord<-c("Q1","Q2","Q3","Q4")
        
        ## Check sorting requirement
        ord<-FALSE
        switch(as.character(pObjList[2]),
               "Month"={df$Month<-factor(df$Month, levels=mon_ord)
                        ordTbl<-"factor(df$Month,levels=mon_ord)"
                        ord<-TRUE},
               "TimeBand"={df$TimeBand<-factor(df$TimeBand, levels=timeband_ord)
                           ordTbl<-"factor(df$TimeBand,levels=timeband_ord)"
                           ord<-TRUE},  
               "AgeBand"={df$AgeBand<-factor(df$AgeBand, levels=ageband_ord)
                          ordTbl<-"factor(df$AgeBand,levels=ageband_ord)"
                          ord<-TRUE},
               "Quarter"={df$Quarter<-factor(df$Quarter, levels =quarter_ord)
                          ordTbl<-"factor(df$Quarter,levels=quarter_ord)"
                          ord<-TRUE}
        )
        
        ord2<-FALSE
        switch(as.character(pObjList[4]),
               "Month"={ordTbl2<-"factor(df$Month,levels=mon_ord)"
               ord2<-TRUE},
               "TimeBand"={ordTbl2<-"factor(df$TimeBand,levels=timeband_ord)"
               ord2<-TRUE},
               "AgeBand"={ordTbl2<-"factor(df$AgeBand,levels=ageband_ord)"
               ord2<-TRUE},
               "Quarter"={ordTbl2<-"factor(df$Quarter,levels=quarter_ord)"
               ord2<-TRUE}
        )
        
        
        ## If sorting is required
        if (ord || ord2) {
           ##Dynamic generateing the sort command and Sorting the table

           ord_str<-paste("df[order(") ## initialize ordering command string
           
           if (ord) { 
               ord_str<-paste(ord_str, ordTbl)
           }

           if (ord2) {  
               if (ord) { ## if ordering by 2 columns
                   ord_str<-paste(ord_str, "," ,ordTbl2)
               } else {
                   ord_str<-paste(ord_str, ordTbl2)
               }
           }

           ord_str<-paste(ord_str, "),]") ## closing ordering command string 
           df <-eval(parse(text=ord_str))
        }
        
        ## Dynamic generating the plot command and creating the plot object
        plot_str<-paste("plot_ly(df, x=", pObjList[2], " , y=",pObjList[3],
                   ", type='bar', color=as.character(",colorgrp,"))")

        bc<-eval(parse(text=plot_str))
        
        mainTitle<-paste(pObjList[2],"vs",pObjList[3])  
        bc <- bc %>% layout(title=mainTitle) ## adding titles
        bc <- bc %>% layout(margin=list(l=50,r=50,b=100,t=100,pad=4)) ##add padding to avoid label text cutoff
        
        bc #print bc 
        
}


##--------------------------------------------------------------------------

### Function to create line chart
plotLineC<-function(pObjList){

  df<-as.data.frame(pObjList[1])
  
  if (as.character(pObjList[4])=="None") { colorgrp<-"'NULL'" } else { colorgrp<-pObjList[4] }
  
  ### Do xaxis sorting
  ## Pre-defined the appropriate ordering
  mon_ord<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  timeband_ord<-c("12am-4am","4am-8am","8am-12pm","12pm-4pm","4pm-8pm","8pm-12pm","Unknown")
  ageband_ord<-c("16-19","20-24","25-29","30-39","40-49","50-59","60-69","70-98","Other","Unknown")
  quarter_ord<-c("Q1","Q2","Q3","Q4")
  
  ## Check sorting requirement
  ord<-FALSE
  switch(as.character(pObjList[2]),
         "Month"={df$Month<-factor(df$Month, levels=mon_ord)
         ordTbl<-"factor(df$Month,levels=mon_ord)"
         ord<-TRUE},
         "TimeBand"={df$TimeBand<-factor(df$TimeBand, levels=timeband_ord)
         ordTbl<-"factor(df$TimeBand,levels=timeband_ord)"
         ord<-TRUE},  
         "AgeBand"={df$AgeBand<-factor(df$AgeBand, levels=ageband_ord)
         ordTbl<-"factor(df$AgeBand,levels=ageband_ord)"
         ord<-TRUE},
         "Quarter"={df$Quarter<-factor(df$Quarter, levels =quarter_ord)
         ordTbl<-"factor(df$Quarter,levels=quarter_ord)"
         ord<-TRUE}
  )
  
  ord2<-FALSE
  switch(as.character(pObjList[4]),
         "Month"={ordTbl2<-"factor(df$Month,levels=mon_ord)"
         ord2<-TRUE},
         "TimeBand"={ordTbl2<-"factor(df$TimeBand,levels=timeband_ord)"
         ord2<-TRUE},
         "AgeBand"={ordTbl2<-"factor(df$AgeBand,levels=ageband_ord)"
         ord2<-TRUE},
         "Quarter"={ordTbl2<-"factor(df$Quarter,levels=quarter_ord)"
         ord2<-TRUE}
  )
  
  
  ## If sorting is required
  if (ord || ord2) {
    ##Dynamic generateing the sort command and Sorting the table
    
    ord_str<-paste("df[order(") ## initialize ordering command string
    
    if (ord) { 
      ord_str<-paste(ord_str, ordTbl)
    }
    
    if (ord2) {  
      if (ord) { ## if ordering by 2 columns
        ord_str<-paste(ord_str, "," ,ordTbl2)
      } else {
        ord_str<-paste(ord_str, ordTbl2)
      }
    }
    
    ord_str<-paste(ord_str, "),]") ## closing ordering command string 
    df <-eval(parse(text=ord_str))
  }
  
  ## Dynamic generating the plot command and creating the plot object
  plot_str<-paste("plot_ly(df, x=", pObjList[2], " , y=",pObjList[3],
                  ", color=as.character(",colorgrp,"))")
  
  lc<-eval(parse(text=plot_str))
  
  mainTitle<-paste(pObjList[2],"vs",pObjList[3])  
  lc <- lc %>% layout(title=mainTitle) ## adding titles
  lc <- lc %>% layout(margin=list(l=50,r=50,b=100,t=100,pad=4)) ##add padding to avoid label text cutoff
  
  lc #print lc 

}

##--------------------------------------------------------------------------

### Function to create pie chart
plotPieC<-function(pObjList){
        
        df<-as.data.frame(pObjList[1])
        
        ### Do xaxis sorting
        ## Pre-defined the appropriate ordering
        mon_ord<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
        timeband_ord<-c("12am-4am","4am-8am","8am-12pm","12pm-4pm","4pm-8pm","8pm-12pm","Unknown")
        ageband_ord<-c("16-19","20-24","25-29","30-39","40-49","50-59","60-69","70-98","Other","Unknown")
        quarter_ord<-c("Q1","Q2","Q3","Q4")
        
        ## Check sorting requirement
        ord<-FALSE
        switch(as.character(pObjList[2]),
               "Month"={df$Month<-factor(df$Month, levels=mon_ord)
                        ordTbl<-"factor(df$Month,levels=mon_ord)"
                        ord<-TRUE},
               "TimeBand"={df$TimeBand<-factor(df$TimeBand, levels=timeband_ord)
                           ordTbl<-"factor(df$TimeBand,levels=timeband_ord)"
                           ord<-TRUE},  
               "AgeBand"={df$AgeBand<-factor(df$AgeBand, levels=ageband_ord)
                          ordTbl<-"factor(df$AgeBand,levels=ageband_ord)"
                          ord<-TRUE},
               "Quarter"={df$Quarter<-factor(df$Quarter, levels =quarter_ord)
                          ordTbl<-"factor(df$Quarter,levels=quarter_ord)"
                          ord<-TRUE}
        )
        
        ## If sorting is required
        if (ord) {
                ##Dynamic generateing the sort command and Sorting the table
                ord_str<-paste("df[order(" , ordTbl, "),]" ) ## create the ordering command string
                df <-eval(parse(text=ord_str))
        }

        ## Set blank background theme to suite background box
        blank_theme <- theme_minimal()+
                theme(
                        axis.title.x = element_blank(),
                        axis.title.y = element_blank(),
                        panel.border = element_blank(),
                        panel.grid=element_blank(),
                        axis.ticks = element_blank(),
                        plot.title=element_text(size=14, face="bold")
                )

        ### Generate ggplot first
        gg_str <-paste("ggplot(df, aes(x='', y=", pObjList[3] , 
                       ", fill=",pObjList[2],")) +",
                       "geom_bar(width = 1, stat = 'identity')")
        
        gg<-eval(parse(text=gg_str)) ## execute ggplot command string

        ### Convert ggplot to pie chart
        pc <- gg + coord_polar("y", start=0)
        pc <- pc + blank_theme + theme(axis.text.x=element_blank())
        
        ## Set Chart title
        mainTitle<-paste(pObjList[3],"\n By \n",pObjList[2])  
        pc <- pc + ggtitle(mainTitle) 

        
        ## Calculate percentages and set label text
        label_str<- paste("geom_text(aes(y =",pObjList[3],"/3 + c(0, cumsum(",pObjList[3],
                          ")[-length(",pObjList[3],")]), label = percent(",pObjList[3],
                          "/sum(",pObjList[3],"))), size=5)") ## set command str
        
        pc<-pc + eval(parse(text=label_str)) ## execute command str

        pc #print pc
        
        
}


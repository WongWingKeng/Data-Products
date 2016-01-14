## @knitr charts

suppressMessages(library(data.table))
suppressMessages(library(DT))
suppressMessages(library(scales))
suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
suppressMessages(library(grid))
suppressMessages(library(gridExtra))


## Reading subset data
miniDT<-data.frame(fread("Sample_DigitalBreathTestData2014.csv",stringsAsFactors=T))

## subset data for pie and bar chart
dt<-subset(miniDT, select=c(Reason,Breath.Alcohol.Level))
dt<- dt %>% group_by(Reason) %>% summarise_each(funs(sum))


### pie chart --------------------------------------------------
## Set blank background theme to suite background box
blank_theme1 <- theme_minimal()+
        theme(
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                panel.border = element_blank(),
                panel.grid=element_blank(),
                axis.ticks = element_blank(),
                plot.title=element_text(size=14, face="bold")
        )


### Generate ggplot first
pc<- ggplot(dt, aes(x="", y=Breath.Alcohol.Level,fill=Reason)) +
        geom_bar(width = 1, stat ="identity")

pc <- pc + coord_polar("y", start=0)
pc <- pc + blank_theme1 + theme(axis.text.x=element_blank())

## Set Chart title
mainTitle<-"Breath.Alcohol.Level"  
pc <- pc + ggtitle(mainTitle) 
pc <- pc + geom_text(aes(y = Breath.Alcohol.Level/3 + c(0, cumsum(Breath.Alcohol.Level)[-length(Breath.Alcohol.Level)]), 
                         label = percent(Breath.Alcohol.Level/sum(Breath.Alcohol.Level))), size=5)
#pc #print pc

### bar chart -------------------------------------------------------
## Set blank background theme to suite background box
blank_theme2 <- theme_minimal()+
        theme(
                panel.border = element_blank(),
                #panel.grid=element_blank(),
                panel.grid.major.x = element_blank() ,
                panel.grid.major.y = element_line( color="gray" ), 
                panel.grid.minor.y = element_line( color="gray" ), 
                axis.ticks = element_blank(),
                plot.title=element_text(size=14, face="bold"),
                axis.text.x = element_text(angle =345, hjust=0)
        )

bc<- ggplot(data=dt, aes(x=Reason, y=Breath.Alcohol.Level, fill=Reason)) +
        blank_theme2 + geom_bar(stat="identity")+ labs(fill="") 
bc <- bc + ggtitle("Reason vs Breath.Alcohol.Level")
#bc ## print bc


### Line chart -------------------------------------------------------
## Subset data
dt2<-subset(miniDT, select=c(Quarter, Reason, Breath.Alcohol.Level))
dt2<- dt2 %>% group_by(Quarter, Reason) %>% summarise_each(funs(sum))


## Set blank background theme to suite background box
blank_theme3 <- theme_minimal()+
        theme(
                panel.border = element_blank(),
                panel.grid.major.y = element_line( color="gray" ), 
                panel.grid.minor.y = element_line( color="gray" ), 
                axis.ticks = element_blank(),
                plot.title=element_text(size=14, face="bold")
        )

lc<- ggplot(data=dt2, aes(x=Quarter, y=Breath.Alcohol.Level, group=Reason, colour=Reason)) +
        blank_theme3 + geom_line(size=1) + geom_point(size=3) 
lc <- lc + ggtitle("Quarter vs Breath.Alcohol.Level") 
#lc  ##print lc

### ----- Arrange plots----------------------------------

grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 3)))
print(bc, vp = viewport(layout.pos.row = 1, layout.pos.col = 1)) 
print(lc, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(pc, vp = viewport(layout.pos.row = 1, layout.pos.col = 3))
grid.rect(.5,.5,width=unit(.99,"npc"), height=unit(0.99,"npc"), 
          gp=gpar(lwd=2, fill=NA, col="grey"))

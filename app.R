library(shiny)
library(ggplot2)
library(dplyr)
library(tseries)
library(stats)
library(RSiteCatalyst)
library(qcc)
library(stats)
library(scales)
library(curl)
library(httr)
ATUS_Comp <-"https://raw.githubusercontent.com/Misterresearch/VideoLandscape/master/All_Comp_DailyTimeSpent.csv"
ATUS_Compdf<-read.csv(ATUS_Comp, header = TRUE, sep = ",")
ATUS_TV_All <-"https://raw.githubusercontent.com/Misterresearch/VideoLandscape/master/All_TV_DailyTimeSpent.csv"
ATUS_TV_Alldf<-read.csv(ATUS_TV_All, header = TRUE, sep = ",")
ATUS_TV_wkday <-"https://raw.githubusercontent.com/Misterresearch/VideoLandscape/master/All_TV_DailyTimeSpent_WkDay.csv"
ATUS_TV_wkdaydf<-read.csv(ATUS_TV_wkday, header = TRUE, sep = ",")
Nielsen_Video <-"https://raw.githubusercontent.com/Misterresearch/VideoLandscape/master/All_TV_Nielsen.csv"
Nielsen_videodf<-read.csv(Nielsen_Video, header = TRUE, sep = ",")
Tokyo_Video<- "https://raw.githubusercontent.com/Misterresearch/VideoLandscape/master/Tokyo_TV_Viewing.csv"
Tokyo_videodf<-read.csv(Tokyo_Video, header = TRUE, sep = ",")
Nielsen_Live <-"https://raw.githubusercontent.com/Misterresearch/VideoLandscape/master/Live_TV_Nielsen.csv"
Nielsen_Livedf<-read.csv(Nielsen_Live, header = TRUE, sep = ",")
TV_Comp_Corr <-"https://raw.githubusercontent.com/Misterresearch/VideoLandscape/master/TV_Comp_Correlation.csv"
TV_Comp_Corrdf<-read.csv(Nielsen_Live, header = TRUE, sep = ",")
predictcomp<- predict(lm(Estimate ~ Year, data = ATUS_Compdf))
ATUSCompplot <- ggplot(ATUS_Compdf, aes(x = Year, y = Estimate)) + geom_point(color="blue")+scale_x_continuous(breaks = ATUS_Compdf$Year)+
geom_line(aes(y = predictcomp))+ylim(0,.5)+ggtitle("ATUS Avg Daily Hours on a Computer")+theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size = 14, hjust = 0))+geom_text(aes(label=Estimate), position = position_dodge(.2), size=3, vjust=-.5)
predictvideo<- predict(lm(Estimate ~ Year, data = ATUS_TV_Alldf))
ATUSTVAllplot <- ggplot(ATUS_TV_Alldf, aes(x = Year, y = Estimate)) + geom_point(color="green")+scale_x_continuous(breaks = ATUS_TV_Alldf$Year)+
geom_line(aes(y = predictvideo))+ylim(0,5)+ggtitle("ATUS Avg Daily Hours TV 7 days")+theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size = 14, hjust = 0))+geom_text(aes(label=Estimate), position = position_dodge(.2), size=3, vjust=-.75)
predictvideowkdy<- predict(lm(Estimate ~ Year, data = ATUS_TV_wkdaydf))
ATUSTVwkplot <- ggplot(ATUS_TV_wkdaydf, aes(x = Year, y = Estimate)) + geom_point(color="red")+scale_x_continuous(breaks = ATUS_TV_wkdaydf$Year)+
geom_line(aes(y = predictvideowkdy))+ylim(0,5)+ggtitle("ATUS Avg Daily Hours TV Weekdays")+theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size = 14, hjust = 0))+geom_text(aes(label=Estimate), position = position_dodge(.2), size=3, vjust=-.75)
predictnielsen<- predict(lm(Audience ~ Year, data = Nielsen_videodf))
NielsenAllplot <- ggplot(Nielsen_videodf, aes(x = Year, y = Audience)) + geom_point(color="black")+scale_x_continuous(breaks = Nielsen_videodf$Year)+
geom_line(aes(y = predictnielsen))+ylim(0,1000000)+ggtitle("Nielsen + Pivotal Avg Audience X-Platform TV & Online Estimate")+theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size = 14, hjust = 0))+geom_text(aes(label=comma(Audience)), position = position_dodge(.2), size=3, vjust=-.75)
predictnielsenlive<- predict(lm(Audience ~ Year, data = Nielsen_Livedf))
NielsenLivePlot <- ggplot(Nielsen_Livedf, aes(x = Year, y = Audience)) + geom_point(color="yellow")+scale_x_continuous(breaks = Nielsen_Livedf$Year)+
geom_line(aes(y = predictnielsenlive))+ylim(0,1000000)+ggtitle("Nielsen + Pivotal Avg Audience Live TV Estimate")+theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size = 14, hjust = 0))+geom_text(aes(label=comma(Audience)), position = position_dodge(.2), size=3, vjust=-.75)
#log transformed
predicttokyo<- predict(lm(Estimate ~ Year, data = Tokyo_videodf))
TokyoVideoplot <- ggplot(Tokyo_videodf, aes(x = Year, y = Estimate)) + geom_point(color="brown")+scale_x_continuous(breaks = Tokyo_videodf$Year)+
geom_line(aes(y = predicttokyo))+ylim(0,5)+ggtitle("Hakuhodo - Tokyo Avg Daily Hours TV")+theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size = 14, hjust = 0))+geom_text(aes(label=Estimate), position = position_dodge(.2), size=3, vjust=-.75)
mychart <- list(ATUS_Online=ATUSCompplot,ATUS_TVAll=ATUSTVAllplot,ATUS_TVWkDy=ATUSTVwkplot,Nielsen_AllVideo=NielsenAllplot,Nielsen_LiveTV=NielsenLivePlot,Tokyo=TokyoVideoplot)
#mvideopplot<-mychart[1]
mychoices <- c("ATUS_Online","ATUS_TVAll","ATUS_TVWkDy","Nielsen_AllVideo","Nielsen_LiveTV","Tokyo")

ui = fluidPage(
  titlePanel("Video Landscape ATUS, Nielsen & Hakuhodo"),
  
  selectInput(inputId = "chartID", 
              label = "Choose the your Data Source",
              choices = mychoices,	selected = NULL, multiple = FALSE,
              selectize = TRUE, width = NULL, size = NULL),
  
  plotOutput("chart")
  
)

server = function(input,output) {
  
  output$chart <-renderPlot({
    
    mychart <- list(ATUS_Online=ATUSCompplot,ATUS_TVAll=ATUSTVAllplot,ATUS_TVWkDy=ATUSTVwkplot,Nielsen_AllVideo=NielsenAllplot,Nielsen_LiveTV=NielsenLivePlot,Tokyo=TokyoVideoplot)
    mychart[(input$chartID)]
    
  })
  
}
shinyApp(ui = ui, server = server)

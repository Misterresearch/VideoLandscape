library(ggplot2)
library(Rmisc)
library(dplyr)
library(tseries)
library(stats)
library(RSiteCatalyst)
library(qcc)
library(stats)
library(scales)
library(curl)
library(httr)
library(showtext)
library(formattable)
library(segmented)
library(prophet)
library(formattable)

#data ingest
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

#plots

predictcomp<- predict(lm(Estimate ~ Year, data = ATUS_Compdf))

ATUSCompplot <- ggplot(ATUS_Compdf, aes(x = Year, y = Estimate)) + geom_point(color="blue")+scale_x_continuous(breaks = ATUS_Compdf$Year)+ 
  geom_line(aes(y = predictcomp))+ylim(0,.5)+ggtitle("ATUS - Series Title: Avg hrs per day - Computer use for leisure")+theme(plot.title = element_text(color="Navy", face="bold", size = 14, hjust = 0))+geom_text(aes(label=round(Estimate, digits = 2)), position = position_dodge(.2), size=5, vjust=-.5)

predictvideo<- predict(lm(Estimate ~ Year, data = ATUS_TV_Alldf))

ATUSTVAllplot<- ggplot(ATUS_TV_Alldf, aes(x = Year, y = Estimate)) + geom_point(color="green")+scale_x_continuous(breaks = ATUS_TV_Alldf$Year)+ 
  geom_line(aes(y = predictvideo))+ylim(0,5)+ggtitle("ATUS - Series Title: Avg hrs per day - Watching TV ")+theme(plot.title = element_text(color="Navy", face="bold", size = 14, hjust = 0))+geom_text(aes(label=round(Estimate, digits = 2)), position = position_dodge(.2), size=5, vjust=-.75)

predictvideowkdy<- predict(lm(Estimate ~ Year, data = ATUS_TV_wkdaydf))



ATUSTVwkplot <- ggplot(ATUS_TV_wkdaydf, aes(x = Year, y = Estimate)) + geom_point(color="red")+scale_x_continuous(breaks = ATUS_TV_wkdaydf$Year)+ 
  geom_line(aes(y = predictvideowkdy))+ylim(0,5)+ggtitle("ATUS -Series Title: Avg hrs per day - Watching TV, Weekdays")+theme(plot.title = element_text(color="Navy", face="bold", size = 14, hjust = 0))+geom_text(aes(label=round(Estimate, digits = 2)), position = position_dodge(.2), size=5, vjust=-.75)

predictnielsen<- predict(lm(Audience ~ Year, data = Nielsen_videodf))

NielsenAllplot <- ggplot(Nielsen_videodf, aes(x = Year, y = (Audience/1000))) + geom_point(color="black")+scale_x_continuous(breaks = Nielsen_videodf$Year)+ geom_line(aes(y = predictnielsen/1000))+ylim(0,1000)+ggtitle("Pivotal Summed Annual Hours (Bn) X-Platform TV")+theme(plot.title = element_text(color="Navy", face="bold", size = 14, hjust = 0))+geom_text(aes(label=comma((Audience/1000), digits=0)), position = position_dodge(.2), size=5, vjust=-.75)

predictnielsenlive<- predict(lm(Audience ~ Year, data = Nielsen_Livedf))

NielsenLivePlot <- ggplot(Nielsen_Livedf, aes(x = Year, y = (Audience/1000))) + geom_point(color="yellow")+scale_x_continuous(breaks = Nielsen_Livedf$Year)+ 
  geom_line(aes(y = predictnielsenlive/1000))+ylim(0,1000)+ggtitle("Pivotal Summed Annual Hours (Bn) Live TV")+theme(plot.title = element_text(color="Navy", face="bold", size = 14, hjust = 0))+geom_text(aes(label=comma((Audience/1000),digits=0)), position = position_dodge(.2), size=5, vjust=-.75)

predicttokyo<- predict(lm(Estimate ~ Year, data = Tokyo_videodf))

TokyoVideoplot <- ggplot(Tokyo_videodf, aes(x = Year, y = Estimate)) + geom_point(color="brown")+scale_x_continuous(breaks = Tokyo_videodf$Year)+ 
  geom_line(aes(y = predicttokyo))+ylim(0,5)+ggtitle("Hakuhodo - Tokyo Avg Daily Hours TV")+theme(plot.title = element_text(color="Navy", face="bold", size = 14, hjust = 0))+geom_text(aes(label=round(Estimate, digits = 2)), position = position_dodge(.2), size=5, vjust=-.75)

cormatrix <- rbind(cor(ATUS_Compdf$Estimate,ATUS_TV_Alldf$Estimate),cor(Tokyo_videodf$Estimate[3:11],Nielsen_Livedf$Audience),cor(ATUS_TV_Alldf$Estimate[6:14],Nielsen_videodf$Audience),cor(ATUS_Compdf$Estimate[6:14],Nielsen_videodf$Audience))
mycor <- data.frame(cormatrix)
rownames(mycor) <- c("ATUS Computer & TV Correlation", "Hakuhodo & Pivotal Live TV Correlation","ATUS TV & Pivotal Xplatform Correlation", "ATUS Computer & Pivotal Xplatform Correlation")
colnames(mycor)<-c("Pearson's R")
formattable(mycor)

Pseries<-ts(ATUS_Compdf$Estimate, start=1, end = NROW(ATUS_Compdf$Estimate), frequency = 1)
#plot(Pseries, col = "blue", lwd = 4, main = "Historical Time Series")

ATUS_CompdfB<-ATUS_Compdf[c(1,3)]
colnames(ATUS_CompdfB)[which(names(ATUS_CompdfB) == "Estimate")] <- "y"
colnames(ATUS_CompdfB)[which(names(ATUS_CompdfB) == "Year")] <- "ds"
ATUS_CompdfB$ds  <- as.Date(as.character(ATUS_CompdfB$ds), "%Y")
m <- prophet(ATUS_CompdfB, yearly.seasonality = TRUE)
#summary(m)
future <- make_future_dataframe(m, periods = 5, freq = 'year')
forecast <- predict(m, future)

ATUS_CompforecastTS <- ts(forecast$yhat, start = c(2003, 1))
ATUS_CompTS <- ts(ATUS_Compdf$Estimate, start = c(2003,1))
ts.plot(ATUS_CompTS, ATUS_CompforecastTS, col = 1:2, lty = 1:2, lwd = 3:3)
tail(forecast$yhat, 5)

myforecast <- data.frame(tail(forecast$yhat, 5))
rownames(myforecast)<-c("2017","2018","2019","2020","2021")
colnames(myforecast)<-c("future prediction")
myforecast$`future prediction`<-comma(myforecast$`future prediction`, digits = 2)
formattable(myforecast)

trainingy <- tail(ATUS_CompdfB$y, n=14)
predictedy<-tail(forecast$yhat, n=14)

fitmatrix <- rbind(CI(trainingy, ci = .95),CI(predictedy, ci = .95))
myfit <- data.frame(fitmatrix)
rownames(myfit) <- c("Actual", "Prediction")
formattable(myfit)


PseriesA<-ts(ATUS_TV_Alldf$Estimate, start=1, end = NROW(ATUS_TV_Alldf$Estimate), frequency = 1)
#plot(Pseries, col = "blue", lwd = 4, main = "Historical Time Series")

ATUS_TV_AlldfB<-ATUS_TV_Alldf[c(1,3)]
colnames(ATUS_TV_AlldfB)[which(names(ATUS_TV_AlldfB) == "Estimate")] <- "y"
colnames(ATUS_TV_AlldfB)[which(names(ATUS_TV_AlldfB) == "Year")] <- "ds"
ATUS_TV_AlldfB$ds  <- as.Date(as.character(ATUS_TV_AlldfB$ds), "%Y")
m1 <- prophet(ATUS_TV_AlldfB, yearly.seasonality = TRUE)
#summary(m)
futureA <- make_future_dataframe(m1, periods = 5, freq = 'year')
forecastA <- predict(m1, futureA)

ATUS_TV_AllforecastTS <- ts(forecastA$yhat, start = c(2003, 1))
ATUS_TV_AllTS <- ts(ATUS_TV_Alldf$Estimate, start = c(2003,1))
ts.plot(ATUS_TV_AllTS, ATUS_TV_AllforecastTS, col = 1:2, lty = 1:2, lwd = 3:3)
tail(forecastA$yhat, 5)
myforecastA <- data.frame(tail(forecastA$yhat, 5))
rownames(myforecastA)<-c("2017","2018","2019","2020","2021")
colnames(myforecastA)<-c("future prediction")
myforecastA$`future prediction`<-comma(myforecastA$`future prediction`, digits = 2)
formattable(myforecastA)

trainingAy <- tail(ATUS_TV_AlldfB$y, n=14)
predictedAy<-tail(forecastA$yhat, n=14)

fitmatrixA <- rbind(CI(trainingAy, ci = .95),CI(predictedAy, ci = .95))
myfitA <- data.frame(fitmatrixA)
rownames(myfitA) <- c("Actual", "Prediction")
formattable(myfitA)

Nielsen_videodfA <- Nielsen_videodf
colnames(Nielsen_videodfA)[which(names(Nielsen_videodfA) == "Audience")] <- "y"
colnames(Nielsen_videodfA)[which(names(Nielsen_videodfA) == "Year")] <- "ds"
Nielsen_videodfA$ds  <- as.Date(as.character(Nielsen_videodfA$ds), "%Y")
m2 <- prophet(Nielsen_videodfA, yearly.seasonality = TRUE)
#summary(m)
futureB <- make_future_dataframe(m2, periods = 5, freq = 'year')
forecastB <- predict(m2, futureB)

Nielsen_VideoforecastTS <- ts(forecastB$yhat, start = c(2008, 1))
Nielsen_VideoTS <- ts(Nielsen_videodf$Audience, start = c(2008,1))
ts.plot(Nielsen_VideoTS, Nielsen_VideoforecastTS, col = 1:2, lty = 1:2, lwd = 3:3)
#tail(forecastB$yhat, 5)
myforecastB <- data.frame(tail(forecastB$yhat, 5))
rownames(myforecastB)<-c("2017","2018","2019","2020","2021")
colnames(myforecastB)<-c("future prediction")
myforecastB$`future prediction`<-comma(myforecastB$`future prediction`, digits = 0)
formattable(myforecastB)


trainingBy <- tail(Nielsen_videodfA$y, n=14)
predictedBy<-tail(forecastB$yhat, n=14)

fitmatrixB <- rbind(CI(trainingBy, ci = .95),CI(predictedBy, ci = .95))
myfitB <- data.frame(fitmatrixB)
rownames(myfitB) <- c("Actual", "Prediction")
formattable(myfitB)

summary_video_forecast <- data.frame(tail(forecast$yhat, 5),tail(forecastA$yhat, 5), tail(forecastB$yhat, 5))
rownames(summary_video_forecast) <- c("2017", "2018", "2019", "2020", "2021")
colnames(summary_video_forecast) <- c("ATUS Online Time", "ATUS All-TV Time", "Pivotal Total Hours (Bn)")
summary_video_forecast$`ATUS Online Time`<- round(summary_video_forecast$`ATUS Online Time`, 2)
summary_video_forecast$`ATUS All-TV Time`<- round(summary_video_forecast$`ATUS All-TV Time`, 2)
summary_video_forecast$`Pivotal Total Hours (Bn)`<-format(summary_video_forecast$`Pivotal Total Hours (Bn)`/1000, big.mark = ",", scientific = FALSE, digits = 0)

formattable(summary_video_forecast)

#app choices

mychart <- list(ATUS_Online=ATUSCompplot,ATUS_TVAll=ATUSTVAllplot,ATUS_TVWkDy=ATUSTVwkplot,Pivotal_AllVideo=NielsenAllplot,Pivotal_LiveTV=NielsenLivePlot,Tokyo=TokyoVideoplot)
#mvideopplot<-mychart[1]
mychoices <- c("ATUS_Online","ATUS_TVAll","ATUS_TVWkDy","Pivotal_AllVideo","Pivotal_LiveTV","Tokyo")

ui = fluidPage(title="Video Quick Look",
  titlePanel(p("Quick Look Video - American Time Survey, Nielsen & Hakuhodo", style = "color:#008000")),
  br(),
  selectInput(inputId = "chartID", 
              label = p(strong("Choose Your Data Source", style = "color:navy; font-size: 20px")),
              choices = mychoices,	selected = NULL, multiple = FALSE,
              selectize = TRUE, width = NULL, size = NULL),
  plotOutput("chart"),
  br(),
  br(),
  br(),
  p(strong("Correlation Table - Key Comparisons", style = "color:navy; font-size: 20px")),
  tableOutput("cortable"),
  br(),
  br(),
  br(),
  p(strong("Forecast Summary", style = "color:navy; font-size: 20px")),
  tableOutput("forecasttable"),
  p(em("All projections are not statistically significant"))
)

server = function(input,output) {
  
  output$chart <-renderPlot({
    
    mychart <- list(ATUS_Online=ATUSCompplot,ATUS_TVAll=ATUSTVAllplot,ATUS_TVWkDy=ATUSTVwkplot,Pivotal_AllVideo=NielsenAllplot,Pivotal_LiveTV=NielsenLivePlot,Tokyo=TokyoVideoplot)
    mychart[(input$chartID)]
    
  })
  
  output$cortable <-renderTable({
    cormatrix <- rbind(cor(ATUS_Compdf$Estimate,ATUS_TV_Alldf$Estimate),cor(Tokyo_videodf$Estimate[3:11],Nielsen_Livedf$Audience),cor(ATUS_TV_Alldf$Estimate[6:14],Nielsen_videodf$Audience),cor(ATUS_Compdf$Estimate[6:14],Nielsen_videodf$Audience))
    videodata <- c("ATUS Computer & TV Correlation", "Hakuhodo & Pivotal Live TV Correlation","ATUS TV & Pivotal Xplatform Correlation", "ATUS Computer & Pivotal Xplatform Correlation")
    mycor <- data.frame(videodata,cormatrix)
    colnames(mycor)<-c("Data","Pearson's R")
    mycor
  })
  
  output$forecasttable <-renderTable({
    summary_video_forecast <- data.frame(tail(forecast$yhat, 5),tail(forecastA$yhat, 5), tail(forecastB$yhat, 5))
    future_years <- c("2017", "2018", "2019", "2020", "2021")
    summary_video_forecast <- data.frame(future_years, summary_video_forecast)
    colnames(summary_video_forecast) <- c("Years","ATUS Online Time", "ATUS All-TV Time", "Pivotal Total Hours (Bn)")
    summary_video_forecast$`ATUS Online Time`<- round(summary_video_forecast$`ATUS Online Time`, 2)
    summary_video_forecast$`ATUS All-TV Time`<- round(summary_video_forecast$`ATUS All-TV Time`, 2)
    summary_video_forecast$`Pivotal Total Hours (Bn)`<-format(summary_video_forecast$`Pivotal Total Hours (Bn)`/1000, big.mark = ",", scientific = FALSE, digits = 0)
    summary_video_forecast
  })
  
}
shinyApp(ui = ui, server = server)

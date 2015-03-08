ReadData<- function()
{
  fileName <- "household_power_consumption.txt"
  tab5rows <- read.table(fileName, header = TRUE, nrows = 5, sep = ";")
  classes <- sapply(tab5rows, class)

  tblAll <- read.table(fileName, header = TRUE, sep = ";", colClasses = classes, nrows =2075259, 
    comment.char = "", na.strings = "?", stringsAsFactors = FALSE)
  
  # in workspace install.packages("sqldf")
  # load sqldf into workspace - library(sqldf)
  query<- "SELECT * FROM tblAll WHERE [Date] in ('1/2/2007', '2/2/2007') "
 
  #make the sbData a global var so it can be used from other functions
  sbData <<- sqldf(query)
}

drawPlot4<-function()
{
  ReadData()
  workData <- sbData
   
  as.Date(workData$Date,format='%d/%m/%Y')
  strptime(workData$Time,format='%H:%M:%S')
  as.numeric(workData$Global_Active_Power)
  
  library(lubridate)
  mydate = dmy(workData$Date) + hms(workData$Time)
  workData<-cbind(workData,mydate)

  library(datasets)
  
  png('plot4.png',width=480,height=480,units="px")
  
  par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))

  with(workData,
       {
         plot(workData$Global_active_power~workData$mydate, type="l",
              ylab="Global Active Power", xlab="")
         
         plot(workData$Voltage~workData$mydate, type="l",
              ylab="Voltage", xlab="datetime")
         
         plot(workData$Sub_metering_1~workData$mydate, type="l",ylab="Energy sub metering", xlab="")
        lines(workData$Sub_metering_2~workData$mydate,col='Red')
        lines(workData$Sub_metering_3~workData$mydate,col='Blue')
        legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, 
               legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
        
        plot(workData$Global_reactive_power~workData$mydate, type="l",
             ylab="Global_reactive_power", xlab="datetime")
       }
  )
 
  dev.off()
}
  

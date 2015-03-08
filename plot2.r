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

drawPlot2<-function()
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
  
  png('plot2.png',width=480,height=480,units="px")

  plot(workData$Global_active_power~workData$mydate, type="l",
      ylab="Global Active Power (kilowatts)", xlab="")
 
  dev.off()
}
  

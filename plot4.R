
#Sys.setlocale(category = "LC_ALL", locale = "eng") If you are using a non-english R studio, unannotate this line

cal_lines <- function(input = "household_power_consumption.txt",sep = c(";","."),start_date = "1/2/2007",end_date = "3/2/2007")
{
  con <- file(input,"r")
  ilines <- 0;
  iskip <- 0;
  flag <- 0;
  while(length(tline <- readLines(con, n = 1, warn = FALSE)) > 0)
  {
    tmp <- strsplit(tline,";")[[1]][1]
    ##print(tmp)
    if (tmp == end_date){print(tmp);break;}
    if (tmp == start_date & flag == 0){print(tmp);flag <- 1;}
    if (flag == 1){ilines <- ilines + 1}
    else
    {iskip <- iskip + 1}
  }
  close(con)
  c(iskip,ilines)
}

getdat1 <- function()
{
  mylines <- cal_lines();
  mydata <- read.table(file = "household_power_consumption.txt", sep = c(";","."), header = F,as.is = T,na.strings = "?",nrows = mylines[2],skip = mylines[1])
  con <- file("household_power_consumption.txt","r")
  tline <- readLines(con,n = 1, warn = F)
  close(con)
  names(mydata) <- strsplit(tline,split = c(";","."))[[1]]
  mydata
}

mydata <- getdat1()
Time <- c(strptime(numeric(0),"%d/%m/%Y %H:%M:%S"))
for (j in 1:dim(mydata)[1])
{Time[j]<-strptime(paste(mydata[["Date"]][j]," ",mydata[["Time"]][j]),"%d/%m/%Y %H:%M:%S")}
mydata <- as.data.frame(cbind(Time,mydata))

png(filename = "plot4.png",width = 480, height = 480, bg = "transparent",type = "cairo")
par(mfrow=c(2,2))
#plot1
plot(x = mydata$Time,y = mydata$Global_active_power,xlab = "",ylab = "Global Active Power (kilowatts)",main = NULL, type = "l")
#plot2
plot(x = mydata$Time,y = mydata$Voltage, xlab = "datetime",ylab = "Voltage",main = "", type = "l", yaxt = "n")
axis(side = 2, at = c(234,236,238,240,242,244,246), labels = c("234","","238","","242","","246"))
#plot3
plot(x = mydata$Time,y = mydata$Sub_metering_1,type = "l",xlab = "",ylab = "Energy sub metering",yaxt = "n")
lines(x = mydata$Time,y = mydata$Sub_metering_2,type = "l",col = "red")
lines(x = mydata$Time,y = mydata$Sub_metering_3,type = "l",col = "blue")
axis(side = 2,at = 10*c(0,1,2,3))
legend("topright",c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),lty = c(1,1,1),col = c("black","red","blue"),bty = "n")
#plot4
plot(x = mydata$Time,y = mydata$Global_reactive_power, type = "l",xlab = "datetime",ylab = "Global_reactive_power",lwd = 0.5)
dev.off()
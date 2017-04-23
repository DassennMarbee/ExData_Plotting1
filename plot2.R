Sys.setlocale(category = "LC_ALL", locale = "eng")

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

png(filename = "plot2.png",width = 480, height = 480, bg = "transparent",type = "cairo")
plot(x = mydata$Time,y = mydata$Global_active_power,xlab = "",ylab = "Global Active Power (kilowatts)",main = NULL, type = "l",yaxt = "n")
axis(side = 2, lwd = 2)
dev.off()
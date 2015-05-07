library(datasets)

read_data <- function(filename) {
  t <- read.table(filename, header = TRUE, sep=";")
  t[t == "?"] <- NA 
  t$Date <- as.Date( as.character(t$Date), "%d/%m/%Y") 
  t <- t[ t$Date > as.Date("2007-01-31", "%Y-%m-%d") & t$Date < as.Date("2007-02-03", "%Y-%m-%d"), ]
  
  return(t)
}

plot4 <- function(filename) {
  data <- read_data(filename)
  time <- strptime(paste(data$Date, data$Time), format="%Y-%m-%d %H:%M:%S")
  subMetering1 <- as.numeric(as.character(as.factor(data$Sub_metering_1)))
  subMetering2 <- as.numeric(as.character(as.factor(data$Sub_metering_2)))
  subMetering3 <- as.numeric(as.character(as.factor(data$Sub_metering_3)))
  activePower <- as.numeric(as.character(as.factor(data$Global_active_power)))
  reactivePower <- as.numeric(as.character(as.factor(data$Global_reactive_power)))
  voltage <- as.numeric(as.character(as.factor(data$Voltage)))
  
  png(filename = "plot4.png", width = 480, height = 480)
  par(mfrow = c(2,2))
  plot(time, activePower, type = "l", main = "", ylab = "Global Active Power", xlab="")
  plot(time, voltage, type = "l", main="", ylab = "Voltage", xlab="datetime")
  plot(time, subMetering1, type = "l", main = "", ylim=c(0,40),ylab = "Energy sub metering", xlab="", col = "black")
  lines(time, subMetering2, type = "l", col = "red")
  lines(time, subMetering3, type = "l", col = "blue")
  legend("topright", bty="n",  lty=c(1,1,1), col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  plot(time, reactivePower, type = "l", main = "", ylab = "Global_reactive_power", xlab="datetime")
  dev.off()
}
library(datasets)

read_data <- function(filename) {
  t <- read.table(filename, header = TRUE, sep=";")
  t[t == "?"] <- NA 
  t$Date <- as.Date( as.character(t$Date), "%d/%m/%Y") 
  t <- t[ t$Date > as.Date("2007-01-31", "%Y-%m-%d") & t$Date < as.Date("2007-02-03", "%Y-%m-%d"), ]
  
  return(t)
}

plot3 <- function(filename) {
  data <- read_data(filename)
  subMetering1 <- as.numeric(as.character(as.factor(data$Sub_metering_1)))
  subMetering2 <- as.numeric(as.character(as.factor(data$Sub_metering_2)))
  subMetering3 <- as.numeric(as.character(as.factor(data$Sub_metering_3)))
  time <- strptime(paste(data$Date, data$Time), format="%Y-%m-%d %H:%M:%S")
  png(filename = "plot3.png", width = 480, height = 480)
  plot(time, subMetering1, type = "l", main = "", ylim=c(0,40),ylab = "Energy sub metering", xlab="", col = "black")
  lines(time, subMetering2, type = "l", col = "red")
  lines(time, subMetering3, type = "l", col = "blue")
  legend("topright", lty=c(1,1,1), col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  dev.off()
}
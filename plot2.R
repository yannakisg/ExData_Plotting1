library(datasets)

read_data <- function(filename) {
  t <- read.table(filename, header = TRUE, sep=";")
  t[t == "?"] <- NA 
  t$Date <- as.Date( as.character(t$Date), "%d/%m/%Y") 
  t <- t[ t$Date > as.Date("2007-01-31", "%Y-%m-%d") & t$Date < as.Date("2007-02-03", "%Y-%m-%d"), ]
  
  return(t)
}

plot2 <- function(filename) {
  data <- read_data(filename)
  activePower <- as.numeric(as.character(as.factor(data$Global_active_power)))
  png(filename = "plot2.png", width = 480, height = 480)
  plot(strptime(paste(data$Date, data$Time), format="%Y-%m-%d %H:%M:%S"), activePower, type = "l", main = "", ylab = "Global Active Power (kilowatts)", xlab="")
  dev.off()
}
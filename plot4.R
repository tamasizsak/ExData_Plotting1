file <- unzip("exdata-data-household_power_consumption.zip")
if(!file.exists("exdata-data-household_power_consumption.zip")) {
        temp <- tempfile()
        download.file(
                "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",temp
        )
        file <- unzip(temp)
        unlink(temp)
}

power <- read.table(file, header = T, sep = ";")
power$Date <- as.Date(power$Date, format = "%d/%m/%Y")
df <- power[(power$Date == "2007-02-01") |
                    (power$Date == "2007-02-02"),]
df$Global_active_power <- as.numeric(as.character(df$Global_active_power))
df$Global_reactive_power <- as.numeric(as.character(df$Global_reactive_power))
df$Voltage <- as.numeric(as.character(df$Voltage))
df <- transform(df, timestamp = as.POSIXct(paste(Date, Time)), "%d/%m/%Y %H:%M:%S")
df$Sub_metering_1 <- as.numeric(as.character(df$Sub_metering_1))
df$Sub_metering_2 <- as.numeric(as.character(df$Sub_metering_2))
df$Sub_metering_3 <- as.numeric(as.character(df$Sub_metering_3)
)

plot4 <- function() {
        par(mfrow = c(2,2))
        
        #PLOT1
        plot(
                df$timestamp,df$Global_active_power, type = "l", xlab = "", ylab = "Global Active Power"
        )
        
        #PLOT2
        plot(
                df$timestamp,df$Voltage, type = "l", xlab = "datetime", ylab = "Voltage"
        )
        
        #PLOT3
        plot(
                df$timestamp,df$Sub_metering_1, type = "l", xlab = "", ylab = "Energy sub metering"
        )
        lines(df$timestamp,df$Sub_metering_2,col = "red")
        lines(df$timestamp,df$Sub_metering_3,col = "blue")
        legend(
                "topright", col = c("black","red","blue"), c("Sub_metering_1  ","Sub_metering_2  ", "Sub_metering_3  "),lty =
                        c(1,1), bty = "n", cex = .5
        )
        
        #PLOT4
        plot(
                df$timestamp,df$Global_reactive_power, type = "l", xlab = "datetime", ylab =
                        "Global_reactive_power"
        )
        
        #Save plots to png
        dev.copy(png, file = "plot4.png", width = 480, height = 480)
        dev.off()
        cat("The plot4.png saved to the following location:", getwd())
}
plot4()


#I use hungarian-language operating system, so in my repo on the uploaded png:
#the "Cs" means "Thu"
#the "P" means "Fri"
#the "Szo" means "Sat"
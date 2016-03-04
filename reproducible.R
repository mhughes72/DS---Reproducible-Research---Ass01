
#This is all the code to complete Asignment #1.
#As is the code won't run, varying chunks need to be commented out or
#data and plots will overlap

repo1 <- function() {
  #Load raw data
  data <- read.csv("activity.csv", header=T, sep=",")
  
  #Mean steps per day
  data.stepsDay <- aggregate(data$steps, by=list(data$date), FUN=sum, na.rm=F)
  data.stepsDayMean <- mean(data.stepsDay$x, na.rm=T)
  data.stepsDayMedian <- median(data.stepsDay$x, na.rm=T)
 hist(data.stepsDay$x, 
       breaks=30,
       main="Steps taken per day", 
       col="green", 
       xlab="Steps")
  
  
  #Average Daily Activity Patern
 data.stepsInterval <- aggregate(data$steps, by=list(data$interval), FUN=mean, na.rm=T)
 plot( x=data.stepsInterval[,1], 
       y=data.stepsInterval[,2], 
       type="l",
       col="green",
       main="Mean of steps per interval",
       ylab="Steps", 
       xlab="Interval")
  
  #Input missing values
 nas   <- sum(is.na(data))
 compl <- sum(complete.cases(data))
 
 data.clean <- cbind(data, data.stepsInterval[,2])
 names(data.clean)[4] <- c("mean")
 
 data.clean$steps <- ifelse( is.na(data.clean$steps), data.clean$mean, data.clean$steps)
 data.clean.stepsDay <- aggregate(data.clean$steps, by=list(data.clean$date), FUN=sum)
 data.clean.stepsDayMean <- mean(data.clean.stepsDay$x)
 data.clean.stepsDayMedian <- median(data.clean.stepsDay$x,)
 
 hist(data.clean.stepsDay$x, 
      breaks=20,
      main="NEW Steps per DAy", 
      col="green", 
      xlab="Steps")
 
 
  
  # What are the differences?
 data.clean$date <- strptime(data.clean$date, "%Y-%m-%d")
 data.clean$weekend <- (weekdays(data.clean$date) %in% c("Sunday", "Saturday"))
 data.weekend <- data.clean[data.clean$weekend == TRUE,]
 data.weekday <- data.clean[data.clean$weekend == FALSE,]
 
 data.weekend.steps <- aggregate(data.weekend$steps, by=list(data.weekend$interval), FUN=mean)
 data.weekday.steps <- aggregate(data.weekday$steps, by=list(data.weekday$interval), FUN=mean)
 
 par(mfrow=c(2,1))
 
 plot( x=data.weekend.steps[,1], 
       y=data.weekend.steps[,2], 
       type="l",
       col=124,
       main="Weekend",
       ylab="Steps", 
       xlab="Interval")
 
 plot( x=data.weekday.steps[,1], 
       y=data.weekday.steps[,2], 
       type="l",
       col=554,
       main="Weekday",
       ylab="Steps", 
       xlab="Interval")
 
  return(data)

}
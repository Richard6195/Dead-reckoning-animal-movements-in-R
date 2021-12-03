#Example process of the MVF protocol
#This script is modeled around one lion Daily Diary (DD) motion sensor data set (exported at 10 Hz) and a technosmart (GiPSy-5) GPS dataset (1 Hz)

#Required libraries for data manipulation
install.packages("zoo") ; install.packages("dplyr") ; install.packages("lubridate") 
library(zoo) ; library(dplyr) ; library(lubridate)

#Required libraries for visual inspection of results
install.packages('ggforce') ; install.packages('tidyverse') ; install.packages('ggplot2')
library(ggforce) ; library(tidyverse) ; library(ggplot2)

#################################################The thresholds#####################################################################################################################################
#The various values used to compute MVF - these values are user-defined, changed according to the species in question. For more information, see the below code where they are implemented

s = 20 #Smoothing window for computing static acceleration and post-smoothing VeDBA (~ 2 s because data = 10 Hz) - The post-smoothing value can obviously be different to that used to derive static acceleration though here they are the same
dist.step = 5 #Stepping interval to compute Haversine distance between - Here we use five fix stepping interval = ~ 5 s (assuming no missing fixes) when computing distance between GPS fixes
flex.1 = 3 #A degree of optional flexibility - This is used to both interpolate between speed values if maximum gap between fixes is <= than 3 s here. Any values converted from NA to an interpolated value assumes that a fix was indeed taken (so passes first stage of MVF protocol)
speed.sm = 5 #the smoothing window used for GPS-derived speed (s)
MeFF = 60 #The median rolling filter to pass over the longitude and latitude coordinates prior to deriving the Z threshold
Z = 100 #Z threshold (units in m) for filtering out extreme outliers
X = 0.11 #The DBA (VeDBA) threshold (units in g)
Y = 0.35 #The GPS~derived speed threshold (units in m/s)
flex.2 = 2  #Another degree of optional flexibility - Potential MVF periods encoded as 1 (that surpassed the above thresholds) occurring <= 2 s (flex.2) from one another (separated by MVF values of zero) are merged prior to the T threshold. But only if GPS fix was present (though see flex.1) and the Z threshold passed the MVF protocol (e.g., < 100 m here)
T = 5 #Time threshold (T) - the uninterrupted (consecutive) time (s) that 'potential' MVF values of 1 must be present for, for a period to be classified as genuine travelling movement

#################################################COMPUTE VEDBA#####################################################################################################################################
#(1) Set working directory and load in motion sensor (DD) data
setwd("D:/Dropbox/MVF.method/Example") #Change as appropriate to set working directory containing the motion sensor data sets and GPS file
#Here, the motion sensor data is composed of multiple .txt files (name ending with '.txt'), with these 2 lines of code we bind them all into one data frame (termed 'df')
temp = list.files(pattern="*.txt") 
df = do.call(rbind,lapply(temp, function(x) read.delim(x, stringsAsFactors = T, header=T))) 
#Now to ensure the data is in the correct order, we arrange based on a column termed 'Total.Event.no.', though this could be any column or multiple columns, e.g., containing month, day, time etc.)
df<-arrange(df, Total.Event.no.)
df$X = NULL #Remove this column, as this was just filled with NA values

#(2) Create time stamp object and subset data frame based on a start and end time stamp reading
df$timestamp = paste(df$Date, df$Time.hh.mm.ss.ddd)
options(digits.secs = 3) #Ensure 3 digits shown for infra-second times
df$timestamp<-as.POSIXct(strptime(df$timestamp, format= "%d/%m/%Y %H:%M:%OS"), tz = "GMT")
head(df$timestamp, n = 1) ; tail(df$timestamp, n = 1) #See what the dtart and end times are
#Arbitrary example, subset 5 days of data
date1 <- as.POSIXct("2019-02-26 08:00:00.000") 
date2 <- as.POSIXct("2019-03-03 08:00:00.000")
int <- interval(date1, date2)
df<-df[df$timestamp %within% int,]

#(3) Compute static acceleration (2 s used here = approx. 20 events/rows))
df$Acc_x.sm = rollapply(df$Acc_x, width=s, FUN=mean, align="center", fill="extend")  
df$Acc_y.sm = rollapply(df$Acc_y, width=s, FUN=mean, align="center", fill="extend")
df$Acc_z.sm = rollapply(df$Acc_z, width=s, FUN=mean, align="center", fill="extend")

#(4) Calculate VeDBA (Also post-smooth VeDBA (2 s used here = approx. 20 events/rows))
df$VeDBA = sqrt((df$Acc_x - df$Acc_x.sm)^2 + (df$Acc_y - df$Acc_y.sm)^2 + (df$Acc_z - df$Acc_z.sm)^2)                     
df$VeDBA.sm = rollapply(df$VeDBA, width = s, FUN=mean, align="center", fill="extend")  


#################################################COMPUTE GPS-derived speed#########################################################################################################################
#(5) load in GPS data (here only 1 file) and term data frame; 'L1.GPS'
L1.GPS <- read.delim("D:/Dropbox/MVF.method/Example/L1.GPS.csv") #Change directory path as appropriate

#(6) Create time stamp object and subset data frame based on a start and end time stamp reading
L1.GPS$timestamp = paste(L1.GPS$Date, L1.GPS$Time)
L1.GPS$timestamp <- as.POSIXct(strptime(L1.GPS$timestamp, format= "%d/%m/%Y %H:%M:%OS"), tz = "GMT")
#Subset to same time period as the motion sensor df
L1.GPS <- L1.GPS[L1.GPS$timestamp %within% int,]
L1.GPS <- L1.GPS[!duplicated(L1.GPS$timestamp), ] #Remove any potential duplicated time readings
#Note that this GPS data file has already been converted to local S.Africa time to match with DD data, this required adding 2 hours (7200 s) because GPS is typically logged with GMT time zone and the
#DD is typically calibrated to local time. In R, this just involved the following; L1.GPS$timestamp = L1.GPS$timestamp + 7200 
L1.GPS <- L1.GPS[order(L1.GPS$timestamp),] #Ensure time data frame is in correct time order

#(7) Compute GPS-derived speed by first computing distance with the Haversine formula (made into a function below)
disty = function(long1, lat1, long2, lat2) { #longitude and latitude supplied in degrees
  long1 = long1 * pi/180 ; long2 = long2 * pi/180 ; lat1 = lat1 * pi/180 ; lat2 = lat2 * pi/180 #Function converts to radians
  a = sin((lat2 - lat1) / 2) * sin((lat2 - lat1) / 2) + cos(lat1) * cos(lat2) * sin((long2 - long1) / 2) * sin((long2 - long1) / 2)
  c = 2 * atan2(sqrt(a), sqrt(1 - a))
  d1 = 6378137 * c
  return(d1)
}

#(8) Specify the stepping range you want between consecutive fixes to compute distance. Too short of an interval for high res GPS data sets (e.g., 1 Hz), may incorporate precision error. Too big of an interval may miss out the tortuoisty involved in genuine movement
L1.GPS$GPS.loni = c(L1.GPS[-c(1:dist.step), 'location.long'], rep(0, dist.step)) ; L1.GPS$GPS.lati = c(L1.GPS[-c(1:dist.step), 'location.lat'], rep(0, dist.step)) #Shift column values forward by the specified stepping range and add relevant number of NA's  to the end (to maintain vector length of column)
L1.GPS$GPS.distance = disty(L1.GPS$location.long, L1.GPS$location.lat, L1.GPS$GPS.loni, L1.GPS$GPS.lati)      #Calculate row-wise distance between successive GPS coordinates (according to stepping range)
L1.GPS$GPS.distance = c(rep(0, dist.step), L1.GPS$GPS.distance[c(1:(nrow(L1.GPS)-dist.step))]) #Shift values back by by the specified stepping range

#(9) Now compute the time difference (TD) between values in similar manner
L1.GPS$TD = c(L1.GPS[-c(1:dist.step), 'timestamp'], rep(NA, dist.step)) 
L1.GPS$TD = c(difftime(L1.GPS$TD, L1.GPS$timestamp, units = "secs")) #Row-wise time difference
L1.GPS$TD = c(rep(0, dist.step), L1.GPS$TD[c(1:(nrow(L1.GPS)-dist.step))]) #Shift values back by by the specified stepping range

#(10) Calculate speed (m/s)
L1.GPS$GPS.speed = L1.GPS$GPS.distance / L1.GPS$TD 
L1.GPS$GPS.speed[1:dist.step] = 0 #Make the first 'x' number of rows (length of assigned dist.step zero rather than NAN value)


#################################################Merge motion sensor and GPS data together##########################################################################################################
#(11) Merge the relevant columns of the GPS data frame with the motion sensor one.
df = merge(df, L1.GPS[, c('timestamp', 'location.long', 'location.lat', 'GPS.distance', 'TD', 'GPS.speed')], by = 'timestamp', all = TRUE) 
#Ensure Time stamps of DD (or any IMU data) and GPS at times when fixes were present match exactly

#(12) Because we have many motion sensor data values between GPS data values, we want to specify two things;
#(i) Are GPS related NA values due to the standard inter-fix (1 s) intervals (between second values, e.g., motion sensor time stamps not ending in '.000'), or
#(ii) Are GPS related NA values due to missing location data where fix success rate dropped out - for greater than 1 s in this case.
df$index.sec = df$timestamp - round_date(df$timestamp, unit="1 seconds") #(i) Subtracting the infra-second time stamps with a rounded version (no decimal seconds) will read a difference of exactly zero at the location of each 'whole second'. #Notably variations of this 'index' computation may be needed if GPS was set at different logging rate (e.g., 2 Hz)
df$index.sec = ifelse(df$index.sec == 0, 1, 0) #(i) Values of one code for 'whole seconds' (where GPS is meant to be present) and values of zero code for infra-second periods
df$GPS.fix.present = ifelse(is.na(df$location.long) == TRUE, 0, 1) #(ii) Values of one code for the GPS fix being present (on the second mark) and values of zero code for no fixes  

#(13) Subset df to 1 Hz (same as GPS logging frequency) #This is based on the 'index.sec' column made above because GPS was set to record at 1 Hz.
df = subset(df, df$index.sec == 1)

#(13) Optional but add a degree of interpolation between speed values if maximum gap between fixes is less than x s
#This essentially gives the user the option of still considering speed values during small periods of missing data.
df$GPS.speed.interp = na.approx(df$GPS.speed, maxgap = flex.1, rule = 2)# Here if values occur <= 3 s to one another, we interpolate
#(14) Optional, but add a degree of (center-aligned) post-smoothing to GPS speed values
df$GPS.speed.interp.sm = rollapply(df$GPS.speed.interp, width = speed.sm, FUN = mean, fill = 'extend') #5 smoothing window used here
df$GPS.speed.interp.sm = ifelse(is.na(df$GPS.speed.interp) == TRUE, NA, df$GPS.speed.interp.sm) #If interpolated speed is NA then max gap between readings > 3 s (flex.1), so replace speed values with NA to ensure they are not considered at such times

#################################################The MVF thresholds################################################################################################################################## 
#(15) Should we consider the VeDBA & GPS speed in first place - This is based on step (13), if NA present we will not consider the fix (or rather 'interpolated speed')
df$thresh.consid = ifelse(is.na(df$GPS.speed.interp.sm) == TRUE, 0, 1) #Values of one mean we pass step one of MVF protocol. Note if step 13 is not performed, then this is essentially the 'GPS.fix.present' column

#(16) Using GPS-derived distance to identify extreme outliers - Distance threshold (Z). This example does not have 'extreme' outlier per se, though as an example... 
#By applying a rolling median using a suitable window length, large distance estimates reflecting either a single or multiple 'batched' outlier(s) can be distinguished from fixes deemed 'accurate' but highly separated 
#in space due to large gaps in locational data. The window length size and Z threshold should be chosen according to the animal in question due to the scales of movement undertaken by different species 
#median filter window length (MeFF) of 60 s and a lenient threshold of 100 m used for lions. 
df$Lon.approx = na.approx(df$location.long, rule = 2) #First linearly interpolate between NA's - ensure first observation is not an NA (row one))
df$Lat.approx = na.approx(df$location.lat, rule = 2)
df$Med.lon = rollapply(df$Lon.approx, width = MeFF, align = 'center', fill = "extend", FUN=median) # 60 s used here
df$Med.lat = rollapply(df$Lat.approx, width = MeFF, align = 'center', fill = "extend", FUN=median)
df$dist.thresh = disty(df$Lon.approx, df$Lat.approx, df$Med.lon, df$Med.lat)
df$Z.thresh = ifelse(df$dist.thresh < Z, 1, 0) #Values of one mean that we pass step two of MVF protocol                                

#Investigatory plots
par(mfrow=c(1,1))
plot(df$dist.thresh, type = "l") ; abline(h=100, col="red", lty=2) #Gives an idea of thresh to use (this thresh could indeed probably be lower than 100)
sub = subset(df, df$Z.thresh == 0)
df %>% 
  ggplot(aes(x = Lon.approx, y = Lat.approx,  color= factor(Z.thresh)))+
  geom_path(group = -1)+
  geom_point(data = sub, aes(x = Lon.approx, y = Lat.approx), col = "red")+
  facet_zoom(xlim = c(20.8380, 20.843), ylim = c(-26.206, -26.196))+ theme_bw()
#Note the 'jitter hotspots'

#(17) The VeDBA (X) and GPS-speed (Y) thresholds (0.11 g and 0.35 m/s respectively here)
df$X.thresh = ifelse(df$VeDBA.sm > X, 1, 0) #Values of one mean we pass step three (DBA component) of MVF protocol
df$Y.thresh = ifelse(df$GPS.speed.interp.sm > Y, 1, 0) #Values of one mean we pass step three (GPS speed component) of MVF protocol

#(18) Potential MVF values prior to the time threshold
df$MVF = ifelse(df$thresh.consid == 1 & df$Z.thresh == 1 & df$X.thresh == 1 & df$Y.thresh == 1, 1, 0) #Values of one mean that steps 1 to 3 of MVF protocol are passed 

#(19) #Time threshold (T) - the uninterrupted (consecutive) time (s) that 'potential' MVF values of 1 must be present for, for a period to be classified as genuine travelling movement
#Here, we use 5 s, #Though to add another degree of optional flexibility (flex.2) - Potential MVF periods encoded as 1 (that surpassed the above thresholds) occurring <= 2 s (flex.2) from one another (separated by MVF values of zero) are merged prior to the T threshold
#First, compute unique recurrence of the MVF values
y1 = rle(df$MVF)$values
y2 = rle(df$MVF)$lengths
z = c()
for(i in unique(y1)){
  z[y1==i] = 1:sum(y1 ==i)
}
df$MVF.unique.occurence = rep(z,y2) #Unique recurrence of MVF values of 0 and 1
df$MVF.and.unique.occurence = paste(df$MVF, df$MVF.unique.occurence) # Acts as the grouping variable to compute maximum time of uninterrupted duration
df = df %>% group_by(MVF.and.unique.occurence) %>% mutate(time.thresh = max(sequence(rle(MVF.and.unique.occurence)$lengths))) #Maximum time (s) of each recurrence
#Now this optional step is to convert MVF periods of zero to one if only 1 or 2 s in duration (as governed by flex.2 input).
#However, we only do this if the first two thresholds 'thresh.consid' and Z.thresh' remain surpassed during such times (e.g., they remained one)
df = df %>% group_by(MVF.and.unique.occurence) %>% mutate(consid.check = min(thresh.consid)) # If one then all good
df = df %>% group_by(MVF.and.unique.occurence) %>% mutate(Z.check = min(Z.thresh)) # If one then all good
df$MVF = ifelse(df$MVF == 0 & df$time.thresh <= flex.2 & df$consid.check > 0 & df$Z.check > 0, 1, ifelse(df$MVF == 1, 1, 0)) #Values converted to one if the above criteria is reached 

#Now repeat with the updated MVF values 
y1 = rle(df$MVF)$values
y2 = rle(df$MVF)$lengths
z = c()
for(i in unique(y1)){
  z[y1==i] = 1:sum(y1 ==i)
}
df$MVF.unique.occurence = rep(z,y2) #Unique recurrence of MVF values of 0 and 1
df$MVF.and.unique.occurence = paste(df$MVF, df$MVF.unique.occurence) # Acts as the grouping variable to compute maximum time of uninterrupted duration
df = df %>% group_by(MVF.and.unique.occurence) %>% mutate(time.thresh = max(sequence(rle(MVF.and.unique.occurence)$lengths))) #Maximum time (s) of each recurrence
#The time threshold
df$T.thresh = ifelse(df$time.thresh >= T, 1, 0) ##Values of one mean either mmoving or non-moving periods >= 5 s in duration
  
#(20) MVF values with all thresholds (1 = genuine movement and 0 = non-travelling movement/missing/erroneous GPS fixes)
df$MVF = ifelse(df$T.thresh == 1 & df$MVF == 1, 1, 0)

#Investigatory plots
df %>% 
  ggplot(aes(x = location.long, y = location.lat,  color= factor(MVF)))+
  geom_path(group = -1)+
  facet_zoom(xlim = c(20.8380, 20.843), ylim = c(-26.206, -26.196))+ theme_bw()
#Subset a section of data to plot
date1 <- as.POSIXct("2019-02-28 06:00:00.000") 
date2 <- as.POSIXct("2019-02-28 12:00:00.000")
int <- interval(date1, date2)
df.sub<-df[df$timestamp %within% int,]
par(mfrow = c(3, 1))
col.list = c("blue", "red") ; palette(col.list)
plot(df.sub$MVF, col=factor(df.sub$MVF), ylab = "MVF")
plot(df.sub$GPS.speed.interp.sm, type = "l", col = "red", ylab = "GPS-speed (m/s)")
plot(df.sub$VeDBA.sm, type = "l", col = "blue", ylab = "VeDBA (g)")
#zoom in on a section of mostly walking
plot(df.sub$MVF[1:500], col=factor(df.sub$MVF), ylab = "MVF")
plot(df.sub$GPS.speed.interp.sm[1:500], type = "l", col = "red", ylab = "GPS-speed (m/s)")
plot(df.sub$VeDBA.sm[1:500], type = "l", col = "blue", ylab = "VeDBA (g)")




############################################################END##########################################################################################################










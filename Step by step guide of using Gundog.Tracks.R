#Example process of dead-reckoning using Gundog.Tracks, with initial computation of heading using Gundog.Compass
#This script is modeled around a raw penguin outgoing trip (included within Supplementary Information (SI))
#The raw penguin file consists of Daily Diary (DD) and Techno-smart GPS data (DD data = 40 Hz ; GPS data = 1 Hz)
#Within this data file, we have;
#(i) Total events column
#(ii) various time outputs from DD
#(iii) Raw tri-axial accelerometer data
#(iv) Raw uncorrected tri-axial magnetometer data
#(v) GPS merged in time to DD data - Observations are carried forward so replicated fixes occur at times of missing relocation data
#(vi) Marked events - Value of one = Identified penguin walking periods
#                   - Value of two = Magnetic calibration period (we need to change this to 'M', required within Gundog.Compass)
#                   - Value of three = The pitch and roll at every anti-cardinal direction followed by 3 circles finishing at North (see SI. 2 for relevance)

#Note the actual penguin walking periods are between total event no. 1237489 to 1452525 
#Note starting GPS longitude (lo) and latitude (la) coordinates are -63.86774 and -42.08654
#Note finishing GPS longitude (lo) and latitude (la) coordinates are -63.86892 and -42.08296 #e.g., if you wanted to test reverse DR

#Required libraries
install.packages("zoo") ; install.packages("dplyr") 
library(zoo) ; library(dplyr)

#Note, currently, scripts are composed of base R syntax, using data frames. Code can be optimized further to become more efficient at computing larger data sets
#For example, note the time difference between the 'data.frame' and 'data.table' versions of implementing a running mean:
library(data.table)
df <- data.frame(Ax=sample(1:1000,10000000, replace=T),
                 Ay=sample(1:1000,10000000, replace=T),
                 Az=sample(1:1000,10000000, replace=T))
w=40
# data.frame / zoo solution
system.time({
  df$Gx = zoo::rollapply(df$Ax, width=w, FUN=mean, align="center", fill="extend")  
  df$Gy = zoo::rollapply(df$Ay, width=w, FUN=mean, align="center", fill="extend")  
  df$Gz = zoo::rollapply(df$Az, width=w, FUN=mean, align="center", fill="extend")  
})
# data.table solution
system.time({
  dt <- setDT(df)[, c("Gx","Gy", "Gz") := lapply(.SD,function(x) frollmean(x, n = w, align="center", adaptive=F)),
                  .SDcols = c("Ax", "Ay", "Az")]
})
View(dt[40:140,])

###############################################################################################################################################################################
#Procedure (assume the scripts and penguin file are located in the same directory folder)
#1) Set working directory and read in the scripts and penguin data file
setwd("D:/Dropbox/DR.paper/Example") #Change as appropriate
df <- read.delim("D:/Dropbox/DR.paper/Example/Test.Data.P10A_selection_split#1_0.txt") #Change as appropriate
source("D:/Dropbox/DR.paper/Example/Gundog.Tracks.R") #Change as appropriate
source("D:/Dropbox/DR.paper/Example/Gundog.Compass.R") #Change as appropriate

#2) Convert time to POSIXct class (date-time object) (set decimal seconds to 3 decimal places)
options(digits.secs = 3)
df$timestamp = as.POSIXct(strptime(paste(df$Date, df$Time.hh.mm.ss.ddd), format = "%d/%m/%Y %H:%M:%OS", tz = "GMT"))
head(df$timestamp)

#3) Replace marked event values of 2 to 'M' (Gundog.Compass identifies the magnetic calibration periods as marked events labeled 'M'. In the original file, the magnetic calibration (cf. SI2) period is denoted as '2')
df$Marked.event = ifelse(df$Marked.event == 2, "M", df$Marked.event)

#4) Compute static acceleration (using 2 s rolling mean (80 events))
df$Acc_x.sm = rollapply(df$Acc_x, width=80, FUN=mean, align="center", fill="extend") #Note extend to used within rollapply wrappers because no NA's permitted in function (though could remove rows filled with NA's (ensuring initial/last VP is kept if located in one of the deleted rows)) 
df$Acc_y.sm = rollapply(df$Acc_y, width=80, FUN=mean, align="center", fill="extend")
df$Acc_z.sm = rollapply(df$Acc_z, width=80, FUN=mean, align="center", fill="extend")

#5) Optional, but smooth raw mag channels somewhat (for 40 Hz data, we set 10 events as default)
df$Mag_x.sm = rollapply(df$Mag_x, width=10, FUN=mean, align="center", fill="extend")
df$Mag_y.sm = rollapply(df$Mag_y, width=10, FUN=mean, align="center", fill="extend")
df$Mag_z.sm = rollapply(df$Mag_z, width=10, FUN=mean, align="center", fill="extend")

#6) Ensure GPS fixes read zero at times when fixes were actually not present - In this original file there is a column called 'GPS.fix.present' which reads zero when no positions  were recorded
df$GPS.Longitude = ifelse(df$GPS.fix.present == 0, 0, df$GPS.Longitude)
df$GPS.Latitude = ifelse(df$GPS.fix.present == 0, 0, df$GPS.Latitude)

#7) Calculate VeDBA (assuming DBA~speed within Gundog.Tracks is desired. Also post-smooth VeDBA (2 s used here)
df$VeDBA = sqrt((df$Acc_x - df$Acc_x.sm)^2 + (df$Acc_y - df$Acc_y.sm)^2 + (df$Acc_z - df$Acc_z.sm)^2)                     
df$VeDBA.sm = rollapply(df$VeDBA, width=80, FUN=mean, align="center", fill="extend")  

#8) Do magnetic calibration and compute heading (see SI3 for details of the Gundog.Compass function and SI2 for details regarding the channel configuration (order of channel input)) - Note acc_x,y,z.sm and mag_x,y,z.sm are normalized within the function
#Essentially, if tri-axial sphere is just offset (not much of an ellipsoid, then use method = 1 or 8, method = 2 and 3 are for rotated ellipsoids and method = 4,5,6,7 are for non-rotated ellipsoids. Default = 3)
#For penguin walking, DD is positioned vertical (x axis = heave dimension, pointing up, z axis = surge dimension pointing away from animal's back), so channel order is the following (here we assume no device offset relative to the body-carried NED frame, as parameterised with Euler angles);
df.corr = Gundog.Compass(mag.x = -df$Mag_z.sm, mag.y = -df$Mag_y.sm, mag.z = -df$Mag_x.sm, acc.x = df$Acc_z.sm, acc.y = df$Acc_y.sm, acc.z = df$Acc_x.sm, ME = df$Marked.event, pitch.offset = 0, roll.offset = 0, yaw.offset = 0, method = 1, plot=TRUE)

#9) The important columns from df.corr are pitch roll and yaw, so merge back into df (see SI.3 for the other channel outputs)
df = cbind(df, df.corr[, c('Pitch', 'Roll', 'Yaw')])

#9) Given this is a large file with the walking track right at the end, lets shorten to relevant section and specify to use ME = 1 (identified walking periods)
#Gundog.Tracks = function(TS, h, v, elv = 0, p = NULL, cs = NULL, ch = NULL, m = 1, c = 0, ME = 1, lo = 0, la = 0, VP.lon = NULL, VP.lat = NULL, VP.ME = FALSE, method = NULL, thresh = NULL, dist.step = 1, bound = TRUE, Outgoing = TRUE, plot = FALSE)   

#In this example, I will use the method = 'dist' (using a stepping range of 5 fixes between GPS points (dist.step = 5) to calculate the cumulative distance in the first place (only fixes where ME = 1 are considered - by stating VP.ME = TRUE))
#I will use thresh = 12, which divides the cumulative distance by 12 and the nearest fix at these 'break points' are used to correct
#It will make function much quicker to shorten data frame to the start and end of relevant trajectory
df.sub = subset(df, df$Total.Event.no. >= 1237489 & df$Total.Event.no. <= 1452525)
penguin.track = Gundog.Tracks(TS = df.sub$timestamp, h = df.sub$Yaw, v = df.sub$VeDBA.sm, elv = 0, p = NULL, cs = NULL, ch = NULL, m = 1.5, c = 0, ME = df.sub$Marked.event, lo = -63.86774, la = -42.08654, VP.lon = df.sub$GPS.Longitude, VP.lat = df.sub$GPS.Latitude, VP.ME = TRUE, method = "distance", thresh = 12, dist.step = 5, bound = TRUE, Outgoing = TRUE, plot = TRUE)

#Now just as an investigatory stage, lets redo the mag calibration procedure with method = 3, to see how much better (or worse) the track becomes (relative to VPs)
df.corr = Gundog.Compass(mag.x = -df$Mag_z.sm, mag.y = -df$Mag_y.sm, mag.z = -df$Mag_x.sm, acc.x = df$Acc_z.sm, acc.y = df$Acc_y.sm, acc.z = df$Acc_x.sm, ME = df$Marked.event, pitch.offset = 0, roll.offset = 0, yaw.offset = 0, method = 3, plot=TRUE)
df[, c('Pitch', 'Roll', 'Yaw')] = df.corr[, c('Pitch', 'Roll', 'Yaw')] #Update these values with the new ones from recalculated df.corr
df.sub = subset(df, df$Total.Event.no. >= 1237489 & df$Total.Event.no. <= 1452525) #Subset this updated df
penguin.track = Gundog.Tracks(TS = df.sub$timestamp, h = df.sub$Yaw, v = df.sub$VeDBA.sm, elv = 0, p = NULL, cs = NULL, ch = NULL, m = 1.5, c = 0, ME = df.sub$Marked.event, lo = -63.86774, la = -42.08654, VP.lon = df.sub$GPS.Longitude, VP.lat = df.sub$GPS.Latitude, VP.ME = TRUE, method = "distance", thresh = 12, dist.step = 5, bound = TRUE, Outgoing = TRUE, plot = TRUE)

#Notice slight changes in heading values improved the path (relative to VPs pre-VPC)

#10) Out of interest, with respect to the information and figure in SI2, to plot out the channel configuration period (to check you channel that the input order/required negations are correct), do the following....
#Note, that for this calibration I held device horizontal, as if penguin was swimming (where horizontal is their predominant posture), so this procedure is actually irrelevant for the above track (when they were walking) as we need a different channel configuration in Gundog.Compass (which makes this check rather pointless, but anyway...)
df = df[, -c(35:37)] # Remove the pitch, roll and yaw columns from previous step (step 9)
df.corr = Gundog.Compass(mag.x = df$Mag_x.sm, mag.y = -df$Mag_y.sm, mag.z = -df$Mag_z.sm, acc.x = -df$Acc_x.sm, acc.y = df$Acc_y.sm, acc.z = df$Acc_z.sm, ME = df$Marked.event, pitch.offset = 0, roll.offset = 0, yaw.offset = 0, method = 1, plot=TRUE) #This is the input for horizontal deployment (z-axis positing up from back and x-axis pointing towards the penguin's head)
df = cbind(df, df.corr[, c('Pitch', 'Roll', 'Yaw')])
df.sub = subset(df, df$Marked.event == 3) #This is to subset the channel configuration period - just the pitch up and down and roll left and right 3 times each at every anti-cardinal direction and finished by turning in a circle 3 times 
plot(df.sub$Roll, type = "l", col = "green") ; lines(df.sub$Pitch, col = "red") #Note these pitch (red) and rolls (green) were carried out too quickly and so the 2-second smoothing to derive static acceleration has diluted the actual signal minima and maxima. Try redo this stage with the previous acceleration channel static smoothing of just 20 events instead of 80 (step 4) to see how plot changes... 
#Use constant progression (speed) value
penguin.track = Gundog.Tracks(TS = df.sub$timestamp, h = df.sub$Yaw, v = 1, elv = 0, p = NULL, cs = NULL, ch = NULL, m = 1, c = 0, ME = 1, lo = 0, la = 0, VP.lon = NULL, VP.lat = NULL, VP.ME = FALSE, method = NULL, thresh = 0, dist.step = 0, bound = TRUE, Outgoing = TRUE, plot = TRUE) #Notice here, there was a temporal gap between the pitches and rolls and the circling. Also note, slight variations about the z-axis (yaw) during pitch and rolls is the reason why the 'approx. square shape' contains some tortuosity  



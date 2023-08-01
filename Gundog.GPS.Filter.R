#############################################Gundog.GPS.Filter#############################################
#richard.m.g@hotmail.com
#https://www.researchgate.net/profile/Richard-Gunner-2/research
#https://orcid.org/0000-0002-2054-9944

####A R function to screen for GPS anomalies###

##Key features##

##Specifically, this script is divided into 2 main components -->

###Component 1 = User defined movement thresholds to identify anomalous fixes###

#There are three user-defined thresholds: i)  [R1] = A threshold value of distance (units in meters) between  two sets of coordinates - the raw fixes the and median filtered equivalent. Locational data above this threshold are deemed outliers
#                                        ii)  [R2] = Erroneous 'spikes'- These are identified by computing the outgoing and incoming speed (units in meters per second) and absolute turning angle (units ranging 0-180°) between every combination of three consecutive GPS fixes. High outgoing and incoming speed coupled with an acute (small) vertex angle indicates erronous fixes ('spikes')
#                                       iii)  [R3] = A threshold value of maximum speed using a user-defined stepping range - defined as the interval between each retained fix: A five-fix stepping range is thus the distance and time difference (s) computed between every fifth fix prior to calculating the resultant speed value (m/s). If this value exceeds the threshold, then in this example, it is the fifth fix that is depicted as the outlier (usually, the stepping range just needs to be left at the default value of 1)

#This means there are 3 important thresholds - i) [R1] Instantaneous distance (m) away from surrounding general fixes (median position)
#                                             - ii) [R2] The tandem thresholds of outgoing and incoming speeds (m/s) and absolute turning angle (0-180°) between 3 consecutive fixes. 
#                                            - iii) [R3] Instantaneous speed threshold (m/s), using a given stepping range

##Unique to this script?##

#Some previous methods for GPS filtering have calculated metrics such as the above (distance moved, speed and turn angles) using various pre-defined static windows (e.g., Bjorneraas et al. 2010) and the user chooses sutibale thresholds to evaluate GPS fix uncertainty. 
#This script is unique in that the window resets per user-defined maximum GPS drop-out. For example, if GPS was scheduled as 1 fix/5 mins, and there was a period of 20 minutes without a fix, and the drop-out threshold was set as 600 s (10 mins), then the window and the various running computations would not overlap either side of the drop-out. This ensures measures of fix inaccuracy are more standardized in time and space.
#Moreover, some GPS devices records fixes in pre-set 'bursts'. For example, every five minutes, there could be a burst of six consecutive fixes at 1 Hz; one fix per second for six seconds
#This script offers four options to pre-process coordinates within each burst:
# i) Retain one median longitude and latitude value per burst 
# ii) Retain one mean longitude and latitude value per burst 
# iii) Only keep the last value per burst
# iv) Do nothing (use this option if GPS does not record in bursts or if the fixes within each burst are not standardised to 1 s apart)

#Component 1 method order:
#1) The script starts by pre-processing burst data if required (see above). Then the user-defined maximum GPS drop-out time (units in seconds) triggers an increment in group number each time the GPS drop-out time is reached/surpassed
#2) Per group number, a user-defined sliding window (number of consecutive fixes) is used to compute a center-aligned running median of the longitude and latitude coordinates (following processing of burst data if required) and then the Haversine distance between each 'current' GPS fix and the equivalent running median value is computed. Values are extended at the tails of the window (fill = "extend).  If the length of the running median window is less than the length of the given group number, then the running median window.length will automatically change to the length of the given group number, unless the group number length is less than 3, in which case, no median fixes are computed.
#3) R1 computed 
#4) R2 computed between every combination of three consecutive GPS fixes per group number
#5) R3 computed (this will be the same as R2's outgoing speed if the stepping range is left at the default value of one). The stepping range resets per unique group number.


###Component 2 = Isolation forest models as a method of unsupervised anomaly detection###

#This script also implements Isolation forest models based on the above metrics of; [R1] Distance from median fixes , [R2] Outgoing/incoming speed and absolute turning angle between 3 consecutive fixes, and [R3] Maximum speed. For explanation and examples, see https://sealavi.github.io/Outliers-in-animal-movement-data/. Shauhin Alavi [https://github.com/sealavi] conceived this idea for use in GPS filtering.
#Isolation forest runs multiple times, once as a multidimensional case including all metrics [R1,R2 & R3], and once with each metric as single dimensions. Essentially, this approach grows random decision trees, and increasingly partitions and isolates data using random threshold values. More isolated observations are identified as anomalous.
#The user supplies a quantile anomaly score (default = 99.5% quantile) The distribution of anomaly scores at or higher than this quantile are deemed outliers for each dimensional case.
#The user can then inspect the resultant data frame of potential outliers determined from the user-defined threshold limits and the Isolation forest approach to evaluate filtering performance

################################################################################
##User-defined inputs -->
############################################################
#TS --> Timestamp (as.POSIXct object) (e.g., '2022-08-06 21:46:12')
#Longitude --> GPS longitude coordinates (decimal format)
#Latitude --> GPS latitude coordinates (decimal format)
#Drop.out --> Maximum GPS drop-out time (units in s) used to compute group numbers from which the various running windows/stepping ranges listed above resets per increment (default value = 300) 
#Window.length --> Sliding window length (number of consecutive fixes) used to compute center-aligned running median values of longitude and latitude values (for the R1 threshold) (default value = 10)
#Burst.method  --> Option to pre-process burst data: "median", "mean", "last", or "none" (default = "last")
#Dist.thresh --> Threshold value (m) for R1
#Angle.speed --> Threshold value (m/s) for both incoming and outgoing speeds between 3 consecutive fixes used in evaluating R2
#Angle.thresh --> The absolute angle vertex between 3 consecutive fixes used in evaluating R2, for this, the 'Ang.vertex' threshold is passed if <= to the 'Angle.thresh' value
#Max.speed --> Threshold value (m/s) for R3 
#Speed.step --> The stepping range between fixes for 'Max.speed' calculation (e.g., if default value of 1 is changed to 5, then speed is computed between every 5 fixes per group number)
#I.F_conf = --> Quantile anomaly score (default = 0.995)
#plot --> If set to TRUE (default), then the following plots are given: 

#plot 1 = 'Thresholds' --> Four histograms showing the density distributions of metrics used in evaluating R1, R2 & R3 thresholds. Dashed ablines denote the 95 (green) and 99 (red) percentile, relative to the user-defined threshold values (blue)
#plot 2 = 'Plot' --> Two side-by-side ggplotly interactive plots of GPS track:
        #[left plot] coloured according to whether fix was determined 'Not anomalous' (no thresholds surpassed), 'Possible outlier' (one or two of R1, R2 & R3 were surpassed), or 'Unanimous outlier' (all thresholds surpassed) --> Based on component 1 user-defined thresholds
        #[right plot] coloured according to whether fix was determined 'Not anomalous' 'Possible outlier' or 'Unanimous outlier' --> Based on component 2 user-defined thresholds Isolation forest models. If quantile anomaly scores from all  metrics considered together are above the 'I.F_conf value', then fix considered 'Unanimous outlier', for single dimensional cases; 'Possible outlier'. Otherwise, 'Not anomalous'.

#If plot = TRUE, then a 3-component list is returned, with 'Thresholds', 'Plot', and 'df' (the latter being the resultant data frame). 
#If plot = FALSE, then just 'df' is returned as a data frame

################################################################################
##Data frame outputs -->
############################################################
#Observation --> The row number number of the original input data (subsequent to filtering e.g., of the burst sequences)
#Timestamp --> Input timestamp
#Time.diff --> Time difference (s)
#Longitude 
#Latitude
#Fix.number --> Consecutive numbers of retained fixes
#Window.group --> Integer value increasing by one for each 'new group' made according to the 'Drop.out' value
#Dist.from.median --> Distance that each fix is a way from the median fix value (calculated according to the 'Window.length' input)
#R1 --> TRUE or FALSE (if TRUE, 'Dist.from.median' >= 'Dist.thresh')
#Ang.vertex --> The absolute vertex angle between 3 fixes (°)
#Outgoing.speed --> Outgoing speed of the 3 fixes (m/s)
#Incoming.speed --> Incoming speed of the 3 fixes (m/s)
#R2 --> TRUE or FALSE (if TRUE, then both 'Outgoing.speed' and 'Incoming.speed' are >= 'Angle.speed', and the 'Ang.vertex' <= 'Angle.thresh')
#Maximum.speed --> Speed (m/s) between fixes of the given user-defined stepping range (the 'Speed.step')
#R3 --> TRUE or FALSE (if TRUE, Maximum.speed >= 'Max.speed')
#Verdict --> 'Not anomalous' (no thresholds of R1, R2, or R3 are reached/surpassed), 'Possible outlier' (one or two thresholds of R1, R2, or R3 are reached/surpassed), 'Unanimous outlier' (all thresholds reached/surpassed)
#I.F_overall_anomaly_score --> Overall Isolation Forest anomaly score from multidimensional case including all metrics in tandem [R1,R2 & R3]
#I.F_anomaly_score_R1 --> Isolation Forest anomaly score just considering single case of R1
#I.F_anomaly_score_R2 --> Isolation Forest anomaly score just considering single case of R2
#I.F_anomaly_score_R3 --> Isolation Forest anomaly score just considering single case of R3
#Verdict_IF --> 'Not anomalous' (anomaly scores, both in single cases, and multidimensional case of R1, R2, and R3, are less than the input quantile ('I.F_conf' - default = 0.995) of there respective distributions), 'Possible outlier' (One or a combination of  R1, R2, or R3 anomaly scores are equal to or greater than the input quantile ('I.F_conf' - default = 0.995) of there respective distributions), 'Unanimous outlier' (the distribution of anomaly scores at or higher than the input quantile ('I.F_conf' - default = 0.995) when considered as the multidimensional case [R1, R2, & R3 considered together]

##Based on the verdicts within the returned data frame (and plots, if plot = TRUE), the user can then decide to filter out the necessary fixes. The 'observation' column is given, if ever resultant columns are required to be merged back into the 'original' df (based on matching row numbers)

#############################################Gundog.GPS.Filter#############################################

###START OF FUNCTION###

Gundog.GPS.Filter = function(TS, Longitude, Latitude, Drop.out = 300, Window.length = 10, Burst.method = "last", Dist.thresh = 50, Max.speed = 1, Speed.step = 1, Angle.speed = 1, Angle.thresh = 30, I.F_conf = 0.995, plot = TRUE){
  
  ################################################################################################################################################  
  
  ###Required packages###
  
  if (!require('zoo')){ install.packages('zoo', dependencies = TRUE, type="source")} ; suppressMessages(require("zoo"))
  if (!require('dplyr')){ install.packages('dplyr', dependencies = TRUE, type="source")} ; suppressMessages(require("dplyr"))
  if (!require('tidyr')){ install.packages('tidyr', dependencies = TRUE, type="source")} ; suppressMessages(require("tidyr"))
  if (!require('ggplot2')){ install.packages('ggplot2', dependencies = TRUE, type="source")} ; suppressMessages(require("ggplot2"))
  if (!require('plotly')){ install.packages('plotly', dependencies = TRUE, type="source")} ; suppressMessages(require("plotly"))
  if (!require('solitude')){ install.packages('solitude', dependencies = TRUE, type="source")} ; suppressMessages(require("solitude"))
  
  #Check that required packages are installed on the system
  areinstaled=data.frame(installed.packages())
  
  if(all(c("zoo","dplyr","tidyr","ggplot2", "plotly", "solitude")%in%areinstaled$Package)==FALSE){
    required_packages=c("zoo","dplyr","tidyr","ggplot2", "plotly", "solitude")
    missing_packages=c("zoo","dplyr","tidyr","ggplot2", "plotly", "solitude")%in%areinstaled$Package
    stop(paste("The following packages are not installed:", required_packages[which(missing_packages==FALSE)], sep = " "))
  }
  
  ###Input argument checking###
  
  options(digits.secs = 3) #Specify the number of decimal places of the fractional seconds to show if relevant   
  is.POSIXct = function(x) inherits(x, "POSIXct") #Function to check variable is of POSIXct type
  
  if(is.POSIXct(TS) == FALSE) { #Ensure TS is of type POSIXct (otherwise terminate function)
    stop("TS must be of type POSIXct", .call = FALSE)
  }
  if(length(unique(TS)) != length(TS)){ #Ensure TS does not contain duplicate timestamps (otherwise terminate function)
    stop("TS must not contain duplicates", .call = FALSE)
  }
  if(min(Longitude, na.rm = TRUE) < -180 || max(Longitude, na.rm = TRUE) > 180) { #Ensure coordinates are in decimal format
    stop("Longitude must be between -180 and 180 degrees", call. = FALSE)
  }
  if(min(Latitude, na.rm = TRUE) < -180 || max(Latitude, na.rm = TRUE) > 180) { #Ensure coordinates are in decimal format
    stop("Latitude must be between -90 and 90 degrees", call. = FALSE)
  }
  if(length(Drop.out) > 1 || is.numeric(Drop.out) == FALSE || Drop.out != round(Drop.out)){ #Ensure a single (whole number) numeric value is supplied
    stop("Drop.out threshold must be one numeric value", .call = FALSE)
  }
  if(length(Window.length) > 1 || is.numeric(Window.length) == FALSE || Window.length != round(Window.length)){ #Ensure a single (whole number) numeric value is supplied
    stop("Window.length threshold must be one numeric value", .call = FALSE)
  }
  if(length(Burst.method) > 1 || Burst.method %in% c("median", "mean", "last", "none") == FALSE ){ #Ensure a valid option is selected
    stop("Pick one of the pre-select options for Burst.method", .call = FALSE)
  }
  if(length(Dist.thresh) > 1 || is.numeric(Dist.thresh) == FALSE){ #Ensure a single numeric value is supplied
    stop("Dist.thresh threshold must be one numeric value", .call = FALSE)
  }  
  if(length(Max.speed) > 1 || is.numeric(Max.speed) == FALSE){ #Ensure a single numeric value is supplied
    stop("Max.speed threshold must be one numeric value", .call = FALSE)
  }
  if(length(Angle.speed) > 1 || is.numeric(Angle.speed) == FALSE){ #Ensure a single numeric value is supplied
    stop("Angle.speed threshold must be one numeric value", .call = FALSE)
  }
  if(length(Angle.thresh) > 1 || is.numeric(Angle.thresh) == FALSE){ #Ensure a single numeric value is supplied
    stop("Angle.thresh threshold must be one numeric value", .call = FALSE)
  }
  
  ################################################################################################################################################
  
  ####Required functions###
  
  #Haversine distance formula 
  disty = function(long1, lat1, long2, lat2) { #Longitude and Latitude supplied in degrees
    long1 = long1 * pi/180 ; long2 = long2 * pi/180 ; lat1 = lat1 * pi/180 ; lat2 = lat2 * pi/180 #Function converts to radians
    a = sin((lat2 - lat1) / 2) * sin((lat2 - lat1) / 2) + cos(lat1) * cos(lat2) * sin((long2 - long1) / 2) * sin((long2 - long1) / 2)
    c = 2 * atan2(sqrt(a), sqrt(1 - a))
    d1 = 6378137 * c
    return(d1)
  }
  
  #Bearing function --> returns degrees - Great circular bearing between 2D positions
  beary = function(long1, lat1, long2, lat2) { #Longitude and Latitude supplied in degrees
    long1 = long1 * pi/180 ; long2 = long2 * pi/180 ; lat1 = lat1 * pi/180 ; lat2 = lat2 * pi/180 #Function converts to radians
    a = sin(long2 - long1) * cos(lat2)
    b = cos(lat1) * sin(lat2) - sin(lat1) * cos(lat2) * cos(long2 - long1)
    c = ((atan2(a, b) / pi) * 180)  #Units returned in degrees (-180 to +180 degree scale)
    return(c)
  }
  
  #Set to 5 decimal places (for plotting long/lat coords purposes)
  scaleFUN <- function(x) sprintf("%.5f", x)
  
  ################################################################################################################################################
  
  ###Create the initial data frame and pre-process the data###
  
  Observation = rep(1:length(TS)) #Row number (used for indexing and merging data frames)
  df = data.frame(Observation, TS, Longitude, Latitude) ; colnames(df) = c("Observation", "Timestamp", "Longitude", "Latitude")
  
  #Remove NA fixes
  df$Longitude = ifelse(df$Longitude == 0, NA, df$Longitude) #In case missing coordinates are filled as zeros, replace with NA's
  df$Latitude = ifelse(df$Latitude == 0, NA, df$Latitude) #In case missing coordinates are filled as zeros, replace with NA's
  df = df[!with(df, is.na(Longitude) | is.na(Latitude)) ,]
  
  #Create a time difference (s) between values
  df = df %>% mutate(Time.diff = as.numeric(c(0, difftime(Timestamp, dplyr::lag(Timestamp), units = "secs")[-1]))) %>% ungroup()
  
  #Make a group column which increments by one each GPS burst. This is going to be used to pre-process each burst
  df$Fix.number = 1
  x = 1
  for(i in 1:nrow(df)){
    if(df$Time.diff[i] > 1){
      x = x+1
    }
    df$Fix.number[i] = x
  }
  
  #Method to process each burst
  if(Burst.method != "none"){
    if(Burst.method == "median"){
      df = df %>% group_by(Fix.number) %>% mutate(Latitude = median(Latitude)) %>% ungroup()
      df = df %>% group_by(Fix.number) %>% mutate(Longitude = median(Longitude)) %>% ungroup()
    }
    if(Burst.method == "mean"){
      df = df %>% group_by(Fix.number) %>% mutate(Latitude = mean(Latitude)) %>% ungroup()
      df = df %>% group_by(Fix.number) %>% mutate(Longitude = mean(Longitude)) %>% ungroup()
    }
    if(Burst.method == "last"){
      df = df %>% group_by(Fix.number) %>% mutate(Latitude = last(Latitude)) %>% ungroup()
      df = df %>% group_by(Fix.number) %>% mutate(Longitude = last(Longitude)) %>% ungroup()
    }
  }
  
  #Remove duplicated processed GPS fixes per burst
  if(Burst.method != "none"){
    df.sub = df[!duplicated(df$Fix.number), ]
    df.sub = df.sub %>% mutate(Time.diff = as.numeric(c(0, difftime(Timestamp, dplyr::lag(Timestamp), units = "secs")[-1]))) %>% ungroup() #Redo time difference between rows after subset
  }else{ 
    df.sub = df
  }
  
  #Make a group column ('Window.group') which increments by one each time the GPS drops-out (missing fixes) >= the user-defined 'Drop.out' threshold (s)
  x = 1
  Time.diff = df.sub$Time.diff
  Window.group = rep(0, length(Time.diff))
  for (i in 1:length(Window.group)) {
    if(Time.diff[i] >= Drop.out){
      x = x+1
    }
    Window.group[i] = x
  }
  df.sub$Window.group = Window.group
  
  ################################################################################################################################################  
  
  #User-defined thresholds component of script
  
  ###Calculate distance between raw fixes and the median equivalent --> R1###
  
  #Median Longitude and Latitude values per Window.group. If the length of the Window.group < Window.length, then the window.length will change to the length of the Window.group, unless the Window.group < 3, in which case, no median fixes are computed.
  df.sub = df.sub %>% group_by(Window.group) %>% mutate(Window.group.length = max(sequence(rle(Window.group)$lengths))) %>% ungroup() #Number of consecutive rows per Window.group
  df.sub$Window.group.length = ifelse(df.sub$Window.group.length >= Window.length, Window.length, df.sub$Window.group.length) #If the number of consecutive rows per Window.group is >= Window.length, then make this value the window.length
  df.sub = df.sub %>% group_by(Window.group) %>% mutate(Longitude.med = ifelse(Window.group.length < 3, NA, #If the number of consecutive rows per Window.group is not a minimum of three then the median fix is not calculated. If the number of consecutive rows per Window.group is > 3 and less than the Window.length, then the running median window shortens to this value. Otherwise, the user-defined Window.length is used
                                                                               rollapply(Longitude, align = "center", width = Window.group.length, FUN = median, fill = NA))) %>% tidyr::fill(Longitude.med, .direction = "updown") %>% ungroup() #Replace last Non-NA value per Window.group in both directions
  df.sub = df.sub %>% group_by(Window.group) %>% mutate(Latitude.med = ifelse(Window.group.length < 3, NA, #If the number of consecutive rows per Window.group is not a minimum of three then the median fix is not calculated. If the number of consecutive rows per Window.group is > 3 and less than the Window.length, then the running median window shortens to this value. Otherwise, the user-defined Window.length is used
                                                                              rollapply(Latitude, align = "center", width = Window.group.length, FUN = median, fill = NA))) %>% tidyr::fill(Latitude.med, .direction = "updown") %>% ungroup() #Replace last Non-NA value per Window.group in both directions
  
  df.sub$Dist.from.median = with(df.sub, disty(Longitude, Latitude, Longitude.med, Latitude.med)) #Distance between raw fixes and the median equivalent
  
  #Evaluating the threshold
  df.sub$R1 = ifelse(df.sub$Dist.from.median >= Dist.thresh, TRUE, FALSE) #R1 = TRUE if GPS fix identified as erroneous
  
  ###Calculate Erroneous 'spikes' by identifying outgoing and incoming speed (m/s) and absolute turning angle (0-180°) between every combination of three consecutive GPS fixes within the user-defined sliding window length per group --> R2###
  
  #Shift longitude and latitude rows forwards (lag) and backwards (lead) by one row per group
  df.sub = df.sub %>% group_by(Window.group) %>% mutate(Longitude.lag = dplyr::lag(Longitude, n = 1, default = NA)) %>% ungroup()
  df.sub = df.sub %>% group_by(Window.group) %>% mutate(Latitude.lag = dplyr::lag(Latitude, n = 1, default = NA)) %>% ungroup()
  df.sub = df.sub %>% group_by(Window.group) %>% mutate(Longitude.lead = dplyr::lead(Longitude, n = 1, default = NA)) %>% ungroup()
  df.sub = df.sub %>% group_by(Window.group) %>% mutate(Latitude.lead = dplyr::lead(Latitude, n = 1, default = NA)) %>% ungroup()
  
  #Calculate bearing (angle) between 'Longitude'/'Latitude', 'Longitude.lag'/'Latitude.lag', and 'Longitude.lead'/'Latitude.lead' (angle from central fix to 'pre-fix' and from central fix to 'post-fix' across the three candidate fixes)
  df.sub = df.sub %>% group_by(Window.group) %>% mutate(Ang.lag = beary(Longitude, Latitude, Longitude.lag, Latitude.lag)) %>% ungroup()
  df.sub = df.sub %>% group_by(Window.group) %>% mutate(Ang.lead = beary(Longitude, Latitude, Longitude.lead, Latitude.lead)) %>% ungroup()
  df.sub$Ang.lag = ifelse(df.sub$Ang.lag < 0, df.sub$Ang.lag + 360, df.sub$Ang.lag) #Because above formula outputs within the scale of -180 to +180 degrees --> This ensures output is 0 to 360 degrees
  df.sub$Ang.lead = ifelse(df.sub$Ang.lead < 0, df.sub$Ang.lead + 360, df.sub$Ang.lead) #Because above formula outputs within the scale of -180 to +180 degrees --> This ensures output is 0 to 360 degrees
  df.sub$Ang.vertex = df.sub$Ang.lead - df.sub$Ang.lag #Turning angle (0-180°) between every combination of three consecutive GPS fixes
  df.sub$Ang.vertex = ifelse(df.sub$Ang.vertex < -180, (df.sub$Ang.vertex + 360), df.sub$Ang.vertex) #Ensure difference does not exceed 180 degrees in either circular direction
  df.sub$Ang.vertex = ifelse(df.sub$Ang.vertex > 180, (df.sub$Ang.vertex - 360), df.sub$Ang.vertex) #Ensure difference does not exceed 180 degrees in either circular direction
  df.sub$Ang.vertex = abs(df.sub$Ang.vertex) #Make angle absolute
  
  #Calculate the incoming and outgoing speeds either side of angle vertex
  df.sub = df.sub %>% group_by(Window.group) %>% mutate(Dist.lag = disty(Longitude, Latitude, Longitude.lag, Latitude.lag)) %>% ungroup()
  df.sub = df.sub %>% group_by(Window.group) %>% mutate(Dist.lead = disty(Longitude, Latitude, Longitude.lead, Latitude.lead)) %>% ungroup()
  
  #Lead of time difference
  #Create lag and lead time difference (s) between values
  df.sub = df.sub %>% group_by(Window.group) %>% mutate(Time.diff.lag = as.numeric(c(NA, difftime(Timestamp, dplyr::lag(Timestamp), units = "secs")[-1]))) %>% ungroup()
  df.sub = df.sub %>% group_by(Window.group) %>% mutate(Time.diff.lead = dplyr::lead(Time.diff.lag, n = 1, default = NA)) %>% ungroup()
  
  #Outgoing and incoming speed (m/s)
  df.sub = df.sub %>% group_by(Window.group) %>% mutate(Outgoing.speed = Dist.lag / Time.diff.lag) %>% ungroup()
  df.sub = df.sub %>% group_by(Window.group) %>% mutate(Incoming.speed = Dist.lead / Time.diff.lead) %>% ungroup()
  
  #Evaluating the threshold
  df.sub$R2 = ifelse(df.sub$Outgoing.speed >= Angle.speed & df.sub$Incoming.speed >= Angle.speed & df.sub$Ang.vertex <= Angle.thresh, TRUE, FALSE) #R2 = TRUE if GPS fix identified as erroneous
  
  ##Calculate Erroneous fixes if maximum speed threshold calculated using a user-defined stepping range is exceed --> R3###
  
  #Shift longitude and latitude values backwards by the specified stepping range (Speed.step)
  df.sub = df.sub %>% group_by(Window.group) %>% mutate(Longitude.lag.2 = dplyr::lag(Longitude, n = Speed.step, default = NA)) %>% ungroup()
  df.sub = df.sub %>% group_by(Window.group) %>% mutate(Latitude.lag.2 = dplyr::lag(Latitude, n = Speed.step, default = NA)) %>% ungroup()
  #Calculate time difference between the specified stepping range (Speed.step)
  df.sub = df.sub %>% group_by(Window.group) %>% mutate(Time.diff.lag.2 = as.numeric(c(NA, difftime(Timestamp, dplyr::lag(Timestamp, n = Speed.step, default = NA), units = "secs")[-1]))) %>% ungroup()
  #Calculate the speed across the specified 'Speed.step' stepping range
  df.sub$Maximum.speed = with(df.sub, disty(Longitude, Latitude, Longitude.lag.2, Latitude.lag.2) / Time.diff.lag.2)
  
  #Evaluating the threshold
  df.sub$R3 = ifelse(df.sub$Maximum.speed >= Max.speed, TRUE, FALSE) #R3 = TRUE if GPS fix identified as erroneous
  
  #Replace possible NAs in R1, R2 & R3 to FALSE
  df.sub$R1 = ifelse(is.na(df.sub$R1) == TRUE, FALSE, df.sub$R1)
  df.sub$R2 = ifelse(is.na(df.sub$R2) == TRUE, FALSE, df.sub$R2)
  df.sub$R3 = ifelse(is.na(df.sub$R3) == TRUE, FALSE, df.sub$R3)
  
  #If at least one threshold is passed, then 'Possible outlier', if all thresholds surpassed, then 'Unanimous outlier', if no thresholds surpassed, then 'Not anomalous' 
  df.sub$Verdict = ifelse(df.sub$R1 == TRUE & df.sub$R2 == TRUE & df.sub$R3 == TRUE, "Unanimous outlier", ifelse(df.sub$R1 == TRUE | df.sub$R2 == TRUE | df.sub$R3 == TRUE, "Possible outlier", "Not anomalous"))
  #df.sub$Verdict = as.character(factor(df.sub$Verdict, levels = c("Not anomalous", "Possible outlier", "Unanimous outlier"))) #Ensure factor levels are ordered for plotting purposes
  
  #Remove redundant columns / vectors
  rm(Observation, x, Window.group)
  rm(df)
  df.sub = df.sub[, c('Observation', 'Timestamp', 'Time.diff', 'Longitude', 'Latitude', 'Fix.number', 'Window.group', 'Dist.from.median', 'R1', 'Ang.vertex', 'Outgoing.speed', 'Incoming.speed', 'R2', 'Maximum.speed', 'R3', 'Verdict')]
  
  ################################################################################################################################################
  ################################################################################################################################################  
  
  ###Isolation forest component of function###
  
  ##check if default isolation forest sample size settings need to be adjusted
  if(nrow(df.sub) < 256){
    sample_size = nrow(df.sub)
  }else{
    sample_size = 256
  }
  
  #Begin anomaly detection
  message("Initiating Isolation forest anomaly detection")
  if(all(diff((df.sub$Timestamp)) > 0) == FALSE){
    stop("Timestamps may be out of order - Ensure that timestamps are in ascending order")
  }
  
  #All variables together
  indexs = with(df.sub, which(complete.cases(Dist.from.median, Ang.vertex, Outgoing.speed, Incoming.speed, Maximum.speed) == TRUE)) #Row numbers with complete cases across the columns of interest (non-NA values)
  COLS = c("Dist.from.median", "Ang.vertex", "Outgoing.speed", "Incoming.speed", "Maximum.speed") #Columns of interest
  COLS = which(names(df.sub) %in% COLS == TRUE) #Index of where columns occur in 'df.sub'
  subset = df.sub[indexs, COLS] #Subset just columns of interest
  iforest = isolationForest$new(sample_size = sample_size)
  invisible(capture.output(iforest$fit(subset)))
  scores = iforest$predict(subset)
  df.sub$I.F_overall_anomaly_score = NA
  df.sub$I.F_overall_anomaly_score[indexs] = scores$anomaly_score
  
  #Just 'Dist.from.median' [R1]
  indexs = with(df.sub, which(complete.cases(Dist.from.median) == TRUE)) #Row numbers with non-NA values
  subset = as.data.frame(df.sub$Dist.from.median[indexs]) #Subset just column of interest
  iforest = isolationForest$new(sample_size = sample_size)
  invisible(capture.output(iforest$fit(subset)))
  scores = iforest$predict(subset)
  df.sub$I.F_anomaly_score_R1 = NA
  df.sub$I.F_anomaly_score_R1[indexs] = scores$anomaly_score
  
  #Just 'Ang.vertex', 'Outgoing.speed' and 'Incoming.speed' [R2]
  indexs = with(df.sub, which(complete.cases(Ang.vertex, Outgoing.speed, Incoming.speed) == TRUE)) #Row numbers with complete cases across the columns of interest (non-NA values)
  COLS = c("Ang.vertex", "Outgoing.speed", "Incoming.speed") #Columns of interest
  COLS = which(names(df.sub) %in% COLS == TRUE) #Index of where columns occur in 'df.sub'
  subset = df.sub[indexs, COLS] #Subset just columns of interest
  iforest = isolationForest$new(sample_size = sample_size)
  invisible(capture.output(iforest$fit(subset)))
  scores = iforest$predict(subset)
  df.sub$I.F_anomaly_score_R2 = NA
  df.sub$I.F_anomaly_score_R2[indexs] = scores$anomaly_score
  
  #Just 'Maximum.speed' [R3]
  indexs = with(df.sub, which(complete.cases(Maximum.speed) == TRUE)) #Row numbers with non-NA values
  subset = as.data.frame(df.sub$Maximum.speed[indexs]) #Subset just column of interest
  iforest = isolationForest$new(sample_size = sample_size)
  invisible(capture.output(iforest$fit(subset)))
  scores = iforest$predict(subset)
  df.sub$I.F_anomaly_score_R3 = NA
  df.sub$I.F_anomaly_score_R3[indexs] = scores$anomaly_score
  
  #Which rows correspond to being >= I.F_conf quantile (for R1, R2, R3, and combined)
  R1_outliers = which(df.sub$I.F_anomaly_score_R1 >= as.numeric(quantile(df.sub$I.F_anomaly_score_R1, I.F_conf, na.rm = TRUE)))
  R2_outliers = which(df.sub$I.F_anomaly_score_R2 >= as.numeric(quantile(df.sub$I.F_anomaly_score_R2, I.F_conf, na.rm = TRUE)))
  R3_outliers = which(df.sub$I.F_anomaly_score_R3 >= as.numeric(quantile(df.sub$I.F_anomaly_score_R3, I.F_conf, na.rm = TRUE)))
  overall_outliers = which(df.sub$I.F_overall_anomaly_score >= as.numeric(quantile(df.sub$I.F_overall_anomaly_score, I.F_conf, na.rm = TRUE)))
  
  R1_outliers = R1_outliers[-which(R1_outliers %in% overall_outliers == TRUE)]
  R2_outliers = R2_outliers[-which(R2_outliers %in% overall_outliers == TRUE)]
  R3_outliers = R3_outliers[-which(R3_outliers %in% overall_outliers == TRUE)]
  
  #Label as "Not anomalous", "Possible outlier", "Unanimous outlier", according to whether none, some or all thresholds were surpassed
  df.sub$Verdict_IF = "Not anomalous"
  df.sub$Verdict_IF[R1_outliers] = "Possible outlier"
  df.sub$Verdict_IF[R2_outliers] = "Possible outlier"
  df.sub$Verdict_IF[R3_outliers] = "Possible outlier"
  df.sub$Verdict_IF[overall_outliers] = "Unanimous outlier"
  
  ###Plot results###
  
  if(plot == TRUE){
    
    #summary plots - 'Thresholds'
    old.par <- par(mar = c(0, 0, 0, 0))
    #(1) Distribution of Distance from median (R1)
    par(old.par)
    par(mfrow = c(2,2))
    h = hist(df.sub$Dist.from.median, breaks="Scott", plot=TRUE, main = "Distance from median (m)",
             xlab = "Distance from median (m) [R1]", ylab = "Density", cex.lab = 1.2,  freq = FALSE)
    text(Dist.thresh, max(h$density), labels = paste("R1 =", Dist.thresh), col = "blue", cex = 0.8) # User defined 'Dist.thresh [R1]'
    text(quantile(df.sub$Dist.from.median, 0.95, na.rm = TRUE), max(h$density)/1.7, labels = "95%", col = "green", cex = 0.8) # 95%
    text(quantile(df.sub$Dist.from.median, 0.99, na.rm = TRUE), max(h$density)/3.2, labels = "99%", col = "red", cex = 0.8) # 99%
    clip(x1 = min(h$breaks),
         x2 = max(h$breaks), 
         y1 = min(h$density),
         y2 = max(h$density) - max(h$density)/10)
    abline(v = Dist.thresh, col="blue", lwd=1.5, lty=2) # User defined 'Dist.thresh [R1]'
    clip(x1 = 0,
         x2 = max(h$breaks), 
         y1 = 0,
         y2 = max(h$density)/2)
    abline(v = quantile(df.sub$Dist.from.median, 0.95, na.rm = TRUE), col="green", lwd=1.5, lty=2) #95% quantile
    clip(x1 = 0,
         x2 = max(h$breaks), 
         y1 = 0,
         y2 = max(h$density)/4)
    abline(v = quantile(df.sub$Dist.from.median, 0.99, na.rm = TRUE), col="red", lwd=1.5, lty=2) #99 % quantile
    #Zoom in
    par(fig = c(0.2, 0.49, 0.65, 0.99), new = T)
    h = hist(df.sub$Dist.from.median, breaks="Scott", plot=TRUE, main = "",
             xlab = "", ylab = "", freq = FALSE, xlim = c(0, quantile(df.sub$Dist.from.median, 0.99, na.rm = TRUE)))
    if(Dist.thresh < quantile(df.sub$Dist.from.median, 0.99, na.rm = TRUE)){
      text(Dist.thresh, max(h$density), labels = paste("R1 =", Dist.thresh), col = "blue", cex = 0.8) # User defined 'Dist.thresh [R1]'
    }
    text(quantile(df.sub$Dist.from.median, 0.95, na.rm = TRUE), max(h$density)/1.7, labels = "95%", col = "green", cex = 0.8) # 95%
    text(quantile(df.sub$Dist.from.median, 0.99, na.rm = TRUE), max(h$density)/3.2, labels = "99%", col = "red", cex = 0.8) # 99%
    if(Dist.thresh < quantile(df.sub$Dist.from.median, 0.99, na.rm = TRUE)){
      clip(x1 = 0,
           x2 = max(h$breaks), 
           y1 = 0,
           y2 = max(h$density) - max(h$density)/10)
    }
    abline(v = Dist.thresh, col="blue", lwd=1.5, lty=2) # User defined 'Dist.thresh [R1]'
    clip(x1 = 0,
         x2 = max(h$breaks), 
         y1 = 0,
         y2 = max(h$density)/2)
    abline(v = quantile(df.sub$Dist.from.median, 0.95, na.rm = TRUE), col="green", lwd=1.5, lty=2) #95% quantile
    clip(x1 = 0,
         x2 = max(h$breaks), 
         y1 = 0,
         y2 = max(h$density)/4)
    abline(v = quantile(df.sub$Dist.from.median, 0.99, na.rm = TRUE), col="red", lwd=1.5, lty=2) #99 % quantile
    
    #(2) Distribution of Angle between 3 fixes (R2)
    par(fig = c(0.5, 1, 0.5, 1), new = T)
    h = hist(df.sub$Ang.vertex, breaks = 60, plot=TRUE, main = "Angle between 3 fixes (°)",
             xlab = "Angle between 3 fixes (°) [R2]", ylab = "Density", cex.lab = 1.2,  freq = FALSE, xaxt = "n")
    axis(1, at=seq(0,180,by=20), labels=seq(0,180,by=20))
    text(Angle.thresh, max(h$density), labels = paste("R2 =", Angle.thresh), col = "blue", cex = 0.8) # User defined 'Angle.thresh [R1]'
    text(quantile(df.sub$Ang.vertex, 0.05, na.rm = TRUE), max(h$density)/1.7, labels = "0.5%", col = "green", cex = 0.8) # 0.5%
    text(quantile(df.sub$Ang.vertex, 0.01, na.rm = TRUE), max(h$density)/3.2, labels = "0.01%", col = "red", cex = 0.8) # 0.01%
    clip(x1 = 0,
         x2 = max(h$breaks), 
         y1 = 0,
         y2 = max(h$density) - max(h$density)/10)
    abline(v = Angle.thresh, col="blue", lwd=1.5, lty=2) # User defined 'Angle.thresh [R2]'
    clip(x1 = 0,
         x2 = max(h$breaks), 
         y1 = 0,
         y2 = max(h$density)/2)
    abline(v = quantile(df.sub$Ang.vertex, 0.05, na.rm = TRUE), col="green", lwd=1.5, lty=2) #95% quantile
    clip(x1 = 0,
         x2 = max(h$breaks), 
         y1 = 0,
         y2 = max(h$density)/4)
    abline(v = quantile(df.sub$Ang.vertex, 0.01, na.rm = TRUE), col="red", lwd=1.5, lty=2) #99 % quantile
    
    #(3) Distribution of Outgoing/Incoming speed (R2)
    par(fig = c(0, 0.5, 0, 0.5), new = T)
    h = hist(c(df.sub$Outgoing.speed, df.sub$Incoming.speed), breaks="Scott", plot=TRUE, main = "Outgoing/Incoming speed (m/s)",
             xlab = "Outgoing/Incoming speed (m/s) [R2]", ylab = "Density", cex.lab = 1.2,  freq = FALSE)
    text(Angle.speed, max(h$density), labels = paste("R2 =", Angle.speed), col = "blue", cex = 0.8) # User defined 'Angle.speed [R2]'
    text(quantile(c(df.sub$Outgoing.speed, df.sub$Incoming.speed), 0.95, na.rm = TRUE), max(h$density)/1.7, labels = "95%", col = "green", cex = 0.8) # 95%
    text(quantile(c(df.sub$Outgoing.speed, df.sub$Incoming.speed), 0.99, na.rm = TRUE), max(h$density)/3.2, labels = "99%", col = "red", cex = 0.8) # 99%
    clip(x1 = 0,
         x2 = max(h$breaks), 
         y1 = 0,
         y2 = max(h$density) - max(h$density)/10)
    abline(v = Angle.speed, col="blue", lwd=1.5, lty=2) # User defined 'Dist.thresh [R1]'
    clip(x1 = 0,
         x2 = max(h$breaks), 
         y1 = 0,
         y2 = max(h$density)/2)
    abline(v = quantile(c(df.sub$Outgoing.speed, df.sub$Incoming.speed), 0.95, na.rm = TRUE), col="green", lwd=1.5, lty=2) #95% quantile
    clip(x1 = 0,
         x2 = max(h$breaks), 
         y1 = 0,
         y2 = max(h$density)/4)
    abline(v = quantile(c(df.sub$Outgoing.speed, df.sub$Incoming.speed), 0.99, na.rm = TRUE), col="red", lwd=1.5, lty=2) #99 % quantile
    #Zoom in
    par(fig = c(0.2, 0.49, 0.15, 0.49), new = T)
    h = hist(c(df.sub$Outgoing.speed, df.sub$Incoming.speed), breaks="Scott", plot=TRUE, main = "",
             xlab = "", ylab = "", freq = FALSE, xlim = c(0, quantile(c(df.sub$Outgoing.speed, df.sub$Incoming.speed), 0.99, na.rm = TRUE)))
    if(Angle.speed < quantile(c(df.sub$Outgoing.speed, df.sub$Incoming.speed), 0.99, na.rm = TRUE)){
      text(Angle.speed, max(h$density), labels = paste("R2 =", Angle.speed), col = "blue", cex = 0.8) # User defined 'Angle.speed [R2]'
    }
    text(quantile(c(df.sub$Outgoing.speed, df.sub$Incoming.speed), 0.95, na.rm = TRUE), max(h$density)/1.7, labels = "95%", col = "green", cex = 0.8) # 95%
    text(quantile(c(df.sub$Outgoing.speed, df.sub$Incoming.speed), 0.99, na.rm = TRUE), max(h$density)/3.2, labels = "99%", col = "red", cex = 0.8) # 99%
    if(Angle.speed < quantile(c(df.sub$Outgoing.speed, df.sub$Incoming.speed), 0.99, na.rm = TRUE)){
      clip(x1 = 0,
           x2 = max(h$breaks), 
           y1 = 0,
           y2 = max(h$density) - max(h$density)/10)
      abline(v = Angle.speed, col="blue", lwd=1.5, lty=2) # User defined 'Dist.thresh [R1]'
    }
    clip(x1 = 0,
         x2 = max(h$breaks), 
         y1 = 0,
         y2 = max(h$density)/2)
    abline(v = quantile(c(df.sub$Outgoing.speed, df.sub$Incoming.speed), 0.95, na.rm = TRUE), col="green", lwd=1.5, lty=2) #95% quantile
    clip(x1 = 0,
         x2 = max(h$breaks), 
         y1 = 0,
         y2 = max(h$density)/4)
    abline(v = quantile(c(df.sub$Outgoing.speed, df.sub$Incoming.speed), 0.99, na.rm = TRUE), col="red", lwd=1.5, lty=2) #99 % quantile
    
    #(4) Distribution of Maximum speed (R3)
    par(fig = c(0.5, 1, 0, 0.5), new = T)
    h = hist(df.sub$Maximum.speed, breaks="Scott", plot=TRUE, main = "Maximum speed (m/s)",
             xlab = "Maximum speed (m/s) [R3]", ylab = "Density", cex.lab = 1.2,  freq = FALSE)
    text(Max.speed, max(h$density), labels = paste("R3 =", Max.speed), col = "blue", cex = 0.8) # User defined 'Max.speed [R3]'
    text(quantile(df.sub$Maximum.speed, 0.95, na.rm = TRUE), max(h$density)/1.7, labels = "95%", col = "green", cex = 0.8) # 95%
    text(quantile(df.sub$Maximum.speed, 0.99, na.rm = TRUE), max(h$density)/3.2, labels = "99%", col = "red", cex = 0.8) # 99%
    clip(x1 = 0,
         x2 = max(h$breaks), 
         y1 = 0,
         y2 = max(h$density) - max(h$density)/10)
    abline(v = Max.speed, col="blue", lwd=1.5, lty=2) # User defined 'Dist.thresh [R1]'
    clip(x1 = 0,
         x2 = max(h$breaks), 
         y1 = 0,
         y2 = max(h$density)/2)
    abline(v = quantile(c(df.sub$Outgoing.speed, df.sub$Incoming.speed), 0.95, na.rm = TRUE), col="green", lwd=1.5, lty=2) #95% quantile
    clip(x1 = 0,
         x2 = max(h$breaks), 
         y1 = 0,
         y2 = max(h$density)/4)
    abline(v = quantile(c(df.sub$Outgoing.speed, df.sub$Incoming.speed), 0.99, na.rm = TRUE), col="red", lwd=1.5, lty=2) #99 % quantile
    #Zoom in
    par(fig = c(0.7, 0.99, 0.15, 0.49), new = T)
    h = hist(c(df.sub$Outgoing.speed, df.sub$Incoming.speed), breaks="Scott", plot=TRUE, main = "",
             xlab = "", ylab = "", freq = FALSE, xlim = c(0, quantile(df.sub$Maximum.speed, 0.99, na.rm = TRUE)))
    if(Max.speed < quantile(df.sub$Maximum.speed, 0.99, na.rm = TRUE)){
      text(Max.speed, max(h$density), labels = paste("R3 =", Max.speed), col = "blue", cex = 0.8) # User defined 'Angle.speed [R2]'
    }
    text(quantile(df.sub$Maximum.speed, 0.95, na.rm = TRUE), max(h$density)/1.7, labels = "95%", col = "green", cex = 0.8) # 95%
    text(quantile(df.sub$Maximum.speed, 0.99, na.rm = TRUE), max(h$density)/3.2, labels = "99%", col = "red", cex = 0.8) # 99%
    if(Max.speed < quantile(df.sub$Maximum.speed, 0.99, na.rm = TRUE)){
      clip(x1 = 0,
           x2 = max(h$breaks), 
           y1 = 0,
           y2 = max(h$density) - max(h$density)/10)
      abline(v = Angle.speed, col="blue", lwd=1.5, lty=2) # User defined 'Dist.thresh [R1]'
    }
    clip(x1 = 0,
         x2 = max(h$breaks), 
         y1 = 0,
         y2 = max(h$density)/2)
    abline(v = quantile(df.sub$Maximum.speed, 0.95, na.rm = TRUE), col="green", lwd=1.5, lty=2) #95% quantile
    clip(x1 = 0,
         x2 = max(h$breaks), 
         y1 = 0,
         y2 = max(h$density)/4)
    abline(v = quantile(df.sub$Maximum.speed, 0.99, na.rm = TRUE), col="red", lwd=1.5, lty=2) #99 % quantile
    
    Thresholds <- recordPlot() #Save plot
    
    ###ggplot Results of filtering (user-defined thresholds)###
    p1 = ggplot(df.sub, aes(x = Longitude, y = Latitude,
                            text =  paste('Timestamp: ', Timestamp,
                                          '</br> Fix number: ', Fix.number,
                                          '</br> Window group: ', Window.group,
                                          '</br> Distance from median (m): ', round(Dist.from.median, 1),
                                          '</br> Angle between 3 fixes (°): ', round(Ang.vertex, 1),
                                          '</br> Outgoing speed of 3 fixes (m/s): ', round(Outgoing.speed, 2),
                                          '</br> Incoming speed of 3 fixes (m/s): ', round(Incoming.speed, 2),
                                          '</br> Maximum speed (m/s): ', round(Maximum.speed, 2),
                                          '</br> R1: ', R1,
                                          '</br> R2: ', R2,
                                          '</br> R3: ', R3)))+
      geom_path(aes(group=1), size = 0.3,  alpha = 0.5, color = "grey30")+
      geom_point(aes(color = Verdict), alpha = 0.8)+
      ggtitle("Gundog GPS filter")+
      xlab("Longitude")+ 
      ylab("Latitude")+
      scale_color_manual(name = "Outlier detection:",
                         labels = c("Not anomalous" ,"Possible outlier", "Unanimous outlier"),
                         values = c("green", "darkorange2" ,"red"),
                         na.translate = FALSE,
                         drop = FALSE)+
      coord_equal(ratio = 1) + #+ scale_x_continuous(labels=scaleFUN)+scale_y_continuous(labels=scaleFUN)
      theme_bw()+
      theme(axis.text.x = element_text(color = "black", size = 12),
            axis.text.y = element_text(color = "black", size = 12),
            axis.title.x = element_text(color = "black", size = 16),
            axis.title.y = element_text(color = "black", size = 16),
            plot.title = element_text(hjust = 0.5),
            legend.position = "none") 
    
    ###ggplot Results of filtering (Isolation forest)####
    p2 = ggplot(df.sub, aes(x = Longitude, y = Latitude,
                            text =  paste('Timestamp: ', Timestamp,
                                          '</br> Fix number: ', Fix.number,
                                          '</br> Window group: ', Window.group,
                                          '</br> Distance from median (m): ', round(Dist.from.median, 1),
                                          '</br> Angle between 3 fixes (°): ', round(Ang.vertex, 1),
                                          '</br> Outgoing speed of 3 fixes (m/s): ', round(Outgoing.speed, 2),
                                          '</br> Incoming speed of 3 fixes (m/s): ', round(Incoming.speed, 2),
                                          '</br> Maximum speed (m/s): ', round(Maximum.speed, 2),
                                          '</br> I.F_anomaly_score_R1: ', round(I.F_anomaly_score_R1, 2),
                                          '</br> I.F_anomaly_score_R2: ', round(I.F_anomaly_score_R2, 2),
                                          '</br> I.F_anomaly_score_R3: ', round(I.F_anomaly_score_R3, 2))))+
      geom_path(aes(group=1), size = 0.3,  alpha = 0.5, color = "grey30")+
      geom_point(aes(color = Verdict_IF), alpha = 0.8)+
      ggtitle("Gundog GPS filter")+
      xlab("Longitude")+ 
      ylab("Latitude")+
      scale_color_manual(name = "Outlier detection:",
                         labels = c("Not anomalous" ,"Possible outlier", "Unanimous outlier"),
                         values = c("green", "darkorange2" ,"red"),
                         na.translate = FALSE,
                         drop = FALSE)+
      coord_equal(ratio = 1) + #+ scale_x_continuous(labels=scaleFUN)+scale_y_continuous(labels=scaleFUN)
      theme_bw()+
      theme(axis.text.x = element_text(color = "black", size = 12),
            axis.text.y = element_text(color = "black", size = 12),
            axis.title.x = element_text(color = "black", size = 16),
            axis.title.y = element_text(color = "black", size = 16),
            plot.title = element_text(hjust = 0.5),
            legend.position = "none") 
    
    #Make Interactive and plot user-defined, and Isolation Forests results side-by-side
    fig <- subplot(ggplotly(p1), ggplotly(p2)) 
    #Add sub headings
    annotations = list( 
      list( 
        x = 0.2,  
        y = 1.0,  
        text = "User-defined threholds",  
        xref = "paper",  
        yref = "paper",  
        xanchor = "center",  
        yanchor = "bottom",  
        showarrow = FALSE 
      ),  
      list( 
        x = 0.8,  
        y = 1,  
        text = "Isolation forest",  
        xref = "paper",  
        yref = "paper",  
        xanchor = "center",  
        yanchor = "bottom",  
        showarrow = FALSE 
      ))
    
    fig <- fig %>%layout(annotations = annotations) 
    print(fig)
    return(list(Thresholds = Thresholds, Plot = fig, df = as.data.frame(df.sub)))
    par(mfrow = c(1,1)) #Return plotting parameters back
  }else{ return(df = as.data.frame(df.sub))
  } #If no plotting, just return the data frame
}

#END OF FUNCTION

#For example.... (assuming original GPS data set is called 'Drogon')
#df.sub = Gundog.GPS.Filter(TS = Drogon$study.local.timestamp, Longitude = Drogon$location.long, Latitude = Drogon$location.lat, Drop.out = 300, Window.length = 10, Burst.method = "median", Dist.thresh = 50, Max.speed = 0.85, Speed.step = 1, Angle.speed = 0.85, Angle.thresh = 30, plot = TRUE, I.F_conf = 0.995)

#And to recover the data frame from the returned list...
#GPS.df = df.sub[["df"]]






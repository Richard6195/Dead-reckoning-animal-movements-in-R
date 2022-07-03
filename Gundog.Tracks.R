#+eval=FALSE 
###########################################################################################################################################
###############################################################Gundog.Tracks###############################################################
##########################################################VPC-DR correction script#########################################################
###########################################################################################################################################
###########################################################################################################################################

#Dead-reckoning in R with option for Verified Position Correction (VPC)
#Outputs a data frame
#Plots provided if plot = TRUE 
#The function inputs explained:

#Gundog.Tracks = function(TS, h, v, elv = 0, p = NULL, cs = NULL, ch = NULL, m = 1, c = 0, ME = 1, lo = 0, la = 0, VP.lon = NULL, VP.lat = NULL, VP.ME = FALSE, method = NULL, thresh = NULL, dist.step = 1, bound = TRUE, Outgoing = TRUE, plot = FALSE)

#TS = Timestamp  --> as.POSIXct object in format - Ensure decimal seconds expressed for data obtained at > 1Hz
#h = Magnetic heading (Preferably smoothed by at least 1 or 2 seconds (circular mean)).
#v = Input speed (m/s) or DBA metric of choice. E.g., VeDBA (Preferably smoothed by at least 1 or 2 seconds (arithmetic mean)).
#elv = Elevation / depth data (m). If supplied, 3-D distance estimates will be output
#p = Pitch - Only supply if user wants radial distance modulated according to pitch (q multiplied by cosine of pitch)
#cs = Current speed (m/s). Supplied as a single value or vector/column of changeable values. NA's are replaced with the most recent non-NA prior to it (observations carried forward)
#ch = Current heading degrees (0 to 360 degrees).  Supplied as a single value or vector/column of changeable values.  NA's are replaced with the most recent non-NA prior to it (observations carried forward)
#m = Coefficient to multiply VeDBA by for speed estimate (e.g. The gradient (default = 1)). These can be changeable values (DBA~speed relationships according to movement modes) the length of other vector inputs (e.g., 'h') or a single value (one gradient used on all DBA values). 
#c = Constant (e.g., The intercept (default = 0)). These can be changeable values (DBA~speed relationships according to movement modes) the length of other vector inputs (e.g. 'h') or a single value (one constant used on all DBA values).
#ME = Marked Events column of integer values: 0 = Not-moving (DR will not advance), > 0 (e.g., 1) = Moving (DR will advance) --> A simple distinction of movement e.g. based from a DBA threshold.
#lo = Initial (or final) longitude coordinate (decimal format) that the beginning (or end) of DR track starts from. See 'outgoing' for more details.
#la = Initial (or final) latitude coordinate (decimal format) that beginning (or end) of DR track starts from. See 'outgoing' for more details.
#VP.lat = VP latitude coordinates (decimal format) synced up in time to movement sensor data --> Can contain NA's or 0's (e.g. between fixes, given movement sensor data is typically recorded at a higher frequency).
#VP.lon = VP longitude coordinates (decimal format) synced up in time to  movement sensor data  --> Can contain NA's or 0's (e.g. between fixes, given movement sensor data is typically recorded at a higher frequency).
  #Note the supplied lo and la coordinates will be used as initial VP  during the VPC dead-reckoning procedure (for both outgoing = TRUE and outgoing = FALSE).

#VP.ME = Prior to the methods of VP under-sampling, VPC and VP summaries, if VP.ME is set to TRUE, then VP positions are disregarded at times when Marked events (ME) are zero (animal deemed stationary). Note this may also affect the 'bound' parameter

#method = How the function under-samples VPs prior to VPC - there are 4 different options; 'all', 'divide', 'time' or 'distance'. If method is unstated in the function input, default is NULL (assume no VPC required). 
#If VP.ME = TRUE, then only VPs present when ME > 0 are used from this moment on. The initial set lo/la coordinates are not subset, even if ME = 0 and VP.ME = TRUE

#If method = 'divide' for VPC method, then under sampling is based on splitting VP data frame (made within function) into segments based on row number. The first and last fix are always included.
#however this method is adapted to ensure the splicing occurs in relation to the number of rows between first and last fix and the 'thresh' value you supply.
#if thresh = 1 (one correction), if thresh = 2 (two corrections, that is; first last and middle coordinates (based on row number) are used) etc.
#This method is only recommended if VP success rate is high, since missing locational data is filtered before splicing so will result in uneven fix correction according to time.
#This method also suffers from similar limitations of the 'time' method, in that if the animal does not move much for long periods, VPC will be biased at correcting more frequently at such times.

#If method = 'time' (in seconds) for VPC method, then VPs are under sampled based on accumulated time between the fixes.
#The time accumulates until the supplied 'thresh' value is reached, after which the corresponding or next available fix is used and the time resets.
#(e.g. If a GPS was recording at 1 Hz and thresh = 60, under sampling will occur 1 fix each minute (assuming no missing GPS data)). 
#This version of under sampling is weighted and based on time (seconds), which means it's not just every 'ith' fix, but rather every fix that matches (or first fix that exceeds) the conditional
#time threshold between fixes --> This means the first fix after a period of no positions (greater than degree of under sampling set) will always be included and so useful if there are large gaps in the locational data provided.
#Note, first and last fix always included as default

#If method = 'distance' (in meters) for VPC method, VPs are under sampled based on the accumulated distance between them. 
#That is, the maximum accumulated 2-D distance calculated between VPs of a given stepping range (interval between each retained fix), divided by the thresh value supplied. This stepping range is governed by the input; 'dist.step'
#Note 'thresh' value for 'distance' method is not meters, but at what proportion of the total accumulated distance.
#E.g. If thresh = 10 and the accumulated distance of VP track was 500 m, then, analogous to 'time' method, each fix at or after each accumulated 50 m is included within the correction procedure.
#Note, first and last fix always included as default, however method is adapted to ensure spacing according to thresh is maintained.
#Note accumulated distance is likely inaccurate due to VP inaccuracy, fix success rate, and sampling frequency of obtained VPs

#method = "all" will result in every VP being used within the VPC dead-reckoning procedure, irrespective of thresh set.
#This is not recommended for high resolution VP data sets (e.g. 1 Hz GPS).

#thresh = Degree of VP under-sampling - the frequency of under sampling depends on the 'method' chosen; (see above):

#dist.step = The stepping interval used for calculating distance between VPs, both within the summary outputs for VP.distance and VP.cumulative.distance and within the method = 'distance' VP under-sampling method. E.g., dist.step = 5 means distance is computed between every 5th VP (irrespective of the time between them)

#If bound = TRUE, VPC dead-reckoning will be bounded by the first and last VP present, in other words, dead-reckoned track will start (like usual) at the supplied lo and la
#coordinates (row number 1) but will finish at the row number location of the last VP (subsequent to VP.ME filtering if set to TRUE) - bound = TRUE is default.
#If bound = FALSE, then DR track will extend beyond the last fix and will continue until the end (of movement sensor data)--> This uncorrected segment will inherit the last available correction factors from the previous corrected segment.

#If Outgoing = FALSE, then dead-reckoning will function in reverse. This is useful when the pre-determined position is unknown, but the finishing coordinates are
#If Outgoing = TRUE, the normal dead-reckoning will occur. Default is 'TRUE'. 

#User can opt for standard dead-reckoning (no VPC) by leaving 'VP.lat', 'VP.lon' and 'method' fields blank in the function input (default is NULL), bound and method parameters within the function will also be ignored.

#max.speed = Speed values (m/s) are capped at this threshold if supplied (default = NULL) pre current integration and/or VPC

#If plot = TRUE, with VPC, a summary of plots showing relationship between DR track and VP track, with error estimates are displayed for:
#2 levels of VPC (1) No VPC (pre and post current integration if supplied) and (2) The level of VPC you stated in your thresh (and method)
#Uncorrected and corrected DR tracks also plotted
#If plot = TRUE, with no VPC, only the original DR track will be plotted

#Warning: VPC too frequently (e.g., >= 0.2 Hz) will likely cause the dead-reckoned track to inherit locational errors of the VPs.
#Warning: VPC too frequently may also result in infinity (Inf) correction values. (dividing a given distance obtained between VPs by a zero-distance obtained between aligned DR positions = Inf). 
#Warning: Ensure the package 'inputeTS' is not required (detached) as this affects the functionality of 'na.locf' used within this script.
#Possible other reasons why this could occur include wrongly assigned Marked events (ME) and speed coefficients (e.g., m value).
#This function will recalculate the degree of VP under-sampling necessary to ensure this is not the case. 
#This may, however, alter the method and threshold of VPC the user initially sets and may take additional time to resolve (dependent on the size of data set and regularity of Inf values).

#Note, the speed of this function will increase if the user pre-sorts VP data to be used for correction and chooses method = 'all'.
#Lastly within the function a threshold of 0.01 m difference between VPs and corresponding corrected DR positions exists. To achieve this level of accuracy, repeats of the correction process may occur. User can find this value and change to larger number if desired.



################################################################################################################################################################################################################################################################################
################################################################################################################################################################################################################################################################################
################################################################################################################################################################################################################################################################################
############################################################# START OF FUNCTION ################################################################################################################################################################################################
################################################################################################################################################################################################################################################################################
################################################################################################################################################################################################################################################################################
################################################################################################################################################################################################################################################################################

Gundog.Tracks = function(TS, h, v, elv = 0, p = NULL, cs = NULL, ch = NULL, m = 1, c = 0, ME = 1, lo = 0, la = 0, VP.lon = NULL, VP.lat = NULL, VP.ME = FALSE, method = NULL, thresh = 1, dist.step = 1, bound = TRUE, Outgoing = TRUE, max.speed = NULL, plot = FALSE) {               
  
 
  if (!require('zoo')){ install.packages('zoo')} ; library('zoo') #Required packages
  if (!require('dplyr')){ install.packages('dplyr')} ; library('dplyr') #Required packages
  if(plot == TRUE & Sys.info()["sysname"] == "Windows") { windows(width=12, height=10) } #Set graphics window if plot = TRUE (and operating system is windows)
  options(warn = -1) #Remove warnings()
  options(digits.secs = 3) #Specify the number of decimal places of the fractional seconds to show if relevant   
  is.POSIXct = function(x) inherits(x, "POSIXct") #Function to check variable is of POSIXct type
  options(digits = 16) #Increase visual numerical precision reference
  
  ##############################################################################################################################################################################################################################################################################
  #1) Prepare input vectors
  ##############################################################################################################################################################################################################################################################################

  la = la * pi/180 #convert starting Latitude coordinate to radians
  lo = lo * pi/180 #convert starting Longitude coordinate to radians
  h = as.numeric(h) #Magnetic Heading
  v = as.numeric(v) #DBA / speed
  elv = as.numeric(elv) ; if(length(elv) == 1) { elv = rep(elv, length(h)) } #Elevation / depth. If only one value supplied (or none supplied (default = 0)), then replicate the length of other vectors 
  ME = as.numeric(ME) ; if(length(ME) == 1){ ME = rep(ME, length(h)) } #Marked event vector (> 0 = moving, 0 = Not moving) - #If only one value supplied (or none supplied (default = 1)), then replicate the length of other vectors 
  m = as.numeric(m) ; if(length(m) == 1) { m = rep(m, length(h)) } #If only one value supplied (or none supplied (default = 1)), then replicate the length of other vectors 
  c = as.numeric(c) ; if(length(c) == 1) { c = rep(c, length(h)) } #If only one value supplied (or none supplied (default = 0)), then replicate the length of other vectors 
  Row.number = rep(1:length(h)) #Row number (used for indexing and merging data frames)
  DR.lon = rep(NA, length(h)) ; DR.lat = rep(NA, length(h)) #Empty vectors --> will become (uncorrected) DR longitude and DR latitude coordinates
  DR.lat[1] = la  #Set 1st row of DR latitude with la, ready for DR procedure
  DR.lon[1] = lo  #Set 1st row DR longitude with lo, ready for DR procedure
  
  if(is.POSIXct(TS) == FALSE) { #Ensure TS is of type POSIXct (otherwise terminate function)
    stop("The function stops - TS must be of type POSIXct.")
  }
  
  if(length(unique(TS)) != length(TS)){ #Ensure TS does not contain duplicate timestamps (otherwise terminate function)
    stop("The function stops - TS must not contain duplicates")
  }
  
  if(is.null(cs) == FALSE | is.null(ch) == FALSE) {
    cs = as.numeric(cs) ; cs = ifelse(cs < 0, 0, cs) #If current variables provided, values placed in vectors (ensure surrent strength is not negative)
    ch = as.numeric(ch)           #If current variables provided, values placed in vectors 
    if(length(ch) == 1){ ch = rep(ch, length(h))} #If only one value supplied, replicate the length of other vectors used in the dead-reckoning process
    if(length(cs) == 1){ cs = rep(cs, length(h))} #If only one value supplied, replicate the length of other vectors used in the dead-reckoning process
    
    if(is.na(cs[1]) == TRUE | is.na(ch[1]) == TRUE ) { #Ensure if cs and ch values supplied, the initial value is not an NA
      stop("The function stops - Initial ch or cs value is missing (NA)")
    }
    
    cs = zoo::na.locf(cs)             #Na's replaced with most recent non-NA value (observations carried forwards)
    ch = zoo::na.locf(ch)             #Na's replaced with most recent non-NA value (observations carried forwards)
    xx = rep(NA, length(cs))      #Empty vector, will contain uncorrected DR longitude coordinates prior to adding current vectors (for comparison purposes)
    yy = rep(NA, length(cs))      #Empty vector, will contain uncorrected DR latitude coordinates prior to adding current vectors (for comparison purposes)
  }
  
  if(is.null(p) == FALSE) { #If pitch supplied, then assume the absolute distance and pitch angle is used to derive horizontal distance moved (q * cos(p)) --> Under equal pitch assumption
    p = as.numeric(p) ; p = p * pi/180 #Convert to radians
    
    if(any(is.na(p) == TRUE)){ #Ensure if p values supplied, no NA's are present (otherwise terminate function)
      stop("The function stops - User has input p data containing NA(s)")
    }
    
  }

  #A quick check to ensure input data does not contain NA's - If not passed, function terminated  
  if(any(is.na(TS) == TRUE)) {
    stop("The function stops - User has input TS data containing NA(s)")
  }
  if(any(is.na(h) == TRUE)) {
    stop("The function stops - User has input h data containing NA(s)")
  }
  if(any(is.na(v) == TRUE)) {
    stop("The function stops - User has input v data containing NA(s)")
  }
  if(any(is.na(elv) == TRUE)) {
    stop("The function stops - User has input elv data containing NA(s)")
  }
  if(max(ME, na.rm = TRUE) < 1) {
    stop("The function stops - User has no identifed walking periods (1's)") #NA's allowed as converting characters (e.g., 'M' required in Gundog.Compass) to numeric format results in NA
  }
  if(any(is.na(m) == TRUE)) {
    stop("The function stops - User has input m data containing NA(s)")
  }
  if(any(is.na(c) == TRUE)) {
    stop("The function stops - User has input c data containing NA(s)")
  }
  ME[is.na(ME)] = 0 #If ME contains NA's then replace with zero
  ##############################################################################################################################################################################################################################################################################
  #2) Prepare 'disty' (distance) and 'beary' (bearing) functions for VPC and/or and summary variables
  ##############################################################################################################################################################################################################################################################################
 
   #disty function for VPC and summary distance moved/speed estimates
   disty = function(long1, lat1, elv1 = 0, long2, lat2, elv2 = 0, method = "Haversine") { #longitude and latitude supplied in degrees, elevation/depth supplied in meters
    #Calculates either: #1) Haversine distance between coordinates (great circular distance 'as the crow flies') -- > output = "Haversine"
                        #2) straight-line distance between sets of Cartesian coordinates (x,y,z), incorporating change in vertical axis (elv - depth/elevation) --> output = "SLD"
                        #3) Modulation of both (Haversine when no change in vertical axis between coordinates and SLD when there is) --> output = "Hav.SLD"
    #Default assumes no elevation data 
     if(length(elv1) == 1){ elv1 = rep(elv1, length(long1)) } ; if(length(elv2) == 1){ elv2 = rep(elv2, length(long1)) }
    #Great circular distance between 2D positions (Haversine)
    long1 = long1 * pi/180 ; long2 = long2 * pi/180 ; lat1 = lat1 * pi/180 ; lat2 = lat2 * pi/180 #Function converts to radians
    a = sin((lat2 - lat1) / 2) * sin((lat2 - lat1) / 2) + cos(lat1) * cos(lat2) * sin((long2 - long1) / 2) * sin((long2 - long1) / 2)
    c = 2 * atan2(sqrt(a), sqrt(1 - a))
    d1 = 6378137 * c
    #Convert lat (degrees), long (degrees), elv (meters) to Cartesian x, y, z coordinates incorporating Earth's oblate spheroid
    #Geodetic latitude is format given by VP  --> Conversion to Geocentric latitude =-> angle measured from Earth's center between a point and the equator
    maj.a = 6378137 #Equatorial radius in meters (semi-major axis)
    min.a = 6356752.314245 #Polar radius in meters  (semi-minor axis)
    e2 = 1 - (min.a^2 / maj.a^2)
    #Distance from the center of the Earth to a given point on its surface --> As a function of geodetic latitude         
    x1 = sqrt(((maj.a * maj.a * cos(lat1))^2 + (min.a * min.a * sin(lat1))^2) / ((maj.a * cos(lat1))^2 + (min.a * sin(lat1))^2)) * cos(long1) * cos(atan((1 - e2) * tan(lat1)))
    x2 = sqrt(((maj.a * maj.a * cos(lat2))^2 + (min.a * min.a * sin(lat2))^2) / ((maj.a * cos(lat2))^2 + (min.a * sin(lat2))^2)) * cos(long2) * cos(atan((1 - e2) * tan(lat2)))
    y1 = sqrt(((maj.a * maj.a * cos(lat1))^2 + (min.a * min.a * sin(lat1))^2) / ((maj.a * cos(lat1))^2 + (min.a * sin(lat1))^2)) * sin(long1) * cos(atan((1 - e2) * tan(lat1)))
    y2 = sqrt(((maj.a * maj.a * cos(lat2))^2 + (min.a * min.a * sin(lat2))^2) / ((maj.a * cos(lat2))^2 + (min.a * sin(lat2))^2)) * sin(long2) * cos(atan((1 - e2) * tan(lat2)))
    z1 = sqrt(((maj.a * maj.a * cos(lat1))^2 + (min.a * min.a * sin(lat1))^2) / ((maj.a * cos(lat1))^2 + (min.a * sin(lat1))^2)) * sin(atan((1 - e2) * tan(lat1)))
    z2 = sqrt(((maj.a * maj.a * cos(lat2))^2 + (min.a * min.a * sin(lat2))^2) / ((maj.a * cos(lat2))^2 + (min.a * sin(lat2))^2)) * sin(atan((1 - e2) * tan(lat2)))
    #Use geodetic latitude to calculate normal vector from the surface -- > incorporating elevation
    x1 = x1 + elv1 * cos(lat1) * cos(long1)
    x2 = x2 + elv2 * cos(lat2) * cos(long2)
    y1 = y1 + elv1 * cos(lat1) * sin(long1)
    y2 = y2 + elv2 * cos(lat2) * sin(long2)
    z1 = z1 + elv1 * sin(lat1)
    z2 = z2 + elv2 * sin(lat2)
    #Euclidean straight-line distance between sets of Cartesian coordinates
    d2 = sqrt((x1 - x2)^2 + (y1 - y2)^2 + (z1 - z2)^2)
    
    #Haversine
    if(method == "Haversine"){
      distance = d1
    }
    #Just SLD
    if(method == "SLD"){
      distance = d2
    }
    #Modulation of Haversine and SLD
    if(method == "Hav.SLD"){
      distance = ifelse(elv1 - elv2 == 0, d1, d2)
    }
    return(distance)
   }
  
  ####################################################
  
  #Bearing function --> returns degrees - Great circular bearing between 2D positions (assumes time-matched VP and dead-reckoned fixes share the same elevation/depth)
  beary = function(long1, lat1, long2, lat2) { #Assumes units supplied as degrees
    long1 = long1 * pi/180 ; long2 = long2 * pi/180 ; lat1 = lat1 * pi/180 ; lat2 = lat2 * pi/180 #Function converts to radians
    a = sin(long2 - long1) * cos(lat2)
    b = cos(lat1) * sin(lat2) - sin(lat1) * cos(lat2) * cos(long2 - long1)
    c = ((atan2(a, b) / pi) * 180)  #Units returned in degrees (-180 to +180 degree scale)
    return(c)
  }
  
  ##############################################################################################################################################################################################################################################################################
  #3) Prepare for normal Dead-reckoning if Outgoing = TRUE
  ##############################################################################################################################################################################################################################################################################
  
  if(Outgoing == TRUE) {
    
    TD = c(0, difftime(TS, lag(TS), units = "secs")[-1]) #Calculate frequency (Hz) of data (time difference (s) between rows)
    s = (v * m) + c ; s = ifelse(s < 0, 0, s) #Speed estimation using proportionality coefficient (m) and constant (c) --> Equally seen as regression 'gradient' and 'intercept' (cap at 0 in case intercept drives negative values) 
    if(is.null(max.speed) == FALSE){s = ifelse(s > max.speed, max.speed, s) } #If maximum speed threshold is supplied, cap speed values at the max.speed value
    q = (s * TD) / 6378137 #Speed estimation multiplied by time between rows for distance. q = Speed coefficient incorporating radius of earth (distance/R (R = approx. 6378137 m)) 
    if(is.null(p) == FALSE) { q = q * cos(p) } #If pitch supplied, multiply radial distance by pitch (assume direction of animal movement coincides with the direction of its longitudinal axis) 
    q = ifelse(ME == 0, 0, q) #If Marked events = 0, distance coefficient becomes 0 m (since animal deemed not moving)
    h = h * pi/180 #Convert heading to radians for trigonometric functions  
    if(is.null(cs) == FALSE | is.null(ch) == FALSE) { ch = ch * pi/180 } #If supplied, convert current heading to radians for trigonometric functions 
    
    #Dead-reckoning with current integration
    ####################################################################
    
    if(is.null(cs) == FALSE | is.null(ch) == FALSE) { #If current speed (cs) and current heading (ch) supplied, these vectors are added per dead-reckoned iteration
      
      #2 equations for dead-reckoning latitude and longitude coordinates (no current integration yet)
      for(i in 2:length(DR.lat)){ 
        DR.lat[i] = asin(sin(DR.lat[i-1]) * cos(q[i]) + cos(DR.lat[i-1]) * sin(q[i]) * cos(h[i])) 
        DR.lon[i] = DR.lon[i-1] + atan2(sin(h[i]) * sin(q[i]) * cos(DR.lat[i-1]), cos(q[i]) - sin(DR.lat[i-1]) * sin(DR.lat[i]))
      }
      
      xxx = DR.lon ; yyy = DR.lat #Save above coordinates as 'xxx' and 'yyy' prior to iterative addition of current vectors
      
      #Now dead-reckoning with current integration
      for(i in 2:length(DR.lat)){
        DR.lat[i] = asin(sin(DR.lat[i-1]) * cos(q[i]) + cos(DR.lat[i-1]) * sin(q[i]) * cos(h[i])) 
        yy[i] = DR.lat[i]   #Save DR coordinate prior to implementing current drift within 'yy' vector
        DR.lon[i] = DR.lon[i-1] + atan2(sin(h[i]) * sin(q[i]) * cos(DR.lat[i-1]), cos(q[i]) - sin(DR.lat[i-1]) * sin(DR.lat[i]))
        xx[i] = DR.lon[i]  #Save DR coordinate prior to implementing current drift within 'xx' vector
        DR.lat[i] = asin(sin(yy[i]) * cos((cs[i] * TD[i]) / 6378137) + cos(yy[i]) * sin((cs[i] * TD[i]) / 6378137) * cos(ch[i]))
        DR.lon[i] = xx[i] + atan2(sin(ch[i]) * sin((cs[i] * TD[i]) / 6378137) * cos(yy[i]), cos((cs[i] * TD[i]) / 6378137) - sin(yy[i]) * sin(DR.lat[i]))
      }
      yyy = 180 * yyy/pi #convert back to grid coordinates
      xxx = 180 * xxx/pi #convert back to grid coordinates 
      
      DR.loni = c(DR.lon[-1], NA) #Shift DR current corrected longitude backwards 1 row
      DR.lati = c(DR.lat[-1], NA) #Shift DR current corrected longitude backwards 1 row
      
      #Recalculate heading after current integration
      drift.h = beary(180 * DR.lon/pi, 180 * DR.lat/pi, 180 * DR.loni/pi, 180 * DR.lati/pi) #Function assumes units supplied in degrees, so convert back from radians
      drift.h = ifelse(drift.h < 0, drift.h + 360, drift.h) #Convert heading output from -180 to +180 degrees to 0 to 360 degrees
      drift.h = c(NA, drift.h[-length(drift.h)]) #Ensure current integrated heading match element-wise to the previous un-corrected version
      #Recalculate distance coefficient (radial distance) after current integration
      drift.q = disty(180 * DR.lon/pi, 180 * DR.lat/pi, 0, 180 * DR.loni/pi, 180 * DR.lati/pi, 0) #Function assumes units supplied in degrees, so convert back from radians
      drift.q = drift.q / 6378137 #Incorporate radius of earth
      drift.q = c(NA, drift.q[-length(drift.q)]) #Ensure current integrated heading match element-wise to the previous un-corrected version
      
      DR.lat = 180 * DR.lat/pi #Convert back to grid coordinates
      DR.lon = 180 * DR.lon/pi #Convert back to grid coordinates
      h = 180 * h/pi       #Convert back to degrees
      ch = 180 * ch/pi     #Convert back to degrees
      la = 180 * la/pi     #Convert back to grid coordinates
      lo = 180 * lo/pi     #Convert back to grid coordinates
      
      df = data.frame(Row.number, TS, TD, h, v, q, ME, elv, cs, ch, DR.lon, DR.lat, h, q, drift.h, drift.q)  #Prepare main data frame called 'df' with initial dead-reckon track coordinates
      colnames(df) = c("Row.number", "Timestamp", "DR.seconds", "Heading", "DBA.or.speed", "Radial.distance", "Marked.events", "Elevation", "Current.speed", "Current.heading", "DR.longitude", "DR.latitude", "h", "q", "Heading.current.integrated", "Radial.distance.current.integrated")
      
      #Dead-reckoning with no current integration
      ####################################################################
      
    }else{ 
      
      for(i in 2:length(DR.lat)){
        DR.lat[i] = asin(sin(DR.lat[i-1]) * cos(q[i]) + cos(DR.lat[i-1]) * sin(q[i]) * cos(h[i])) #2 equations for dead-reckoning of latitude and longitude coordinates
        DR.lon[i] = DR.lon[i-1] + atan2(sin(h[i]) * sin(q[i]) * cos(DR.lat[i-1]), cos(q[i]) - sin(DR.lat[i-1]) * sin(DR.lat[i])) 
      }
      
      DR.lat = 180 * DR.lat/pi #Convert back to grid coordinates   
      DR.lon = 180 * DR.lon/pi #Convert back to grid coordinates
      h = 180 * h/pi # Convert back to degrees
      la = 180 * la/pi     #Convert back to grid coordinates
      lo = 180 * lo/pi     #Convert back to grid coordinates
      
      df = data.frame(Row.number, TS, TD, h, v, q, ME, elv, DR.lon, DR.lat, h, q)  #Prepare main data frame called 'df' with initial dead-reckon track coordinates
      colnames(df) = c("Row.number", "Timestamp", "DR.seconds", "Heading", "DBA.or.speed", "Radial.distance", "Marked.events", "Elevation", "DR.longitude", "DR.latitude", "h", "q")   
      
    }
    
    df$DR.seconds = as.numeric(df$DR.seconds) #Ensure this column is numeric
    
  }
  
  ##############################################################################################################################################################################################################################################################################
  #5) Reverse dead-reckoning if Outgoing = FALSE
  ##############################################################################################################################################################################################################################################################################
  
  if(Outgoing == FALSE){   
    
    TD = c(0, (difftime(TS, lag(TS), units = "secs"))[-1]) #Calculate frequency (Hz) of data (time difference (s) between rows)
    df = data.frame(Row.number, TS, TD, h, v, ME, m, c, elv)   #Bind relevant vectors to reverse
    if(is.null(cs) == FALSE | is.null(ch) == FALSE) { df$cs = cs ; df$ch = ch } #Include current speed and heading if supplied
    if(is.null(p) == FALSE) { df$p = p } #Include pitch if supplied 
    
    df = df[dim(df)[1]:1, ]              #Reverses df, so last row is first, second to last row is second etc...
    Row.number = df[, 'Row.number'] ; TS = df[, 'TS']; TD = df[, 'TD'] ; h = df[, 'h'] ; v = df[, 'v'] ; ME = df[, 'ME'] ; m = df[, 'm'] ; c = df[, 'c'] ; elv = df[, 'elv'] #Change the base vectors to the reversed df equivalents
    if(is.null(cs) == FALSE | is.null(ch) == FALSE) { cs = df[, 'cs'] ; ch = df[, 'ch'] } #Change the base vectors of current speed and heading if supplied
    if(is.null(p) == FALSE) { p = df[, 'p'] } #Change the base vector of pitch if supplied 
     
    #This is required so DR function works in completely the opposite to normal way --> Shift values across by one 
    h.rev = c(NA, h[-length(h)]) ; ME.rev = c(NA, ME[-length(ME)]) ;  v.rev = c(NA, v[-length(v)]) ; TD.rev = c(NA, TD[-length(TD)]) ; m.rev = c(NA, m[-length(m)]) ; c.rev = c(NA, c[-length(c)])
    if(is.null(cs) == FALSE | is.null(ch) == FALSE) { cs.rev = c(NA, cs[-length(cs)]) ; ch.rev = c(NA, ch[-length(ch)]) } #Same applies if current speed and heading supplied
    if(is.null(p) == FALSE) { p.rev = c(NA, p[-length(p)]) } ##Same applies if pitch supplied
    
    #reverse DR equivalent vectors
    h.rev = h.rev - 180                  #Heading needs to be rotated 180 degrees for reverse dead-reckoning
    h.rev = ifelse(h.rev < 0, h.rev + 360, h.rev)             #heading is circular so need to ensure subtracting 180 does not result in negative numbers
    if(is.null(cs) == FALSE | is.null(ch) == FALSE) { ch.rev = ch.rev - 180 ; ch.rev = ifelse(ch.rev < 0, ch.rev + 360, ch.rev) } #Same applies if current heading supplied
    s.rev = (v.rev * m.rev) + c.rev ; s.rev = ifelse(s.rev < 0, 0, s.rev) #Speed estimation using proportionality coefficient (m) and constant (c) --> Equally seen as regression 'gradient' and 'intercept' (cap at 0 in case intercept drives negative values) 
    if(is.null(max.speed) == FALSE){s.rev = ifelse(s.rev > max.speed, max.speed, s.rev) } #If maximum speed threshold is supplied, cap speed values at the max.speed value
    q.rev = (s.rev * TD.rev) / 6378137 #Speed estimation multiplied by the time between rows for distance. q = distance coefficient incorporating radius of earth (distance/R (R = approx. 6378137 m)) 
    if(is.null(p) == FALSE) { q.rev = q.rev * cos(p.rev) } #If pitch supplied, multiply radial distance by pitch (assume direction of animal movement coincides with the direction of its longitudinal axis)   
    q.rev = ifelse(ME.rev == 0, 0, q.rev) #If Marked events = 0, distance coefficient becomes 0 m (since animal deemed not moving) 
    h.rev = h.rev * pi/180 #Convert heading to radians for trigonometric functions
    if(is.null(cs) == FALSE | is.null(ch) == FALSE) { ch.rev = ch.rev * pi/180 } #If supplied, convert current heading to radians for trigonometric functions 
    
    #Reverse dead-reckoning with current integration
    ####################################################################
    
    if(is.null(cs) == FALSE | is.null(ch) == FALSE) { #If current speed and heading supplied, these vectors are added per dead-reckoned iteration
       
      #2 equations for dead-reckoning latitude and longitude coordinates (no current integration yet)
      for(i in 2:length(DR.lat)){ 
        DR.lat[i] = asin(sin(DR.lat[i-1]) * cos(q.rev[i]) + cos(DR.lat[i-1]) * sin(q.rev[i]) * cos(h.rev[i])) 
        DR.lon[i] = DR.lon[i-1] + atan2(sin(h.rev[i]) * sin(q.rev[i]) * cos(DR.lat[i-1]), cos(q.rev[i]) - sin(DR.lat[i-1]) * sin(DR.lat[i]))
      }
      xxx = DR.lon ; yyy = DR.lat #Save above coordinates as 'xxx' and 'yyy' prior to iterative addition of current vectors
      
      #Now dead-reckoning with current integration
      for(i in 2:length(DR.lat)){
        DR.lat[i] = asin(sin(DR.lat[i-1]) * cos(q.rev[i]) + cos(DR.lat[i-1]) * sin(q.rev[i]) * cos(h.rev[i])) 
        yy[i] = DR.lat[i]   #Save DR coordinate prior to implementing current drift within 'yy' vector
        DR.lon[i] = DR.lon[i-1] + atan2(sin(h.rev[i]) * sin(q.rev[i]) * cos(DR.lat[i-1]), cos(q.rev[i]) - sin(DR.lat[i-1]) * sin(DR.lat[i]))
        xx[i] = DR.lon[i]  #Save DR coordinate prior to implementing current drift within 'xx' vector
        DR.lat[i] = asin(sin(yy[i]) * cos((cs.rev[i] * TD.rev[i]) / 6378137) + cos(yy[i]) * sin((cs.rev[i] * TD.rev[i]) / 6378137) * cos(ch.rev[i]))
        DR.lon[i] = xx[i] + atan2(sin(ch.rev[i]) * sin((cs.rev[i] * TD.rev[i]) / 6378137) * cos(yy[i]), cos((cs.rev[i] * TD.rev[i]) / 6378137) - sin(yy[i]) * sin(DR.lat[i]))
      }
      yyy = 180 * yyy/pi #convert back to grid coordinates
      xxx = 180 * xxx/pi #convert back to grid coordinates 
      
      DR.loni = c(DR.lon[-1], NA) #Shift DR current corrected longitude backwards 1 row
      DR.lati = c(DR.lat[-1], NA) #Shift DR current corrected longitude backwards 1 row
      
      #Recalculate heading after current integration
      drift.h = beary(180 * DR.lon/pi, 180 * DR.lat/pi, 180 * DR.loni/pi, 180 * DR.lati/pi) #Function assumes units supplied in degrees, so convert back from radians
      drift.h = ifelse(drift.h < 0, drift.h + 360, drift.h) #Convert heading output from -180 to +180 degrees to 0 to 360 degrees
      drift.h = c(NA, drift.h[-length(drift.h)]) #Ensure current integrated heading match element-wise to the previous un-corrected version
      #Recalculate distance coefficient (radial distance) after current integration
      drift.q = disty(180 * DR.lon/pi, 180 * DR.lat/pi, 0, 180 * DR.loni/pi, 180 * DR.lati/pi, 0) #Function assumes units supplied in degrees, so convert back from radians
      drift.q = drift.q / 6378137 #Incorporate radius of earth
      drift.q = c(NA, drift.q[-length(drift.q)]) #Ensure current integrated heading match element-wise to the previous un-corrected version
      
      DR.lat = 180 * DR.lat/pi #Convert back to grid coordinates
      DR.lon = 180 * DR.lon/pi #Convert back to grid coordinates
      h.rev = 180 * h.rev/pi #Convert back to degrees
      la = 180 * la/pi     #Convert back to grid coordinates
      lo = 180 * lo/pi     #Convert back to grid coordinates
      
      df = data.frame(Row.number, TS, TD, h, v, q.rev, ME, elv, cs, ch, DR.lon, DR.lat, h.rev, q.rev, drift.h, drift.q)  #Prepare main data frame called 'df' with initial dead-reckon track coordinates
      df = df[dim(df)[1]:1,] #Change df dimensions back to original
      colnames(df) = c("Row.number", "Timestamp", "DR.seconds", "Heading", "DBA.or.speed", "Radial.distance", "Marked.events", "Elevation", "Current.speed", "Current.heading", "DR.longitude", "DR.latitude", "h", "q", "Heading.current.integrated", "Radial.distance.current.integrated")
      
      #Reverse dead-reckoning with no current integration
      ####################################################################
      
       }else{ 
         
      for(i in 2:length(DR.lat)){
        DR.lat[i] = asin(sin(DR.lat[i-1]) * cos(q.rev[i]) + cos(DR.lat[i-1]) * sin(q.rev[i]) * cos(h.rev[i])) #2 equations for dead-reckoning of latitude and longitude coordinates
        DR.lon[i] = DR.lon[i-1] + atan2(sin(h.rev[i]) * sin(q.rev[i]) * cos(DR.lat[i-1]), cos(q.rev[i]) - sin(DR.lat[i-1]) * sin(DR.lat[i])) 
      }
         
      DR.lat = 180 * DR.lat/pi #Convert back to grid coordinates   
      DR.lon = 180 * DR.lon/pi #Convert back to grid coordinates
      h.rev = 180 * h.rev/pi # Convert back to degrees
      la = 180 * la/pi     #Convert back to grid coordinates
      lo = 180 * lo/pi     #Convert back to grid coordinates
      
      df = data.frame(Row.number, TS, TD, h, v, q.rev, ME, elv, DR.lon, DR.lat, h.rev, q.rev)  #Prepare main data frame called 'df' with initial dead-reckon track coordinates
      df = df[dim(df)[1]:1,] #Change df dimensions back to original
      colnames(df) = c("Row.number", "Timestamp", "DR.seconds", "Heading", "DBA.or.speed", "Radial.distance", "Marked.events", "Elevation", "DR.longitude", "DR.latitude", "h", "q")   
       
       }
    
    df$DR.seconds = as.numeric(df$DR.seconds) #Ensure this column is numeric
    
  }
  
  ##############################################################################################################################################################################################################################################################################
  #6) NO VPC (VP.lat / VP.lon / method inputs are 'NULL') - Pseudo tracks - finish summary outputs and return df
  ##############################################################################################################################################################################################################################################################################
  
  if(is.null(VP.lat) == TRUE | is.null(VP.lon) == TRUE | is.null(method) == TRUE){
    
    print(paste("No VPC - Finishing summary outputs/plots"))

    df$la = df$DR.latitude[1] ; df$lo = df$DR.longitude[1]  #Add columns filled with starting coordinates ready for disty method (note for Outgoing = FALSE, this may be 'estimated' starting coordinates)
    df$Loni = c(df[-1, 'DR.longitude'], 0) ; df$Lati = c(df[-1, 'DR.latitude'], 0)  #Shift longitude and latitude column values back by one row, ready for the disty method (compute row wise distance between coordinates)
    
    #If elevation/depth data supplied (meters) then additional distance moved and speed metrics calculated, incorporating vertical displacement 
    if(max(abs(diff(df$Elevation))) > 0) { 
    
      #Haversine distance when no difference in elevation/depth. SLD distance when a difference (Hav.SLD)
      df$elo = df$Elevation[1] #Add columns filled with starting elevation/depth ready for disty method 
      df$Elvi = c(df[-1, 'Elevation'], 0) #Shift elevation/depth column values back by one row 
      df$DR.distance.3D = disty(df$DR.longitude, df$DR.latitude, df$Elevation, df$Loni, df$Lati, df$Elvi, method = "Hav.SLD")      #Calculate distance moved between rows and add this as a column ('DR.distance') to the data frame
      df$DR.distance.3D = c(0, df$DR.distance.3D[-nrow(df)]) #Shift values forward by 1
      df$DR.straightline.distance.from.start.3D = disty(df$lo, df$la, df$elo, df$DR.longitude, df$DR.latitude, df$Elevation, method = "Hav.SLD") #Calculate distance moved along track in relation to the straight-line distance to the supplied coordinates (lo and la)  
      df$DR.cumulative.distance.3D = df$DR.distance.3D #Prepare a column what will become cumulative distance (m)
      df$DR.cumulative.distance.3D = ifelse(is.na(df$DR.cumulative.distance.3D == TRUE), 0, df$DR.cumulative.distance.3D) #Ensure NA's replaced by zero 
      df$DR.cumulative.distance.3D = cumsum(df$DR.cumulative.distance.3D) # #Accumulate distance 
      df$DR.speed.3D = df$DR.distance.3D / df$DR.seconds    #Calculate (absolute 3D) speed (speed (m/s) = distance (m) / time (s) )
      df$DR.speed.3D[1] = 0 #Ensure first speed value is zero (not NA)
      df$Elevation.diff = c(0, diff(df$Elevation))/df$DR.seconds ; df$Elevtion.diff[1] = 0  #Calculate rate change of elevation/depth (m/s) ; ensure first row is 0 (since first row or DR.seconds is 0)
      
    }
    
    #Just Haversine distance
    df$DR.distance.2D = disty(df$DR.longitude, df$DR.latitude, 0, df$Loni, df$Lati, 0, method = "Haversine") #Calculate distance moved between rows and add this as a column ('DR.distance') to the data frame
    df$DR.distance.2D = c(0, df$DR.distance.2D[-nrow(df)]) #Shift values forward by 1
    df$DR.straightline.distance.from.start.2D = disty(df$lo, df$la, 0, df$DR.longitude, df$DR.latitude, 0, method = "Haversine") #Calculate distance moved along track in relation to the straight-line distance to the supplied coordinates (lo and la)  
    df$DR.cumulative.distance.2D = df$DR.distance.2D #Prepare a column what will become cumulative distance (m)
    df$DR.cumulative.distance.2D = ifelse(is.na(df$DR.cumulative.distance.2D == TRUE), 0, df$DR.cumulative.distance.2D) #Ensure NA's replaced by zero 
    df$DR.cumulative.distance.2D = cumsum(df$DR.cumulative.distance.2D) # #Accumulate distance 
    df$DR.speed.2D = df$DR.distance.2D / df$DR.seconds    #Calculate (horizontal 2D) speed (speed (m/s) = distance (m) / time (s) )
    df$DR.speed.2D[1] = 0 #Ensure first speed value is zero (not NA)
    df[, 'DR.seconds'] = cumsum(df[, 'DR.seconds'])
    
    df$Loni = NULL ;  df$Lati = NULL ; df$lo = NULL ; df$la = NULL ; df$h = NULL ; df$q = NULL ; if(max(abs(diff(df$Elevation))) > 0) { df$elo = NULL ; df$Elvi = NULL }  #Remove non important columns from data frame to be returned
    
    if(plot == TRUE){        #Plot DR track    
      par(mfrow = c(1,1))
      
      #If current variables supplied, plot dead-reckoned tracks - both current integrated and non-current integrated
      if(is.null(cs) == FALSE | is.null(ch) == FALSE) { #Start my finding out dimensions of all coordinates to be plotted
        x = df$DR.longitude
        x = c(x, xxx) ; min.lon = min(x, na.rm = TRUE) ; max.lon = max(x, na.rm = TRUE)
        y = df$DR.latitude
        y = c(y, yyy) ; min.lat = min(y, na.rm = TRUE) ; max.lat = max(y, na.rm = TRUE) #These values will be used in xlim and ylim limits
        if(Outgoing == TRUE) { plot(df$DR.longitude, df$DR.latitude, type = "l", col = "blue", main = "DR track", xlab = "Longitude", ylab = "Latitude", xlim = c(min.lon, max.lon), ylim = c(min.lat, max.lat)) }
        if(Outgoing == FALSE) { plot(df$DR.longitude, df$DR.latitude, type = "l", col = "blue", main = "DR track", xlab = "Longitude", ylab = "Latitude", xlim = c(min.lon, max.lon), ylim = c(min.lat, max.lat), sub = paste("(Reverse dead-reckoning)")) }
        lines(xxx, yyy, col = "green")
        legend("bottomleft", legend = c("DR (current drift)", "DR (no current drift)"),
               col = c("blue", "green"), lty = 1:1, cex = 0.65)
        points(lo, la, col = "black", cex = 1.5, pch = 19)
      
      #If current variables not supplied, plot dead-reckoned tracks - non-current integrated  
      } else { 
        if(Outgoing == TRUE) { plot(df$DR.longitude, df$DR.latitude, type = "l", col = "blue", main = "DR track", xlab = "Longitude", ylab = "Latitude") }
        if(Outgoing == FALSE) { plot(df$DR.longitude, df$DR.latitude, type = "l", col = "blue", main = "DR track", xlab = "Longitude", ylab = "Latitude", sub = paste("(Reverse dead-reckoning)")) }
        points(lo, la, col = "black", cex = 1.5, pch = 19)
        
        }
    }
    
    #Reorder columns according to if currents data / elevation data supplied
    if(is.null(cs) == TRUE | is.null(ch) == TRUE) { #If current not supplied
      
      if(max(abs(diff(df$Elevation))) > 0) { #Elevation/depth data supplied
        
        df = df[c("Row.number", "Timestamp", "DR.seconds", "Heading", "Marked.events", "DBA.or.speed", "Radial.distance", "Elevation", "Elevation.diff", "DR.longitude", 
                  "DR.latitude", "DR.distance.2D", "DR.distance.3D", "DR.cumulative.distance.2D", "DR.cumulative.distance.3D", "DR.straightline.distance.from.start.2D", 
                  "DR.straightline.distance.from.start.3D", "DR.speed.2D", "DR.speed.3D")]
      }
      
      if(max(abs(diff(df$Elevation))) == 0) { #Elevation/depth data not supplied
        
        df$Elevation = NULL  #Remove elevation/depth data if none supplied
        
        df = df[c("Row.number", "Timestamp", "DR.seconds", "Heading", "Marked.events", "DBA.or.speed", "Radial.distance", "DR.longitude", "DR.latitude", 
                  "DR.distance.2D", "DR.cumulative.distance.2D", "DR.straightline.distance.from.start.2D", "DR.speed.2D")]
      }
    }
    
    if(is.null(cs) == FALSE | is.null(ch) == FALSE) { #If current supplied
      
      if(max(abs(diff(df$Elevation))) > 0) { #Elevation/depth data supplied
        
        df = df[c("Row.number", "Timestamp", "DR.seconds", "Heading", "Marked.events", "DBA.or.speed", "Radial.distance", "Elevation", "Elevation.diff", "Current.speed", 
                  "Current.heading", "Heading.current.integrated", "Radial.distance.current.integrated", "DR.longitude", "DR.latitude", "DR.distance.2D",
                  "DR.distance.3D", "DR.cumulative.distance.2D", "DR.cumulative.distance.3D", "DR.straightline.distance.from.start.2D", 
                  "DR.straightline.distance.from.start.3D", "DR.speed.2D", "DR.speed.3D")]
      }
      
    if(max(abs(diff(df$Elevation))) == 0) { #Elevation/depth data not supplied
      
      df$Elevation = NULL #Remove elevation/depth data if none supplied
      
      df = df[c("Row.number", "Timestamp", "DR.seconds", "Heading", "Marked.events", "DBA.or.speed", "Radial.distance", "Current.speed", 
                "Current.heading", "Heading.current.integrated", "Radial.distance.current.integrated", "DR.longitude", "DR.latitude", 
                "DR.distance.2D", "DR.cumulative.distance.2D", "DR.straightline.distance.from.start.2D", "DR.speed.2D")]
    }
      
      if(Outgoing == FALSE) {
          df$Heading.current.integrated = df$Heading.current.integrated + 180 ; df$Heading.current.integrated = ifelse(df$Heading.current.integrated > 360, df$Heading.current.integrated - 360, df$Heading.current.integrated) #Revert the current integrated heading (pre-correction) (currently 180 degrees out)
        }
    }
    
    #If pitch supplied, incorporate (degree format) into data frame after 'DBA.or.speed' column
    if(is.null(p) == FALSE) {
      ref = which(names(df) == "DBA.or.speed")
      df = data.frame(df[1:ref], Pitch = (p * 180/pi), df[(ref+1):ncol(df)])  
    }
    
    #Remove vectors not required any more
    rm(la, lo, DR.lat, DR.lon, DR.loni, DR.lati, TS, TD, s, q, v, p, h, cs, ch, ME, elv, drift.q, drift.h, Row.number, h.rev, s.rev, ME.rev, v.rev, TD.rev, c.rev, m.rev, cs.rev, ch.rev, x, y, xx, yy, xxx, yyy, max.lon, max.lat, min.lon, min.lat)
    
    return(df)
    
  }
  
  ##############################################################################################################################################################################################################################################################################
  #7) VPC ENABLED (VP.lat & VP.lon inputs utilized) - VPs supplied --> Format data frame 
  ##############################################################################################################################################################################################################################################################################
    
  else{
    
    print(paste("Initial dead-reckoned tracks computed - VPC initialised - Stage 1 complete"))
    
    if(lo == 0 & la == 0) { print(paste("Warning, lo and la not supplied (0 = default)")) }
    
    VP.lat = as.numeric(VP.lat); VP.lon = as.numeric(VP.lon) #VP longitude and VP latitude vectors
    df$VP.longitude = VP.lon ; df$VP.latitude = VP.lat   #Include VP variables into the main data frame
    
    if(Outgoing == TRUE){
      df$VP.latitude[1] = la ; df$VP.longitude[1] = lo # If Outgoing = TRUE, Make sure first rows contains lo and la coordinates
      
    } else {
      df$VP.latitude[nrow(df)] = la ; df$VP.longitude[nrow(df)] = lo    #If Outgoing = FALSE, Make sure last rows contains lo and la coordinates
      df = df[dim(df)[1]:1,]                                              #If Outgoing = FALSE, Make sure data frame is in reversed order
      df$Row.number = rev(df$Row.number) }                                #If Outgoing = FALSE, row number remains in ascending order (for referencing purposes when merging data frames)
    
    df$VP.latitude = ifelse(df$VP.latitude == 0, NA, df$VP.latitude) #In case missing coordinates are filled as zeros, replace with NA's
    df$VP.longitude = ifelse(df$VP.longitude == 0, NA, df$VP.longitude) #In case missing coordinates are filled as zeros, replace with NA's
    
    #If VP.ME is TRUE, then VP that occurs at times of non-translocational behaviours are disregarded (Marked events must be 1)
    if(VP.ME == TRUE){
      df$VP.latitude = ifelse(df$Marked.events == 0 & df$Row.number != 1, NA, df$VP.latitude) #Ensure initial set lo/la coordinates are maintained
      df$VP.longitude = ifelse(df$Marked.events == 0 & df$Row.number != 1, NA, df$VP.longitude) #Ensure initial set lo/la coordinates are maintained
    }
    
    #'VP.ref.df' (Verified position reference data frame) becomes the reference data frame for VPC (rows of missing relocation data removed)
    VP.ref.df = df 
    VP.ref.df = VP.ref.df[!with(VP.ref.df, is.na(VP.longitude) | is.na(VP.latitude)) ,] #Remove rows if VP longitude / VP latitude contains NA's (missing relocation data)
    
    if(Outgoing == FALSE) { 
      VP.ref.df$VP.seconds = abs(c(difftime(VP.ref.df$Timestamp, lag(VP.ref.df$Timestamp), units = "secs")[-1], 0)) #Create differential of time between VP rows (for outgoing = FALSE)
      
    } else { VP.ref.df$VP.seconds = c(0, difftime(VP.ref.df$Timestamp, lag(VP.ref.df$Timestamp), units = "secs")[-1]) } #Create differential of time between VP rows (for outgoing = TRUE)
    
    VP.ref.df$VP.seconds = as.numeric(VP.ref.df$VP.seconds) #Ensure this column is numeric
    
    #Error estimate (m) of DR track (no VPC) in relation to VP track per unit time (when a VP is present)
    VP.ref.df$Distance.error.before.correction = disty(VP.ref.df$DR.longitude, VP.ref.df$DR.latitude, 0, VP.ref.df$VP.longitude, VP.ref.df$VP.latitude, 0, method = "Haversine") 
  
    ##############################################################################################################################################################################################################################################################################
    #8) #bound = TRUE --> shorten DR tracks to the bounds of first and last VP
        #bound = FALSE --> Use correction factors for whole track (even if VPs are only present for part of track, so track extends the length of your original vectors - Last available correction factors used for remaining track)
        #Merge relevant columns from above into main data frame
    ##############################################################################################################################################################################################################################################################################
    
    if(bound == TRUE) {
      
      #Shorten 'main' df to the last recorded VP
      x = min(VP.ref.df$Row.number) ; y = max(VP.ref.df$Row.number)
      df = df[c(x:y) ,]
      
    }
    
    #Merge the relevant columns of interest from sub-sampled outputs from VP.ref.df into the initial 'main' data frame (df) via the 'Row.number' column
    df = merge(df, VP.ref.df[, c('Row.number', 'VP.seconds', 'Distance.error.before.correction')], by.x = "Row.number", by.y = "Row.number", all = TRUE)  
    
    la = la * pi/180 #convert starting Lat coordinates to radians
    lo = lo * pi/180 #convert starting Lon coordinates to radians
    
    ##############################################################################################################################################################################################################################################################################
    #9) VPC preparation = Iterative corrections based on your method and thresh set; Prepare type and degree of VP under-sampling
    ##############################################################################################################################################################################################################################################################################
    
    #### method = "time" #######
    
    if(method == "time") {
      TD.VP = as.numeric(VP.ref.df$VP.seconds) #Vector of the time difference between VPs
      cum.t = rep(0, length(TD.VP)) #Vector that will be filled with the cumulative time (s) between VPs, which resets each time thresh is reached/surpassed
      t = rep(0, length(TD.VP)) #Vector that will be used as logical operator to specify if fix will be used in VPC dead-reckoning procedure
      keep = rep(0, length(TD.VP)) #Vector that will be filled with the cumulative number of VPs used in the VPC dead-reckoning procedure
      thresh = rep(thresh, length(TD.VP)) #Vector filled with the thresh value stated in function
      fix = rep(0, length(TD.VP)) #Vector that will be filled with The exact VP longitude and latitude coordinates used in the VPC dead-reckoning procedure (1 = Fix used to correct / 0 = Fix ignored)
      
      z = 0
      for (i in 2:length(TD.VP)) {  #Conditional time reset with grouping variable to specify which fix to keep based on thresh set
        if (cum.t[i-1] < thresh[i]) { 
          cum.t[i] = cum.t[i-1] + TD.VP[i] #Add the time between VPs to previous value if thresh value is not surpassed
          t[i] = t[i-1] +1 } 
        else if (cum.t[i] >= thresh[i]) { 
          cum.t[i] = 0 # Cumulative time reset to 0 if thresh is reached/surpassed
          t[i] = 0 }} 
      for (i in 2:length(TD.VP)) {
        if (t[i] - t[i-1] == 1) {  #Due to cumulative nature of 't', subtracting current row from previous will always result in 1, unless time resets (so provides index of when 'resets' occur)
          keep[i] = z
        } else if(t[i] - t[i-1] != 1) { #If time resets, then the unique number of VPs used in VPC procedure increments by 1
          z = z + 1
          keep[i] = z
          fix[i] = 1 #Binary column, each time there is a 1, this refers to the exact row-wise VP Longitude and Latitude coordinate to use in the VPC dead-reckoning procedure
          } 
        } 
      
      #Paste resulting vectors with suitable column names to data frame entitled 'VP.ref.df'
      VP.ref.df$Number.of.VPCs = keep 
      VP.ref.df$VP.used.to.correct = fix ; VP.ref.df$VP.used.to.correct[1] = 1 #Ensure first VP is used as the first VPC coordinates
      VP.ref.df$Number.of.VPCs[nrow(VP.ref.df)] = ifelse(VP.ref.df$VP.used.to.correct[nrow(VP.ref.df)] == 0,  (VP.ref.df$Number.of.VPCs[nrow(VP.ref.df)] + 1), VP.ref.df$Number.of.VPCs[nrow(VP.ref.df)]) #Ensure the number of VPCs is updated if last row originally was not going to be used in VPC operation (always ensure last row is included, regardless of thresh)
      VP.ref.df$VP.used.to.correct[nrow(VP.ref.df)] = 1 #Ensure last VP is used within the VPC operation
      VP.ref.df$VP.thresh = thresh #Replicate the thresh value set
    }
    
    #### method = "distance" #######          
    dist.step[is.na(dist.step)] = 1 ; dist.step = ifelse(dist.step < 1, 1, dist.step) # Ensure dist.step is 1 (or greater if specified)
    if(method == "distance") {
      #First calculate cumulative distance between VPs
      VP.longitude = VP.ref.df$VP.longitude ; VP.latitude = VP.ref.df$VP.latitude #Retrieve VP longitude and latitude coordinates as vectors from the data frame 'VP.ref.df'
      VP.loni = c(VP.longitude[-c(1:dist.step)], rep(NA, dist.step)) ; VP.lati = c(VP.longitude[-c(1:dist.step)], rep(NA, dist.step)) #Shift vector values forward  by the specified stepping range and add relevant number of NA's  to the end (to maintain vector length as original)
      VP.distance = disty(VP.longitude, VP.latitude, 0, VP.loni, VP.lati, 0)      #Calculate row wise distance between successive VP coordinates
      VP.distance = c(rep(NA, dist.step), VP.distance[c(1:(length(VP.distance)-dist.step))]) #Shift values back by by the specified stepping range
      VP.distance = ifelse(is.na(VP.distance == TRUE), 0, VP.distance)  #Replace NA's with 0's
      VP.distance = cumsum(VP.distance) #Calculate cumulative distance between VPs
      
      fix = rep(0, length(VP.distance)) #Vector that will be filled with The exact VP longitude and latitude coordinates used in the VPC dead-reckoning procedure (1 = Fix used to correct / 0 = Fix ignored)
      keep = rep(0, length(VP.distance)) #Vector that will be filled with the cumulative number of VPs used in the VPC dead-reckoning procedure
      t = rep(0, length(VP.distance)) #Vector that will be used as logical operator to specify if fix will be used in VPC dead-reckoning procedure
      output = round(max(VP.distance) / thresh[1]) + 1 #Calculate what the thresh specifies in terms of distance (+1 ensures compulsory last coordinates are not too close to that given from this loop)
      z = output
      x = 0
      for (i in 2:length(VP.distance)) {
        if (VP.distance[i-1] < output) { 
          t[i] = t[i-1] + 1}
        else if (VP.distance[i-1] >= output) { 
          t[i] = 0
          output = output + z }}
      for (i in 2:length(VP.distance)) {
        if (t[i] - t[i-1] == 1){  #Due to cumulative nature of 't', subtracting current row from previous will always result in 1, unless time resets (so provides index of when 'resets' occur)
          keep[i] = x
        } else if(t[i] - t[i-1] != 1) { #If distance resets, then the unique number of VPs used in DR correcting procedure increments by 1
          x = x + 1
          keep[i] = x
          fix[i] = 1 #Binary column, each time there is a 1, this refers to the exact row-wise VP Longitude and Latitude coordinate to use in the VPC dead-reckoning procedure
          }
        } 
      
      #Paste resulting vectors with suitable column names to data frame entitled 'VP.ref.df'
      VP.ref.df$Number.of.VPCs = keep 
      VP.ref.df$VP.used.to.correct = fix ; VP.ref.df$VP.used.to.correct[1] = 1 #Ensure first VP is used as the first VPC coordinates
      VP.ref.df$Number.of.VPCs[nrow(VP.ref.df)] = ifelse(VP.ref.df$VP.used.to.correct[nrow(VP.ref.df)] == 0,  (VP.ref.df$Number.of.VPCs[nrow(VP.ref.df)] + 1), VP.ref.df$Number.of.VPCs[nrow(VP.ref.df)]) #Ensure the number of VPCs is updated if last row originally was not going to be used in VPC operation (always ensure last row is included, regardless of thresh)
      VP.ref.df$VP.used.to.correct[nrow(VP.ref.df)] = 1 #Ensure last VP is used within the VPC operation
      VP.ref.df$VP.thresh = thresh #Replicate thresh value
      }
    
    #### method = "divide" #######
    
    if(method == "divide") {
      divide = round(nrow(VP.ref.df)/thresh[1]) + 1 #(+1 ensures compulsory last coordinates are not too close to that given from this section of code)
      output = slice(VP.ref.df, c(seq(1, nrow(VP.ref.df), divide), nrow(VP.ref.df))) #Slice between first and last coordinates at the value of 'divide'
      output = output[, 1] #Row number column
      VP.ref.df$Number.of.VPCs = rep(0, nrow(VP.ref.df)) #column that will be filled with the cumulative number of VPs used in the VPC dead-reckoning procedure
      VP.ref.df$VP.used.to.correct = as.integer(sub("\\.$", "", VP.ref.df$Row.number) %in% output) #Paste 1 if row number of output (fixes to be used in correction) matches row number of the data frame 'VP.ref.df'
      VP.ref.df$VP.thresh = thresh #Replicate thresh in a column
      VP.ref.df$Number.of.VPCs = cumsum(VP.ref.df$VP.used.to.correct) #Accumulate the number of VPs used 
      VP.ref.df$Number.of.VPCs = VP.ref.df$Number.of.VPCs - 1 #Ensure first row is not regarded as the first correction (so minus one from value)
    }
    
    #### method = "all" #######
    
    if(method == "all") {     
      VP.ref.df$Number.of.VPCs = c(0, 1:nrow(VP.ref.df[-1,])) #Cumulative number of VPs used in the VPC dead-reckoning procedure
      VP.ref.df$VP.used.to.correct = rep(1, nrow(VP.ref.df))           #Use every VP within the VPC dead-reckoning operation
      VP.ref.df$VP.thresh = rep("Every VP used", nrow(VP.ref.df))     #Column specifying every VP was used in the VPC dead-reckoning procedure
    }
    
    ##############################################################################################################################################################################################################################################################################
    #10) VPC - Round 1 - Prepare correction factors based on kept VPs and re-sample VPs where necessary to remove Inf values
    ##############################################################################################################################################################################################################################################################################
    
    print(paste("VP formatted according to user defined correction - Calculating correction factors and checking for Inf values - Stage 2 complete"))
    
    ref.df = df #Ensure original 'main' data frame is saved as a separate reference data frame (ref.df) --> This becomes default data frame after each round of correction
    #Merge the relevant columns of interest from sub-sampled outputs from VP.ref.df into the initial 'main' data frame (df) via the 'Row.number' column
    df = merge(df, VP.ref.df[, c('Row.number', 'Number.of.VPCs', 'VP.used.to.correct', 'VP.thresh')], by.x = "Row.number", by.y = "Row.number", all = TRUE) #Merge the sub sampled outputs from temp.df, with the initial 'main' data frame (df) and via the 'Row.number' column
    temp.df = df #Create a temporary data frame from the 'main' df  
    temp.df = subset(temp.df, temp.df$VP.used.to.correct == 1) #Sub-sample temp.df to only include rows used in the VPC procedure (at frequency of thresh - dependent on under sampling method used)
    temp.df$DR.loni = c(temp.df[-1, 'DR.longitude'], NA) ; temp.df$DR.lati = c(temp.df[-1, 'DR.latitude'], NA)  #Shift DR.longitude and DR.latitude rows back by one (ready for disty function)
    temp.df$VP.loni = c(temp.df[-1, 'VP.longitude'], NA) ; temp.df$VP.lati = c(temp.df[-1, 'VP.latitude'], NA) #Shift VP.longitude and VP.latitude rows back by one (ready for disty funtion)
    
    #2D distance between successive VP coordinates and DR coordinates for distance correction ratio
    temp.df$DR.distance = disty(temp.df$DR.longitude, temp.df$DR.latitude, 0, temp.df$DR.loni, temp.df$DR.lati, 0) #distance between successive DR coordinates (default = Haversine method (elevation ignored))
    temp.df$VP.distance = disty(temp.df$VP.longitude, temp.df$VP.latitude, 0, temp.df$VP.loni, temp.df$VP.lati, 0) #distance between successive VP coordinates (default = Haversine method (elevation ignored))
    
    ###############The Inf value check and subsequent re-sampling regime where required###############
    ##################################################################################################################################################################
    #If VP distance > 0 m and dead-reckoned distance = 0 m between fixes, correction factor of distance becomes Inf!
    #This is likely due to VPC too frequently and VP 'positional noise' (or 'jitter') portrays movement when the animal is not. Or due to wrongly assigned speed values (incl. ME)
    #This while loop will ensure only iterations of correction where a valuable correction factor is obtained are used
    ##################################################################################################################################################################
    
    temp.df$check = ifelse(temp.df$DR.distance == 0, 1, 0) #If value = 1, then do not use for correction as Inf value likely
    temp.df$check = c(0, temp.df$check[-nrow(temp.df)]) #Ensure first VP kept
    
    if(max(temp.df$check, na.rm = TRUE) > 0) {
      print(paste("Inf values produced (distance correction ratio). Function is recalulating the degree of VP undersampling to ensure this is not the case. This may alter the method and thresh set"))
      while(max(temp.df$check, na.rm = TRUE) > 0) { #This loop continues until the condition is met (likely not needed to be a while statement as only one iteration should be needed)
     print(paste("re-scaling VPC segments"))
     temp.df = subset(temp.df, temp.df$check == 0) #Remove rows where Inf values occurred (DR distance = 0 m)
     #Recalculate DR and VP distance from subset rows
     temp.df$DR.loni = c(temp.df[-1, 'DR.longitude'], NA) ; temp.df$DR.lati = c(temp.df[-1, 'DR.latitude'], NA)  #Shift DR.longitude and DR.latitude rows back by one (ready for disty method)
     temp.df$VP.loni = c(temp.df[-1, 'VP.longitude'], NA) ; temp.df$VP.lati = c(temp.df[-1, 'VP.latitude'], NA)  #Shift VP.longitude and VP.latitude rows back by one (ready for disty method)
     temp.df$DR.distance = disty(temp.df$DR.longitude, temp.df$DR.latitude, 0, temp.df$DR.loni, temp.df$DR.lati, 0) #distance between successive DR coordinates (default = Haversine method (elevation ignored))
     temp.df$VP.distance = disty(temp.df$VP.longitude, temp.df$VP.latitude, 0, temp.df$VP.loni, temp.df$VP.lati, 0) #distance between successive VP coordinates (default = Haversine method (elevation ignored))
     temp.df$check = ifelse(temp.df$DR.distance == 0, 1, 0)  #If value = 1, then do not use for correction as Inf value likely
     temp.df$check = c(0, temp.df$check[-nrow(temp.df)])  #Ensure first fix is kept
     } #While loop complete
    
    #Correct relevant variables following any required re-scaling  
    temp.df$Number.of.VPCs = c(0, 1:nrow(temp.df[-1,])) #Re-scale the number of VPCs following this procedure
    temp.df$VP.thresh = rep("Warning, Inf values occured. Scale of VPC altered", nrow(temp.df)) #User is informed the thresh set may no longer be valid
    df = ref.df #Replace the 'main' df with the default reference (ref.df) version prior to the original VP columns specifying what fixes to use in the VPC process (since this has now altered) 
    df = merge(df, temp.df[, c('Row.number', 'Number.of.VPCs', 'VP.used.to.correct', 'VP.thresh')], by.x = "Row.number", by.y = "Row.number", all = TRUE) #Merge initial 'main' data frame (df) with the various new outputs within temp.df, specifying when to VP correct - following removal of Inf values. Merge via the 'Row.number' column
    df$Number.of.VPCs = zoo::na.locf(df$Number.of.VPCs) #Replace each NA with the most recent non-NA (observations carried forwards) 

    print(paste("Iterations of VPC rate successfully rescaled - VPC dead-reckoning procedure next - Stage 3 complete"))
    
    } else { print(paste("No Inf values - No alterations to the VPC rate required  - VPC dead-reckoning procedure next - Stage 3 complete")) } #No Inf values occurred
    
    #Correction ratio of distance to be applied to original DR distance values 
    temp.df$Dist.corr.factor = ifelse(temp.df$VP.distance == 0 & temp.df$DR.distance == 0, 0, temp.df$VP.distance / temp.df$DR.distance) #Ensure 'NaN' values are not produced (occurs when both successive dead-reckoned and VP distances are 0 m)
    
    ########################################End of the Inf value check###########################################
    
    #Calculate bearing between successive dead-reckoned coordinates and VP coordinates for heading correction ratio
    temp.df$DR.head = beary(temp.df$DR.longitude, temp.df$DR.latitude, temp.df$DR.loni, temp.df$DR.lati)  
    temp.df$VP.head = beary(temp.df$VP.longitude, temp.df$VP.latitude, temp.df$VP.loni, temp.df$VP.lati)
      
    #The below logical expressions are implemented for final interpretation of heading correction within summary outputs (difference between dead-reckoned and VP bearing never exceeds 180 degrees)
    temp.df$DR.head = ifelse(temp.df$DR.head < 0, temp.df$DR.head + 360, temp.df$DR.head) #Because above formula outputs within the scale of -180 to +180 degrees --> This ensures output is 0 to 360 degrees
    temp.df$VP.head = ifelse(temp.df$VP.head < 0, temp.df$VP.head + 360, temp.df$VP.head) #Because above formula outputs within the scale of -180 to +180 degrees --> This ensures output is 0 to 360 degrees
    temp.df$Head.corr.factor = temp.df$VP.head - temp.df$DR.head #Heading correction to apply to original heading values
    temp.df$Head.corr.factor = ifelse(temp.df$Head.corr.factor < -180, (temp.df$Head.corr.factor + 360), temp.df$Head.corr.factor) #Ensure difference does not exceed 180 degrees in either circular direction
    temp.df$Head.corr.factor = ifelse(temp.df$Head.corr.factor > 180, (temp.df$Head.corr.factor - 360), temp.df$Head.corr.factor) #Ensure difference does not exceed 180 degrees in either circular direction
   
    df = merge(df, temp.df[, c('Row.number', 'Dist.corr.factor', 'Head.corr.factor')], by.x = "Row.number", by.y = "Row.number", all = TRUE) #Merge the 'main' df with the calculated correction factors from the temp.df, via the Row.number column
    temp.df =  temp.df[, c('Row.number', 'Number.of.VPCs', 'VP.used.to.correct', 'VP.thresh')] #Subset to only include relevant columns, which becomes apparent as the reference for VPs used in DR correcting procedure when further Inf checks may be required (because more than one round of VPC adjustment may be necessary for dead-reckoning fixes to accord exactly with ground-truthed locations)
    
    df$Dist.corr.factor = zoo::na.locf(df$Dist.corr.factor) #Replace each NA with the most recent non-NA (observations carried forwards) 
    df$Head.corr.factor = zoo::na.locf(df$Head.corr.factor) #Replace each NA with the most recent non-NA (observations carried forwards)
    df$VP.used.to.correct = ifelse(is.na(df$VP.used.to.correct) == TRUE, 0, df$VP.used.to.correct)  #Replace NA's with zero
    df$VP.thresh = zoo::na.locf(df$VP.thresh) #Replicate thresh value with no NA's
    df$Number.of.VPCs = zoo::na.locf(df$Number.of.VPCs) #Replace each NA with the most recent non-NA (observations carried forwards)
    
    #The way in which correction factors were calculated (and merged back to 'main' df) means that the 'next' correction factor is always one row too early, so need to shift correction factors back by one row
    df$Dist.corr.factor = c(NA, df$Dist.corr.factor[-nrow(df)])
    df$Head.corr.factor = c(NA, df$Head.corr.factor[-nrow(df)]) 

    ##############################################################################################################################################################################################################################################################################
    #11) VPC - Round 2 - The first round of correction
    ##############################################################################################################################################################################################################################################################################
    
    #VPC original DR path
    #Vectors used in DR procedure
    #Also if Outgoing = FALSE, ensure vectors are in reverse format
    TS = df$Timestamp 
    DR.lon = rep(NA, length(TS)) ; DR.lat = rep(NA, length(TS))
    DR.lat[1] = la  #Set 1st row of DR latitude ready for DR procedure
    DR.lon[1] = lo  #Set 1st row of DR longitude ready for DR procedure
    Row.number = df$Row.number
    fixy = df$VP.used.to.correct
    VP.lon = as.numeric(df$VP.longitude)
    VP.lat = as.numeric(df$VP.latitude)
    
    #Correct radial distance values at every correction iteration (multiply original row-wise distance by correction factor)
    if(is.null(cs) == FALSE | is.null(ch) == FALSE) { corr.q = (df$Radial.distance.current.integrated * df$Dist.corr.factor) } #If currents supplied, ensure current integrated radial distance used
    else { corr.q = (df$q * df$Dist.corr.factor) } #If no currents supplied, original radial distance adjusted (note 'q' also refers to the reverse dead-reckoned 'q.rev' variable)
    
    #Correct heading values at every correction iteration (add the difference of heading between VP and DR bearing to each original DR heading value)
    if(is.null(cs) == FALSE | is.null(ch) == FALSE) { corr.h = (df$Heading.current.integrated + df$Head.corr.factor) } #If currents supplied, ensure current integrated heading used
    else { corr.h = (df$h + df$Head.corr.factor) } #If no currents supplied, original heading adjusted (note 'h' also refers to the reverse dead-reckoned 'h.rev' variable)
    corr.h = ifelse(corr.h > 360, corr.h - 360, corr.h) #Ensure circular range is between 0 and 360 degrees
    corr.h = ifelse(corr.h < 0, corr.h + 360, corr.h) #Ensure circular range is between 0 and 360 degrees
    corr.h = corr.h * pi/180 #Convert corrected heading to radians
      
    for(i in 2:length(DR.lat)) {
      DR.lat[i] = asin(sin(DR.lat[i-1]) * cos(corr.q[i]) + cos(DR.lat[i-1]) * sin(corr.q[i]) * cos(corr.h[i])) #2 equations for dead-reckoning of latitude and longitude coordinates using corrected distance and heading
      DR.lon[i] = DR.lon[i-1] + atan2(sin(corr.h[i]) * sin(corr.q[i]) * cos(DR.lat[i-1]), cos(corr.q[i]) - sin(DR.lat[i-1]) * sin(DR.lat[i])) 
    }
    
    DR.lat = 180 * DR.lat/pi #Convert back to grid coordinates
    DR.lon = 180 * DR.lon/pi #Convert back to grid coordinates
    corr.h = 180 * corr.h/pi #Convert back to degrees
    
    ##############################################################################################################################################################################################################################################################################
    #12) VPC - Round 3 - The correction loops - while VP and dead-reckoned tracks do not exactly accord (default = within 0.01 m at correction points), iterations of the VPC procedure repeat - This may manifest in further VPC re-scaling to avoid Inf values
    ##############################################################################################################################################################################################################################################################################
    
    #Slight discrepancies in the application of correction factors (e.g. due to numerical precision) can accumulate, hence we repeat correction procedure in a while loop until both tracks accord to a great level of accuracy
    print(paste("First round of VPC complete - Checking adherence between aligned dead-reckoned coordinates and VP cooridnates, per ground-truthed fix - Stage 4 complete"))
  
    #Calculate distance between corrected dead-reckoned track and VP fixes used within the VPC procedure
    z = data.frame(cbind(DR.lon, DR.lat, VP.lon, VP.lat, fixy)) #Bind relevant vectors into data frame termed 'z'
    z = subset(z, z$fixy == 1) #Only include rows where VPs were used in the VPC procedure
    cond = disty(z$DR.lon, z$DR.lat, 0, z$VP.lon, z$VP.lat, 0) #cond = condition; the conditional distance between corrected DR coordinates and VP coordinates (thresh <= 0.01 m)
   
    #Save the original distance and heading correction factors - These will be updated based on subsequent corrections
    prop.dist.corr = df$Dist.corr.factor 
    prop.head.corr = df$Head.corr.factor

    ##############################################################################################################################################################################
    ##############################################################################################################################################################################
    #The while() loop continues until maximum 'cond' (distance m) is <= 0.01
    #User receives updates of the 'cond' value while the while loop operates through R console
    ##############################################################################################################################################################################
    while(max(cond, na.rm = TRUE) > 0.01) { #User can change this value (note 0 m may not be possible)
    ############################################################################################################################################################################## 
    ##############################################################################################################################################################################
      
      print(paste("Maximum distance (m) between DR postions and ground-truthed positions (used within the VPC procedure) =", round(max(cond, na.rm = TRUE), 5), "- Correction iteration continues (threshold = 0.01 m)"))

      df = ref.df #Replace the 'main' df with the default reference (ref.df - prior to calculating correction factors)
      df$VP.longitude = VP.lon ; df$VP.latitude = VP.lat #Add VP longitude and latitude coordinates to the 'main' df
      df = merge(df, temp.df, by.x = "Row.number", by.y = "Row.number", all = TRUE) #Merge 'main' df and temp.df (temp.df contains positions of VP's used to correct), via the 'Row.number' column
      temp.df.2 = data.frame(Row.number, DR.lon, DR.lat, corr.q, corr.h)  #Save required variables (incl. correction factors) within another temp.df termed 'temp.df.2'
      colnames(temp.df.2) = c("Row.number" , "DR.longitude.corr", "DR.latitude.corr", "corr.q", "corr.h")   #Add appropriate column names to temp.df.2
      df = merge(df, temp.df.2, by.x = "Row.number", by.y="Row.number", all = TRUE) #Merge 'main' df and temp.df.2 (temp.df.2 contains the correction factors), via the 'Row.number' column
      temp.df = df #Reassign temp.df to now have the variables within the main 'df'
      temp.df = subset(temp.df, temp.df$VP.used.to.correct == 1) #Sub sample temp.df to only include rows used in within the VPC procedure (at the frequency of thresh (dependent on method of under-sampling chosen)
      temp.df$DR.loni = c(temp.df[-1, 'DR.longitude.corr'], NA) ; temp.df$DR.lati = c(temp.df[-1, 'DR.latitude.corr'], NA)  #Shift corrected DR.lon and DR.lat rows back by one (ready for disty method)
      temp.df$VP.loni = c(temp.df[-1, 'VP.longitude'], NA) ; temp.df$VP.lati = c(temp.df[-1, 'VP.latitude'], NA) #Shift VP.lon and VP.lat rows back by one (ready for disty method)
      
      #2D distance between successive VP coordinates and dead-reckoned coordinates for distance correction ratio
      temp.df$DR.distance = disty(temp.df$DR.longitude.corr, temp.df$DR.latitude.corr, 0, temp.df$DR.loni, temp.df$DR.lati, 0) #Distance between successive (corrected) dead-reckoned fixes (default = Haversine method (elevation ignored))
      temp.df$VP.distance = disty(temp.df$VP.longitude, temp.df$VP.latitude, 0, temp.df$VP.loni, temp.df$VP.lati, 0) #Distance between successive VP fixes (default = Haversine method (elevation ignored))
      
      ###############The looping Inf value check and subsequent re-sampling regime where required - Can occur if more than one rounds of corrections are performed ###############
      ############################################################################################################################################################################
      #If VP distance > 0 m and dead-reckoned distance = 0 m between fixes, correction factor of distance becomes Inf!
      
      temp.df$check = ifelse(temp.df$DR.distance == 0, 1, 0) #If value = 1, then do not use for correction as Inf value likely
      temp.df$check = c(0, temp.df$check[-nrow(temp.df)]) #Ensure first fix kept
      
      if(max(temp.df$check, na.rm = TRUE) > 0) {
        print(paste("Inf values produced following further iterations of the VPC procedure. Function is recalulating the degree of VPC undersampling"))
        while(max(temp.df$check, na.rm = TRUE) > 0) { #This loop continues until the condition is met... again likely only one iteration required...
          print(paste("re-scaling VPC segments"))
          temp.df = subset(temp.df, temp.df$check == 0) #Remove rows where Inf values occurred (DR distance = 0 m and VP distance > 0 m)
          #Recalculate DR and VP distance from subset rows
          temp.df$DR.loni = c(temp.df[-1, 'DR.longitude.corr'], NA) ; temp.df$DR.lati = c(temp.df[-1, 'DR.latitude.corr'], NA)  #Shift DR.longitude.corr and DR.latitude.corr rows back by one (ready for disty method)
          temp.df$VP.loni = c(temp.df[-1, 'VP.longitude'], NA) ; temp.df$VP.lati = c(temp.df[-1, 'VP.latitude'], NA)  #Shift VP.longitude and VP.latitude rows back by one (ready for disty method)
          temp.df$DR.distance = disty(temp.df$DR.longitude.corr, temp.df$DR.latitude.corr, 0, temp.df$DR.loni, temp.df$DR.lati, 0) #distance between successive (corrected) dead-reckoned fixes (default = Haversine method (elevation ignored))
          temp.df$VP.distance = disty(temp.df$VP.longitude, temp.df$VP.latitude, 0, temp.df$VP.loni, temp.df$VP.lati, 0) #distance between successive VPs (default = Haversine method (elevation ignored))
          temp.df$check = ifelse(temp.df$DR.distance == 0, 1, 0)  #If value = 1, then do not use for correction as Inf value likely
          temp.df$check = c(0, temp.df$check[-nrow(temp.df)]) #Ensure first fix kept
          
        } #While loop complete
        
        #Correct relevant variables following any required re-scaling  
        temp.df$Number.of.VPCs = c(0, 1:nrow(temp.df[-1,])) #Re-scale the number of VPCs following this procedure
        temp.df$VP.thresh = rep("Warning, Inf values occured. Scale of VPC altered", nrow(temp.df)) #User is informed the thresh set may no longer be valid
       
        
        temp.df.2 = data.frame(Row.number, DR.lon, DR.lat, corr.q, corr.h)  #Save required variables (incl. correction factors) within another temp.df termed temp.df.2
        colnames(temp.df.2) = c("Row.number" , "DR.longitude.corr", "DR.latitude.corr", "corr.q", "corr.h") #Add appropriate column names to data frame temp.df.2
        df = ref.df #Replace the 'main' df with the default reference (ref.df - prior to calculating correction factors)
        df$VP.longitude = VP.lon ; df$VP.latitude = VP.lat #Add VP longitude and latitude coordinates to this data frame
        df = merge(df, temp.df[, c('Row.number', 'Number.of.VPCs', 'VP.used.to.correct', 'VP.thresh')], by.x = "Row.number", by.y = "Row.number", all = TRUE) #Merge 'main' df and relevant variables of temp.df (positions of VP's used to correct), via the Row number
        df = merge(df, temp.df.2, by.x = "Row.number", by.y="Row.number", all = TRUE) #Merge 'main' df and temp.df.2 (temp.df.2 contains the correction factors), via the 'Row.number' column
        df$Number.of.VPCs = zoo::na.locf(df$Number.of.VPCs) #Replace each NA with the most recent non-NA (observations carried forwards) 
        df$VP.thresh = zoo::na.locf(df$VP.thresh) #Replace each NA with the most recent non-NA (observations carried forwards) 
        
        print(paste("Iterations of VPC rate successfully rescaled - VPC procedure continues"))
        
      } else { print(paste("No Inf values produced following further iteration(s) of VPC - the VPC procedure continues")) }
      
      #Correction ratio of distance to be applied to original DR distance values 
      temp.df$Dist.corr.factor = ifelse(temp.df$VP.distance == 0 & temp.df$DR.distance == 0, 0, temp.df$VP.distance / temp.df$DR.distance) #Ensure 'NaN' values are not produced (occurs when both successive dead-reckoned and VP distances are 0 m)
      
      ########################################End of the Inf value check###########################################
      
      #Calculate bearing between successive (corrected) dead-reckoned coordinates and VP coordinates for heading correction ratio
      temp.df$DR.head = beary(temp.df$DR.longitude.corr, temp.df$DR.latitude.corr, temp.df$DR.loni, temp.df$DR.lati)  
      temp.df$VP.head = beary(temp.df$VP.longitude, temp.df$VP.latitude, temp.df$VP.loni, temp.df$VP.lati)
      
      #The below logical expressions are implemented for final interpretation of heading correction within summary outputs (difference between dead-reckoned and VP bearing never exceeds 180 degrees)
      temp.df$DR.head = ifelse(temp.df$DR.head < 0, temp.df$DR.head + 360, temp.df$DR.head) #Because above formula outputs within the scale of -180 to +180 degrees --> This ensures output is 0 to 360 degrees
      temp.df$VP.head = ifelse(temp.df$VP.head < 0, temp.df$VP.head + 360, temp.df$VP.head) #Because above formula outputs within the scale of -180 to +180 degrees --> This ensures output is 0 to 360 degrees
      temp.df$Head.corr.factor = temp.df$VP.head - temp.df$DR.head #Heading correction to apply to original heading values
      temp.df$Head.corr.factor = ifelse(temp.df$Head.corr.factor < -180, (temp.df$Head.corr.factor + 360), temp.df$Head.corr.factor) #Ensure difference does not exceed 180 degrees in either circular direction
      temp.df$Head.corr.factor = ifelse(temp.df$Head.corr.factor > 180, (temp.df$Head.corr.factor - 360), temp.df$Head.corr.factor) #Ensure difference does not exceed 180 degrees in either circular direction
      
      df = merge(df, temp.df[, c('Row.number', 'Dist.corr.factor', 'Head.corr.factor')], by.x="Row.number", by.y="Row.number", all = TRUE) #Merge the 'main' df with the corrected correction factors from the temp.df, via the Row.number column
      temp.df = temp.df[, c('Row.number', 'Number.of.VPCs', 'VP.used.to.correct', 'VP.thresh')] #Subset relevant columns from temp.df as the reference for VPs used in DR correcting procedure
      df$Dist.corr.factor = zoo::na.locf(df$Dist.corr.factor) #Replace each NA with the most recent non-NA (observations carried forwards) 
      df$Head.corr.factor = zoo::na.locf(df$Head.corr.factor) #Replace each NA with the most recent non-NA (observations carried forwards)
      df$VP.used.to.correct = ifelse(is.na(df$VP.used.to.correct) == TRUE, 0, df$VP.used.to.correct)  #Replace NA's with zero
      df$VP.thresh = zoo::na.locf(df$VP.thresh) #Replicate thresh value with no NA's
      df$Number.of.VPCs = zoo::na.locf(df$Number.of.VPCs) #Replace each NA with the most recent non-NA (observations carried forwards)
     
      #The way in which correction factors were calculated (and merged back to 'main' df) means that the 'next' correction factor is always one row too early, so need to shift correction factors back by one row
      df$Dist.corr.factor = c(NA, df$Dist.corr.factor[-nrow(df)])
      df$Head.corr.factor = c(NA, df$Head.corr.factor[-nrow(df)]) 
      
      #Update previous correction factors
      prop.head.corr = prop.head.corr + df$Head.corr.factor #Add any difference between correction factors
      prop.head.corr = ifelse(prop.head.corr < -180, prop.head.corr + 360, prop.head.corr) #Ensure change does not exceed -180
      prop.head.corr = ifelse(prop.head.corr > 180, prop.head.corr - 360, prop.head.corr) #Ensure change does not exceed +180
      prop.dist.corr =  prop.dist.corr * df$Dist.corr.factor #Update distance correction factor by multiplying by the difference between 'previous' and 'updated'
   
      ####################################Now repeat correction (on already corrected dead-reckoned coefficients) with updated correction factors#################################
      TS = df$Timestamp
      DR.lon = rep(NA, length(TS)) ; DR.lat = rep(NA, length(TS))
      DR.lat[1] = la  #Set 1st row of DR latitude ready for DR procedure
      DR.lon[1] = lo  #Set 1st row of DR longitude ready for DR procedure
      Row.number = df$Row.number
      fixy = df$VP.used.to.correct
      VP.lon = as.numeric(df$VP.longitude) 
      VP.lat = as.numeric(df$VP.latitude) 
  
      corr.q = (df$corr.q * df$Dist.corr.factor) #Correct distance values at every correction iteration
      corr.h = (df$corr.h + df$Head.corr.factor) #Correct heading values at every correction iteration
      corr.h = ifelse(corr.h > 360, corr.h - 360, corr.h) #Ensure circular range is between 0 and 360 degrees
      corr.h = ifelse(corr.h < 0, corr.h + 360, corr.h) #Ensure circular range is between 0 and 360 degrees
      corr.h = corr.h * pi/180 #Convert corrected heading to radians
      
      for(i in 2:length(DR.lat)) {
          DR.lat[i] = asin(sin(DR.lat[i-1]) * cos(corr.q[i]) + cos(DR.lat[i-1]) * sin(corr.q[i]) * cos(corr.h[i])) #2 equations for dead-reckoning of latitude and longitude coordinates using corrected distance and heading
          DR.lon[i] = DR.lon[i-1] + atan2(sin(corr.h[i]) * sin(corr.q[i]) * cos(DR.lat[i-1]), cos(corr.q[i]) - sin(DR.lat[i-1]) * sin(DR.lat[i])) 
      }
      
      DR.lat = 180 * DR.lat/pi #Convert back to grid coordinates
      DR.lon = 180 * DR.lon/pi #Convert back to grid coordinates
      corr.h = 180 * corr.h/pi #Convert back to degrees
  
      #Calculate current distance between the 'updated' iteration of DR corrected track and VP fixes used within the VPC procedure
      z = data.frame(cbind(DR.lon, DR.lat, VP.lon, VP.lat, fixy))
      z = subset(z, z$fixy == 1) #Only include rows where VPs were used within the VPC procedure (wherby correction factors update)
      cond = disty(z$DR.lon, z$DR.lat, 0, z$VP.lon, z$VP.lat, 0)  #cond = condition; the conditional distance between corrected DR coordinates and VP coordinates (thresh <= 0.01 m)

    #####################################################################The end of the correction loops #######################################################################
    } #If condition is met; While loop complete... - message given to user to inform iterative VPC procedure is complete)
    print(paste("Maximum distance (m) between DR postions and ground-truthed positions (used within the VPC procedure) =", round(max(cond, na.rm = TRUE), 5), 
                "- Final summaries being computed (threshold has been met (0.01 m))"))
    ############################################################################################################################################################################
    
    la = 180 * la/pi     #Convert back to grid coordinates
    lo = 180 * lo/pi     #Convert back to grid coordinates
    
    #Note when Outgoing = FALSE, radial.distance radial.distance.corr and heading.corr will be one row displaced from true (analogous to problem above) but comparable row-wise with each other
    #Reassign previous 'df' columns with 'newly' corrected values
    df$DR.longitude.corr = DR.lon
    df$DR.latitude.corr = DR.lat
    df$Heading.corr = corr.h
    df$Radial.distance.corr = corr.q

    #Error estimate of DR track in relation to all VPs
    df$Distance.error.after.correction = disty(df$DR.longitude.corr, df$DR.latitude.corr, 0, df$VP.longitude, df$VP.latitude, 0) 
    
    #If currents supplied, save the error estimate of DR track (no currents integrated (xxx, yyy)) in relation to all VPs (DE.NC)
    if(is.null(cs) == FALSE | is.null(ch) == FALSE) { #If currents supplied...
      DE.NC = disty(xxx, yyy, 0, df$VP.longitude, df$VP.latitude, 0) } 
    
    #Include correction factors to main df
    df$Dist.corr.factor = prop.dist.corr
    df$Head.corr.factor = prop.head.corr
    
    ##############################################################################################################################################################################################################################################################################
    #13) VPC procedure finished - Final dead-reckoned and VP summaries being computed
    ##############################################################################################################################################################################################################################################################################
    
    print(paste("VPC dead-reckoning finished - Final dead-reckoned and VP summaries being computed - Stage 5 complete"))
    
    df$VP.present = ifelse(is.na(df$VP.longitude | df$VP.latitude) == TRUE, 0, 1) #1 = VP present, 0 = VP not present
    
    if(Outgoing == FALSE) {
      df = df[dim(df)[1]:1,] #Final reversion of data frame if Outgoing = FALSE (back to original)
      df$Row.number = rev(df$Row.number)
      df$VP.longitude = zoo::na.locf(df$VP.longitude, fromLast = TRUE) #Replace each NA with the most recent non-NA (observations carried backwards)
      df$VP.latitude = zoo::na.locf(df$VP.latitude, fromLast = TRUE) #Replace each NA with the most recent non-NA (observations carried backwards)
      df$Distance.error.before.correction = zoo::na.locf(df$Distance.error.before.correction, fromLast = TRUE) #Replace each NA with the most recent non-NA (observations carried backwards)
      df$Distance.error.after.correction = zoo::na.locf(df$Distance.error.after.correction, fromLast = TRUE) #Replace each NA with the most recent non-NA (observations carried backwards)
      df$Heading.corr =  df$Heading.corr + 180 ; df$Heading.corr = ifelse(df$Heading.corr > 360, df$Heading.corr - 360, df$Heading.corr) #Revert the corrected heading (currently 180 degrees out)
      if(is.null(cs) == FALSE | is.null(ch) == FALSE) { #If currents supplied....
      df$Heading.current.integrated = df$Heading.current.integrated + 180 ; df$Heading.current.integrated = ifelse(df$Heading.current.integrated > 360, df$Heading.current.integrated - 360, df$Heading.current.integrated) #Revert the current integrated heading (pre-correction) (currently 180 degrees out)
      }
    }
    
    if(Outgoing == TRUE) {
      df$VP.longitude = zoo::na.locf(df$VP.longitude) #Replace each NA with the most recent non-NA (observations carried forwards) 
      df$VP.latitude = zoo::na.locf(df$VP.latitude) #Replace each NA with the most recent non-NA (observations carried forwards)
      df$Distance.error.before.correction = zoo::na.locf(df$Distance.error.before.correction) #Replace each NA with the most recent non-NA (observations carried forwards) 
      df$Distance.error.after.correction = zoo::na.locf(df$Distance.error.after.correction) #Replace each NA with the most recent non-NA (observations carried forwards) 
    }
    
    ###########DR summaries###########
    df$Loni = c(df[-1, 'DR.longitude.corr'], 0) ; df$Lati = c(df[-1, 'DR.latitude.corr'], 0) #Shift longitude and latitude column values back by one row, ready for the disty method (compute row wise distance between coordinates)
    
    df$la = df$DR.latitude.corr[1] ; df$lo = df$DR.longitude.corr[1]  #Add columns filled with starting coordinates ready for disty method (note for Outgoing = FALSE, this may be 'estimated' starting coordinates)
   
    #If elevation/depth data supplied (meters) then additional distance moved and speed metrics calculated, incorporating vertical displacement 
    if(max(abs(diff(df$Elevation))) > 0) {
    
      #Haversine distance when no difference in elevation/depth. SLD distance when a difference (Hav.SLD)
      df$Elvi = c(df[-1, 'Elevation'], 0) #Shift elevation/depth column values back by one row (this may be irrelevant as if no elevation/depth data supplied)
      df$elo = df$Elevation[1] #Add columns filled with starting elevation/depth ready for disty method 
      df$DR.distance.3D = disty(df$DR.longitude.corr, df$DR.latitude.corr, df$Elevation, df$Loni, df$Lati, df$Elvi, method = "Hav.SLD")      #Calculate distance moved between rows and add this as a column ('DR.distance') to the data frame
      df$DR.distance.3D = c(0, df$DR.distance.3D[-nrow(df)]) #Shift values forward by 1
      df$DR.straightline.distance.from.start.3D = disty(df$lo, df$la, df$elo, df$DR.longitude.corr, df$DR.latitude.corr, df$Elevation, method = "Hav.SLD") #Calculate distance moved along track in relation to the straight-line distance to the supplied coordinates (lo and la)  
      df$DR.cumulative.distance.3D = df$DR.distance.3D #Prepare a column what will become cumulative distance (m)
      df$DR.cumulative.distance.3D = ifelse(is.na(df$DR.cumulative.distance.3D == TRUE), 0, df$DR.cumulative.distance.3D) #Ensure NA's replaced by zero 
      df$DR.cumulative.distance.3D = cumsum(df$DR.cumulative.distance.3D) # #Accumulate distance 
      df$DR.speed.3D = df$DR.distance.3D / df$DR.seconds    #Calculate (absolute 3D) speed (speed (m/s) = distance (m) / time (s) )
      df$DR.speed.3D[1] = 0 #Ensure first speed value is zero (not NA)
      df$Elevation.diff = c(0, diff(df$Elevation))/df$DR.seconds ; df$Elevtion.diff[1] = 0 #Calculate rate change of elevation/depth (m/s) ; ensure first row is 0 (since first row or DR.seconds is 0)
      
    }
    
    #Just Haversine distance
    df$DR.distance.2D = disty(df$DR.longitude.corr, df$DR.latitude.corr, df$Elevation, df$Loni, df$Lati, df$Elvi, method = "Haversine")      #Calculate distance moved between rows and add this as a column ('DR.distance') to the data frame
    df$DR.distance.2D = c(0, df$DR.distance.2D[-nrow(df)]) #Shift values forward by 1
    df$DR.straightline.distance.from.start.2D = disty(df$lo, df$la, df$elo, df$DR.longitude.corr, df$DR.latitude.corr, df$Elevation, method = "Haversine") #Calculate distance moved along track in relation to the straight-line distance to the supplied coordinates (lo and la)  
    df$DR.cumulative.distance.2D = df$DR.distance.2D #Prepare a column what will become cumulative distance (m)
    df$DR.cumulative.distance.2D = ifelse(is.na(df$DR.cumulative.distance.2D == TRUE), 0, df$DR.cumulative.distance.2D) #Ensure NA's replaced by zero 
    df$DR.cumulative.distance.2D = cumsum(df$DR.cumulative.distance.2D) # #Accumulate distance 
    df$DR.speed.2D = df$DR.distance.2D / df$DR.seconds    #Calculate (horizontal 2D) speed (speed (m/s) = distance (m) / time (s) )
    df$DR.speed.2D[1] = 0 #Ensure first speed value is zero (not NA)
    df[, 'DR.seconds'] = cumsum(df[, 'DR.seconds'])
  
    
    ###########VP summaries###########
    df$VP.loni = c(df[-c(1:dist.step), 'VP.longitude'], rep(0, dist.step)) ; df$VP.lati = c(df[-c(1:dist.step), 'VP.latitude'], rep(0, dist.step)) #Shift column values forward by the specified stepping range and add relevant number of NA's  to the end (to maintain vector length of column)
    df$VP.distance.2D = disty(df$VP.longitude, df$VP.latitude, 0, df$VP.loni, df$VP.lati, 0)      #Calculate row-wise distance between successive VP coordinates (according to stepping range)
    df$VP.distance.2D = c(rep(0, dist.step), df$VP.distance.2D[c(1:(nrow(df)-dist.step))]) #Shift values back by by the specified stepping range
    df$VP.cumulative.distance.2D = df$VP.distance
    df$VP.cumulative.distance.2D = ifelse(is.na(df$VP.cumulative.distance.2D == TRUE), 0, df$VP.cumulative.distance.2D)  
    df$VP.cumulative.distance.2D = cumsum(df$VP.cumulative.distance.2D) # #Accumulate distance column as separate column
    df$VP.seconds[is.na(df$VP.seconds) == TRUE] = 0.00  #Due to NA's present, replace with zeros before cum sum function
    df[, 'VP.seconds'] = cumsum(df[, 'VP.seconds']) #Accumulate VP.seconds column
    
    ##############################################################################################################################################################################################################################################################################
    #14) Summary plots
    ##############################################################################################################################################################################################################################################################################
    
    if(plot == FALSE) { print(paste("Final summaries computed - Returning data frame - Stage 6 complete")) }
    if(plot == TRUE) {  print(paste("Final summaries computed - Returning data frame and summary plots - Stage 6 complete")) 
   
      #4 plotting panels
      par(mfrow = c(2,2))
      
      ###########################################################################################################
      #Plot 1 = dead-reckoned track (uncorrected) in relation to VP track (black point refers to start of track)    
      ###########################################################################################################
      
      #Approximate dimensions of all coordinates to be plotted
      
      if(is.null(cs) == TRUE | is.null(ch) == TRUE) { #If current not supplied
        f = df$DR.longitude
        c = df$VP.longitude
        f = c(f, c) ; min.lon = min(f, na.rm = TRUE) ; max.lon = max(f, na.rm = TRUE)
        f = df$DR.latitude
        c = df$VP.latitude
        f = c(f, c) ; min.lat = min(f, na.rm = TRUE) ; max.lat = max(f, na.rm = TRUE) #These values will be used in xlim and ylim limits
      }
        
      if(is.null(cs) == FALSE | is.null(ch) == FALSE) { #If current supplied (incorporate dimensions of non-current integrated dead-reckoned tracks (xxx, yyy))
        f = df$DR.longitude
        c = df$VP.longitude
        f = c(f, c, xxx) ; min.lon = min(f, na.rm = TRUE) ; max.lon = max(f, na.rm = TRUE)
        f = df$DR.latitude
        c = df$VP.latitude
        f = c(f, c, yyy) ; min.lat = min(f, na.rm = TRUE) ; max.lat = max(f, na.rm = TRUE) #These values will be used in xlim and ylim limits
     }
      
      #Outgoing = FALSE and no current data supplied
      if(Outgoing == FALSE & (is.null(cs) == TRUE | is.null(ch) == TRUE)) {
        plot(df$DR.longitude, df$DR.latitude, type = "l", col = "blue", main = "No VP correction", xlab = "Longitude", ylab = "Latitude", xlim = c(min.lon, max.lon), ylim = c(min.lat, max.lat), sub = paste("(Reverse dead-reckoning)"))
        lines(na.omit(df$VP.longitude), na.omit(df$VP.latitude), col = "red")
        legend("topleft", legend = c("DR", "VP"),
               col = c("blue", "red"), lty = 1:1, cex = 0.65)
        points(lo, la, col = "black", cex = 1.5, pch = 19)
      }
      #Outgoing = TRUE and no current data supplied
      if(Outgoing == TRUE & (is.null(cs) == TRUE | is.null(ch) == TRUE)) {
        plot(df$DR.longitude, df$DR.latitude, type = "l", col = "blue", main = "No VP correction", xlab = "Longitude", ylab = "Latitude", xlim = c(min.lon, max.lon), ylim = c(min.lat, max.lat))
        lines(na.omit(df$VP.longitude), na.omit(df$VP.latitude), col = "red")
        legend("topleft", legend = c("DR", "VP"),
               col = c("blue", "red"), lty = 1:1, cex = 0.65)
        points(lo, la, col = "black", cex = 1.5, pch = 19)
      }
      #Outgoing = FALSE and current data is supplied
      if(Outgoing == FALSE & (is.null(cs) == FALSE | is.null(ch) == FALSE)) {
        plot(df$DR.longitude, df$DR.latitude, type = "l", col = "blue", main = "No VP correction", xlab = "Longitude", ylab = "Latitude", xlim = c(min.lon, max.lon), ylim = c(min.lat, max.lat), sub = paste("(Reverse dead-reckoning)"))
        lines(na.omit(df$VP.longitude), na.omit(df$VP.latitude), col = "red")
        lines(xxx, yyy, col = "green")
        legend("topleft", legend = c("DR (current drift)", "VP", "DR (no current drift)"),
               col = c("blue", "red", "green"), lty = 1:1, cex = 0.65)
        points(lo, la, col = "black", cex = 1.5, pch = 19)
      }
      #Outgoing = TRUE and current data is supplied
      if(Outgoing == TRUE & (is.null(cs) == FALSE | is.null(ch) == FALSE)) {
        plot(df$DR.longitude, df$DR.latitude, type = "l", col = "blue", main = "No VP correction", xlab = "Longitude", ylab = "Latitude", xlim = c(min.lon, max.lon), ylim = c(min.lat, max.lat))
        lines(na.omit(df$VP.longitude), na.omit(df$VP.latitude), col = "red")
        lines(xxx, yyy, col = "green")
        legend("topleft", legend = c("DR (current drift)", "VP", "DR (no current drift)"),
               col = c("blue", "red", "green"), lty = 1:1, cex = 0.65)
        points(lo, la, col = "black", cex = 1.5, pch = 19)
      }
      
      ###########################################################################################################
      #Plot 2 = dead-reckoned track (corrected) in relation to VP track (black point refers to start of track)    
      ###########################################################################################################

      a = subset(df, df$VP.used.to.correct == 1)
      c = thresh[1] ; d = max(a$Number.of.VPCs)
      if(method == "time") {
        plot(df$DR.longitude.corr, df$DR.latitude.corr, type = "l", col = "blue", main = paste("Correction appox. every", c, "s", "-",d,"corrections"), xlab = "Longitude", ylab = "Latitude", sub = paste("(N.B. If Inf values occured, degree of VPC would have changed)"))
      } else if(method == "distance") {
        plot(df$DR.longitude.corr, df$DR.latitude.corr, type = "l", col = "blue", main = paste(d, "corrections"), xlab = "Longitude", ylab = "Latitude", sub = paste("(N.B. If Inf values occured, degree of VPC would have changed)"))
      } else if(method == "all") {
        plot(df$DR.longitude.corr, df$DR.latitude.corr, type = "l", col = "blue", main = paste(d, "corrections (all fixes used)"), xlab = "Longitude", ylab = "Latitude", sub = paste("(N.B. If Inf values occured, degree of VPC may have changed)"))
      } else{ plot(df$DR.longitude.corr, df$DR.latitude.corr, type = "l", col = "blue", main = paste(d, "corrections"), xlab = "Longitude", ylab = "Latitude", sub = paste("(N.B. If Inf values occured, degree of VPC would have changed)")) }
      lines(na.omit(df$VP.longitude), na.omit(df$VP.latitude), col = "red")
      legend("topleft", legend = c("DR", "VP"),
             col = c("blue", "red"), lty = 1:1, cex = 0.65)
      points(a$DR.longitude.corr, a$DR.latitude.corr, col = "black", cex = 1, pch = 19)
      
      ###########################################################################################################
      #Plot 3 = Distance error for uncorrected and corrected track   
      ###########################################################################################################
      
      #Plot 3 = Distance error at 0 and thresh scales of VPC
      if(is.null(cs) == FALSE | is.null(ch) == FALSE) { #If currents supplied...
        y = round(max(DE.NC, df$Distance.error.before.correction, na.rm = TRUE)) #Y-axis limits
      }else { y = round(max(df$Distance.error.before.correction, na.rm = TRUE)) }
      
      plot(df$DR.seconds[!is.na(df$Distance.error.before.correction)], df$Distance.error.before.correction[!is.na(df$Distance.error.before.correction)], type = "l", col = "red", main = "Distance between VP and DR tracks", xlab = "Time (s)", ylab = "Distance error (m)", ylim = c(0, y))
      if(is.null(cs) == FALSE | is.null(ch) == FALSE) { #If currents supplied...
        lines(df$DR.seconds[!is.na(DE.NC)], DE.NC[!is.na(DE.NC)], col = "green") }
      lines(df$DR.seconds[!is.na(df$Distance.error.after.correction)], df$Distance.error.after.correction[!is.na(df$Distance.error.after.correction)], col = "black")
     
      if(is.null(cs) == FALSE | is.null(ch) == FALSE) {
        if(method == "time" & Outgoing == TRUE) {
          legend("topleft", legend = c("No correction, currents integrated", "No correction, currents not integrated", paste("Correction appox. every", c, "s")),
                 col = c("red", "green", "black"), lty = 1:1, cex = 0.65)
        } else if(method == "time" & Outgoing == FALSE) {
          legend("topright", legend = c("No correction, currents integrated", "No correction, currents not integrated", paste("Correction appox. every", c, "s")),
                 col = c("red", "green", "black"), lty = 1:1, cex = 0.65)
        } else if(method == "divide" & Outgoing == TRUE) { 
          legend("topleft", legend = c("No correction, currents integrated", "No correction, currents not integrated", paste(d, "corrections")),
                 col = c("red", "green", "black"), lty = 1:1, cex = 0.65)
        } else if(method == "divide" & Outgoing == FALSE){ 
          legend("topright", legend = c("No correction, currents integrated", "No correction, currents not integrated", paste(d, "corrections")),
                 col = c("red", "green", "black"), lty=1:1, cex=0.65)
        } else if(method == "distance" & Outgoing == TRUE) { 
          legend("topleft", legend = c("No correction, currents integrated", "No correction, currents not integrated", paste(d, "corrections")),
                 col=c("red", "green", "black"), lty = 1:1, cex = 0.65)
        } else if(method == "distance" & Outgoing == FALSE) { 
          legend("topright", legend = c("No correction, currents integrated", "No correction, currents not integrated", paste(d, "corrections")),
                 col = c("red", "green", "black"), lty = 1:1, cex = 0.65)
        } else if(method == "all" & Outgoing == TRUE) { 
          legend("topleft", legend = c("No correction, currents integrated", "No correction, currents not integrated", paste("All fixes used (unless Inf re-scaling)")),
                 col = c("red", "green", "black"), lty = 1:1, cex = 0.65)
        } else{legend("topright", legend = c("No correction, currents integrated", "No correction, currents not integrated", paste("All fixes used (unless Inf re-scaling)")),
                      col = c("red", "green", "black"), lty = 1:1, cex = 0.65)
        }
      } else if(is.null(cs) == TRUE | is.null(ch) == TRUE) {
        if(method == "time" & Outgoing == TRUE) {
          legend("topleft", legend = c("No correction", paste("Correction appox. every", c, "s")),
                 col = c("red", "black"), lty = 1:1, cex = 0.65)
        } else if(method == "time" & Outgoing == FALSE) {
          legend("topright", legend = c("No correction", paste("Correction appox. every", c, "s")),
                 col = c("red", "black"), lty = 1:1, cex = 0.65)
        } else if(method == "divide" & Outgoing == TRUE) { 
          legend("topleft", legend = c("No correction", paste(d, "corrections")),
                 col = c("red", "black"), lty = 1:1, cex = 0.65)
        } else if(method == "divide" & Outgoing == FALSE){ 
          legend("topright", legend = c("No correction", paste(d, "corrections")),
                 col = c("red", "black"), lty=1:1, cex=0.65)
        } else if(method == "distance" & Outgoing == TRUE) { 
          legend("topleft", legend = c("No correction", paste(d, "corrections")),
                 col=c("red", "black"), lty = 1:1, cex = 0.65)
        } else if(method == "distance" & Outgoing == FALSE) { 
          legend("topright", legend = c("No correction", paste(d, "corrections")),
                 col = c("red", "black"), lty = 1:1, cex = 0.65)
        } else if(method == "all" & Outgoing == TRUE) { 
          legend("topleft", legend = c("No correction", paste("All fixes used (unless Inf re-scaling)")),
                 col = c("red", "black"), lty = 1:1, cex = 0.65)
        } else{legend("topright", legend = c("No correction", paste("All fixes used (unless Inf re-scaling)")),
                      col = c("red", "black"), lty = 1:1, cex = 0.65)
        }
      }
        
      ###########################################################################################################
      #Plot 4 = dead-reckoned track (corrected)   
      ###########################################################################################################
      
      z = round(max(df$DR.cumulative.distance.2D, na.rm = TRUE))
      
      if(max(abs(diff(df$Elevation))) > 0)  { 
        y = round(max(df$DR.cumulative.distance.3D, na.rm = TRUE))
        plot(df$DR.longitude.corr, df$DR.latitude.corr, type = "l", col = "blue", main = paste("VPC DR track"), xlab = "Longitude", ylab = "Latitude",
             sub = paste("Accumulated 2D DR distance =", z, "m.", "Accumulated 3D DR distance =", y, "m"))
      } else { plot(df$DR.longitude.corr, df$DR.latitude.corr, type = "l", col = "blue", main = paste("VPC DR track"), xlab = "Longitude", ylab = "Latitude",
                   sub = paste("Accumulated 2D DR distance =", z, "m")) }
      
      #Set plotting parameters back to normal
      par(mfrow = c(1,1)) 
      
    }
    
    ##############################################################################################################################################################################################################################################################################
    #15) Clear up variables and columns created in function that are not required any more - Return data frame
    ##############################################################################################################################################################################################################################################################################

    df$Loni = NULL ;  df$Lati = NULL ; df$lo = NULL ; df$la = NULL  ; df$VP.loni = NULL ;  df$VP.lati = NULL ; df$h = NULL ; df$q = NULL ; df$corr.h = NULL ; df$corr.q = NULL ; if(max(abs(df$Elevation - df$Elevation)) > 0) { df$elo = NULL ; df$Elvi = NULL }
    
    #reorder column variables 
    if(is.null(cs) == TRUE | is.null(ch) == TRUE) { #If current not supplied
      
      if(max(abs(diff(df$Elevation))) > 0) { #Elevation/depth data supplied
        
        df = df[c("Row.number", "Timestamp", "DR.seconds", "Heading", "Marked.events", "DBA.or.speed", "Radial.distance", "Elevation", "Elevation.diff", "DR.longitude", 
                  "DR.latitude", "DR.longitude.corr", "DR.latitude.corr", "Dist.corr.factor", "Head.corr.factor", "Heading.corr", "Radial.distance.corr", 
                  "Distance.error.before.correction", "Distance.error.after.correction", "DR.distance.2D", "DR.distance.3D", "DR.cumulative.distance.2D", 
                  "DR.cumulative.distance.3D", "DR.straightline.distance.from.start.2D", "DR.straightline.distance.from.start.3D", 
                  "DR.speed.2D", "DR.speed.3D", "VP.seconds", "VP.longitude", "VP.latitude", "VP.present", "VP.used.to.correct", 
                  "Number.of.VPCs", "VP.thresh", "VP.distance.2D", "VP.cumulative.distance.2D")]
      }
      
      if(max(abs(diff(df$Elevation))) == 0) { #Elevation/depth data not supplied
        
        df$Elevation = NULL  #Remove elevation/depth data if none supplied
        
        df = df[c("Row.number", "Timestamp", "DR.seconds", "Heading", "Marked.events", "DBA.or.speed", "Radial.distance", "DR.longitude", "DR.latitude", 
                  "DR.longitude.corr", "DR.latitude.corr", "Dist.corr.factor", "Head.corr.factor", "Heading.corr", "Radial.distance.corr", 
                  "Distance.error.before.correction", "Distance.error.after.correction", "DR.distance.2D", "DR.cumulative.distance.2D", 
                  "DR.straightline.distance.from.start.2D", "DR.speed.2D", "VP.seconds", "VP.longitude", "VP.latitude", 
                  "VP.present", "VP.used.to.correct", "Number.of.VPCs", "VP.thresh", "VP.distance.2D", "VP.cumulative.distance.2D")]
      }
    }
    
    if(is.null(cs) == FALSE | is.null(ch) == FALSE) { #If current supplied
      
      if(max(abs(diff(df$Elevation))) > 0) { #Elevation/depth data supplied
        
        df = df[c("Row.number", "Timestamp", "DR.seconds", "Heading", "Marked.events", "DBA.or.speed", "Radial.distance", "Elevation", "Elevation.diff", "Current.speed", 
                  "Current.heading", "Heading.current.integrated", "Radial.distance.current.integrated", "DR.longitude", "DR.latitude", "DR.longitude.corr", 
                  "DR.latitude.corr", "Dist.corr.factor", "Head.corr.factor", "Heading.corr", "Radial.distance.corr", "Distance.error.before.correction",
                  "Distance.error.after.correction", "DR.distance.2D", "DR.distance.3D", "DR.cumulative.distance.2D", "DR.cumulative.distance.3D", 
                  "DR.straightline.distance.from.start.2D", "DR.straightline.distance.from.start.3D", "DR.speed.2D", 
                  "DR.speed.3D","VP.seconds", "VP.longitude", "VP.latitude", "VP.present", "VP.used.to.correct","Number.of.VPCs", 
                  "VP.thresh", "VP.distance.2D", "VP.cumulative.distance.2D")]
      }
      
    if(max(abs(diff(df$Elevation))) == 0) { #Elevation/depth data not supplied
      
      df$Elevation = NULL #Remove elevation/depth data if none supplied
      
      df = df[c("Row.number", "Timestamp", "DR.seconds", "Heading", "Marked.events", "DBA.or.speed", "Radial.distance", "Current.speed", 
                "Current.heading", "Heading.current.integrated", "Radial.distance.current.integrated", "DR.longitude", "DR.latitude", 
                "DR.longitude.corr", "DR.latitude.corr", "Dist.corr.factor", "Head.corr.factor", "Heading.corr", "Radial.distance.corr", 
                "Distance.error.before.correction", "Distance.error.after.correction", "DR.distance.2D", "DR.cumulative.distance.2D", 
                "DR.straightline.distance.from.start.2D", "DR.speed.2D", "VP.seconds", "VP.longitude", "VP.latitude", 
                "VP.present", "VP.used.to.correct", "Number.of.VPCs", "VP.thresh", "VP.distance.2D", "VP.cumulative.distance.2D")]
      }
    }
    
    #If pitch supplied, incorporate (degree format) into data frame after 'DBA.or.speed' column
    if(is.null(p) == FALSE) {
      ref = which(names(df) == "DBA.or.speed")
      df = data.frame(df[1:ref], Pitch = (p * 180/pi), df[(ref+1):ncol(df)])  
    }
    
    rm(la, lo, DR.lat, DR.lon, DR.loni, DR.lati, TS, TD, s, q, v, p, h, cs, ch, ME, elv, drift.q, drift.h, Row.number, a, c, d, ref, VP.lon, VP.lat, VP.distance, VP.loni, VP.lati, fixy, z, h.rev, s.rev, ME.rev, v.rev, TD.rev, c.rev, m.rev, cs.rev, ch.rev, x, y, xx, yy, xxx, yyy, DE.NC, keep, thresh, cum.t, t, TD.VP, corr.h, corr.q, corr.s, VP.temp, fix, cond, max.lon, min.lon, max.lat, min.lat, prop.dist.corr, prop.head.corr, VP.ref.df, ref.df, temp.df, temp.df.2, dist.step)
      
    return(df) #Return data frame
    
  }
  
}
     
    
#####################################################################################################################################################################################################################################
#####################################################################################################################################################################################################################################
#####################################################################################################################################################################################################################################
#####################################################################################################################################################################################################################################
##############################################################################################################################################################

#Order of data frame output if VPC is initialized, (*Present according to input)
##############################################################################################################################################################
# 1) Row.number
# 2) Timestamp  
# 3) DR.seconds --> (Time difference between rows (s) accumulated)
# 4) Heading --> (Original)
# 5) Marked.events --> (Original)
# 6) DBA.or.speed --> (Original)
# 7) *Pitch --> (Original)
# 8) Radial.distance --> (The calculated q coefficient (prior to VPC factors)
# 9) *Elevation --> (Original)
# 10) *Elevation.diff --> (Rate change of supplied elevation/depth (m/s)) 
# 11) *Current.speed --> (Original)
# 12) *Current.heading --> (Original)
# 13) *Heading.current.integrated --> (Updated heading following addition of current vectors (prior to VPC))
# 14) *Radial.distance.current.integrated --> (Updated q coefficient following addition of current vectors (prior to VPC))
# 15) DR.longitude --> (Original dead-reckoned longitude, no VPC (possibly with currents integrated))
# 16) DR.latitude --> (Original dead-reckoned latitude, no VPC (possibly with currents integrated))
# 17) *DR.longitude.corr --> (Corrected dead-reckoned longitude coordinates (post VPC))
# 18) *DR.latitude.corr --> (Corrected dead-reckoned latitude coordinates (post VPC))
# 19) *Dist.corr.factor --> (Distance correction factor (observations carried forward))
# 20) *Head.corr.factor --> (Heading correction factor (observations carried forward))
# 21) *Heading.corr  --> (Corrected heading)
# 22) *Radial.distance.corr --> (Corrected q)
# 23) *Distance.error.before.correction --> (Distance (m) between uncorrected dead-reckoned positions and VPs (observations carried forward))
# 24) *Distance.error.after.correction --> (Distance (m) between corrected dead-reckoned positions and VPs (observations carried forward))
# 25) DR.distance.2D --> (Two-dimensional distance moved (m) between dead-reckoned fixes)
# 26) *DR.distance.3D --> (Three-dimensional distance moved (m) between dead-reckoned fixes)
# 27) DR.cumulative.distance.2D --> (Accumulated two-dimensional distance moved (m) between dead-reckoned fixes)
# 28) *DR.cumulative.distance.3D --> (Accumulated three-dimensional distance moved (m) between dead-reckoned fixes)
# 29) DR.distance.from.start.2D --> (Two-dimensional (straight-line) distance moved (m) from starting position)
# 30) *DR.distance.from.start.3D --> (Three-dimensional (straight-line) distance moved (m) from the starting position)
# 31) DR.speed.2D --> Horizontal speed (m/s) (DR.distance.2D / time difference between rows)
# 32) *DR.speed.3D --> Total speed (m/s) (DR.distance.3D / time difference between rows)
# 33) *VP.seconds --> (Accumulated time (s) between supplied VPs positions (observations carried forward))
# 34) *VP.longitude --> (Supplied VP longitude coordinates (observations carried forward), sub-sampled according to Marked.events if VP.ME = TRUE)
# 35) *VP.latitude --> (Supplied VP latitude coordinates (observations carried forward), sub-sampled according to Marked.events if VP.ME = TRUE)
# 36) *VP.present --> (Denotes when a VP was present (1) or absent (0), sub-sampled according to Marked.events if VP.ME = TRUE)
# 37) *VP.used.to.correct --> (Denotes which fixes were used within the VPC procedure (1 = used) or (0 = ignored))
# 38) *Number.of.VPCs --> (Increments by 1 each time a VP was used within the VPC procedure (observations carried forward))
# 39) *VP.thresh --> (Replicates the thresh value set (or default) or warns the user that additional VP under-sampling was required if 'Inf' values were produced)
# 40) *VP.distance.2D --> (Two-dimensional distance moved (m) between VPs (following subset if VP.ME = TRUE), using the stepping interval 'dist.step')
# 41) *VP.cumulative.distance.2D --> (Accumulated two-dimensional distance moved (m) between VPs (following subset if VP.ME = TRUE), using the stepping interval 'dist.step'))


#Order of plots if VPC is initialized and plot = TRUE
###############################################
# Top left) Original DR track (blue) in relation to VP track (red) (black point refers to beginning of DR track (supplied coordinates (this may technically be end of track if reverse dead-reckoned)))
# Top right) DR track (blue) corrected at frequency as governed by thresh in relation to VP track (red) --> First and last VPs always used, regardless of thresh/method set (black points refers to areas of correction)
# Bottom left) Distance between un-corrected and 'thresh-fix' corrected DR tracks in relation to VP track per unit time (when VPs are present))
# Bottom right) Just the VPC DR track
##############################################################################################################################################################
#If VPC is not enabled and plot = TRUE
###############################################
#Original DR 'pseudo' track only

#Example for outgoing trip on land
#x = Gundog.Tracks(TS = df$Timestamp, h = df$Mag.heading.smoothed, v = df$VeDBA.smoothed, p = NULL, cs = NULL, ch = NULL, m = 3, c = 0, ME = df$Walking, lo =  head(df$GPS.Longitude[df$GPS.Longitude != 0],1), la = head(df$GPS.Latitude[df$GPS.Latitude != 0], 1), VP.lon = df$GPS.Longitude, VP.lat = df$GPS.Latitude, VP.ME = TRUE, dist.step = 5, method = "time", thresh = 500, bound = TRUE,  Outgoing = TRUE, max.speed = 3, plot = TRUE)

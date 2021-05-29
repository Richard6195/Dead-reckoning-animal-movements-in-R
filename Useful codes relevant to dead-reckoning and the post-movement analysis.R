#Potentially useful codes within the dead-reckoning framework of animal movement analysis


#(1)
#Transform coordinates from long-lat projection, into metres and zero off to start at 0,0 m
#Assume the dataframe of interest is called 'df'
library(rgdal) ; library(dplyr)
x <- data.frame(lon = df$DR.longitude.corr, lat = df$DR.latitude.corr) #get the releavant long and lat coords from original df into a new df called 'x'
coordinates(x) <- c("lon", "lat")
proj4string(x) <- CRS("+init=epsg:4326") # WGS 84
CRS.new <- CRS("+proj=somerc +lat_0=46.9524056 +lon_0=7.43958333 +ellps=bessel +x_0=2600000 +y_0=1200000 +towgs84=674.374,15.056,405.346 +units=m +k_0=1 +no_defs") #Example proj4string describing the coordinate Reference System (CRS) 
coords.transf <- spTransform(x, CRS.new) #Transforms the cooridnated based on the supplied projection (for an overview, see; https://www.nceas.ucsb.edu/sites/default/files/2020-04/OverviewCoordinateReferenceSystems.pdf)
df$lon = coords.transf$lon #Place these transformed coords back into original df
df$lat = coords.transf$lat #Place these transformed coords back into original df
#Zero off coorids start of each indivdual
df <- df  %>% group_by(ID) %>% mutate("long.Z.m" = lon - lon[1]) #Zero off the coordinates from the starting position, grouping by the'ID' coloumn (assuming multiple individuals are in one df)
df <- df  %>% group_by(ID) %>% mutate("lat.Z.m" = lat - lat[1])
colnames(df)[colnames(df)=="long.Z.m"] <- "Distance E-W (m)" #Rename these columns
colnames(df)[colnames(df)=="lat.Z.m"] <- "Distance N-S (m)"



#(2)
#Function to calulate rolling (2-D) tortuosity values every 'x' seconds (based on time)
#Tortuoisty is calculated by dividing the straight-line distance (SLD), that is the two-dimensional Euclidean distance between the initial fix and the end fix of the path, by the sum of the consecutive individual distance steps (SDS) between fix1, fix2,..., fixn that constituted the total path length
#x = supplied longitude values
#y = supplied latitude values
#s = supplied  time theshold - tortosity calulated over 's' seconds e.g., s = 10 = tortuosity calultated over 10 s intervals
library(zoo)
Gundog.Tortuosity <- function(x, y, s){
  w<-round(s/2)
  library(zoo)
  SLD <- rep(NA, length(x)) 
  SDS <- rep(NA, length(x)) 
  TORT<-rep(NA, length(x)) 
  disty.x<-rep(0, length(x)) ;disty.y<-rep(0, length(x))
  for(i in 1:length(x-w)){
    SLD[i+w] <- ((x[i+s]-x[i])^2+(y[i+s]-y[i])^2)^0.5} 
  for(i in 2:length(SLD)){
    disty.x[i]<- (x[i] - x[i-1])^2
    disty.y[i]<- (y[i] - y[i-1])^2
    SDS[i]<- sqrt(disty.x[i] + disty.y[i])}  
  SDS<-rollsum(SDS, s, align = 'center', fill=NA)
  for(i in (1+w):length(x-w)){
    TORT[i]<-SLD[i]/SDS[i] 
    TORT[i]<- (1 - TORT[i])}
  return(TORT)}



#(3)
#Function to calculate a tortuosity value over every 'x' metres moved. This makeshift function iterates per supplied group (e.g., individual)
#Tortuoisty is calculated by dividing the straight-line distance (SLD), that is the two-dimensional Euclidean distance between the initial fix and the end fix of the path, by the sum of the consecutive individual distance steps (SDS) between fix1, fix2,..., fixn that constituted the total path length
#x = supplied longitude values
#y = supplied latitude values
#d = supplied distance values (distance travelled between fixes, e.g., computed with Haversine formula). Note, do not supply accumulated distance
#thresh = Tortosity calculated every 'thresh' cumulated distance moved. E.g., thresh = 10 - one tort value per 10 m moved
#Group = Secondary grouping function. E.g., If wanting to separate results for multiple individuals / habitat types etc.
library(plyr) ; library(zoo)
Gundog.Tortuosity.2 <- function(x, y, d, thresh, Group){   
  thresh= rep(thresh, length(x))
  #First function - Calculate rolling distance moved which resets every 'x' metres as governed by your thresh value and adds unique group number
  #Also resets according to inital group you entered into the 'overall function'.
  succ<-function(x, y, d, thresh){
    cum.d<-rep(0, length(x))  ; t<-rep(0, length(x)) ; TR<-rep(NA, length(x))
    z<-1
    for (i in 2:length(x)){
      if (cum.d[i-1] < thresh){
        cum.d[i]<-cum.d[i-1] + d[i]
        t[i]<-t[i-1] +1}
      else if (cum.d[i-1] >= thresh){
        cum.d[i] = 0
        t[i] = 0}}
    for (i in 2:length(x)){
      if (t[i] - t[i-1] == 1){
        TR[i]<-z
      } else if(t[i] - t[i-1] != 1){
        z<-z+1
        TR[i]<-z}}
    TR[[1]]<-1  
    dist.df<-as.data.frame(cbind(d, cum.d, t, TR, x, y, thresh))
    colnames(dist.df)<-c("succ.dist", "Cumulative distance", "seconds", "Group", "Longitude", "Latitude", "thresh")
    return(dist.df)}
  dist.df<-as.data.frame(cbind(x, y,d, Group, thresh))
  colnames(dist.df)<-c("Longitude", "Latitude", "DIST", "Group", "thresh")
  dist.df$Longitude<-as.numeric(as.character(dist.df$Longitude))
  dist.df$DIST<-as.numeric(as.character(dist.df$DIST))
  dist.df$Latitude<-as.numeric(as.character(dist.df$Latitude))
  dist.df$thresh<-as.numeric(as.character(dist.df$thresh))
  #Ensure only walking bouts are used
  #dist.df<-subset(dist.df, dist.df$DIST > 0)
  out<-split(dist.df , f = dist.df$Group)
  #Now use first funtion over list of 'x m' moved per list element
  test<-lapply(out, function(x,y,d,thresh) succ(x$Longitude, x$Latitude, x$DIST, x$thresh))
  df <- ldply (test, data.frame)
  colnames(df)[colnames(df)==".id"] <- "ID"
  df$Group.2<-paste(df$ID, df$Group)
  df$Row.Number<-rep(1:nrow(df))
  #Group.2 is original group and the new groups of every 'x' m moved
  out <- split(df , f = df$Group.2)
  #Second function --> New tortuosity function (0 = straight-line, 1 = very tortous)
  #This is calulated per group.2 (from first function --> per x m moved per Group (ID, trip number, condition etc...))
  Gundog.Tortuosity.v<-function(x, y, rn){
    s<-length(x)
    w<-round((s+0.5)/2)
    SLD <- rep(NA, length(x))
    SDS <- rep(NA, length(x))
    TORT<-rep(NA, length(x))
    disty.x<-rep(0, length(x))
    disty.y<-rep(0, length(x))
    for(i in 1:length(s)){
      SLD[i+(s-1)] <- ((x[i+(s-1)]-x[i])^2+(y[i+(s-1)]-y[i])^2)^0.5}
    for(i in 1+1:length(SLD)){
      disty.x[i]<- (x[i] - x[i-1])^2
      disty.y[i]<- (y[i] - y[i-1])^2}
    for(i in 1:length(x)){
      SDS[i]<- sqrt(disty.x[i] + disty.y[i])}
    SDS<-rollsum(SDS, s, align = 'right', fill=NA)
    for(i in 1:length(x)){
      TORT[i]<-SLD[i]/SDS[i]
      TORT[i]<-ifelse((TORT[i] == "NaN"), 1, TORT[i])
      TORT[i]<- 1-TORT[i]}
    #Tortuosity
    dist.df.2<-as.data.frame(cbind(rn, TORT))
  }
  test.20<-lapply(out, function(x,y) Gundog.Tortuosity.v(x$Longitude, x$Latitude, x$Row.Number))
  df.20.w <- ldply (test.20, data.frame)
  df.20.w<-df.20.w[, -c(1)]
  colnames(df.20.w)<-c("Row.Number", "Tortuosity (0 = Straight-line distance)")
  df<-merge(df, df.20.w, by.x="Row.Number", by.y="Row.Number")
  df<-na.omit(df)      #Can remove this to see 'complete' df... tort values will only appear at end of each group.2
  a<-thresh[1]
  lower<-a-(a*0.25)  #Arbitary.. can change this (this removes groups that are less/more than 25% of thresh value)
  upper<-a+(a*0.25)  #Arbitary.. can change this (this removes groups that are less/more than 25% of thresh value)
  df<-subset(df, df$Cumulative.distance >= lower) #Arbitary.. can change this (this removes groups that are less/more than 20% of thresh value)
  df<-subset(df, df$Cumulative.distance <= upper) #Arbitary.. can change this (this removes groups that are less/more than 20% of thresh value)
  return(df)}



#(4)
#Calculate the shortest horizontal (Haversine) distance between an animals track and a given line transect (assuming they are vertically in alignment per unit time (quicker alternative to Dist2line from 'geosphere' package)
library(DescTools) ; library(geosphere) ; library(dplyr) ; library(imputeTS) ; library(geosphere)
#First lets make up a line transect by linearly interpolating between an animal's starting position and finishing position
#Track of Familiarity (ToF)
df$TOF.lon = rep(NA, nrow(df))
df$TOF.lat = rep(NA, nrow(df))
df = df %>% mutate(TOF.lon = replace(TOF.lon, row_number() == 1, DR.longitude.corr[1]))#Replace first row of each group (assuming multiple individuals in df) with the given individual's initial longitude coordinate
df = df %>% mutate(TOF.lat = replace(TOF.lat, row_number() == 1, DR.latitude.corr[1])) #Replace first row of each group (assuming multiple individuals in df) with the given individual's initial latitude coordinate
df = df %>% mutate(TOF.lon = c(TOF.lon[-n()], tail(DR.longitude.corr, 1))) #Replace last row of each group with the given individual's final longitude coordinate
df = df %>% mutate(TOF.lat = c(TOF.lat[-n()], tail(DR.latitude.corr, 1))) #Replace last row of each group with the given individual's final latitude coordinate
df$TOF.lon = na.interpolation(df$TOF.lon, option = "linear", maxgap = Inf) #Linearly interpolate, replacing the NA's between these 2 supplied coords
df$TOF.lat = na.interpolation(df$TOF.lat, option = "linear", maxgap = Inf) #Linearly interpolate, replacing the NA's between these 2 supplied coords
DR.latitude.corr = df$DR.latitude.corr ; TOF.lat = df$TOF.lat
x = rep(NA, length(DR.latitude.corr))
for(i in 1:length(DR.latitude.corr)){
  x[i] = Closest(TOF.lat, DR.latitude.corr[i], which = TRUE, na.rm = FALSE)
}
df$TOF.lat.time.corr = x
df$TOF.lat.time.corr = df$TOF.lat[df$TOF.lat.time.corr]
df$TOF.lon.time.corr = x
df$TOF.lon.time.corr = df$TOF.lon[df$TOF.lon.time.corr]
df<- df %>% rowwise() %>% mutate(ToF = distHaversine(c(DR.longitude.corr, DR.latitude.corr), c(TOF.lon.time.corr, TOF.lat.time.corr))) 



#(5)
#Grid up maps/tracks into approx 10 by 10 m and compute summary stats per grid
#Now lets grid up map and create density plots 
library(ggplot2) ; library(viridis)
#First grid up map
ji <- function(xy, origin=c(0,0), cellsize=c(0.0001,0.0001)) {
  t(apply(xy, 1, function(z) cellsize/2+origin+cellsize*(floor((z - origin)/cellsize))))
}
JI <- ji(cbind(df$DR.longitude.corr , df$DR.latitude.corr))
df$gridded.X <- JI[, 1]
df$gridded.Y <- JI[, 2]
df$Grid.cell <- paste(df$gridded.X, df$gridded.Y)
#Create gridded plots
grid.summaries <- df %>%group_by(Grid.cell, ) %>% summarise_at(vars("DR.Speed", "AVeY"), mean, na.rm =T) #Compute mean per grid.cell, for veriables termed 'Dr.Speed' and 'AVeY'
grid.summaries <- data.frame(cbind(do.call('rbind', strsplit(as.character(grid.summaries$Grid.cell),' ',fixed=TRUE)), v[, 2:3])) #Separate gridded coords
colnames(grid.summaries)<-c("Longitude", "Latitude" ,"Speed", "AVeY") #rename
grid.summaries$Longitude<-as.numeric(as.character(grid.summaries$Longitude)) #Ensure all columns are numeric class
grid.summaries$Latitude<-as.numeric(as.character(grid.summaries$Latitude))
grid.summaries$Speed<-as.numeric(as.character(grid.summaries$Speed))
grid.summaries$AVeY<-as.numeric(as.character(grid.summaries$AVeY))
#plot using Viridis
ggplot() + geom_raster(aes(x = Longitude, y = Latitude, fill = Speed), data = grid.summaries) + 
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction = -1) +
  theme_bw() #+limits=c(0,3))

#Count unique number of times each grid has been occupied (can only be occupied once per bird per track) - Unique density of movement
df$one <-rep(1, nrow(df)) #replicate '1' throughout the column termed 'one'
grid.summaries <- df %>% group_by(group, Grid.cell) %>% summarise(n_distinct(one), na.rm =T) #assume group is separating individuals within the df
grid.summaries <- grid.summaries %>% group_by(Grid.cell) %>% summarise_at(vars("n_distinct(one)"), sum, na.rm =T)
grid.summaries <- data.frame(cbind(do.call('rbind', strsplit(as.character(grid.summaries$Grid.cell),' ',fixed=TRUE)), v[, 2])) #Separate gridded coords
colnames(grid.summaries)<-c("Longitude", "Latitude" ,"Frequency") #Rename
grid.summaries$Longitude<-as.numeric(as.character(grid.summaries$Longitude)) #Ensure all columns are numeric class
grid.summaries$Latitude<-as.numeric(as.character(grid.summaries$Latitude))


#(6)
#Compute distance in 2- or 3-D
#disty function 
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


#Now compute distance using a given stepping interval between Verified Positions (VPs)
dist.step = 5 #stepping interval of 5 fixes
VP.loni = c(VP.longitude[-c(1:dist.step)], rep(NA, dist.step)) ; VP.lati = c(VP.longitude[-c(1:dist.step)], rep(NA, dist.step)) #Shift vector values forward  by the specified stepping range and add relevant number of NA's  to the end (to maintain vector length as original)
VP.distance = disty(VP.longitude, VP.latitude, 0, VP.loni, VP.lati, 0)      #Calculate row wise 2-D (Haversine) distance between successive VP coordinates
VP.distance = c(rep(NA, dist.step), VP.distance[c(1:(length(VP.distance)-dist.step))]) #Shift values back by by the specified stepping range
VP.distance = ifelse(is.na(VP.distance == TRUE), 0, VP.distance)  #Replace NA's with 0's
VP.distance = cumsum(VP.distance) #Calculate cumulative distance between VPs

#(7)
#Compute bearing (heading) between sets of coords
#Bearing function --> returns degrees - Great circular bearing between 2D positions (assumes time-matched VP and dead-reckoned fixes share the same elevation/depth)
beary = function(long1, lat1, long2, lat2) { #Assumes units supplied as degrees
  long1 = long1 * pi/180 ; long2 = long2 * pi/180 ; lat1 = lat1 * pi/180 ; lat2 = lat2 * pi/180 #Function converts to radians
  a = sin(long2 - long1) * cos(lat2)
  b = cos(lat1) * sin(lat2) - sin(lat1) * cos(lat2) * cos(long2 - long1)
  c = ((atan2(a, b) / pi) * 180)  #Units returned in degrees (-180 to +180 degree scale)
  return(c)
}

(8)


#(8)
#Counting occurrence and re-occurrence or a given variable
#Uninterrupted 'Step' occurrence (accumulates the number of rows a given variable remains unchanged)
df$Behaviour.duration<-sequence(rle(df$Behaviour)$lengths) #Count uninterrupted sequence of the variable 'Behaviour'
#Unique incremental increase per re-occurrence of a 'Behaviour'
y1<- rle(df$Behaviour)$values
y2<- rle(df$Behaviour)$lengths
z<-c()
for(i in unique(y1)){
  z[y1 == i]<-1:sum(y1 == i)
}
df$Behaviour.occurrence<-rep(z,y2) 
df$Behaviour.Group <- paste(df$Behaviour, df$Behaviour.occurrence)
#Proportion of time of dataset
x<-as.numeric(df$Total.Event.no.)
rescale <- function(x) (x-min(x))/(max(x) - min(x))
df$Prop.of.total.time<-rescale(x)
#Proportion of time per behaviour occurrence
df <- df  %>%
  group_by(Behaviour.Group) %>%
  mutate(prop_of_time_per_behaviour = rescale(Behaviour.duration) )
#Sequential row numbers
df$Observation <- 1:nrow(df) 

#(9)
#Calculate when a VP is within a pre-set up polygon
library(SDMTools)
x<-pnt.in.poly(df, POLY) #POLY = df of longitude and latitude coordinates of polygon (df = df of longitude and latitude coordinates of the VP track)
df$Area<-x$pip     #1 pasted if track inside polygon or on polygon boundary
#Plot points outside of polygon with an X
plot(df$Longitude, df$Latitude)
polygon(POLY, col='#99999990')
points(df[which(Area$pip==0),1:2],pch='X') #Assuming column 1 and 2 and longitude and latitude, respectively


#(10)
#Cumulative heading function #group = binary, will only advance if 1, otherwise will not accumulate if 0
#cumulative heading" (CuHe), assesses the percentage coverage (0%-100%) about the yaw axis
#from the culmination of angular rotations in both directions, resetting
#each time the animal had (at least once), rotated through all 360°.
#x = heading (0 to 360 degrees)
CuHe <- function(x, group){
  AVeY = rep(0, length(x)) 
  if(length(group) == 1){ group = rep(group, length(x))}
  cumulative.heading<-rep(0, length(x))
  a<-0
  heading <- round(x, 0)
  AVeY = c(0, diff(heading))
  AVeY <-ifelse(AVeY < -180, (AVeY +360), AVeY)
  AVeY <-ifelse(AVeY > 180, (AVeY - 360), AVeY)
  for(i in 2:length(heading)){
    if(AVeY[i] > 0 & group[i] == 1 & (heading[i] > heading[i-1])){
      a<-c(a, seq.int(heading[i-1], heading[i], 1))}
    if(AVeY[i] > 0 & group[i] == 1 & (heading[i] < heading[i-1])){
      a<-c(a, (seq.int(heading[i-1], 360, 1)), (seq.int(0, heading[i], 1)))}
    if(AVeY[i] < 0 & group[i] == 1 & (heading[i] < heading[i-1])){
      a<-c(a, seq.int(heading[i-1], heading[i], -1))}
    if(AVeY[i] < 0 & group[i] == 1 & (heading[i] > heading[i-1])){
      a<-c(a, (seq.int(heading[i-1], 0, -1)), (seq.int(360, heading[i], -1)))}
    else{ a<- c(a, 0)} 
    a<-ifelse(a == 0, 360, a)
    a<-unique(a)
    cumulative.heading[i] <- (as.numeric(length(a))/360)*100
    if(cumulative.heading[i] == 100){
      a<-0}
  }
  return(as.vector(cumulative.heading))
}

#(11)
#My arbitary cumulative rate change function for assessing 'significant' turns
#x = x = heading (0 to 360 degrees), thresh = threhold of turn angle, s = time period turn has to occur in
cumsum_with_reset <- function(x, thresh, s){
  cumsum.left = rep(0, length(x)) 
  cumsum.right = rep(0, length(x))
  s = as.numeric(s)
  heading<-as.numeric(x)
  Turn.reached<-rep(0, length(x)) 
  z<-rep(0, length(x)) ; y<-rep(0, length(x))
  thresh.L<-(thresh* -1) ; thresh.R<-thresh
  AVeY = c(0, diff(heading))
  AVeY <-ifelse(AVeY < -180, (AVeY + 360), AVeY)
  AVeY <-ifelse(AVeY > 180, (AVeY - 360), AVeY)
  AVeY.2 = c(AVeY, 0)
  for(i in 2:length(heading)){
    cumsum.left[i] <- cumsum.left[i-1] + AVeY[i]
    z[i]<-(z[i-1])+1
    if (cumsum.left[i] <= thresh.L & AVeY.2[i+1] > 0){ 
      cumsum.left[i-1] <- cumsum.left[i] ; cumsum.left[i]<-0
      Turn.reached[i]<- 1
      z[i]<-0
    }else if(cumsum.left[i] > 0){ 
      cumsum.left[i] <- 0
      z[i]<-0
    }else if((z[i] > s) & (AVeY[i] > 0)){
      cumsum.left[i] <- 0
      z[i]<-0}}
  for(i in 2:length(heading)){
    cumsum.right[i] <- cumsum.right[i-1] + AVeY[i]
    y[i]<-(y[i-1])+1
    if(cumsum.right[i] >= thresh.R & AVeY[i+1] < 0){ 
      cumsum.right[i-1] <- cumsum.right[i] ; cumsum.right[i]<-0
      Turn.reached[i]<- 1
      y[i]<- 0
    }else if(cumsum.right[i] < 0){ 
      cumsum.right[i] <- 0
      y[i]<- 0
    }else if((y[i] > s) & (AVeY[i] < 0)){
      cumsum.right[i] <- 0
      y[i]<- 0}}
  d<-as.data.frame(cbind(heading, cumsum.left, cumsum.right, Turn.reached))
  colnames(d)<-c("Rounded yaw (°)", "Left turn", "Right turn", "Turn point")
  return(d)} #i.e. a<-cumsum_with_reset(df$`Yaw (°)`, 30, 400)
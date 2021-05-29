#######Tilt compensated compass method with initial soft and hard iron distortion correction and optional tag rotation correction#########
##########################################################################################################################################
#+eval=FALSE 
#Raw tri-axial magnetometer data (mag.x,y,z)
#Tri-axial static acceleration data (acc.x,y,z) (for computation of pitch and roll)
#Input assumes North-East-Down (NED) coordinate system (measures + 1 g when pointing directly down) - Assume the x, y and z input fields for both magnetometry and accelerometery data represent the surge (forward-backward), sway (side-to-side) heave (up-down) dimensions of movement, respectively, which the user allocates the appropriate channel and/or required negation to (dependent on the local coordinate frame of the device used). 
#Marked events data (ME) specifying the period of the magnetic calibration period (as denoted by 'M' - any other input signifies data acquisition not part of calibration procedure). 
#Up to 8 method variants of correction to choose from. For rotated ellipsoids use method = 2 (more influenced by noise or more spherical) or method = 3 (default)
#For simple orthogonal re-scaling use method = 1. For non-rotated ellipsoids use method = 4, 5 (x & y axis equal), 6 (x & z axis equal), 7 (y & z axis are equal) or 8 (spherical data)
#The magnetic and gravity vectors are converted from the NED-carried device frame to the NED-carried animal's body frame via de-rotation according to the supplied Euler angles (pitch, roll and yaw; see section 3 of the main paper for details). Default assumes no offset
#Outputs: #Normalised tri-axial static acceleration data expressed in the animal's body frame (NGbx,y,z).
          #Calibrated tri-axial magnetometry data (Mx,y,z) 
          #Calibrated, normalised tri-axial magnetometry data expressed in the animal's body frame  (NMbx,y,z) 
          #Calibrated, normalised tri-axial magnetometry data expressed in the animal's body frame, after tilt-correction (NMbfx,y,z)
          #Marked events (ME)
          #Pitch
          #Roll
          #Yaw (heading - 0 to 360 degs)
#Summary plots of correction if plot = TRUE
#Method = 1 based on mathematical protocols outlined by here ; https://github.com/kriswiner/MPU6050/wiki/Simple-and-Effective-Magnetometer-Calibration ; Winer (2017)
#Method = 2 to 8 based on mathematical protocols outlined here; https://www.st.com/resource/en/design_tip/dm00286302-ellipsoid-or-sphere-fitting-for-sensor-calibration-stmicroelectronics.pdf ; Vitali (2016)

Gundog.Compass = function(mag.x, mag.y, mag.z, acc.x, acc.y, acc.z, ME, pitch.offset = 0, roll.offset = 0, yaw.offset = 0, method = 3, plot=TRUE){
  
  #Ensure ME calibration period is denoted
  if(any(which(ME == "M")) == FALSE){ 
    stop("The function stops - Ensure ME has a denoted calibration ('M') period")
  }
  
  if(any(is.na(mag.x) == TRUE) | any(is.na(mag.y) == TRUE) | any(is.na(mag.z) == TRUE) | any(is.na(acc.x) == TRUE) | any(is.na(acc.y) == TRUE) | any(is.na(acc.z) == TRUE)) {
    stop("The function stops - User has input data containing NA(s)")
  }
  if(plot == TRUE & Sys.info()["sysname"] == "Windows") { windows(width = 4.5, height = 15) } #Set graphics window if plot = TRUE (and operating system is windows)

  #Create 'main' data frame
  df = data.frame(mag.x, mag.y, mag.z, acc.x, acc.y, acc.z, ME) #Bind data into data frame
  
  #Subset data from magnetic calibration period
  df.cal = subset(df, df$ME == "M") #Subset to only include calibration period
  Bx = as.numeric(df.cal$mag.x) ; Bx = matrix(Bx, ncol=1)
  By = as.numeric(df.cal$mag.y) ; By = matrix(By, ncol=1)
  Bz = as.numeric(df.cal$mag.z) ; Bz = matrix(Bz, ncol=1)
  
  #Very simple orthogonal re-scaling and channel biases (method = 1)
  if(method == 1){ 
    Mx = df$mag.x ; My = df$mag.y ; Mz = df$mag.z
    #Calculate hard iron (HI) distortions
    HOx = 0.5 * (max(Bx) + min(Bx))  #average x mag bias                                                                                                    
    HOy = 0.5 * (max(By) + min(By))  #average y mag bias                                                                                                   
    HOz = 0.5 * (max(Bz) + min(Bz))  #average z mag bias                                                                                                              
    SOx = (max(Bx) - min(Bx)) / 2  #average x axis max chord length                                                                                                           
    SOy = (max(By) - min(By)) / 2  #average y axis max chord length                                                                                                             
    SOz = (max(Bz) - min(Bz)) / 2  #average z axis max chord length                                                                                                              
    S = (SOx + SOy + SOz) / 3                                                                                                                           
    df$Mx = (Mx - HOx) * (S/SOx) #Apply corrections                                                                                                                             
    df$My = (My - HOy) * (S/SOy)#Apply corrections                                                                                                                              
    df$Mz = (Mz - HOz) * (S/SOz) #Apply corrections 
    
    #Fit a rotated ellipsoid to a set of xyz data points
  } else if(method == 3) {
    D = matrix(c(Bx*Bx, By*By, Bz*Bz, 2*Bx*By, 2*Bx*Bz, 2*By*Bz, 2*Bx, 2*By, 2*Bz), byrow= FALSE, ncol= 9) #Construct 9 column matrix (data points not co-planar)
    #least square fitting using the pseudo-inverse of the non-square matrix
    v = solve(t(D) %*% D, t(D) %*% matrix(1, length(Bx), 1))  #solve(x,y) = Left matrix division #t(x) = Matrix transpose #%*% = Matrix product #ones(x) = 1 filled array
    A = matrix(c(v[1], v[4], v[5], v[7] , v[4], v[2], v[6], v[8], v[5], v[6], v[3], v[9], v[7], v[8], v[9], -1), byrow = TRUE, ncol = 4) #Matrix
    Off_set = solve(-A[1:3,1:3], matrix(c(v[7], v[8], v[9]), ncol = 1)) #offset is center of ellipsoid
    Tm = diag(4) #diag(x) = Identity matrix - diagonal of a matrix of dimensions 'x' (filled with 1)
    Tm[4,1:3] = t(Off_set) 
    AT = Tm %*% A %*% t(Tm)  #ellipsoid translated to (0,0,0)
    #Gains and cross-axis gains can be computed from eigenvalues and eigenvectors, respectively
    rotM = eigen(AT[1:3, 1:3] / -AT[4,4], symmetric = FALSE)$vectors[, 3:1] # eigenvectors (rotation) (matrix flipped left to right) #eigen(x)$vector = Eigen vector 
    ev = rev(eigen(AT[1:3, 1:3] / -AT[4,4], symmetric = FALSE)$values) #eigenvalues (gain) (vector reversed) #eigen(x)$values = Eigen values 
    gain=sqrt(1/ev)  #gain is the ellipsoid radius
    
    #Alternative implementation for near spherical data with little rotation or influenced by heavy noise
  }else if(method == 2){ 
    x2 = Bx*Bx ; y2 = By*By ; z2 = Bz*Bz
    D = matrix(c(x2+y2-2*z2, x2-2*y2+z2, 4*Bx*By, 2*Bx*Bz, 2*By*Bz, 2*Bx, 2*By, 2*Bz, matrix(1, length(Bx), 1)), byrow= FALSE, ncol= 9)
    R = matrix(x2 + y2 + z2, ncol=1)
    b = solve(t(D) %*% D, t(D) %*% R) #least square fitting
    mtx = matrix(c(3, 1, 1, 0, 0, 0, 0, 0, 0, 0, 3, 1, -2, 0, 0, 0, 0, 0, 0, 0, 3, -2, 1, 0, 0, 0, 0, 0, 0, 0,
                   0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
                   0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 
                   0, 0, 0, 0, 0, 0, 0, 0, 0, 1), byrow = TRUE, ncol = 10)
    v = mtx %*% rbind(-1/3, b)
    nn=v[10]
    v = -v[1:9]
    A = matrix(c(v[1], v[4], v[5], v[7] , v[4], v[2], v[6], v[8], v[5], v[6], v[3], v[9], v[7], v[8], v[9], -nn), byrow = TRUE, ncol = 4)
    Off_set = solve(-A[1:3,1:3], matrix(c(v[7], v[8], v[9]), ncol = 1)) #offset is center of ellipsoid
    Tm = diag(4) 
    Tm[4,1:3]= t(Off_set)
    AT = Tm %*% A %*% t(Tm)  #ellipsoid translated to (0,0,0)
    rotM = eigen(AT[1:3, 1:3] / -AT[4,4], symmetric = FALSE)$vectors[, 3:1] # eigenvectors (rotation) (matrix flipped left to right)
    ev = rev(eigen(AT[1:3, 1:3] / -AT[4,4], symmetric = FALSE)$values) #eigenvalues (gain) (vector reversed)
    gain=sqrt(1/ev)  #gain is the ellipsoid radius
  }
    #Non rotated ellipsoid / sphere
    else if(method == 4){ 
    D = matrix(c(Bx*Bx, By*By, Bz*Bz, 2*Bx, 2*By, 2*Bz), byrow = FALSE, ncol = 6) #non-rotated ellipsoids
    v = solve(t(D) %*% D, t(D) %*% matrix(1, length(Bx), 1))  
    v = matrix(c(v[1], v[2], v[3], 0, 0, 0, v[4], v[5], v[6]), byrow = TRUE, ncol = 9)
  } else if(method == 5){
    D = matrix(c(Bx*Bx+By*By, Bz*Bz, 2*Bx,  2*By, 2*Bz), byrow = FALSE, ncol = 5) #non-rotated ellipsoid, x & y axis equal
    v = solve(t(D) %*% D, t(D) %*% matrix(1, length(Bx), 1))  
    v = matrix(c(v[1], v[1], v[2], 0, 0, 0, v[3], v[4], v[5]), byrow = TRUE, ncol = 9)
  } else if(method == 6){
    D = matrix(c(Bx*Bx+Bz*Bz, By*By, 2*Bx,  2*By, 2*Bz), byrow = FALSE, ncol = 5) #non-rotated ellipsoid, x & z axis equal
    v = solve(t(D) %*% D, t(D) %*% matrix(1, length(Bx), 1)) 
    v = matrix(c(v[1], v[2], v[1], 0, 0, 0, v[3], v[4], v[5]), byrow = TRUE, ncol = 9)
  } else if(method == 7){
    D = matrix(c(By*By+Bz*Bz, Bx*Bx, 2*Bx,  2*By, 2*Bz), byrow = FALSE, ncol = 5) #non-rotated ellipsoid, y & z axis equal
    v = solve(t(D) %*% D, t(D) %*% matrix(1, length(Bx), 1))  
    v = matrix(c(v[2], v[1], v[1], 0, 0, 0, v[3], v[4], v[5]), byrow = TRUE, ncol = 9)
  } else if(method == 8){
    D = matrix(c(Bx*Bx+By*By+Bz*Bz, 2*Bx,  2*By, 2*Bz), byrow = FALSE, ncol = 4) #Spherical
    v = solve(t(D) %*% D, t(D) %*% matrix(1, length(Bx), 1))  
    v = matrix(c(v[1], v[1], v[1], 0, 0, 0, v[2], v[3], v[4]), byrow = TRUE, ncol = 9)
  }
  
  if(method == 4 | method == 5 | method == 6 |method == 7 |method == 8) {
    Off_set = t(-c(v[7:9])/ c(v[1:3])) #Left array division and then transpose
    rotM = diag(3) 
    g = 1+(v[7]^2/v[1]+v[8]^2/v[2]+v[9]^2/v[3]) 
    gain = t(sqrt(g/c(v[1:3]))) 
  }
  
  #Rotation matrix refinement --> largest element should be on diagonal
  #Least possible de-rotation a given  axis should be aligned with the nearest reference axis and no flipping should happen
  if(method == 2 | method == 3){
    if(max(abs(rotM)) > 0){
      rm = which(abs(rotM) == max(abs(rotM)), arr.ind = TRUE)[1] #Record row position of max absolute value
      cm = which(abs(rotM) == max(abs(rotM)), arr.ind = TRUE)[2] #Record column position of max absolute value
    }
    if(rm != cm){ # swap cols if not on diagonal
      t = rotM[, cm] ; rotM[, cm] = rotM[, rm] ; rotM[, rm] = t
      t = gain[cm] ; gain[cm] = gain[rm] ; gain[rm]= t #Change order of gain values if matrix order changed
    }
    # do the same on remaining 2x2 matrix
    if(rm == 1){k = c(2,3)} ; if(rm == 2){k = c(1,3)} ; if(rm == 3){k = c(1,2)}
    rm = 0 ; cm = 0 ; m = 0
    r = c(1:2) ; c = c(1:2)
    for(i in 1:length(r)){
      for (j in 1:length(c)){
        if(abs(rotM[k[i], k[j]]) > m){
          m = abs(rotM[k[i], k[j]]) ; rm = k[i] ; cm = k[j]
        }
      }
    }
    if(rm != cm){ #swap columns if not on the diagonal
      t = rotM[, cm] ; rotM[, cm] = rotM[, rm] ; rotM[, rm] = t
      t = gain[cm] ; gain[cm] = gain[rm] ; gain[rm]= t
    }
    #negate columns to make it positive along the diagonal
    if(rotM[1,1] < 0) {rotM[, 1]= -rotM[, 1]}
    if(rotM[2,2] < 0) {rotM[, 2]= -rotM[, 2]}
    if(rotM[3,3] < 0) {rotM[, 3]= -rotM[, 3]}
  
  }
  if(method != 1){
   #Correct offsets
   #All mag data
   Mx = as.numeric(df$mag.x) ; Mx = matrix(Mx, ncol=1)
   My = as.numeric(df$mag.y) ; My = matrix(My, ncol=1)
   Mz = as.numeric(df$mag.z) ; Mz = matrix(Mz, ncol=1)
   #Hard iron offset
   Mx = Mx - Off_set[1]; My = My - Off_set[2]; Mz = Mz - Off_set[3]
   MXYZ = matrix(c(Mx, My, Mz), byrow = FALSE, ncol = 3)
   MXYZ = MXYZ %*% rotM # rotate to XYZ axes
   ref = sqrt(Mx^2 + My^2 + Mz^2) #Reference radius
   gain = ifelse(is.nan(gain), 0, gain)
 
   #Scale to sphere (soft iron offsets)
   MXYZ[, 1] = MXYZ[, 1]/gain[1]*ref
   MXYZ[, 2] = MXYZ[, 2]/gain[2]*ref
   MXYZ[, 3] = MXYZ[, 3]/gain[3]*ref
  
   ##########################################
   MXYZ = MXYZ %*% t(rotM) # rotate data back #Here data is rotated back after corrections applied
   ########################################## #(may not always be wanted to rotate back)
   
   #Corrected magnetometer outputs
   df$Mx = MXYZ[, 1]
   df$My = MXYZ[, 2]
   df$Mz = MXYZ[, 3]
  }
  
  #Summary plots (before normalizing data) showing 2D dimensions of the xy, xz and zy planes
  if(plot == TRUE){
    par(mfrow=c(3,1))
    a <- subset(df, df$ME == "M")
    #Find dimension limits
    M <- rbind(a$Mx, a$mag.x, a$My, a$mag.y, a$Mz, a$mag.z) ; Mmin <- min(M, na.rm = T) ; Mmax <- max(M, na.rm = T)
    
    #xy
    plot(a$mag.x, a$mag.y, col ="red", main = "XY", xlab= "Mag.X", ylab= "Mag.Y", xlim = c(Mmin, Mmax), ylim = c(Mmin, Mmax))
    points(a$Mx, a$My, col="blue") 
    legend("topleft", legend=c("Uncorrected", "Corrected"),
           col=c("red", "blue"), lty=1:1, cex=1)
    points(x = 0, y = 0, col = "black", pch = 19, cex = 2)
    
    #xz
    plot(a$mag.x, a$mag.z, col ="red", main = "XZ", xlab= "Mag.X", ylab= "Mag.Z", xlim = c(Mmin, Mmax), ylim = c(Mmin, Mmax))
    points(a$Mx, a$Mz, col="blue") ; points(x = 0, y = 0, col = "black", pch = 19, cex = 2)
    
    #zy
    plot(a$mag.z, a$mag.y, col ="red", main = "ZY", xlab= "Mag.Z", ylab= "Mag.Y", xlim = c(Mmin, Mmax), ylim = c(Mmin, Mmax))
    points(a$Mz, a$My, col="blue") ; points(x = 0, y = 0, col = "black", pch = 19, cex = 2)
    
    par(mfrow=c(1,1))
  }
 
  #Normalize mag. and acc. data
  df$NMx = df$Mx / sqrt(df$Mx^2 + df$My^2 + df$Mz^2) #Corrected and normalized
  df$NMy = df$My / sqrt(df$Mx^2 + df$My^2 + df$Mz^2) #Corrected and normalized
  df$NMz = df$Mz / sqrt(df$Mx^2 + df$My^2 + df$Mz^2) #Corrected and normalized
  df$NGx = df$acc.x / sqrt(df$acc.x^2 + df$acc.y^2 + df$acc.z^2) #Normalized
  df$NGy = df$acc.y / sqrt(df$acc.x^2 + df$acc.y^2 + df$acc.z^2) #Normalized
  df$NGz = df$acc.z / sqrt(df$acc.x^2 + df$acc.y^2 + df$acc.z^2) #Normalized
  
  #If pitch, roll and/or yaw offsets are supplied, then de-rotate the normalized tag magnetic and gravity vectors to the animal body frame via:
  if(roll.offset != 0 | pitch.offset != 0 | yaw.offset != 0) {
    
    df$RollSinAngle = sin(roll.offset * pi/180)                                                                                        
    df$RollCosAngle = cos(roll.offset * pi/180)                                      
    df$PitchSinAngle = sin(pitch.offset * pi/180)                                    
    df$PitchCosAngle = cos(pitch.offset * pi/180)                                   
    df$YawSinAngle = sin(yaw.offset * pi/180)                                                                                                     
    df$YawCosAngle = cos(yaw.offset * pi/180)                                                                                                    
    df$NGbx =  df$NGx * df$YawCosAngle * df$PitchCosAngle + df$NGy * (df$YawCosAngle * df$PitchSinAngle * df$RollSinAngle - df$YawSinAngle * df$RollCosAngle) + df$NGz * (df$YawCosAngle * df$PitchSinAngle * df$RollCosAngle + df$YawSinAngle * df$RollSinAngle)
    df$NGby =  df$NGx * df$YawSinAngle * df$PitchCosAngle + df$NGy * (df$YawSinAngle * df$PitchSinAngle * df$RollSinAngle + df$YawCosAngle * df$RollCosAngle) +  df$NGz * (df$YawSinAngle * df$PitchSinAngle * df$RollSinAngle - df$YawCosAngle * df$RollSinAngle)
    df$NGbz =  -df$NGx * df$PitchSinAngle  +  df$NGy * df$PitchCosAngle * df$RollSinAngle +  df$NGz * df$PitchCosAngle * df$RollCosAngle
    df$NMbx =  df$NMx * df$YawCosAngle * df$PitchCosAngle + df$NMy * (df$YawCosAngle * df$PitchSinAngle * df$RollSinAngle - df$YawSinAngle * df$RollCosAngle) + df$NMz * (df$YawCosAngle * df$PitchSinAngle * df$RollCosAngle + df$YawSinAngle * df$RollSinAngle)
    df$NMby =  df$NMx * df$YawSinAngle * df$PitchCosAngle + df$NMy * (df$YawSinAngle * df$PitchSinAngle * df$RollSinAngle + df$YawCosAngle * df$RollCosAngle) +  df$NMz * (df$YawSinAngle * df$PitchSinAngle * df$RollSinAngle - df$YawCosAngle * df$RollSinAngle)
    df$NMbz =  -df$NMx * df$PitchSinAngle  +  df$NMy * df$PitchCosAngle * df$RollSinAngle +  df$NMz * df$PitchCosAngle * df$RollCosAngle
    
    df$RollSinAngle = NULL ; df$RollCosAngle = NULL
    df$PitchSinAngle = NULL ; df$PitchCosAngle = NULL
    df$YawSinAngle = NULL ; df$YawCosAngle = NULL
    
 }
  
  if(roll.offset == 0 & pitch.offset == 0 & yaw.offset == 0) {
    df$NGbx = df$NGx 
    df$NGby = df$NGy 
    df$NGbz = df$NGz 
    df$NMbx = df$NMx 
    df$NMby = df$NMy
    df$NMbz = df$NMz 
  }
  
  #Tilt compensated compass method using corrected mag readings
  #Calculate roll angle (-180deg, 180deg) and sin, cos
  df$sign  = ifelse(df$NGbz >= 0, 1, -1) # Value of 1 if Gz is non negative, or -1, if negative 
  miu = 0.01 #Add a tiny fraction into the denominator to prevent it being zero
  df$Roll  = atan2(df$NGby, df$sign*sqrt(df$NGbz*df$NGbz + miu*df$NGbx*df$NGbx))*180/pi
  df$RollSinAngle = sin(df$Roll * pi/180)
  df$RollCosAngle = cos(df$Roll * pi/180)
  #Calculate pitch angle Theta (-90deg, 90deg) and sin, cos
  df$Pitch = atan2(-df$NGbx, sqrt(df$NGby*df$NGby + df$NGbz*df$NGbz))*180/pi
  df$PitchSinAngle = sin(df$Pitch * pi/180)
  df$PitchCosAngle = cos(df$Pitch * pi/180)
  
  #De-rotate by pitch and roll
  df$NMbfx =  df$NMbx * df$PitchCosAngle + df$NMby * df$PitchSinAngle * df$RollSinAngle + df$NMbz * df$PitchSinAngle * df$RollCosAngle
  df$NMbfy = df$NMby * df$RollCosAngle  -  df$NMbz  * df$RollSinAngle 
  df$NMbfz = -df$NMbx * df$PitchSinAngle + df$NMby * df$PitchCosAngle * df$RollSinAngle  + df$NMbz * df$PitchCosAngle * df$RollCosAngle 
  
  #Calculate yaw = -180deg, +180deg
  df$Yaw = atan2(-df$NMbfy, df$NMbfx) * 180/pi
  df$Yaw = ifelse(df$Yaw < 0, df$Yaw + 360, df$Yaw) #yaw = 0 deg to +360deg

  df = df[, c(17:19, 8:10, 20:22, 30:32, 7, 27,24, 33)] #Reorder columns / remove uneccary inputs
  
  return(df)
  
}


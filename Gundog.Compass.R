#########################################################Gundog.Compass#######################################################

#######Tilt compensated compass method with optional initial soft and hard iron distortion correction and optional tag rotation correction#########
#Outputs angles (pitch, roll, yaw (heading)) computed either by using the standard euler angle approach, or via SAAM (Super-fast Attitude of Accelerometer and Magnetometer - using quaternions) - cf. Wu et al., (2018) [https://github.com/zarathustr/SAAM]

##########################################################################################################################################
#+eval=FALSE 

#Required packages installed: rgl

########################
#Required inputs:

#Raw tri-axial magnetometer data (mag.x,y,z)
#Tri-axial static acceleration data (acc.x,y,z) (for computation of pitch and roll) --> Note, these are not raw values - it is the static ('gravity') component of acceleration. One method to derive these values from their raw equivalent is to implement a running mean (e.g., 1-2 s in window length) per channel and use the resultant values
#Marked events data (ME) specifying the period of the magnetic calibration period (as denoted by 'M' - any other input signifies data acquisition not part of calibration procedure). The mangetic calibration period should involve rotating the device slowly ideally in an open space, away from potential sources of magnetic disturbance, relative to the Earth’s magnetic field.  Each orientation of roll, pitch and yaw should be incorporated in the device rotations (simply put, imagine a pen is attached to the end of the device being rotated and the aim is to ‘colour in’ all parts of a sphere). This section of data can then be used as a reference for the vectorial sum of magnetometry data across all three spatial dimensions, from which ‘hard’ and ‘soft iron’ errors which can occur in magnetometry data can be corrected. 

#Up to 8 method variants of correction to choose from. For rotated ellipsoids use method = 2 (more influenced by noise or more spherical) or method = 3 (default)
#For simple orthogonal re-scaling use method = 1. For non-rotated ellipsoids use method = 4, 5 (x & y axis equal), 6 (x & z axis equal), 7 (y & z axis are equal) or 8 (spherical data)
#As a starting point, I recommend method = 3 if a good calibration period is present. If the calibration period is not great (only 'some' of a sphere is covered when calibrating), then use method = 1.

#The magnetic and gravity vectors are converted from the NED-carried device frame to the NED-carried animal's body frame via de-rotation according to the supplied Euler angles (pitch, roll and yaw --> default values = 'pitch.offset' = 0, 'roll.offset' = 0, 'yaw.offset' = 0 (default assumes no offset). For example, a positive supplied pitch value of 45°, reflects that the device is pitched up from level by 45° and so the function de-rotates by this value. A positive supplied roll value reflects a bank angle device tilt to the right about this axis and a positive supplied yaw value reflects a clock-wise rotated offset.

#algorithm = "standard" (standard euler angle approach) or "SAAM" (using quaternions)

#####################acc.ref.frame  | positive.g | mag.ref.frame######################
#Computation of pitch, roll and yaw (heading) uses the right-handed North-East-Down (NED) coordinate system (measures + 1 g when pointing directly down) - This assumes that the x, y and z input fields for both magnetometry and accelerometery data represent the surge (forward-backward), sway (side-to-side) and heave (up-down) dimensions of movement.
#In line with the above point, #The NED system measures +1 g when facing directly downwards, -1 g when facing directly upwards and 0 g when orientated parallel to the earth's surface. Readings of the magnetometer x- and y-axis are at a maximum and minimum when they are pointed at magnetic North and South, respectively. Assuming that the orientation of the encapsulated device (positioned flat on a table) starts at North, readings of the y-axis will be at a maximum and minimum when the device is rotated 90 degrees East and 90 degrees West, respectively, with the z-axis remaining constant
#Some devices may have a different local coordinate frame, e.g., the END (East-North-Down) coordinate system assume the surge dimension of movement is represented by the z-channel, and the sway and heave dimension, by the x- and y-channels, respectively.

#################Changing coordinate frame of the data to the NED frame used here#################
#This function comes with three inputs to allow the user to transform their device's coordinate frame into the NED that the specific Direction Cosine Matrix (DCM) and quaternion augmentation uses to compute angles:'acc.ref.frame', 'positive.g', and 'mag.ref.frame'

#With the NED coordinate frame, the user should imagine that the x-, y, and z- axes of the device are the surge (which represents North/South), sway (which represents East/West) and heave (which represents Up/Down) dimensions of movement. If this is not the case, then acc.ref.frame and mag.ref.frame should be changed so that the function allocates the appropriate channel and/or required negation to readings
#All 48 combinations of coordinate frame configurations are possible to correct for to the NED system:
#acc.ref.frame (or mag.ref.frame) = "NED" | "NEU" | "NWD" | "NWU" | "SED" | "SEU" | "SWD" | "SWU"
                                   #"DEN" | "DES" | "DWN" | "DWS" | "UEN" | "UES" | "UWN" | "UWS"
                                   #"END" | "ENU" | "ESD" | "ESU" | "WND" | "WNU" | "WSD" | "WSU"
                                   #"NDE" | "NDW" | "NUE" | "NUW" | "SDE" | "SDW" | "SUE" | "SUW"
                                   #"DNE" | "DNW" | "DSE" | "DSW" | "UNE" | "UNW" | "USE" | "USW"
                                   #"EDN" | "EDS" | "EUN" | "EUS" | "WDN" | "WDS" | "WUN" | "WUS"
#Essentially, always assume that the x-axis of your device is your index finger, and your middle finger is always the y-axis of the device and your thumb is always the z-axis.
#Then hold up your two fingers and thumb perpendicular (at right-angles to one another) with either your left or right hand to make the shape of your device coordinate system (use which ever hand that can make the shape)
#Starting with the index ('x'-axis) finger, where is it pointing? North, East, South, West, Up, Down? 
#Then, where is your middle ('y'-axis) finger pointing? North, East, South, West, Up, Down?
#Lastly, where is your thumb ('z'-axis) pointing? North, East, South, West, Up, Down?
#Take the first letter of the given direction (N, E, S, W, U, D) that each of these fingers/thumb are pointing in the order stated above, and that is the local coordinate system of your device
#If the magnetometer channels are not aligned to that of the accelerometer channels, then repeat the above process
#A good tip if the device configuration is unknown, is to position the device as it would be on the animal, and starting at North, slowly pitch the device down and up three times, then roll the device left and right three times. Do the same at the other cardinal directions (E, W, S), and finish by circling three times, starting and finishing at North.
#Then see which channel has most sinusoidal wave output when pitching and when rolling. That will reflect the surge and sway dimensions, respectively. The sinusoidal waves of two of the magnetometer channels during circling will reveal the surge and sway dimensions of the magentometry configuration (with the channel that records either highest or lowest values when pointing directly North (depending on if it is pointing directly towards or against Magnetic North) being the surge dimension.
#For example, a DSE coordinate frame requires you with your left hand to point your index finger down, your middle finger towards your body, and your thumb pointing East. This therefore assumes that the surge axis is represented by the y-axis pointing towards South (away from the head of the body (or, in other words, towards your body)). The sway axis is represented by the z-channel, pointing East (towards the right of the body), and the heave axis is represented by the x-channel, pointing down (towards the ground).
#Some devices measure +1 g when a channel points directly towards the gravity vector (upwards), while others measure -1 g. So that any required negations of values are correct, the user needs to specify positive.g = "up" or positive.g = "down". In the above calibration tip, when pitching down first, are values of the channel reflecting the surge dimension increasing in g or decreasing in g. If increasing, then positive.g = "down"

#plot = TRUE - Inspection plots of corrected tri-axial magnetometery data provided

##############################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################
#Outputs: Depending on input, output can include:

#Normalized tri-axial static acceleration data expressed in the animal's body frame (NGbx,y,z).
#Calibrated tri-axial magnetometer data (Mx,y,z) 
#Calibrated, normalized tri-axial magnetometer data expressed in the animal's body frame  (NMbx,y,z) 
#Calibrated, normalized tri-axial magnetometer data expressed in the animal's body frame, after tilt-correction (NMbfx,y,z) - If method = "standard"
#q.w, q.x, q.y, q.z (Unitary (normalised) Quaternions) - (otherwise known as q0, q1, q2, q3) - If method = "SAAM"
#Marked events (ME)
#Pitch
#Roll
#Yaw (heading - 0 to 360 degs)
#Summary plots of correction if plot = TRUE (and method is not = 0)
#Method = 1 based on mathematical protocols outlined by here ; https://github.com/kriswiner/MPU6050/wiki/Simple-and-Effective-Magnetometer-Calibration ; Winer (2017)
#Method = 2 to 8 based on mathematical protocols outlined here; https://www.st.com/resource/en/design_tip/dm00286302-ellipsoid-or-sphere-fitting-for-sensor-calibration-stmicroelectronics.pdf ; Vitali (2016)
#If method = 0, then it is assumed the raw mag data is already corrected, and only pitch, roll and heading is computed. Only pitch, roll and heading (and quats if method = "SAAM") are returned
#If algorithm = "standard", then typical tilt-compensated compass approach using Euler angles. Otherwise if "SAAM", then the Super-fast attitude from accelerometer and magnetometer algorithm is used (simplified version of Davenport's solution for solving Wahba's problem with the magnetic and gravitational reference vectors)

##############################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################

#########Start of function######

Gundog.Compass = function(mag.x, mag.y, mag.z, acc.x, acc.y, acc.z, 
                          ME, acc.ref.frame = "NED", positive.g = "down", mag.ref.frame = "NED", 
                          pitch.offset = 0, roll.offset = 0, yaw.offset = 0, 
                          method = 3, algorithm = "standard", plot = TRUE){
  
  if(any(is.na(mag.x) == TRUE) | any(is.na(mag.y) == TRUE) | any(is.na(mag.z) == TRUE) | any(is.na(acc.x) == TRUE) | any(is.na(acc.y) == TRUE) | any(is.na(acc.z) == TRUE)) {
    stop("The function stops - User has input data containing NA(s)")
  }
  
  #Coordinate frame transformation to the NED if required for both acceleration and magnetic vectors#
  
  ax = acc.x ; ay = acc.y ; az = acc.z ; mx = mag.x ; my = mag.y ; mz = mag.z
  
  #North-East-Down ; +g against the gravity vector (up)
   if(acc.ref.frame == "NED" & positive.g == "up"){
    acc.x = -ax
    acc.y = -ay
    acc.z = -az
  }
  #North-East-Down ; +g with the gravity vector (down)
  if(acc.ref.frame == "NED" & positive.g == "down"){
    acc.x = ax
    acc.y = ay
    acc.z = az
  }
  #North-East-Down ; magnetism
  if(mag.ref.frame == "NED"){
    mag.x = mx
    mag.y = my
    mag.z = mz
  }
  
  ########################################################
  
  #North-East-Up ; +g against the gravity vector (up)
  if(acc.ref.frame == "NEU" & positive.g == "up"){
    acc.x = -ax
    acc.y = -ay
    acc.z = az
  }
  #North-East-Up ; +g with the gravity vector (down)
  if(acc.ref.frame == "NEU" & positive.g == "down"){
    acc.x = ax
    acc.y = ay
    acc.z = -az
  }
  #North-East-Up ; magnetism
  if(mag.ref.frame == "NEU"){
    mag.x = mx
    mag.y = my
    mag.z = -mz
  }
  
  ########################################################
  
  #North-West-Down ; +g against the gravity vector (up)
  if(acc.ref.frame == "NWD" & positive.g == "up"){
    acc.x = -ax
    acc.y = ay
    acc.z = -az
  }
  #North-West-Down ; +g with the gravity vector (down)
  if(acc.ref.frame == "NWD" & positive.g == "down"){
    acc.x = ax
    acc.y = -ay
    acc.z = az
  }
  #North-West-Down ; magnetism
  if(mag.ref.frame == "NWD"){
    mag.x = mx
    mag.y = -my
    mag.z = mz
  }
  
  ########################################################
  
  #North-West-Up ; +g against the gravity vector (up)
  if(acc.ref.frame == "NWU" & positive.g == "up"){
    acc.x = -ax
    acc.y = ay
    acc.z = az
  }
  #North-West-Up ; +g with the gravity vector (down)
  if(acc.ref.frame == "NWU" & positive.g == "down"){
    acc.x = ax
    acc.y = -ay
    acc.z = -az
  }
  #North-West-Up ; magnetism
  if(mag.ref.frame == "NWU"){
    mag.x = mx
    mag.y = -my
    mag.z = -mz
  }
  
  ########################################################
  
  #South-East-Down ; +g against the gravity vector (up)
  if(acc.ref.frame == "SED" & positive.g == "up"){
    acc.x = ax
    acc.y = -ay
    acc.z = -az
  }
  #South-East-Down ; +g with the gravity vector (down)
  if(acc.ref.frame == "SED" & positive.g == "down"){
    acc.x = -ax
    acc.y = ay
    acc.z = az
  }
  #South-East-Down ; magnetism
  if(mag.ref.frame == "SED"){
    mag.x = -mx
    mag.y = my
    mag.z = mz
  }
  
  ########################################################
  
  #South-East-Up ; +g against the gravity vector (up)
  if(acc.ref.frame == "SEU" & positive.g == "up"){
    acc.x = ax
    acc.y = -ay
    acc.z = az
  }
  #South-East-Up ; +g with the gravity vector (down)
  if(acc.ref.frame == "SEU" & positive.g == "down"){
    acc.x = -ax
    acc.y = ay
    acc.z = -az
  }
  #South-East-Up ; magnetism
  if(mag.ref.frame == "SEU"){
    mag.x = -mx
    mag.y = my
    mag.z = -mz
  }
  
  ########################################################
  
  #South-West-Down ; +g against the gravity vector (up)
  if(acc.ref.frame == "SWD" & positive.g == "up"){
    acc.x = ax
    acc.y = ay
    acc.z = -az
  }
  #South-West-Down ; +g with the gravity vector (down)
  if(acc.ref.frame == "SWD" & positive.g == "down"){
    acc.x = -ax
    acc.y = -ay
    acc.z = az
  }
  #South-West-Down ; magnetism
  if(mag.ref.frame == "SWD"){
    mag.x = -mx
    mag.y = -my
    mag.z = mz
  }
  
  ########################################################
  
  #South-West-Up ; +g against the gravity vector (up)
  if(acc.ref.frame == "SWU" & positive.g == "up"){
    acc.x = ax
    acc.y = ay
    acc.z = az
  }
  #South-West-Up ; +g with the gravity vector (down)
  if(acc.ref.frame == "SWU" & positive.g == "down"){
    acc.x = -ax
    acc.y = -ay
    acc.z = -az
  }
  #South-West-Up ; magnetism
  if(mag.ref.frame == "SWU"){
    mag.x = -mx
    mag.y = -my
    mag.z = -mz
  }
  
  ########################################################
  
  #Down-East-North ; +g against the gravity vector (up)
  if(acc.ref.frame == "DEN" & positive.g == "up"){
    acc.x = -az
    acc.y = -ay
    acc.z = -ax
  }
  #Down-East-North ; +g with the gravity vector (down)
  if(acc.ref.frame == "DEN" & positive.g == "down"){
    acc.x = az
    acc.y = ay
    acc.z = ax
  }
  #Down-East-North ; magnetism
  if(mag.ref.frame == "DEN"){
    mag.x = mz
    mag.y = my
    mag.z = mx
  }
  
  ########################################################
  
  #Down-East-South ; +g against the gravity vector (up)
  if(acc.ref.frame == "DES" & positive.g == "up"){
    acc.x = az
    acc.y = -ay
    acc.z = -ax
  }
  #Down-East-South ; +g with the gravity vector (down)
  if(acc.ref.frame == "DES" & positive.g == "down"){
    acc.x = -az
    acc.y = ay
    acc.z = ax
  }
  #Down-East-South ; magnetism
  if(mag.ref.frame == "DES"){
    mag.x = -mz
    mag.y = my
    mag.z = mx
  }
  
  ########################################################
  
  #Down-West-North ; +g against the gravity vector (up)
  if(acc.ref.frame == "DWN" & positive.g == "up"){
    acc.x = -az
    acc.y = ay
    acc.z = -ax
  }
  #Down-West-North ; +g with the gravity vector (down)
  if(acc.ref.frame == "DWN" & positive.g == "down"){
    acc.x = az
    acc.y = -ay
    acc.z = ax
  }
  #Down-West-North ; magnetism
  if(mag.ref.frame == "DWN"){
    mag.x = mz
    mag.y = -my
    mag.z = mx
  }
  
  ########################################################
  
  #Down-West-South ; +g against the gravity vector (up)
  if(acc.ref.frame == "DWS" & positive.g == "up"){
    acc.x = az
    acc.y = ay
    acc.z = -ax
  }
  #Down-west-South ; +g with the gravity vector (down)
  if(acc.ref.frame == "DWS" & positive.g == "down"){
    acc.x = -az
    acc.y = -ay
    acc.z = ax
  }
  #Down-West-South ; magnetism
  if(mag.ref.frame == "DWS"){
    mag.x = -mz
    mag.y = -my
    mag.z = mx
  }
  
  ########################################################
  
  #Up-East-North ; +g against the gravity vector (up)
  if(acc.ref.frame == "UEN" & positive.g == "up"){
    acc.x = -az
    acc.y = -ay
    acc.z = ax
  }
  #Up-East-North ; +g with the gravity vector (down)
  if(acc.ref.frame == "UEN" & positive.g == "down"){
    acc.x = az
    acc.y = ay
    acc.z = -ax
  }
  #Up-East-North ; magnetism
  if(mag.ref.frame == "UEN"){
    mag.x = mz
    mag.y = my
    mag.z = -mx
  }
  
  ########################################################
  
  #Up-East-South ; +g against the gravity vector (up)
  if(acc.ref.frame == "UES" & positive.g == "up"){
    acc.x = az
    acc.y = -ay
    acc.z = ax
  }
  #Up-East-South ; +g with the gravity vector (down)
  if(acc.ref.frame == "UES" & positive.g == "down"){
    acc.x = -az
    acc.y = ay
    acc.z = -ax
  }
  #Up-East-South ; magnetism
  if(mag.ref.frame == "UES"){
    mag.x = -mz
    mag.y = my
    mag.z = -mx
  }
  
  ########################################################
  
  #Up-West-North ; +g against the gravity vector (up)
  if(acc.ref.frame == "UWN" & positive.g == "up"){
    acc.x = -az
    acc.y = ay
    acc.z = ax
  }
  #Up-West-North ; +g with the gravity vector (down)
  if(acc.ref.frame == "UWN" & positive.g == "down"){
    acc.x = az
    acc.y = -ay
    acc.z = -ax
  }
  #Up-West-North ; magnetism
  if(mag.ref.frame == "UWN"){
    mag.x = mz
    mag.y = -my
    mag.z = -mx
  }
  
  ########################################################
  
  #Up-West-South ; +g against the gravity vector (up)
  if(acc.ref.frame == "UWS" & positive.g == "up"){
    acc.x = az
    acc.y = ay
    acc.z = ax
  }
  #Up-West-South ; +g with the gravity vector (down)
  if(acc.ref.frame == "UWS" & positive.g == "down"){
    acc.x = -az
    acc.y = -ay
    acc.z = -ax
  }
  #Up-West-South ; magnetism
  if(mag.ref.frame == "UWS"){
    mag.x = -mz
    mag.y = -my
    mag.z = -mx
  }
  
  ########################################################
  
  #East-North-Down ; +g against the gravity vector (up)
  if(acc.ref.frame == "END" & positive.g == "up"){
    acc.x = -ay
    acc.y = -ax
    acc.z = -az
  }
  #East-North-Down ; +g with the gravity vector (down)
  if(acc.ref.frame == "END" & positive.g == "down"){
    acc.x = ay
    acc.y = ax
    acc.z = az
  }
  #East-North-Down ; magnetism
  if(mag.ref.frame == "END"){
    mag.x = my
    mag.y = mx
    mag.z = mz
  }
  
  ########################################################
  
  #East-North-Up ; +g against the gravity vector (up)
  if(acc.ref.frame == "ENU" & positive.g == "up"){
    acc.x = -ay
    acc.y = -ax
    acc.z = az
  }
  #East-North-Up ; +g with the gravity vector (down)
  if(acc.ref.frame == "ENU" & positive.g == "down"){
    acc.x = ay
    acc.y = ax
    acc.z = -az
  }
  #East-North-Up ; magnetism
  if(mag.ref.frame == "ENU"){
    mag.x = my
    mag.y = mx
    mag.z = -mz
  }
  
  ########################################################
  
  #East-South-Down ; +g against the gravity vector (up)
  if(acc.ref.frame == "ESD" & positive.g == "up"){
    acc.x = ay
    acc.y = -ax
    acc.z = -az
  }
  #East-South-Down ; +g with the gravity vector (down)
  if(acc.ref.frame == "ESD" & positive.g == "down"){
    acc.x = -ay
    acc.y = ax
    acc.z = az
  }
  #East-South-Down ; magnetism
  if(mag.ref.frame == "ESD"){
    mag.x = -my
    mag.y = mx
    mag.z = mz
  }
  
  ########################################################
  
  #East-South-Up ; +g against the gravity vector (up)
  if(acc.ref.frame == "ESU" & positive.g == "up"){
    acc.x = ay
    acc.y = -ax
    acc.z = az
  }
  #East-South-Up ; +g with the gravity vector (down)
  if(acc.ref.frame == "ESU" & positive.g == "down"){
    acc.x = -ay
    acc.y = ax
    acc.z = -az
  }
  #East-South-Up ; magnetism
  if(mag.ref.frame == "ESU"){
    mag.x = -my
    mag.y = mx
    mag.z = -mz
  }
  
  ########################################################
  
  #West-North-Down ; +g against the gravity vector (up)
  if(acc.ref.frame == "WND" & positive.g == "up"){
    acc.x = -ay
    acc.y = ax
    acc.z = -az
  }
  #West-North-Down ; +g with the gravity vector (down)
  if(acc.ref.frame == "WND" & positive.g == "down"){
    acc.x = ay
    acc.y = -ax
    acc.z = az
  }
  #West-North-Down ; magnetism
  if(mag.ref.frame == "WND"){
    mag.x = my
    mag.y = -mx
    mag.z = mz
  }
  
  ########################################################
  
  #West-North-Up ; +g against the gravity vector (up)
  if(acc.ref.frame == "WNU" & positive.g == "up"){
    acc.x = -ay
    acc.y = ax
    acc.z = az
  }
  #West-North-Up ; +g with the gravity vector (down)
  if(acc.ref.frame == "WNU" & positive.g == "down"){
    acc.x = ay
    acc.y = -ax
    acc.z = -az
  } 
  #West-North-Up ; magnetism
  if(mag.ref.frame == "WNU"){
    mag.x = my
    mag.y = -mx
    mag.z = -mz
  }
  
  ########################################################
  
  #West-South-Down ; +g against the gravity vector (up)
  if(acc.ref.frame == "WSD" & positive.g == "up"){
    acc.x = ay
    acc.y = ax
    acc.z = -az
  }
  #West-South-Down ; +g with the gravity vector (down)
  if(acc.ref.frame == "WSD" & positive.g == "down"){
    acc.x = -ay
    acc.y = -ax
    acc.z = az
  } 
  #West-South-Down ; magnetism
  if(mag.ref.frame == "WSD"){
    mag.x = -my
    mag.y = -mx
    mag.z = mz
  }
  
  ########################################################
  
  #West-South-Up ; +g against the gravity vector (up)
  if(acc.ref.frame == "WSU" & positive.g == "up"){
    acc.x = ay
    acc.y = ax
    acc.z = az
  }
  #West-South-Up ; +g with the gravity vector (down)
  if(acc.ref.frame == "WSU" & positive.g == "down"){
    acc.x = -ay
    acc.y = -ax
    acc.z = -az
  } 
  #West-South-Up ; magnetism
  if(mag.ref.frame == "WSU"){
    mag.x = -my
    mag.y = -mx
    mag.z = -mz
  }
  
  ########################################################
  
  #North-Down-East ; +g against the gravity vector (up)
  if(acc.ref.frame == "NDE" & positive.g == "up"){
    acc.x = -ax
    acc.y = -az
    acc.z = -ay
  }
  #North-Down-East ; +g with the gravity vector (down)
  if(acc.ref.frame == "NDE" & positive.g == "down"){
    acc.x = ax
    acc.y = az
    acc.z = ay
  }
  #North-Down-East ; magnetism
  if(mag.ref.frame == "NDE"){
    mag.x = mx
    mag.y = mz
    mag.z = my
  }
  
  ########################################################
  
  #North-Down-West ; +g against the gravity vector (up)
  if(acc.ref.frame == "NDW" & positive.g == "up"){
    acc.x = -ax
    acc.y = az
    acc.z = -ay
  }
  #North-Down-West ; +g with the gravity vector (down)
  if(acc.ref.frame == "NDW" & positive.g == "down"){
    acc.x = ax
    acc.y = -az
    acc.z = ay
  }
  #North-Down-West ; magnetism
  if(mag.ref.frame == "NDW"){
    mag.x = mx
    mag.y = -mz
    mag.z = my
  }
  
  ########################################################
  
  #North-Up-East ; +g against the gravity vector (up)
  if(acc.ref.frame == "NUE" & positive.g == "up"){
    acc.x = -ax
    acc.y = -az
    acc.z = ay
  }
  #North-Up-East ; +g with the gravity vector (down)
  if(acc.ref.frame == "NUE" & positive.g == "down"){
    acc.x = ax
    acc.y = az
    acc.z = -ay
  }
  #North-Up-East ; magnetism
  if(mag.ref.frame == "NUE"){
    mag.x = mx
    mag.y = mz
    mag.z = -my
  }
  
  ########################################################
  
  #North-Up-West ; +g against the gravity vector (up)
  if(acc.ref.frame == "NUW" & positive.g == "up"){
    acc.x = -ax
    acc.y = az
    acc.z = ay
  }
  #North-Up-West ; +g with the gravity vector (down)
  if(acc.ref.frame == "NUW" & positive.g == "down"){
    acc.x = ax
    acc.y = -az
    acc.z = -ay
  }
  #North-Up-West ; magnetism
  if(mag.ref.frame == "NUW"){
    mag.x = mx
    mag.y = -mz
    mag.z = -my
  }
  
  ########################################################
  
  #South-Down-East ; +g against the gravity vector (up)
  if(acc.ref.frame == "SDE" & positive.g == "up"){
    acc.x = ax
    acc.y = -az
    acc.z = -ay
  }
  #South-Down-East ; +g with the gravity vector (down)
  if(acc.ref.frame == "SDE" & positive.g == "down"){
    acc.x = -ax
    acc.y = az
    acc.z = ay
  }
  #South-Down-East ; magnetism
  if(mag.ref.frame == "SDE"){
    mag.x = -mx
    mag.y = mz
    mag.z = my
  }
  
  ########################################################
  
  #South-Down-West ; +g against the gravity vector (up)
  if(acc.ref.frame == "SDW" & positive.g == "up"){
    acc.x = ax
    acc.y = az
    acc.z = -ay
  }
  #South-Down-West ; +g with the gravity vector (down)
  if(acc.ref.frame == "SDW" & positive.g == "down"){
    acc.x = -ax
    acc.y = -az
    acc.z = ay
  }
  #South-Down-West ; magnetism
  if(mag.ref.frame == "SDW"){
    mag.x = -mx
    mag.y = -mz
    mag.z = my
  }
  
  ########################################################
  
  #South-Up-East ; +g against the gravity vector (up)
  if(acc.ref.frame == "SUE" & positive.g == "up"){
    acc.x = ax
    acc.y = -az
    acc.z = ay
  }
  #South-Up-East ; +g with the gravity vector (down)
  if(acc.ref.frame == "SUE" & positive.g == "down"){
    acc.x = -ax
    acc.y = az
    acc.z = -ay
  }
  #South-Up-East ; magnetism
  if(mag.ref.frame == "SUE"){
    mag.x = -mx
    mag.y = mz
    mag.z = -my
  }
  
  ########################################################
  
  #South-Up-West ; +g against the gravity vector (up)
  if(acc.ref.frame == "SUW" & positive.g == "up"){
    acc.x = ax
    acc.y = az
    acc.z = ay
  }
  #South-Up-West ; +g with the gravity vector (down)
  if(acc.ref.frame == "SUW" & positive.g == "down"){
    acc.x = -ax
    acc.y = -az
    acc.z = -ay
  }
  #South-Up-West ; magnetism
  if(mag.ref.frame == "SUW"){
    mag.x = -mx
    mag.y = -mz
    mag.z = -my
  }
  
  ########################################################
  
  #Down-North-East ; +g against the gravity vector (up)
  if(acc.ref.frame == "DNE" & positive.g == "up"){
    acc.x = -ay
    acc.y = -az
    acc.z = -ax
  }
  #Down-North-East ; +g with the gravity vector (down)
  if(acc.ref.frame == "DNE" & positive.g == "down"){
    acc.x = ay
    acc.y = az
    acc.z = ax
  }
  #Down-North-East ; magnetism
  if(mag.ref.frame == "DNE"){
    mag.x = my
    mag.y = mz
    mag.z = mx
  }
  
  ########################################################
  
  #Down-North-West ; +g against the gravity vector (up)
  if(acc.ref.frame == "DNW" & positive.g == "up"){
    acc.x = -ay
    acc.y = az
    acc.z = -ax
  }
  #Down-North-West ; +g with the gravity vector (down)
  if(acc.ref.frame == "DNW" & positive.g == "down"){
    acc.x = ay
    acc.y = -az
    acc.z = ax
  }
  #Down-North-West ; magnetism
  if(mag.ref.frame == "DNW"){
    mag.x = my
    mag.y = -mz
    mag.z = mx
  }
  
  ########################################################
  
  #Down-South-East ; +g against the gravity vector (up)
  if(acc.ref.frame == "DSE" & positive.g == "up"){
    acc.x = ay
    acc.y = -az
    acc.z = -ax
  }
  #Down-South-East ; +g with the gravity vector (down)
  if(acc.ref.frame == "DSE" & positive.g == "down"){
    acc.x = -ay
    acc.y = az
    acc.z = ax
  }
  #Down-South-East ; magnetism
  if(mag.ref.frame == "DSE"){
    mag.x = -my
    mag.y = mz
    mag.z = mx
  }
  
  ########################################################
  
  #Down-South-West ; +g against the gravity vector (up)
  if(acc.ref.frame == "DSW" & positive.g == "up"){
    acc.x = ay
    acc.y = az
    acc.z = -ax
  }
  #Down-South-West ; +g with the gravity vector (down)
  if(acc.ref.frame == "DSW" & positive.g == "down"){
    acc.x = -ay
    acc.y = -az
    acc.z = ax
  }
  #Down-South-West ; magnetism
  if(mag.ref.frame == "DSW"){
    mag.x = -my
    mag.y = -mz
    mag.z = mx
  }
  
  ########################################################
  
  #Up-North-East ; +g against the gravity vector (up)
  if(acc.ref.frame == "UNE" & positive.g == "up"){
    acc.x = -ay
    acc.y = -az
    acc.z = ax
  }
  #Up-North-East ; +g with the gravity vector (down)
  if(acc.ref.frame == "UNE" & positive.g == "down"){
    acc.x = ay
    acc.y = az
    acc.z = -ax
  }
  #Up-North-East ; magnetism
  if(mag.ref.frame == "UNE"){
    mag.x = my
    mag.y = mz
    mag.z = -mx
  }
  
  ########################################################
  
  #Up-North-West ; +g against the gravity vector (up)
  if(acc.ref.frame == "UNW" & positive.g == "up"){
    acc.x = -ay
    acc.y = az
    acc.z = ax
  }
  #Up-North-West ; +g with the gravity vector (down)
  if(acc.ref.frame == "UNW" & positive.g == "down"){
    acc.x = ay
    acc.y = -az
    acc.z = -ax
  }
  #Up-North-West ; magnetism
  if(mag.ref.frame == "UNW"){
    mag.x = my
    mag.y = -mz
    mag.z = -mx
  }
  
  ########################################################
  
  #Up-South-East ; +g against the gravity vector (up)
  if(acc.ref.frame == "USE" & positive.g == "up"){
    acc.x = ay
    acc.y = -az
    acc.z = ax
  }
  #Up-South-East ; +g with the gravity vector (down)
  if(acc.ref.frame == "USE" & positive.g == "down"){
    acc.x = -ay
    acc.y = az
    acc.z = -ax
  }
  #Up-South-East ; magnetism
  if(mag.ref.frame == "USE"){
    mag.x = -my
    mag.y = mz
    mag.z = -mx
  }
  
  ########################################################
  
  #Up-South-West ; +g against the gravity vector (up)
  if(acc.ref.frame == "USW" & positive.g == "up"){
    acc.x = ay
    acc.y = az
    acc.z = ax
  }
  #Up-South-West ; +g with the gravity vector (down)
  if(acc.ref.frame == "USW" & positive.g == "down"){
    acc.x = -ay
    acc.y = -az
    acc.z = -ax
  }
  #Up-South-West ; magnetism
  if(mag.ref.frame == "USW"){
    mag.x = -my
    mag.y = -mz
    mag.z = -mx
  }
  
  ########################################################
  
  #East-Down-North ; +g against the gravity vector (up)
  if(acc.ref.frame == "EDN" & positive.g == "up"){
    acc.x = -az
    acc.y = -ax
    acc.z = -ay
  }
  #East-Down-North ; +g with the gravity vector (down)
  if(acc.ref.frame == "EDN" & positive.g == "down"){
    acc.x = az
    acc.y = ax
    acc.z = ay
  }
  #East-Down-North ; magnetism
  if(mag.ref.frame == "EDN"){
    mag.x = mz
    mag.y = mx
    mag.z = my
  }
  
  ########################################################
  
  #East-Down-South ; +g against the gravity vector (up)
  if(acc.ref.frame == "EDS" & positive.g == "up"){
    acc.x = az
    acc.y = -ax
    acc.z = -ay
  }
  #East-Down-South ; +g with the gravity vector (down)
  if(acc.ref.frame == "EDS" & positive.g == "down"){
    acc.x = -az
    acc.y = ax
    acc.z = ay
  }
  #East-Down-South ; magnetism
  if(mag.ref.frame == "EDS"){
    mag.x = -mz
    mag.y = mx
    mag.z = my
  }
  
  ########################################################
  
  #East-Up-North ; +g against the gravity vector (up)
  if(acc.ref.frame == "EUN" & positive.g == "up"){
    acc.x = -az
    acc.y = -ax
    acc.z = ay
  }
  #East-Up-North ; +g with the gravity vector (down)
  if(acc.ref.frame == "EUN" & positive.g == "down"){
    acc.x = az
    acc.y = ax
    acc.z = -ay
  }
  #East-Up-North ; magnetism
  if(mag.ref.frame == "EUN"){
    mag.x = mz
    mag.y = mx
    mag.z = -my
  }
  
  ########################################################
  
  #East-Up-South ; +g against the gravity vector (up)
  if(acc.ref.frame == "EUS" & positive.g == "up"){
    acc.x = az
    acc.y = -ax
    acc.z = ay
  }
  #East-Up-South ; +g with the gravity vector (down)
  if(acc.ref.frame == "EUS" & positive.g == "down"){
    acc.x = -az
    acc.y = ax
    acc.z = -ay
  }
  #East-Up-South ; magnetism
  if(mag.ref.frame == "EUS"){
    mag.x = -mz
    mag.y = mx
    mag.z = -my
  }
  
  ########################################################
  
  #West-Down-North ; +g against the gravity vector (up)
  if(acc.ref.frame == "WDN" & positive.g == "up"){
    acc.x = -az
    acc.y = ax
    acc.z = -ay
  }
  #West-Down-North ; +g with the gravity vector (down)
  if(acc.ref.frame == "WDN" & positive.g == "down"){
    acc.x = az
    acc.y = -ax
    acc.z = ay
  }
  #West-Down-North ; magnetism
  if(mag.ref.frame == "WDN"){
    mag.x = mz
    mag.y = -mx
    mag.z = my
  }
  
  ########################################################
  
  #West-Down-South ; +g against the gravity vector (up)
  if(acc.ref.frame == "WDS" & positive.g == "up"){
    acc.x = az
    acc.y = ax
    acc.z = -ay
  }
  #West-Down-South ; +g with the gravity vector (down)
  if(acc.ref.frame == "WDS" & positive.g == "down"){
    acc.x = -az
    acc.y = -ax
    acc.z = ay
  }
  #West-Down-South ; magnetism
  if(mag.ref.frame == "WDS"){
    mag.x = -mz
    mag.y = -mx
    mag.z = my
  }
  
  ########################################################
  
  #West-Up-North ; +g against the gravity vector (up)
  if(acc.ref.frame == "WUN" & positive.g == "up"){
    acc.x = -az
    acc.y = ax
    acc.z = ay
  }
  #West-Up-North ; +g with the gravity vector (down)
  if(acc.ref.frame == "WUN" & positive.g == "down"){
    acc.x = az
    acc.y = -ax
    acc.z = -ay
  }
  #West-Up-North ; magnetism
  if(mag.ref.frame == "WUN"){
    mag.x = mz
    mag.y = -mx
    mag.z = -my
  }
  
  ########################################################
  
  #West-Up-South ; +g against the gravity vector (up)
  if(acc.ref.frame == "WUS" & positive.g == "up"){
    acc.x = az
    acc.y = ax
    acc.z = ay
  }
  #West-Up-South ; +g with the gravity vector (down)
  if(acc.ref.frame == "WUS" & positive.g == "down"){
    acc.x = -az
    acc.y = -ax
    acc.z = -ay
  }
  #West-Up-South ; magnetism
  if(mag.ref.frame == "WUS"){
    mag.x = -mz
    mag.y = -mx
    mag.z = -my
  }
  
  ###################################################################################################################################################################################
  
  if(method > 0){
    #Ensure ME calibration period is denoted
    if(any(which(ME == "M")) == FALSE){ 
      stop("The function stops - Ensure ME has a denoted calibration ('M') period")
    }
    
    if(plot == TRUE & Sys.info()["sysname"] == "Windows") { windows(width = 4.5, height = 15) } #Set graphics window if plot = TRUE (and operating system is windows)
    
    ###################################################################################################################################################################################
   
     #Create 'main' data frame
    df = data.frame(mag.x, mag.y, mag.z, acc.x, acc.y, acc.z, ME) #Bind data into data frame
  }else{ df = data.frame(mag.x, mag.y, mag.z, acc.x, acc.y, acc.z) } #Bind data into data frame
  
  #Subset data from magnetic calibration period
  if(method > 0){
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
    if(method > 1){
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
      
      ###Required package rgl if plot = TRUE###
      if (!require('rgl')){ install.packages('rgl', dependencies = TRUE, type="source")} ; suppressMessages(require("rgl"))
      #Check that required package is installed on the system
      areinstaled=data.frame(installed.packages())
      if(all(c("rgl")%in%areinstaled$Package)==FALSE){
        required_packages=c("rgl")
        missing_packages=c("rgl")%in%areinstaled$Package
        stop(paste("The following packages are not installed:", required_packages[which(missing_packages==FALSE)], sep = " "))
      }
      
      #Base plotting parameters
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
      
      #3D interactive plot - see https://github.com/KiranLDA/PAMLr
      rgloptions = list("windowRect"=c(30, 30, 650, 650)) #Larger plotting window
      
      Interactive_sphere<- function(x ,y ,z,
                                    spherecolor = "cyan",
                                    linecolor = "black",
                                    linewidth = 2,
                                    ptcol = "orangered",
                                    ptsize = 0.01,
                                    arrows = TRUE,
                                    cex = 1.2,
                                    ...){
        open3d()
        do.call(rgl::par3d, rgloptions)
        spheres3d(0,0,0, radius = mean(sqrt(x^2 + y^2 + z^2)), lit = FALSE, color = spherecolor, alpha = 0.3)
        spheres3d(0,0,0, radius = mean(sqrt(x^2 + y^2 + z^2)), lit = FALSE, color = linecolor, front = "lines")
        spheres3d(x, y, z, radius=ptsize, col=ptcol,...)
        arrow3d(p0 = c(0, -mean(sqrt(x^2 + y^2 + z^2))*2 ,0), p1 = c(0, mean(sqrt(x^2 + y^2 + z^2))*2, 0), barblen = .06, s = 1/7, lwd = 0.08, type = "rotation")
        text3d(y = mean(sqrt(x^2 + y^2 + z^2))*2.2, x = 0, z = 0, texts = 'X', cex = cex)
        arrow3d(p0 = c(-mean(sqrt(x^2 + y^2 + z^2))*2, 0, 0), p1 = c(mean(sqrt(x^2 + y^2 + z^2))*2, 0, 0), barblen = .06, s = 1/7, lwd = 0.08, type = "rotation")
        text3d(x = mean(sqrt(x^2 + y^2 + z^2))*2.2, y = 0, z = 0, texts = 'Y', cex = cex)
        arrow3d(p0 = c(0, 0, mean(sqrt(x^2 + y^2 + z^2))*2), p1 = c(0, 0, -mean(sqrt(x^2 + y^2 + z^2))*2), barblen = .06, s = 1/7, lwd = 0.08, type = "rotation")
        text3d(z = -mean(sqrt(x^2 + y^2 + z^2))*2.2, y=0,x=0, texts = 'Z', cex = cex)
         bgplot3d({
          plot.new()
          title(main = 'Corrected Magnetometry Data', line = 2, cex = 1.5)
        })
        
      }
      
      #Plot the corrected mag data
      Interactive_sphere(x = a$Mx,
                         y = a$My,
                         z = a$Mz)
    }
  }
  
  #If method = 0..
  if(method == 0) {
    df$Mx = df$mag.x
    df$My = df$mag.y
    df$Mz = df$mag.z
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
    df$NGbx = df$NGx * df$YawCosAngle * df$PitchCosAngle + df$NGy * (df$YawCosAngle * df$PitchSinAngle * df$RollSinAngle - df$YawSinAngle * df$RollCosAngle) + df$NGz * (df$YawCosAngle * df$PitchSinAngle * df$RollCosAngle + df$YawSinAngle * df$RollSinAngle)
    df$NGby = df$NGx * df$YawSinAngle * df$PitchCosAngle + df$NGy * (df$YawSinAngle * df$PitchSinAngle * df$RollSinAngle + df$YawCosAngle * df$RollCosAngle) +  df$NGz * (df$YawSinAngle * df$PitchSinAngle * df$RollCosAngle - df$YawCosAngle * df$RollSinAngle)
    df$NGbz = -df$NGx * df$PitchSinAngle  +  df$NGy * df$PitchCosAngle * df$RollSinAngle +  df$NGz * df$PitchCosAngle * df$RollCosAngle
    df$NMbx = df$NMx * df$YawCosAngle * df$PitchCosAngle + df$NMy * (df$YawCosAngle * df$PitchSinAngle * df$RollSinAngle - df$YawSinAngle * df$RollCosAngle) + df$NMz * (df$YawCosAngle * df$PitchSinAngle * df$RollCosAngle + df$YawSinAngle * df$RollSinAngle)
    df$NMby = df$NMx * df$YawSinAngle * df$PitchCosAngle + df$NMy * (df$YawSinAngle * df$PitchSinAngle * df$RollSinAngle + df$YawCosAngle * df$RollCosAngle) +  df$NMz * (df$YawSinAngle * df$PitchSinAngle * df$RollCosAngle - df$YawCosAngle * df$RollSinAngle)
    df$NMbz = -df$NMx * df$PitchSinAngle  +  df$NMy * df$PitchCosAngle * df$RollSinAngle +  df$NMz * df$PitchCosAngle * df$RollCosAngle
    
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
  
  if(algorithm == "standard"){
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
    
    if(method > 0){ df = df[, c(17:19, 8:10, 20:22, 30:32, 7, 27,24, 33)] #Reorder columns / remove unnecessary inputs
    }else{ df = df[, c('Pitch', 'Roll', 'Yaw')] }
    
  }else{ #SAAM method - A Super Fast Attitude Determination Algorithm for Accelerometer and Magnetometer data
    
    mD = df$NGbx * df$NMbx + df$NGby * df$NMby + df$NGbz * df$NMbz
    mN = sqrt(1 - mD * mD)
    
    #Quaternions
    q0 = (df$NGbz - 1) * (mN + df$NMbx) + df$NGbx * (mD - df$NMbz)
    q1 = (df$NGbz - 1) * df$NMby + df$NGby * (mD - df$NMbz)
    q2 = df$NGbz * mD - df$NGbx * mN - df$NMbz
    q3 = -df$NGby * (mN + df$NMbx) + df$NGbx * df$NMby
    q = matrix(c(q0, q1, q2, q3), byrow = TRUE, nrow = 4)
    
    #Unitary - Normalize the quaternions
    quat.norm = function(x){ return(rep(sqrt(sum(x^2)))) }
    norm_q = matrix(c(rep(apply(q, 2, quat.norm), 4)), byrow = TRUE, nrow = 4)
    q = q / norm_q
    
    #invert if scalar is negative
    q[, c(which(q[1,] < 0)) ] <- q[, c(which(q[1,] < 0))] *-1
    
    #extract individual quaternion elements
    df$q.w = q[1 ,]
    df$q.x = q[2 ,]
    df$q.y = q[3 ,]
    df$q.z = q[4 ,]
    
    df$Roll = atan2(2*(df$q.x * df$q.y + df$q.w * df$q.z), df$q.y^2 + df$q.z^2 - df$q.w^2 - df$q.x^2)*180/pi
    df$Pitch = asin(-2*(df$q.w * df$q.y - df$q.x * df$q.z))*180/pi
    df$Yaw = atan2(2*(df$q.y * df$q.z + df$q.w * df$q.x), df$q.w^2 - df$q.x^2 - df$q.y^2 + df$q.z^2)*180/pi
    df$Yaw = ifelse(df$Yaw < 0, df$Yaw + 360, df$Yaw) #yaw = 0 deg to +360deg
    
   # if(method > 0){ df = df[, c('acc.x', 'acc.y', 'acc.z', 17:19, 8:10, 20:22, 7, 23:26, 28, 27, 29)] #Reorder columns / remove unnecessary inputs
    #}else{ df = df[, c('acc.x', 'acc.y', 'acc.z', 'q.w', 'q.x', 'q.y', 'q.z', 'Pitch', 'Roll', 'Yaw')] }
  }      
  
  return(df)
  
}


#E.g_1 --> df.corr = Gundog.Compass(mag.x = df$Mag_x.sm, mag.y = df$Mag_y.sm, mag.z = df$Mag_z.sm, acc.x = df$Acc_x.sm, acc.y = df$Acc_y.sm, acc.z = df$Acc_z.sm, ME = df$Marked.event, acc.ref.frame = "NWU", positive.g = "up", mag.ref.frame = "NWU", pitch.offset = 0, roll.offset = 0, yaw.offset = 0, method = 3, algorithm = "standard", plot = TRUE)
#E.g_2 --> df.corr = Gundog.Compass(mag.x = df$Mag_x.sm, mag.y = df$Mag_y.sm, mag.z = df$Mag_z.sm, acc.x = df$Acc_x.sm, acc.y = df$Acc_y.sm, acc.z = df$Acc_z.sm, ME = df$Marked.event, acc.ref.frame = "ENU", positive.g = "up", mag.ref.frame = "ENU", pitch.offset = 0, roll.offset = 0, yaw.offset = 0, method = 0, algorithm = "SAAM", plot = TRUE)
  

  
  

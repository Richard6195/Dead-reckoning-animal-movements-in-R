# Gundog.Dead-reckoning in R
R functions to dead-reckon reckon animal movement from raw motion sensor data (specifically, the tilt-compensated compass approach using tri-axial accelerometers & magnetometers) with option to correct for drift with Verified Positions (VPs - e.g., GPS). 

#Gundog.Peaks = a peak finder (local maxima) in a data series.

#Gundog.Compass = computes heading from the accelerometer-magnetometer (Euler angle) approach (excl. gyros), including an initial magnetic distortion correction procedure and rotation correction (if there are discrepancies between the tag-carried North-East-Down (NED) coordinate frame and animal-carried NED frame).

#Gundog.Tracks = Verified Position Correction (VPC) dead-reckoning script.

#Step by step guide of using Gundog.Tracks (incl. Gundog.Compass operations) - to use in conjunction with supplied raw data of a penguin walking to the sea.

#Additional R script containing various operations and functions for assessing estimates of fine-scale movements.

For bugs, queries and suggestions, free to email the corresponding author; richard.m.g@hotmail.com

#Important to note that any loaded packages that mask operations within the 'dplyr' or 'zoo' packages can cause error within these function. I specifically note to not have the 'imputeTS' or 'plyr' packages loaded!
	


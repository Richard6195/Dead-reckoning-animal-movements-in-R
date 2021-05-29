# Gundog.Dead-reckoning in R
R functions to dead-reckon reckon animal movement from raw motion sensor data (specifically, the tilt-compensated compass approach using tri-axial accelerometers & magnetometers) with option to correct for drift with verified positions (e.g., GPS)

#Gundog.Peaks = a peak finder (local maxima) in a data series
#Gundog.Compass = computes heading from the accelrometer-magnetometer (Euler angle) approach (excl. gyros), including an initial magnetic distrotion correction procedure and rotation correction 
#Gundog.Tracks = verified position correction dead-reckoning script
#Step by step guide of using Gundog.Tracks (incl. Gundog.Compass operations) - to use in conjuntion with suplied raw data of a penguin walking to the sea

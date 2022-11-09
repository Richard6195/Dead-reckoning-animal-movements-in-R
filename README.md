# Gundog.Dead-reckoning in R 
R functions to dead-reckon reckon animal movement from raw motion sensor data (specifically, the tilt-compensated compass approach using tri-axial accelerometers & magnetometers) with option to correct for drift with Verified Positions (VPs - e.g., GPS). (See DOI: 10.1186/s40317-021-00245-z for main paper detailling the below 'Gundog.R' scripts)

#Gundog.Peaks = a peak finder (local maxima) in a data series.

#Gundog.Compass = computes heading from the accelerometer-magnetometer (Euler angle) approach (excl. gyros), including an initial magnetic distortion correction procedure and rotation correction (if there are discrepancies between the tag-carried North-East-Down (NED) coordinate frame and animal-carried NED frame).

#Gundog.Tracks = Verified Position Correction (VPC) dead-reckoning script.

#Step by step guide of using Gundog.Tracks (incl. Gundog.Compass operations) - to use in conjunction with supplied raw data of a penguin walking to the sea (zipped).

#Additional R script containing various operations and functions for assessing estimates of fine-scale movements.

#Supplementary information (word doc) from main paper (DOI: 10.1186/s40317-021-00245-z) further detailing the functionality of the afformentioned scripts, including additional R code (e.g., formatting time in R)

For bugs, queries and suggestions, free to email the corresponding author; richard.m.g@hotmail.com

#Important to note that any loaded packages that mask operations within the 'dplyr' or 'zoo' packages can cause error within these function. I specifically note to not have the 'imputeTS' or 'plyr' packages loaded!
	
# MVF method in R
#MVF method = A Movement Verified Filtering (MVF) protocol to aid in classifying geunine travelling movement and identify GPS error within high-res GPS data sets. Here is an example workflow of the MVF method which primarily involves deriving DBA from tri-axial accelerometery data, computing speed from GPS data, and evaluating how both covary during travelling movement. The user then decides on the threshold limits that DBA and GPS speed must exceed (both in terms of magnitude and duration) for a movement bout to be verified. This script is modelled to use in conjunction with supplied raw data files of a lion (zipped).

# Gundog.GPS.Filter 
#Gundog.GPS.Filter = An R function to screen for GPS anomalies, including pre-processing burst data, and combining user-defined movement thresholds (with features to account for temporal discrepancies between fixes), with Isolation forest models as a comparative method of unsupervised anomaly detection interactive summary plots are made for users to inspect the output.

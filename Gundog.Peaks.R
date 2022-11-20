########################################Calculate local peaks (maxima)####################################################################
##########################################################################################################################################
#+eval=FALSE
#This function allows user to set locale maximum 'LoM' and threshold (%) above constant value to identify peaks. 
#Gundog.Peaks was modelled from the find_peaks() function within ggpmisc package, though with discernible differences to input and output as outlined below 
#TS = input (POSIXct class) timestamp
#x =  data values you want to find peaks from
#thresh = the minimum height of the candidate peaks (%): size threshold relative to the tallest peak considered (though see outlier) with constant baseline subtracted (thresh)
#LoM = your span for local maximum, default is 5, ensure value is odd (otherwise function will add 1)
#Constant as default is set at the median of your x values, though user can use the mean; 'mn' can be input - both are calculated from all values >= 0 and have an ME value (see later) greater than 0 (e.g., 1). 
#User can also input 'roll.mn' which specifies that a rolling mean will be used as the constant. The length of the window has to be supplied with 'w' (an interger number >= 2 ; default value = 100) 
#User can also opt to supply their own arbitrary y-axis constant value, e.g., 'constant = 5'
#Marked events (ME) (default = 1) specifies which values to locate peaks on. Only values > 0 considered (e.g., periods of moving already demarked with a one (1), as opposed to periods of stationary behaviour to ignore, marked with a zero (0))
#plot = TRUE (default) shows the peak spectrum over time with the identified peaks, height threshold, constant baseline and marked events demarked with values > 0 (cf. SI4 Fig. 1)
#outlier = FALSE (default). If changed, then the max value is not used when scaling height threshold (%) but rather, the quantile value input instead of FALSE (do not input TRUE). For example, rather than scaling a 35 % threshold height from the specified constant and the maximum value in the spectrum (with ME of 1), scale it in relation to outlier = 0.99 (0.99 quantile of data (with ME > 0)).
#Returns Timestamp, Index (the original row/element position of input data), Peak.Amplitude (value of x at peak location) ,Peak.Period (duration between peak (s)) & Marked.events

#Required packages: 'dplyr' and 'data.table'.

#Note his function only considers values > 0. If the troughs are desired (negative peaks) then set peaks = "negative".

Gundog.Peaks = function(TS, x, thresh = 0, LoM = 5, constant = "med", ME = 1, plot = TRUE, outlier = FALSE, w = 100, peaks = "positive") {  
  if(plot == TRUE & Sys.info()["sysname"] == "Windows") { windows(width=10, height=7) } #Set graphics window if plot = TRUE (and operating system is windows)
  is.POSIXct = function(x) inherits(x, "POSIXct") #Function to check variable is of POSIXct type
  if (!require('dplyr')){ install.packages('dplyr')} ; library('dplyr') #Requires the 'dplyr' package
  if (!require('data.table')){ install.packages('data.table')} ; library('data.table') #Requires the 'data.table' package
  
  #Check that required packages are installed on the system
  areinstaled=data.frame(installed.packages())
  
  if(all(c("dplyr", "data.table")%in%areinstaled$Package)==FALSE){
    required_packages=c("dplyr", "data.table")
    missing_packages=c("dplyr", "data.table")%in%areinstaled$Package
    stop(paste("The following packages are not installed:", required_packages[which(missing_packages==FALSE)], sep = " "))
  }
   #Ensure Timestamp is correct format
  if(is.POSIXct(TS) == FALSE) { #Ensure TS is of type POSIXct (otherwise terminate function)
    stop("The function stops - TS must be of type POSIXct.")
  }
  
  if(length(unique(TS)) != length(TS)){ #Ensure TS does not contain duplicate timestamps (otherwise terminate function)
    stop("The function stops - TS must not contain duplicates")
  }
  if(constant == "roll.mn" & (is.numeric(w) == FALSE | w < 2)){ #Ensure TS does not contain duplicate timestamps (otherwise terminate function)
    stop("The function stops - the rolling window (w) needs to be a numeric value >= 2")
  }
  if(outlier != FALSE & (is.numeric(outlier) != TRUE | (outlier <= 0 | outlier >= 1))){ #Ensure appriate outlier quantile stated
    stop("The function stops - if outlier supplied, then it needs to be a quantile value > 0 & < 1")
  }
  range_x = range(x[ME > 0], finite = TRUE) #Range of data spectrum values
  mini = range_x[1] #Min value in range
  maxi = range_x[2] #Max value in range
  x = ifelse(!is.finite(x), mini, x)
  y = x #Original data
  if(peaks == "negative"){ #If negative peaks, negate data
    x = x*-1 # Inverse values) 
  }
  x = ifelse(x < 0, 0, x) #Ensure no negative data
  
  ME = as.numeric(ME) ; if(length(ME) == 1){ ME = rep(ME, length(x)) } #Marked event vector (> 0 = peaks considered, 0 = Not considered) - #If only one value supplied (or none supplied (default = 1)), then replicate the length of x
  ME[is.na(ME)] = 0 #If ME contains NA's then replace with zero

  if(constant == "med"){ #If constant is median
    constant = median(x[ME > 0 & x >= 0], na.rm = TRUE)
  }else if(constant == "mn"){ #If constant is mean
    constant = mean(x[ME > 0 & x >= 0], na.rm = TRUE)
  }else if(constant == "roll.mn"){ #Rolling mean
    df <- data.frame(x = x)
    df <- setDT(df)[, c("constant") := lapply(.SD,function(x) frollmean(x, n = as.integer(w), align="center", adaptive=F)), 
                    .SDcols = "x"] ; constant =  df$constant ; rm(df) ; constant = ifelse(is.na(constant) == TRUE, 0, constant)
    constant[1:round((as.integer(w)+0.5)/2)] = constant[which(constant > 0)[1]]
    constant[(length(constant) - round((as.integer(w)+0.5)/2)):length(constant)] = rev(constant[which(constant > 0)])[1]
  }else{constant = abs(constant)} #Arbitrary y-axis value supplied
  
  check = thresh > 0.0 #Returns logical (if thresh supplied, then thresh = TRUE)
  
  if(length(constant) != 1){
    ref = rep(NA, length(constant))
    SHT = rep(NA, length(constant))
    for(i in 1:length(ref)){
      ref[i] = (range(x[ME > 0], finite = TRUE)[2] - constant[i]) #Maximum value in relation to constant
      if(outlier != FALSE){
        ref[i] = quantile(x[ME > 0], outlier, na.rm = TRUE) - constant[i] #User can change outlier value in function input 
        SHT[i] = ref[i] * (abs(thresh) / 100) #Scaled Height Threshold (SHT) (from constant)
      }
    }
  } 
  if(length(constant) == 1){
    ref = (range(x[ME > 0], finite = TRUE)[2] - constant) #Maximum value in relation to constant
    if(outlier != FALSE){
      ref = quantile(x[ME > 0], outlier, na.rm = TRUE) - constant #User can change outlier value in function input 
    }
    SHT = ref * (abs(thresh) / 100) #Scaled Height Threshold (SHT) (from constant)
  }
  
  LoM = as.integer(LoM)
  if(LoM %% 2 != 1) { #Ensure LoM is odd
    LoM = LoM + 1
    cat("LoM increased to next odd value: ", LoM, "\n")
  }
  
  x.max = unlist(frollapply(as.data.table(x), LoM, max, align = "center", fill = 0)) #Rolling window of max values
  pks = c(FALSE, diff(diff(x) >= 0) < 0, FALSE) #Candidate peaks (allow for repeated values at the peak - return TRUE for last occurrence of the peak value)
  #Ensure local max 
  pks = ifelse(x >= x.max & ME > 0 & pks == TRUE, TRUE, FALSE) #Ensure candidate peaks fall at the index of maximum local value and occur when ME > 0
  
  #Ensure thresh height met
  if(length(constant) != 1){ #If rolling mean (many different values)
    for(i in 1:length(pks)){
      if (abs(thresh) < 1e-5) {
        pks[i] = pks[i]
      } else if (check == TRUE) { 
        pks[i] = ifelse((x[i] - constant[i]) > SHT[i] & pks[i] == TRUE, TRUE, FALSE) 
      } else {
        pks[i] = ifelse(ref[i] > SHT[i] & pks[i] == TRUE, TRUE, FALSE)
      }
    }
  }
  if(length(constant) == 1){ #If not rolling mean (only one value for SHT and ref etc.)
    if (abs(thresh) < 1e-5) {
      pks = pks
    } else if (check == TRUE) { 
      pks = ifelse((x - constant) > SHT & pks == TRUE, TRUE, FALSE) 
    } else {
      pks = ifelse(ref > SHT & pks == TRUE, TRUE, FALSE)
    }
  }
  
  index=rep(1:length(x)) #Row number
  TD = c(0, difftime(TS, dplyr::lag(TS), units = "secs")[-1]) # Time difference 
  TD = cumsum(TD) #Accumulated time (s)
  
  #Plot
  if(plot == TRUE){ #Diagnostic plot of results
    i.max = which(pks == TRUE)
    x.min = mini
    a = list(index = TD[i.max], i = i.max, x.hat = y)
    plot(TD, y, type="l", col="black", main=paste("LoM = ", LoM, ", Threshold Height (%) = ", thresh, sep=""), xlab = "Time (s)")
    sapply(a$i, function(i) lines(c(TD[i], TD[i]), c(x.min, a$x.hat[i]), col="Red", lty=2)) #Plot vertical lines at each peak for reference
    if(peaks == "negative" & length(constant) == 1){
      lines(TD, rep(constant*-1, length(y)), col="blue", lty=2, lwd = 2) #Plot horizontal constant line for reference
      lines(TD, rep(((constant*-1)-SHT), length(y)), col="blue", lty=2, lwd = 2) #Plot horizontal Threshold below constant required
    }else if(peaks == "negative" & length(constant) != 1){
      lines(TD, constant*-1, col="blue", lty=2, lwd = 2) #Plot horizontal constant line for reference
      lines(TD, (constant*-1)-SHT, col="blue", lty=2, lwd = 2) #Plot horizontal Threshold below constant required
    }else if(peaks == "positive" & length(constant) == 1){
      lines(TD, rep(constant, length(y)), col="blue", lty=2, lwd = 2) #Plot horizontal constant line for reference
      lines(TD, rep((constant+SHT), length(y)), col="blue", lty=2, lwd = 2) #Plot horizontal Threshold above constant required
    }else{lines(TD, constant, col="blue", lty=2, lwd = 2) #Plot horizontal constant line for reference
         lines(TD, constant+SHT, col="blue", lty=2, lwd = 2) #Plot horizontal Threshold below constant required
    }
    points(TD[a$i], a$x.hat[a$i], col="Red", pch=19, cex=1) #Plot points at top of each identified peak
    points(TD[ME > 0], rep((maxi + (maxi*0.02)), length(TD[ME > 0])), col="green", pch=19, cex=0.5) #Marked events (green) (positioned slightly higher than max value)
  }
        
  df = data.frame(TS, index, y, pks, ME) #Place important variables within a data frame
  df = subset(df, df$pks == "TRUE") 
  df$Periodicity = c(difftime(df$TS[1], TS[1], units= "secs"), difftime(df$TS, dplyr::lag(df$TS), units = "secs")[-1]) #Periodicity in seconds
  rm(constant, x.max, mini, SHT, ref, check, maxi, range_x, a, i.max, index, outlier, TD, ME, x, y) #Remove variables 
  df$pks= NULL #Remove this column
  colnames(df) = c("Timestamp", "Index" , "Peak.Amplitude", "Marked.events", "Peak.Period") ; df = df[, c(1:3, 5,4)] #Rename and reorder columns
  rownames(df) <- NULL # Reorder row number 
  return(df)
}


#example
# x = Gundog.Peaks(TS = df$timestamp, x = df$Acc_y, thresh = 50, LoM = 5, constant = "roll.mn", ME = df$Marked.event, plot = TRUE, outlier = 0.99, w = 200, peaks = "positive"){  
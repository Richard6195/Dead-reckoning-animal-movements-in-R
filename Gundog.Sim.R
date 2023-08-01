###################################### Gundog.sim ######################################

# Developed by Richard Gunner (richard.m.g@hotmail.com / rgunner@ab.mpg.de) and Miguel Lurgi (miguel.lurgi@swansea.ac.uk)

# Brief description:

# Information for this individual-based model (IBM), including 'fundamental' turn angles, step lengths,
# and their associated overall compass headings (H) is divided into distinct data sets (or sectors) based on proportional 
# cumulative distances traveled. This segmentation facilitates the creation of unique Empirical Cumulative Distribution Functions (ECDFs)
# for turn angles, step lengths, and H for each sector. An individual's behavior is dictated by the extent of it cumulative distance travelled.
# At each time step, turn angles and step lengths are randomly drawn from their respective ECDF distributions.
# The direction of the turn (left or right) is determined by its compatibility with the current H,
# selected from its distribution (the agent will choose to either turn left or right depending on which direction will
# take it closer to the selected H value). After each step, the agent dead-reckoning this information, recalculates its 
# cumulative distance travelled, and if the new distance places it into a new sector (number of sectors determined by the user), 
# the agent updates its internal state with the new sector's ECDF information for turn angles, step lengths, and H. 
# The simulation concludes for each agent once they've covered the total cumulative distance set by the user, as set by the user.
# An array of other features are included to increase flexibility in use:

# Function parameters:

# Gundog.sim(heading, turn.angle = NULL, step.length, ID = 1, ID_head = NULL, agents = 1,
  # sectors.heading = 1, sectors.turn.angle = 1, sectors.step.length = 1, 
  # max.distance.moved = 1000, quant.mean.head = TRUE,
  # quantile.turn.angle = 1, quantile.step.length = 1, quantile.group = TRUE, 
  # bins.heading = 2, bins.turn.angle = 2, bins.step.length = 5, mid.point = TRUE, 
  # autocorr.head = FALSE, autocorr.turn.angle = FALSE, autocorr.step.length = FALSE,
  # lo = 0, la = 0, seed = NULL, plot = TRUE)

# heading: Represents raw compass heading or bearing between identified turn points.
# turn.angle: The angle changes associated with the change in heading, measured in degrees within a range from -180° to +180°. By default, it's set to NULL, meaning only heading and step length values are considered.
# step.length: The distance (in meters) moved between 'turn points'.
# ID: Specifies a grouping variable to differentiate data from different individuals, if there are multiple individuals included. By default, it's set to 1.
# ID_head: A string that specifies the individual (within 'ID') to subset the heading data for. This is helpful when different individuals have significantly different overall headings. By default, it's set to NULL, meaning no subset is required.
# agents: The number of simulations or tracks to produce (default = 1).
# sectors.heading, sectors.turn.angle, sectors.step.length: The number of sectors to divide heading, turn angle, and step length data into, respectively. Each sector represents a fraction of the total cumulative distance moved.# E.g., If ten sectors is specified, the total cumulative distance is divided into ten sectors, each representing a tenth of the maximum cumulative distance travelled from the start
# max.distance.moved: The overall cumulative distance (in meters) that the agent is required to move. By default, it's set to 1000.
# quant.mean.head: When set to TRUE, and if the selected 'step.length' value is greater or equal to the quantile value set by 'quantile.step.length', then the heading defaults to the mean circular heading of the given sector. By default, it's set to TRUE.
# quantile.turn.angle, quantile.step.length: These parameters constrain the distribution of turn angles and step lengths to values below or equal to the specified quantile, which helps to remove outliers. By default, they're set to 1 (no subset). Note, this does not subset quantile.step,length if quant.mean.head is TRUE.
# quantile.group: When set to TRUE, quantiles are estimated and used to subset data per sector. Otherwise, an overall quantile is estimated and used. By default, it's set to TRUE.
# bins.heading, bins.turn.angle, bins.step.length: Specifies the bin width for raw heading (default = 2), turn angles (default = 2), and step lengths (default = 5), respectively.
# mid.point: When set to TRUE, bins of data greater than 0 are shifted back by half the size of the bin width. By default, it's set to TRUE.
# autocorr.head, autocorr.turn.angle, autocorr.step.length: When set to TRUE, first-order autocorrelation (estimated per sector) is incorporated for selected heading, turn angle, or step length values, respectively. By default, they're set to FALSE.
# lo, la: The starting longitude and latitude, respectively, in decimal geographical coordinate format. By default, they're set to 0.
# seed: An integer number for setting the seed for reproducibility. If more than one agent is specified, the seed increases by one per agent. Default = NULL (not specified)
# plot: When set to TRUE, summary frequency distribution and agents' reconstructed tracks plots are produced. By default, it's set to TRUE.


################################################### Start of function ###################################################

Gundog.sim <- function(heading, turn.angle = NULL, step.length, ID = 1, ID_head = NULL, agents = 1,
                       sectors.heading = 1, sectors.turn.angle = 1, sectors.step.length = 1, 
                       max.distance.moved = 1000, quant.mean.head = TRUE,
                       quantile.turn.angle = 1, quantile.step.length = 1, quantile.group = TRUE, 
                       bins.heading = 2, bins.turn.angle = 2, bins.step.length = 5, mid.point = TRUE, 
                       autocorr.head = FALSE, autocorr.turn.angle = FALSE, autocorr.step.length = FALSE,
                       lo = 0, la = 0, seed = NULL, plot = TRUE){
  
  ###Required packages###
  if (!require('zoo')){ install.packages('zoo', dependencies = TRUE, type="source")} ; suppressMessages(require("zoo"))
  if (!require('dplyr')){ install.packages('dplyr', dependencies = TRUE, type="source")} ; suppressMessages(require("dplyr"))
  if (!require('ggplot2')){ install.packages('ggplot2', dependencies = TRUE, type="source")} ; suppressMessages(require("ggplot2"))
  if (!require('gridExtra')){ install.packages("gridExtra", dependencies = TRUE, type="source")} ; suppressMessages(require("gridExtra"))
  if (!require('circular')){ install.packages("circular", dependencies = TRUE, type="source")} ; suppressMessages(require("circular"))
  if (!require('devtools')){ install.packages('devtools', dependencies = TRUE, type="source")} ; suppressMessages(require("devtools"))
  
  #Check that required packages are installed on the system
  areinstaled=data.frame(installed.packages())
  
  if(all(c("zoo","dplyr","ggplot2", "gridExtra", "circular")%in%areinstaled$Package)==FALSE){
    required_packages=c("zoo","dplyr","ggplot2", "gridExtra", "circular")
    missing_packages=c("zoo","dplyr","ggplot2", "gridExtra", "circular")%in%areinstaled$Package
    stop(paste("The following packages are not installed:", required_packages[which(missing_packages==FALSE)], sep = " "))
  }
  
  # Input checks
  if(is.null(heading) == TRUE) {
    stop("The function stops - User has not input heading values", call. = FALSE)
  }
  if(min(heading, na.rm = TRUE) < 0) {
    stop("The function stops - Heading values must be in the 0 to 360 degree range", call. = FALSE)
  }
  
  if(is.null(step.length) == TRUE) {
    stop("The function stops - User has not input step length values", call. = FALSE)
  }
  if(length(ID == 1)){
    ID_head <- NULL
  }
  
  suppressWarnings({
    #### Required functions ###
    
    #### Required function - Haversine distance (m) ####
    disty = function(long1, lat1, long2, lat2) { #longitude and latitude supplied in degrees
      long1 = long1 * pi/180 ; long2 = long2 * pi/180 ; lat1 = lat1 * pi/180 ; lat2 = lat2 * pi/180 # convert to radians
      a = sin((lat2 - lat1) / 2) * sin((lat2 - lat1) / 2) + cos(lat1) * cos(lat2) * sin((long2 - long1) / 2) * sin((long2 - long1) / 2)
      c = 2 * atan2(sqrt(a), sqrt(1 - a))
      d1 = 6378137 * c
      return(d1)
    }
    ####
    
    #### Required function - Rescale (normalise between 0 and 1) ####
    rescale <- function(x) (x-min(x))/(max(x) - min(x)) 
    ####
    
    #### Required function - Overall function for creating frequency ECDF for a given input variable
    freq.distr <- function(x, bins, mids = mid.point) {
      # Estimate the bandwidth
      bw <- bw.ucv(x) 
      # Vector the range of x at the user specified interval bins 
      x0 <- seq(min(round(x / bins) * bins), max(round(x / bins) * bins), bins)
      # Shift back by half bin length?
      if(mids == TRUE){
        x0 = ifelse(x0 == 0, 0, x0 - bins.heading/2)
      }
      x.density <- density(x, bw = bw)
      x0.density <- approx(x = x.density$x, y = x.density$y, xout = x0)$y
      x0.cdf <- cumsum(x0.density) / sum(x0.density)
      df.freq <- data.frame(x0 = x0, x0.cdf = x0.cdf)  # Sequence of data and associated ECDF value
    }
    ####
    
    #### Required function - Circular mean
    circ.mean <-function(x){
      V_east= mean(sin(x * pi/180))
      V_north = mean(cos(x * pi/180))
      mean_MH = (atan2(V_east, V_north))* 180/pi
      mean_MH = (360 + mean_MH) %% 360
      return(mean_MH)
    }
    ####
    
    #### Required function - Bearing function --> returns degrees - Great circular bearing between 2D positions
    beary = function(long1, lat1, long2, lat2) { #Longitude and Latitude supplied in degrees
      long1 = long1 * pi/180 ; long2 = long2 * pi/180 ; lat1 = lat1 * pi/180 ; lat2 = lat2 * pi/180 #Function converts to radians
      a = sin(long2 - long1) * cos(lat2)
      b = cos(lat1) * sin(lat2) - sin(lat1) * cos(lat2) * cos(long2 - long1)
      c = ((atan2(a, b) / pi) * 180)  #Units returned in degrees (-180 to +180 degree scale)
      return(c)
    }
    ####
    ####
    
    # Convert step.dist into cumulative distance moved and rescale between 0-1 per ID
    distance.df = data.frame(distance.moved = step.length, ID = ID)
    distance.df$distance.moved = with(distance.df, ifelse(is.na(distance.moved) == TRUE, 0, distance.moved))
    distance.df = distance.df %>% group_by(ID) %>% mutate(distance.moved = cumsum(distance.moved)) %>% ungroup()
    distance.df = distance.df %>% group_by(ID) %>% mutate(prop.dist = rescale(distance.moved)) %>% ungroup()
    
    ### Heading data frame ###
    
    #How many sectors of overall heading?
    if(is.null(ID_head) != TRUE){
      df.heading <- data.frame(heading = heading, prop.dist = distance.df$prop.dist, ID = ID, ID_head = ID_head) # Make this into a data frame
      #Subset df.heading according to the selected ID of heading values to be selected from
      df.heading = subset(df.heading, ID == ID_head)
    } else {
      df.heading <- data.frame(heading = heading, prop.dist = distance.df$prop.dist, ID = ID)
    }
    
    # Remove any NA's
    df.heading <- na.omit(df.heading)
    
    #Sectors    
    df.heading$sectors <- cut(df.heading$prop.dist, 
                              breaks = seq(0, 1, 1/sectors.heading), # How many breaks? -  Fractions of 0-1 for the given number of sectors requested
                              include.lowest = TRUE, # If an ‘x[i]’ equal to the highest, ‘breaks’ value should be included
                              right = FALSE, # Intervals should be closed on the left (and open on the right) -  current bin does not include the rightmost edge value, that is given to subsequent bin
                              labels = seq(1, sectors.heading), # Labels increase by one per break
                              ordered = TRUE) # Breaks are an ordered factor
    # Make a copy
    df.heading$heading.orig = df.heading$heading
    
    ### Turn angle data frame ###
    if(is.null(turn.angle) != TRUE){
      df.turn.angle <- data.frame(turn.angle = turn.angle, prop.dist = distance.df$prop.dist, ID = ID)  # Make this into a data frame
      
      # Remove any NA's
      df.turn.angle <- na.omit(df.turn.angle)
      
      # Sectors    
      df.turn.angle$sectors <- cut(df.turn.angle$prop.dist, 
                                   breaks = seq(0, 1, 1/sectors.turn.angle), # How many breaks? -  Fractions of 0-1 for the given number of sectors requested
                                   include.lowest = TRUE, # If an ‘x[i]’ equal to the highest, ‘breaks’ value should be included
                                   right = FALSE, # Intervals should be closed on the left (and open on the right) -  current bin does not include the rightmost edge value, that is given to subsequent bin
                                   labels = seq(1, sectors.turn.angle), # Labels increase by one per break
                                   ordered = TRUE) # Breaks are an ordered factor
      
      # Remove outliers if quantiles are set (per sector if quantile.group = TRUE)
      if(quantile.group == TRUE){
        df.turn.angle <- df.turn.angle %>%
          group_by(sectors) %>%
          filter(turn.angle <= quantile(turn.angle, quantile.turn.angle)) %>% ungroup()
      } else { # No subset based on quantiles
        df.turn.angle <- df.turn.angle %>%
          filter(turn.angle <= quantile(turn.angle, quantile.turn.angle)) %>% ungroup()
      }
      # Make a copy
      df.turn.angle$turn.angle.orig = df.turn.angle$turn.angle
    }
    
    ### Step length data frame ###
    df.step.length <- data.frame(step.length = step.length, prop.dist = distance.df$prop.dist, ID = ID)  # Make this into a data frame
    
    # Remove any NA's
    df.step.length <- na.omit(df.step.length)
    
    # Sectors    
    df.step.length$sectors <- cut(df.step.length$prop.dist, 
                                  breaks = seq(0, 1, 1/sectors.step.length), # How many breaks? -  Fractions of 0-1 for the given number of sectors requested
                                  include.lowest = TRUE, # If an ‘x[i]’ equal to the highest, ‘breaks’ value should be included
                                  right = FALSE, # Intervals should be closed on the left (and open on the right) -  current bin does not include the rightmost edge value, that is given to subsequent bin
                                  labels = seq(1, sectors.step.length), # Labels increase by one per break
                                  ordered = TRUE) # Breaks are an ordered factor
    
    # Remove outliers if quantiles are set (per sector if quantile.group = TRUE)
    if(quantile.group == TRUE & quant.mean.head == FALSE){
      df.step.length <- df.step.length %>%
        group_by(sectors) %>%
        filter(step.length <= quantile(step.length, quantile.step.length)) %>% ungroup()
    } else if(quantile.group != TRUE & quant.mean.head == FALSE){ # No subset based on quantiles
      df.step.length <- df.step.length %>%
        filter(step.length <= quantile(step.length, quantile.step.length)) %>% ungroup()
    } else if(quantile.group == TRUE & quant.mean.head == TRUE){ # If quant.mean.head = TRUE, then we just report quantiles 
      quant <- df.step.length %>%
        group_by(sectors) %>%
        summarise(quant = quantile(step.length, quantile.step.length))
      quant <- quant$quant
    } else {
      quant <- df.step.length %>%
        summarise(quant = quantile(step.length, quantile.step.length))
      quant <- quant$quant
    }
    
    # Make a copy
    df.step.length$step.length.orig = df.step.length$step.length
    
    ############################################################################################################
    
    #Bin data
    df.heading$heading = with(df.heading, round(heading / bins.heading) * bins.heading)
    if(is.null(turn.angle) != TRUE){
      df.turn.angle$turn.angle = with(df.turn.angle, abs(round(turn.angle / bins.turn.angle) * bins.turn.angle)) #Also make these values absolute
    }
    df.step.length$step.length = with(df.step.length, round(step.length / bins.step.length) * bins.step.length)
    
    #Shift bins back by half?
    if(mid.point == TRUE){
      df.heading$heading = with(df.heading, ifelse(heading == 0, 0, heading - bins.heading/2))
      if(is.null(turn.angle) != TRUE){
        df.turn.angle$turn.angle = with(df.turn.angle, ifelse(turn.angle == 0, 0, turn.angle - bins.turn.angle/2))
      }
      df.step.length$step.length = with(df.step.length, ifelse(step.length == 0, 0, step.length - bins.step.length/2))
    }
    ############################################################################################################
    
    # Summary plots of frequency distributions for raw heading, turn angles and step length, if plot = TRUE
    if(plot == TRUE){
      ######################################### Heading ###########################################
      
      plot_list <- list()
      for(i in 1:length(unique(df.heading$sectors))){
        x = subset(df.heading, df.heading$sectors == unique(df.heading$sectors)[i])
        
        #Make density and ECDF and count and scale
        x <- x %>% 
          mutate(density = density(heading)$y[findInterval(heading, density(heading)$x)],
                 cum_dist = ecdf(heading)(heading)) %>% ungroup()
        
        x <- x %>% group_by(heading) %>%
          mutate(freq_count = n()) %>% ungroup()
        
        # Normalize density to fit on the cumulative distribution's scale
        x$density <- x$density / max(x$density) * max(x$cum_dist)
        x$freq_count <- x$freq_count / max(x$freq_count) * max(x$cum_dist)
        x = x[order(x$heading), ]
        
        fig <- ggplot(data = x[!duplicated(x$heading) ,], aes(x = heading, weight = freq_count)) +
          geom_histogram(stat = "count", alpha = 0.4, aes(fill = after_stat(count)), colour = "black") +
          geom_line(aes(y = rollmean(density, k = 2, fill = "extend")), colour = "purple", size = 1.5) +
          geom_line(aes(y = cum_dist), colour = "darkgreen", size = 1.4, linetype = "dashed") +
          scale_y_continuous(sec.axis = sec_axis(~ ., name = "ECDF")) +
          xlab('Heading (°)') + ylab('Density') +
          theme_minimal() +
          theme(axis.text.x = element_text(color = "black", size = 12),
                axis.text.y = element_text(color = "purple", size = 12),
                axis.title.x = element_text(color = "black", size = 14),
                axis.title.y = element_text(color = "purple", size = 14),
                axis.title.y.right = element_text(color = "darkgreen"),
                axis.text.y.right = element_text(color = "darkgreen", size = 14),
                legend.title=element_blank()) +
          scale_fill_gradient('Density', low = 'blue', high = 'red')+ guides(fill="none") + ggtitle(unique(df.heading$sectors)[i])
        
        plot_list[[i]] <- fig
        
      }
      plot_list.1 <- plot_list[!sapply(plot_list, is.null)]
      ######################################### Turn angle ###########################################
      
      if(is.null(turn.angle) != TRUE){
        plot_list <- list()
        for(i in 1:length(unique(df.turn.angle$sectors))){
          x = subset(df.turn.angle, df.turn.angle$sectors == unique(df.turn.angle$sectors)[i])
          
          #Make density and ECDF and count and scale
          x <- x %>% 
            mutate(density = density(turn.angle)$y[findInterval(turn.angle, density(turn.angle)$x)],
                   cum_dist = ecdf(turn.angle)(turn.angle)) %>% ungroup()
          
          x <- x %>% group_by(turn.angle) %>%
            mutate(freq_count = n()) %>% ungroup()
          
          # Normalize density to fit on the cumulative distribution's scale
          x$density <- x$density / max(x$density) * max(x$cum_dist)
          x$freq_count <- x$freq_count / max(x$freq_count) * max(x$cum_dist)
          x = x[order(x$turn.angle), ]
          
          fig <- ggplot(data = x[!duplicated(x$turn.angle) ,], aes(x = turn.angle, weight = freq_count)) +
            geom_histogram(stat = "count", alpha = 0.4, aes(fill = after_stat(count)), colour = "black") +
            geom_line(aes(y = rollmean(density, k = 2, fill = "extend")), colour = "purple", size = 1.5) +
            geom_line(aes(y = cum_dist), colour = "darkgreen", size = 1.4, linetype = "dashed") +
            scale_y_continuous(sec.axis = sec_axis(~ ., name = "ECDF")) +
            xlab('Turn angle (°)') + ylab('Density') +
            theme_minimal() +
            theme(axis.text.x = element_text(color = "black", size = 12),
                  axis.text.y = element_text(color = "purple", size = 12),
                  axis.title.x = element_text(color = "black", size = 14),
                  axis.title.y = element_text(color = "purple", size = 14),
                  axis.title.y.right = element_text(color = "darkgreen"),
                  axis.text.y.right = element_text(color = "darkgreen", size = 14),
                  legend.title=element_blank()) +
            scale_fill_gradient('Density', low = 'blue', high = 'red')+ guides(fill="none") + ggtitle(unique(df.turn.angle$sectors)[i])
          
          plot_list[[i]] <- fig
          
        }
        plot_list.2 <- plot_list[!sapply(plot_list, is.null)]
      }
      ######################################### step length ###########################################
      plot_list <- list()
      for(i in 1:length(unique(df.step.length$sectors))){
        x = subset(df.step.length, df.step.length$sectors == unique(df.step.length$sectors)[i])
        
        #Make density and ECDF and count and scale
        x <- x %>% 
          mutate(density = density(step.length)$y[findInterval(step.length, density(step.length)$x)],
                 cum_dist = ecdf(step.length)(step.length)) %>% ungroup()
        
        x <- x %>% group_by(step.length) %>%
          mutate(freq_count = n()) %>% ungroup()
        
        # Normalize density to fit on the cumulative distribution's scale
        x$density <- x$density / max(x$density) * max(x$cum_dist)
        x$freq_count <- x$freq_count / max(x$freq_count) * max(x$cum_dist)
        x = x[order(x$step.length), ]
        
        fig <- ggplot(data = x[!duplicated(x$step.length) ,], aes(x = step.length, weight = freq_count)) +
          geom_histogram(stat = "count", alpha = 0.4, aes(fill = after_stat(count)), colour = "black") +
          geom_line(aes(y = rollmean(density, k = 2, fill = "extend")), colour = "purple", size = 1.5) +
          geom_line(aes(y = cum_dist), colour = "darkgreen", size = 1.4, linetype = "dashed") +
          scale_y_continuous(sec.axis = sec_axis(~ ., name = "ECDF")) +
          xlab('Step length (m)') + ylab('Density') +
          theme_minimal() +
          theme(axis.text.x = element_text(color = "black", size = 12),
                axis.text.y = element_text(color = "purple", size = 12),
                axis.title.x = element_text(color = "black", size = 14),
                axis.title.y = element_text(color = "purple", size = 14),
                axis.title.y.right = element_text(color = "darkgreen"),
                axis.text.y.right = element_text(color = "darkgreen", size = 14),
                legend.title=element_blank()) +
          scale_fill_gradient('Density', low = 'blue', high = 'red')+ guides(fill="none") + ggtitle(unique(df.step.length$sectors)[i])
        
        plot_list[[i]] <- fig
        
      }
      plot_list.3 <- plot_list[!sapply(plot_list, is.null)]
    }
    #################################################################### End of summary plots ##################################################################
    #1st order autocorrelation?
    if(autocorr.head == TRUE){
      #Heading
      ACF_heading_list <- list()
      
      for(i in 1:length(unique(df.heading$sectors))){
        x = subset(df.heading, df.heading$sectors == unique(df.heading$sectors)[i])
        #Need to consider circular correlation
        heading_circular <- circular(x$heading, units = "degrees", template = "geographics")
        ACF_heading_list[[i]] <- cor.circular(heading_circular, dplyr::lead(heading_circular, 1))
      }
      names(ACF_heading_list) <- unique(df.heading$sectors)
    }
    
    #Turn angle
    if(is.null(turn.angle) != TRUE){
      if(autocorr.turn.angle == TRUE){
        ACF_turn.angle_list <- list()
        
        for(i in 1:length(unique(df.turn.angle$sectors))){
          x = subset(df.turn.angle, df.turn.angle$sectors == unique(df.turn.angle$sectors)[i])
          autocorr_values.turn.angle <- acf(x$turn.angle, plot=FALSE)
          ACF_turn.angle_list[[i]] <- autocorr_values.turn.angle$acf[2] # lag 1
        }
        names(ACF_turn.angle_list) <- unique(df.turn.angle$sectors)
      }
    }
    
    #Step length
    if(autocorr.step.length == TRUE){
      ACF_step.length_list <- list()
      
      for(i in 1:length(unique(df.step.length$sectors))){
        x = subset(df.step.length, df.step.length$sectors == unique(df.step.length$sectors)[i])
        autocorr_values.step.length <- acf(x$step.length, plot=FALSE)
        ACF_step.length_list[[i]] <- autocorr_values.step.length$acf[2] # lag 1
      }
      names(ACF_step.length_list) <- unique(df.step.length$sectors)
    }
    ############################################################################################################
    # Split the data frame into a list of data frames based on sectors for each variable
    
    li.heading <- split(df.heading, df.heading$sectors)  
    if(is.null(turn.angle) != TRUE){
      li.turn.angle <- split(df.turn.angle, df.turn.angle$sectors) 
    }
    li.step.length <- split(df.step.length, df.step.length$sectors) 
    
    # Use lapply to apply the 'freq.distr' function to each data frame in the lists for variables: 'heading', 'turn.angle', and 'step.length'
    freq_heading_list <- lapply(li.heading, function(x) freq.distr(x = x$heading.orig, bins = bins.heading, mids = mid.point))
    if(is.null(turn.angle) != TRUE){
      freq_turn.angle_list <- lapply(li.turn.angle, function(x) freq.distr(x = x$turn.angle.orig, bins = bins.turn.angle, mids = mid.point))
    }
    freq_step.length_list <- lapply(li.step.length, function(x) freq.distr(x = x$step.length.orig, bins = bins.step.length, mids = mid.point))
    
    
    ########################### Start agent based model ###############################
    
    #Set initial heading value to mean circular heading of first sector
    h = circ.mean(li.heading[[1]]$heading) #Circular mean of first sector
    
    #Set seed?
    if(is.null(seed) != TRUE){
      set.seed(seed) 
    }
    
    df = data.frame(long = numeric(), lat = numeric(), id = numeric()) # Empty df to add results into
    
    # Start... per agent...
    for(a in 1:agents){
      
      n.head <- length(unique(df.heading$sectors)) # How many sectors?
      dist.split.h = round(max.distance.moved/n.head) # Divide overall cumulative distance moved that you want agent to move by the number of sectors
      orig.dist.h = dist.split.h # Original distance
      
      if(is.null(turn.angle) != TRUE){
        n.turn <- length(unique(df.turn.angle$sectors)) # How many sectors?
        dist.split.t = round(max.distance.moved/n.turn) # Divide overall cumulative distance moved that you want agent to move by the number of sectors
        orig.dist.t = dist.split.t # Original distance
      }
      n.step <- length(unique(df.step.length$sectors)) # How many sectors?
      dist.split.s = round(max.distance.moved/n.step) # Divide overall cumulative distance moved that you want agent to move by the number of sectors
      orig.dist.s = dist.split.s # Original distance
      
      cum.dist = 0 # Initial distance moved
      DR.lat <- vector(mode = "numeric", length = 0) # Empty vector to be filled with dead-reckoned longitude coordinates
      DR.lon <- vector(mode = "numeric", length = 0) # Empty vector to be filled with dead-reckoned latitude coordinates
      dist <- vector(mode = "numeric", length = 0) # Empty vector to be filled with dead-reckoned latitude coordinates
      DR.lat[1] = la * pi/180 #Set 1st element of DR latitude with la, ready for DR procedure
      DR.lon[1] = lo * pi/180  #Set 1st element DR longitude with lo, ready for DR procedure
      simulated.head <- vector(mode = "numeric", length = 0) ; simulated.head[1] = h # Empty vector to be filled with selected heading values from freq. distr. Initial value is h
      simulated.dist <- vector(mode = "numeric", length = 0) ; simulated.dist[1] = 0 # Empty vector to be filled with selected step length (distance) values from freq. distr. Initial value is 0
      if(is.null(turn.angle) != TRUE){
        simulated.turns <- vector(mode = "numeric", length = 0) ; simulated.turns[1] = 0 # If turn angles supplied; empty vector to be filled with selected turn angle values from freq. distr. Initial value is 0
      }
      
      i = 2 ; j = 1; k = 1 ; l = 1 # Starting indexes
      
      while(cum.dist <= max.distance.moved){
        while(cum.dist < dist.split.h & cum.dist < dist.split.t & cum.dist < dist.split.s) { #Per sector...
          
          # Simulate a heading value from ECDF
          prob.1.rn <- runif(1, 0, 1) #Random decimal numbers between 0 and 1
          if(autocorr.head != TRUE){
            simulated.head[i] <- approx(y = freq_heading_list[[j]]$x0, x = freq_heading_list[[j]]$x0.cdf, xout= prob.1.rn, rule=2)$y #Select value of heading from CDF distr.
          } else {
            #Need to consider circular nature
            simulated.head[i] <- as.numeric(circular::mean.circular(circular(c(simulated.head[i-1], 
                                                                               approx(y = freq_heading_list[[j]]$x0, x = freq_heading_list[[j]]$x0.cdf, xout= prob.1.rn, rule=2)$y), 
                                                                             type = "angles", units = "degrees", template = "geographics"),  w = c(ACF_step.length_list[[j]], 1 - ACF_step.length_list[[j]]))) # Select value of heading from CDF distr. with autocorrelation incorporated
          }
          
          # Simulate a step length value from ECDF
          prob.1.rn <- runif(1, 0, 1) #Random decimal numbers between 0 and 1
          if(autocorr.step.length != TRUE){
            simulated.dist[i] <- approx(y = freq_step.length_list[[l]]$x0, x = freq_step.length_list[[l]]$x0.cdf, xout= prob.1.rn, rule=2)$y #Select value of step length from CDF distr.
          } else {
            simulated.dist[i] <- ACF_step.length_list[[l]] * simulated.dist[i-1] + approx(y = freq_step.length_list[[l]]$x0, x = freq_step.length_list[[l]]$x0.cdf, xout= prob.1.rn, rule=2)$y * (1 - ACF_step.length_list[[l]]) # Select value of step length from CDF distr. with autocorrelation incorporated
            simulated.dist[i] <- ifelse(simulated.dist[i] < 0, 0, simulated.dist[i])
          }
          q =  simulated.dist[i] / 6378137 #Incorporate radius of Earth
          
          if(is.null(turn.angle) != TRUE){
            # Simulate a turn angle value from ECDF
            prob.1.rn <- runif(1, 0, 1) # Random decimal numbers between 0 and 1
            if(autocorr.turn.angle != TRUE){
              simulated.turns[i] <- approx(y = freq_turn.angle_list[[k]]$x0, x = freq_turn.angle_list[[k]]$x0.cdf, xout= prob.1.rn, rule=2)$y # Select value of turn angle from CDF distr.
            } else {
              simulated.turns[i] <- ACF_turn.angle_list[[k]] * simulated.turns[i-1] + approx(y = freq_turn.angle_list[[k]]$x0, x = freq_turn.angle_list[[k]]$x0.cdf, xout= prob.1.rn, rule=2)$y * (1 - ACF_turn.angle_list[[k]]) # Select value of turn angle from CDF distr. with autocorrelation incorporated
              simulated.turns[i] <- ifelse(simulated.turns[i] < 0, 0, simulated.turns[i])
            }
            
            # Which turn angle (left or right direction) of selected magnitude results in an overall heading being closest to the selected heading value (or if 1st run, closest to mean heading of first sector)
            # Maintain 360 deg range
            x = (h + simulated.turns[i])  %% 360
            y = (h - simulated.turns[i])  %% 360
            
            xx = (simulated.head[i] - x) ; xx = ifelse(xx < -180, (xx + 360), xx) ; xx = ifelse(xx > 180, (xx - 360), xx)
            yy = (simulated.head[i] - y) ; yy = ifelse(yy < -180, (yy + 360), yy) ; yy = ifelse(yy > 180, (yy - 360), yy)
            
            simulated.turns[i] = ifelse(which.min(c(abs(xx), abs(yy))) == 2, simulated.turns[i]*-1, simulated.turns[i]) 
            
            # Maintain 360 deg range
            h = (h + simulated.turns[i]) %% 360
          } else {
            h = simulated.head[i] # If no turn angles supplied, just base on selected heading
          }
          
          # If quant.mean.head = TRUE, then we want to use the mean heading value of the sector
          if(quant.mean.head != FALSE){
            if(quantile.group == TRUE & simulated.dist[i] >= as.numeric(quant[l])){
              h = circ.mean(li.heading[[j]]$heading)
            } else if(quantile.group == FALSE & simulated.dist[i] >= as.numeric(quant[1])){
              h = circ.mean(li.heading[[j]]$heading)
            }
          }
         
          # DR
          DR.lat[i] = asin(sin(DR.lat[i-1]) * cos(q) + cos(DR.lat[i-1]) * sin(q) * cos(h* pi/180)) 
          DR.lon[i] = DR.lon[i-1] + atan2(sin(h* pi/180) * sin(q) * cos(DR.lat[i-1]), cos(q) - sin(DR.lat[i-1]) * sin(DR.lat[i]))
          
          # Current updated maximum cumulated distance travelled...
          dist[i] = disty(DR.lon[i]*180/pi, DR.lat[i]*180/pi, DR.lon[i-1]*180/pi, DR.lat[i-1]*180/pi) #What is the distance moved (m)
          cum.dist = sum(dist, na.rm = TRUE) #Max cumulated distance moved from the start so far...
          
          i = i + 1 # Next ith element to be examined...
        } # End of sub while loop
        
        #Update user of the progress
        print(paste("Agent", a, ":", "Heading Sector", j)) ; print(paste("Agent", a, ":", "Turn angle Sector", k)) ; print(paste("Agent", a, ":", "Step length Sector", l))
        
        # Sector distance surpassed -> increment the distance and repeat for next segment
        # Heading 
        if(cum.dist >= dist.split.h){
          dist.split.h = dist.split.h + orig.dist.h
          j = j + 1
        }
        # Turn angle 
        if(cum.dist >= dist.split.t){
          dist.split.t = dist.split.t + orig.dist.t
          k = k + 1
        }
        # Step length
        if(cum.dist >= dist.split.s){
          dist.split.s = dist.split.s + orig.dist.s
          l = l + 1
        }
      } # End on main while loop. Max cumulative distance reached
      
      #Repeat if more than one agents
      if(is.null(seed) != TRUE){
        seed = seed + 1 #If more than one agent, increase seed by one each time
        set.seed(seed) 
      }
      
      # Add results into 'df'
      df = rbind(df, data.frame(long = DR.lon*180/pi, lat = DR.lat*180/pi, id = rep(a, length(DR.lon))))
      
    }
    
    #Descriptive parameters
    # Compute the values
    df <- df %>% group_by(id) %>% mutate(step.length = disty(lag(long), lag(lat), long, lat)) %>% ungroup()
    df <- df %>% group_by(id) %>% mutate(heading = beary(lag(long), lag(lat), long, lat)) %>% ungroup()
    df$heading <- ifelse(df$heading < 0, df$heading + 360, df$heading)
    df <- df %>% group_by(id) %>%  mutate(turn.angle = lag(heading) - heading) %>% ungroup()
    df$turn.angle <- ifelse(df$turn.angle < -180, df$turn.angle + 360, df$turn.angle)
    df$turn.angle <- ifelse(df$turn.angle > 180, df$turn.angle - 360, df$turn.angle)
    
    ### Plot of the main result ###
    if(plot == TRUE){
      x_gridlines <- seq(min(df$long), max(df$long), length.out = 6)  
      y_gridlines <- seq(min(df$lat), max(df$lat), length.out = 6)  
      
      agent.based.model <- ggplot(data = df, aes(x = long, y = lat))+
        annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "#4C00FF") +
        theme_bw() +
        theme(axis.text.x = element_text(color = "black", size = 12),
              axis.text.y = element_text(color = "black", size = 12),
              axis.title.x = element_text(color = "black", size = 14),
              axis.title.y = element_text(color = "black", size = 14)) +  
        stat_density2d(aes(fill = ..level..), geom = "polygon", n = 100, contour = TRUE) +
        scale_fill_gradientn(colors = topo.colors(a)) + guides(fill = "none") + 
        # Annotate horizontal gridlines
        annotate("segment", x = -Inf, xend = Inf, y = y_gridlines, yend = y_gridlines, color = "grey90", linetype = "dashed") +
        # Annotate vertical gridlines
        annotate("segment", x = x_gridlines, xend = x_gridlines, y = -Inf, yend = Inf, color = "grey90", linetype = "dashed") +
        geom_path(data = df, aes(x = long, y = lat, group = factor(id), colour = factor(id))) +
        xlab("Relative distance E-W") + ylab("Relative distance N-S") + guides(colour = "none")
      
      #########
      
      # Show each plot one by one, waiting for user input before proceeding to the next
      # List of plot lists
      if(is.null(turn.angle) != TRUE){
        plot_lists <- list(plot_list.1, plot_list.2, plot_list.3)
      } else {
        plot_lists <- list(plot_list.1, plot_list.3)
      }
      
      # Show each group of plots one by one, waiting for user input before proceeding to the next
      for(i in 1:length(plot_lists)) {
        plots <- plot_lists[[i]]
        grid.arrange(grobs = plots)
        readline(prompt = "Press [Enter] to continue")
      }
      # Now print the fourth plot
      print(agent.based.model)
    }
    
    readline(prompt = "Press [Enter] to continue")
    #Print autocorrelations
    if(autocorr.head == TRUE){
      print(paste("1st order autocorrelations of heading series by sector:"))
      print(as.numeric(paste(lapply(ACF_heading_list, round, digits = 3))))
    }
    if(is.null(turn.angle) != TRUE){
      if(autocorr.turn.angle == TRUE){
        print(paste("1st order autocorrelations of turn angle series by sector:"))
        print(as.numeric(paste(lapply(ACF_turn.angle_list, round, digits = 3))))
      }
    }
    if(autocorr.step.length == TRUE){
      print(paste("1st order autocorrelations of step length series by sector:"))
      print(as.numeric(paste(lapply(ACF_step.length_list, round, digits = 3))))
    }
    
    # Return data frame
    return(df)
  })
}    


################################################### End of function ###################################################
  
# Example
# z = Gundog.sim(heading = df$Step.bear, turn.angle = df$Step.ang, step.length = df$Step.dist, ID = df$ID, ID_head = "P19B", agents = 30,
              # sectors.heading = 10, sectors.turn.angle = 4, sectors.step.length = 4, 
              # max.distance.moved = 173000, quant.mean.head = TRUE,
              # quantile.turn.angle = 0.99, quantile.step.length = 0.95, quantile.group = TRUE, 
              # bins.heading = 2, bins.turn.angle = 2, bins.step.length = 5, mid.point = TRUE, 
              # autocorr.head = TRUE, autocorr.turn.angle = TRUE, autocorr.step.length = TRUE,
              # lo = 0, la = 0, seed = 1, plot = TRUE)

#' NEE an ET calc
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' This function is meant to perform the linear and non-linear fitting to
#' `NEE` data.
#' @param path files path name.
#' @param param chose fitting model "nee" or "et".
#' @param skip integer: the number of lines of the data file to skip
#'              before beginning to read data.
#' @param vol The volume of the chamber in cubic meters.
#' @param area The area of the chamber in square meters.
#'
#' @return  It will produce a plot of the data, queery the user if
#'          they would like to modify the time interval over which
#'          the data is fit, then print the paste statement of data
#'          values.
#' @export
#' @author Alex Brummer
nee_et_calc <- function(path = NULL, param = "et", skip = 9, vol = 2.197, area = 1.69){
  if (is.null(path)) {
    readline("Please set the working directory to the folder \n that contains the LiCOR files to be analyzed. \n Do so with the upcoming prompt. \n Note that you must choose a(any) file in the \n folder that you want to set as the working directory. \n Please press 'return' to continue.")
    files <- list.files(dirname(file.choose()), full.names = TRUE)
  } else {
    files <- list.files(path, full.names = TRUE)
  }
  # select txt files
  files <- grep("\\.txt$", files,value = TRUE)

  ##' Identify all file names corresponding to photosynthesis and respiration measurements
  photo.names <- grep("[^resp].txt", grep("[^_a]\\.txt", files, value = TRUE), value = TRUE)
  ambient.names <- grep("a.txt", files, value = TRUE)
  resp.names <- grep("resp.txt", files, value = TRUE)

  neeet.fit <- function(filename){
    ##' For reading the .txt files, replace read.csv with read.table, and add a skip = 9 parameter to the function.
    input <- utils::read.table(filename, header = FALSE, skip = skip)
    if(nrow(input) > 90){
      input <- input[1:90,]
    }
    #####
    if(length(ambient.names) < 1){
      ambient <- input[1:5,]
    }
    else if(length(ambient.names) >= 1){
      if(length(grep("resp.txt", filename, ignore.case = TRUE, value = FALSE)) == 1){
        ambient_file <- paste(strsplit(filename, "resp.txt"), "a.txt", sep = "")
      }else if(length(grep("photo", filename, ignore.case = TRUE, value = FALSE)) == 1){
        ambient_file <- paste(strsplit(filename, "photo.txt"), "a.txt", sep = "")
      }else{
        ambient_file <- paste(strsplit(filename, ".txt"), "a.txt", sep = "")
      }

      if(ambient_file %in% ambient.names){
        ambient <-  utils::read.table(ambient_file, header = FALSE, skip = skip)
      }
      else{
        ambient <- input[1:5,]
      }
    }
    #  /// define constants - for Enquist Tent///
    vol   # m^3, big tent volume
    area   # m^2, big tent area
    R = 8.314472 	# J/mol K

    ## Define vectors to work with
    ## variables clas corrections form factor to numeric
    time <- as.numeric(as.character(input[,1])) #s
    co2 <- as.numeric(as.character(input[,8])) #umol/mol
    h2o <- as.numeric(as.character(input[,12])) #mmol/mol
    par <- as.numeric(as.character(input[,4])) # don't think there's any data here
    press <- as.numeric(as.character(input[,3])) #kPa
    temp <- as.numeric(as.character(input[,2])) #C # review this var with CESAR

    #  /// average T and P per measurement ///
    tav <- mean(temp)
    pav <- mean(press)
    cav <- mean(co2)
    wav <- mean(h2o)

    ## This is the dilution correction used to account for the fact that measurements are made using wet air, but values need to be reported with respect to dry air.  Calculation is done with both CO2 and H2O measurements.
    cprime <- co2/(1-(h2o/1000))
    wprime <- h2o/(1-(h2o/1000))

    # in addition to dilution corretion for h2o, calculate average value
    #for use later.
    wav_dil <- mean(h2o/(1-(h2o/1000)))

    #ambient co2 and water measurement to determine if leak is occurring.
    camb <- mean(as.numeric(as.character(ambient[,8]))/
                   (1-(as.numeric(as.character(ambient[,12]))/1000)))

    wamb <- mean(as.numeric(as.character(ambient[,12]))/
                   (1-(as.numeric(as.character(ambient[,12]))/1000)))



    #  /// Plotting the CO2 vs. Time Curve //

    if("nee" == param){
      cw_prime <- cprime
    }
    else if("et" == param){
      cw_prime <- wprime
    }


    plot(cw_prime~(time), main = filename)

    ## Queery user for start time for fitting.  Default is set to 10 in the if() statement
    tstart <- readline("Enter preferred start time for fitting. \n Do not include units. \n Round to nearest integer second. \n Do not use 0. \n  If default of 10s is preferred, press 'return':")
    if(!grepl("^[0-9]+$", tstart)){
      tstart <- 10
    }
    tstart <- as.integer(tstart)

    ## Queery user for finish time for fitting.  Default is set to 80 in the if() statement
    tfinish <- readline("Enter preferred finish time for fitting. \n Do not include units. \n Round to nearest integer second. \n  If default of 80s is preferred, press 'return':")
    if(!grepl("^[0-9]+$", tfinish)){
      tfinish <- 80
    }
    tfinish <- as.integer(tfinish)

    ## Linear fitting code.

    linear.fit <- stats::lm(cw_prime[tstart:tfinish]~(time[tstart:tfinish]))

    ## AIC value for linear model for later comparison with non-linear model.
    aic.lm <- stats::AIC(linear.fit)

    # Calculate intercept
    inter<- as.numeric(linear.fit$coeff[1])

    # Calculate slope
    dcw_dt <- as.numeric(linear.fit$coeff[2])

    # Calculate r-squared (we're not reporting chi-squared significance from the non-linear fit, so this may not be so necessary)
    rsqd <- summary(linear.fit)$r.sq


    # Calculate nee from linear model.  Factor of 1000 is to convert pressure from kilo-Pascals to Pascals, minus sign is to account for direction of flux.
    if("nee" == param){
      param_lm <- -(vol*pav*(1000)*dcw_dt) / (R*area*(tav + 273.15))	# in umol/m2/s
    }
    else if("et" == param){
      param_lm <- (vol*pav*(1000)*dcw_dt) / (R*area*(tav + 273.15))	# in umol/m2/s
    }

    # Make plot of line.
    abline(inter,dcw_dt, col=6)

    ## Non-linear fitting code.
    # Set cnot to the actual first value of cprime used in the fitting time domain.
    cw_not = cw_prime[tstart]

    # Define a temporary data frame from which the functional variables come from.
    df = data.frame(cw_prime, time)

    # Try subsetting df external to nls and nls2 functions.
    df <- subset(x = df, subset = (time > tstart & time < tfinish))

    # Define boundaries of parameter grid.  May need to modify for evapotranspiration.
    strt <- data.frame(A = c(150, 850), B = c(0, 1000))

    # Use nls2() to scan through parameter grid, searching for "best" actual starting points.  control variable is set to prevent warnings from ending loop.
    optimize.start <- nls2::nls2(cw_prime ~ (cw_not - A)*exp(-time/B) + A,
                                 data = df,
                                 start = strt,
                                 algorithm = "brute-force",
                                 control = stats::nls.control(warnOnly = TRUE),
                                 trace = FALSE) #(A=375, B=40)

    # Run nls() with the optimized starting values from previous nls2().
    # Control variable is set to prevent warnings from ending loop.  However,
    # they will still be printed at end of run.  When this happens, it is
    # indicative of the fact that the function parameters (A and B) are large
    # (non-physical) for the fitting, yet still produce a fit.  This is worth
    # further investigation.  However, it appears that the nee value produced by
    # the exponential model in such circumstances does not deviate from the linear
    # model by much more than half a percent.  Add a "trace = TRUE" parameter
    # setting to the nls() function to be able to watch the values of A and B
    # change with each iteration.  A is Css and B is tau, from Saleska et al.,
    # and to translate to variables extracted from fit further down.
    uptake.fm <- stats::nls(cw_prime ~ (cw_not - A)*exp(-time/B) + A,
                     data = df,
                     start = stats::coef(optimize.start),
                     control = stats::nls.control(warnOnly = TRUE),
                     trace = FALSE)

    ##
    sigma <- summary(uptake.fm)$sigma

    ## AIC value for non-linear model for later comparison with linear model.
    aic.nlm <- stats::AIC(uptake.fm)

    cw_ss = summary(uptake.fm)$param[1]
    tau = summary(uptake.fm)$param[2]

    if("nee" == param){
      nee_exp <- ((camb-cw_ss)/(area*tau))*(vol*pav*(1000)/(R*(tav + 273.15)))
      #equation 4 in Saleska 1999
    }
    else if("et" == param){
      flux_exp <- -((wamb-cw_ss)/(area*tau))*(vol*pav*(1000-wav)/(R*(tav + 273.15)))
      #equation 4 in Saleska 1999
    }

    curve((cw_not - cw_ss)*exp(-(x)/tau) + cw_ss, col = 4, add = TRUE)	#equation 3 in Saleska 1999 to plot for visual inspection.##

    #### Extracting filename information with flexibility

    ## Season can be extracted from the parent folder.  Use if/else if/else statement to check for early, peak, late within the parent folder name to indicate season.
    ## Now searching for time.  this will be the day/night indicator also in the parent folder name.  just use that.

    if(grepl("day", filename) == TRUE){
      time <- "day"
    }else if(grepl("night", filename) == TRUE){
      time <- "night"
    }

    if(length(grep("resp", filename, ignore.case = TRUE, value = FALSE)) == 1){
      time <- paste(time, "resp", sep = "")
    }

    if("nee" == param){
      print(tibble::tibble("filename" = filename, "tstart" = tstart, "tfinish" = tfinish,
                           "time" = time, "nee_lm" = param_lm, "nee_exp" = nee_exp,
                           "rsqd" = rsqd, "sigma" = sigma, "aic.lm" = aic.lm,
                           "aic.nlm" = aic.nlm))
      replicate <- readline("Would you like to redo the fitting with \n a different time domain? (y/n)")
      if(replicate == "y"){
        neeet.fit(filename)
      } else {
        return(tibble::tibble(filename, tstart,tfinish,time,camb,tav,pav,param_lm,nee_exp,rsqd,sigma,aic.lm, aic.nlm))
      }
    }
    else if("et" == param){
      print(tibble::tibble("filename" = filename, "tstart" = tstart, "tfinish" = tfinish,
                           "time" = time, "flux_lm" = param_lm, "flux_nlm" = flux_exp,
                           "rsqd" = rsqd, "nlm_sigma" = sigma, "aic.lm" = aic.lm,
                           "aic.nlm" = aic.nlm))
      replicate <- readline("Would you like to redo the fitting with \n a different time domain? (y/n)")
      if(replicate == "y"){
        neeet.fit(filename)
      } else {
        return(tibble::tibble(filename, tstart,tfinish,time,wamb,tav,pav,cav,param_lm,flux_exp,rsqd,sigma,aic.lm,aic.nlm))
        # comment/uncomment to use print statement for including non-linear fitting with aic scores
        # return(c(tstart,tfinish,time,wamb,tav,pav,cav,flux_lm,flux_exp,rsqd,sigma,aic.lm, aic.nlm))
      }
    }

  }

  ##########################
  if(length(photo.names) > 1 & length(resp.names) > 1){
    stats.df <- purrr::map_df(c(photo.names, resp.names), ~neeet.fit(.))
  }
  else if(length(resp.names) >= 1){
    stats.df <- purrr::map_df(resp.names, ~neeet.fit(.))
  }
  else if(length(photo.names) >= 1){
    stats.df <- purrr::map_df(photo.names, ~neeet.fit(.))
  }
  # rename dataframe
  if("nee" == param){
    names.vec <- c("filename","tstart", "tfinish",
                   "time", "camb", "tav", "pav", "nee_lm", "nee_exp",
                   "LM rsqd", "non-linear sigma", "aic_lm", "aic_nlm")
  }
  else if("et" == param){
    names.vec <- c("filename","tstart", "tfinish",
                   "time", "wamb", "tav", "pav", "cav", "flux_lm", "flux_nlm",
                   "LM rsqd", "non-linear sigma", "aic_lm", "aic_nlm")
  }

  names(stats.df) <- names.vec
  #return
  utils::head(stats.df)
  return(stats.df)
 ### utils::write.csv(stats.df,
 ###           file = paste(getwd(), "/", path, "/",
 ###                        toupper(param),
 ###                        "summary_new",
 ###                        ".csv", sep = ""),
 ###           row.names = FALSE)
}

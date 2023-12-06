#' NEE and ET Calculation from LiCOR Data
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This function is designed to calculate Net Ecosystem Exchange (NEE) and
#' Evapotranspiration (ET) by performing linear and non-linear fitting to LiCOR data.
#' It incorporates various data preprocessing steps to ensure accurate interpretation,
#' including unit conversion, dilution corrections, and handling of different types of
#'  variables.
#'
#' @param fluxfiles A list of LiCOR data files obtained from the `read_files` function,
#' including 'photo_names', 'resp_names', and 'ambient_names'.
#' @param param A character string specifying the model to be fitted. Use "nee" for NEE
#' calculation or "et" for ET calculation.
#' @param skip An integer representing the number of lines to skip when reading
#' the data files.
#' @param vol The volume of the chamber in cubic meters.
#' @param area The area of the chamber in square meters.
#'
#' @return A data frame containing calculated parameters, including model coefficients,
#' R-squared values, AIC scores, and other relevant information.
#'
#' @author Alex Brummer
#' @export
#'
flux_calc <- function(fluxfiles, param = "et", skip = 9, vol = 2.197, area = 1.69) {


  neeet.fit <- function(filename) {
    input <- utils::read.table(filename, header = FALSE, skip = skip)
    # Take only the first 90 observations of each file
    if (nrow(input) > 90) {
      input <- input[1:90, ]
    }
    # If no ambient measurements, take the first five measurements of the file as ambient
    if (length(fluxfiles$ambient_names) < 1) {
      ambient <- input[1:5, ]
    # If ambient exists, use that instead
    } else if (length(fluxfiles$ambient_names) >= 1) {
      # How to parse if we're working with A, P, or R
      if (length(grep("resp.txt", filename, ignore.case = TRUE, value = FALSE)) == 1) {
        ambient_file <- paste(strsplit(filename, "resp.txt"), "a.txt", sep = "")
      } else if (length(grep("photo.txt", filename, ignore.case = TRUE, value = FALSE)) == 1) {
        ambient_file <- paste(strsplit(filename, "photo.txt"), "a.txt", sep = "")
      } else {
        ambient_file <- paste(strsplit(filename, ".txt"), "a.txt", sep = "")
      }
    # If we have ambient, use it. Otherwise use first five.
      if (ambient_file %in% fluxfiles$ambient_names) {
        ambient <- utils::read.table(ambient_file, header = FALSE, skip = skip)
      } else {
        ambient <- input[1:5, ]
      }
    }
    #  /// define constants ///
    vol # m^3, big tent volume
    area # m^2, big tent area
    R <- 8.314472 # J/mol K

    ## Define vectors to work with
    ## variables class corrections form factor to numeric
    # RENAMES THE COLUMNS. FIX TO MAKE REPRODUCIBLE. 
    time <- as.numeric(as.character(input[, 1])) # s
    co2 <- as.numeric(as.character(input[, 8])) # umol/mol
    h2o <- as.numeric(as.character(input[, 12])) # mmol/mol
    par <- as.numeric(as.character(input[, 4])) # don't think there's any data here
    press <- as.numeric(as.character(input[, 3])) # kPa
    temp <- as.numeric(as.character(input[, 2])) # C # review this var with CESAR

    #  /// average T and P per measurement ///
    tav <- mean(temp)
    pav <- mean(press)
    cav <- mean(co2)
    wav <- mean(h2o)

    ## This is the dilution correction used to account for the fact that measurements are made using wet air, but values need to be reported with respect to dry air.  Calculation is done with both CO2 and H2O measurements.
    cprime <- co2 / (1 - (h2o / 1000))
    wprime <- h2o / (1 - (h2o / 1000))

    # in addition to dilution corretion for h2o, calculate average value
    # for use later.
    wav_dil <- mean(h2o / (1 - (h2o / 1000)))

    # ambient co2 and water measurement to determine if leak is occurring.
    # USE COLUMN NAME INSTEAD
    camb <- mean(as.numeric(as.character(ambient[, 8])) /
                   (1 - (as.numeric(as.character(ambient[, 12])) / 1000)))
    # USE COLUMN NAME INSTEAD
    wamb <- mean(as.numeric(as.character(ambient[, 12])) /
                   (1 - (as.numeric(as.character(ambient[, 12])) / 1000)))

    #  /// Plotting the CO2 vs. Time Curve //

    if ("nee" == param) {
      cw_prime <- cprime
    } else if ("et" == param) {
      cw_prime <- wprime
    }

    if ("nee" == param) {
      tag <- "c_prime"
    } else if ("et" == param) {
      tag <- "w_prime"
    }

    # Would it be better to use GGPLOT?
    plot(cw_prime ~ (time), main = filename, ylab = tag)

    # Query user for start time for fitting.  Default is set to 10 in
    # the if() statement
    tstart <- readline("Enter preferred start time for fitting. Do not include units. \n Round to nearest integer second. \n Do not use 0. \n  If default of 10s is preferred, press 'return':")
    if (!grepl("^[0-9]+$", tstart)) {
      tstart <- 10
    }
    tstart <- as.integer(tstart)

    tfinish <- readline("Enter preferred finish time for fitting. Do not include units. \n Round to nearest integer second. \n  If default of 80s is preferred, press 'return':")
    if (!grepl("^[0-9]+$", tfinish)) {
      tfinish <- 80
    }
    tfinish <- as.integer(tfinish)

    ## Linear fitting code.

    linear.fit <- stats::lm(cw_prime[tstart:tfinish] ~ (time[tstart:tfinish]))

    ## AIC value for linear model for later comparison with non-linear model.
    aic.lm <- stats::AIC(linear.fit)

    # Calculate intercept
    inter <- as.numeric(linear.fit$coeff[1])

    # Calculate slope
    dcw_dt <- as.numeric(linear.fit$coeff[2])

    rsqd <- summary(linear.fit)$r.sq
    if ("nee" == param) {
      param_lm <- -(vol * pav * (1000) * dcw_dt) / (R * area * (tav + 273.15)) # in umol/m2/s
    } else if ("et" == param) {
      param_lm <- (vol * pav * (1000) * dcw_dt) / (R * area * (tav + 273.15)) # in umol/m2/s
    }

    # Make plot of line.
    abline(inter, dcw_dt, col = 6)

    cw_not <- cw_prime[tstart]

    df <- data.frame(cw_prime, time)

    df <- subset(x = df, subset = (time > tstart & time < tfinish))

    strt <- data.frame(A = c(150, 850), B = c(0, 1000))
    # Required for the least squared
    optimize.start <- nls2::nls2(cw_prime ~ (cw_not - A) * exp(-time / B) + A,
                                 data = df,
                                 start = strt,
                                 algorithm = "brute-force",
                                 control = stats::nls.control(warnOnly = TRUE),
                                 trace = FALSE
    )
    # (A=375, B=40)
    # Least squared model
    uptake.fm <- stats::nls(cw_prime ~ (cw_not - A) * exp(-time / B) + A,
                            data = df,
                            start = stats::coef(optimize.start),
                            control = stats::nls.control(warnOnly = TRUE),
                            trace = FALSE
    )

    ##
    sigma <- summary(uptake.fm)$sigma

    ## AIC value for non-linear model for later comparison with linear model.
    aic.nlm <- stats::AIC(uptake.fm)

    cw_ss <- summary(uptake.fm)$param[1]
    tau <- summary(uptake.fm)$param[2]
    # If working with NEE, use this
    if ("nee" == param) {
      nee_exp <- ((camb - cw_ss) / (area * tau)) * (vol * pav * (1000) / (R * (tav + 273.15)))
      # equation 4 in Saleska 1999
      # If working with ET, use this
    } else if ("et" == param) {
      flux_exp <- -((wamb - cw_ss) / (area * tau)) * (vol * pav * (1000 - wav) / (R * (tav + 273.15)))
      # equation 4 in Saleska 1999
    }

    curve((cw_not - cw_ss) * exp(-(x) / tau) + cw_ss, col = 4, add = TRUE) # equation 3 in Saleska 1999 to plot for visual inspection.##

    #if (grepl("day", filename) == TRUE) {
    #  time <- "day"
    #} else if (grepl("night", filename) == TRUE) {
    #  time <- "night"
    #}
    #
    #if (length(grep("resp", filename, ignore.case = TRUE, value = FALSE)) == 1) {
    #  time <- paste(time, "resp", sep = "")
    #}

    if ("nee" == param) {
      print(tibble::tibble(
        "filename" = filename, "tstart" = tstart, "tfinish" = tfinish,
        "nee_lm" = param_lm, "nee_exp" = nee_exp,
        "rsqd" = rsqd, "sigma" = sigma, "aic.lm" = aic.lm,
        "aic.nlm" = aic.nlm
      ))
      replicate <- readline("Would you like to redo the fitting with \n a different time domain? (y/n)")
      if (replicate == "y") {
        neeet.fit(filename)
      } else {
        return(tibble::tibble(filename, tstart, tfinish, camb, tav,
                              pav, param_lm, nee_exp, rsqd, sigma, aic.lm, aic.nlm))
      }
    } else if ("et" == param) {
      print(tibble::tibble(
        "filename" = filename, "tstart" = tstart, "tfinish" = tfinish,
        "flux_lm" = param_lm, "flux_nlm" = flux_exp,
        "rsqd" = rsqd, "nlm_sigma" = sigma, "aic.lm" = aic.lm,
        "aic.nlm" = aic.nlm
      ))
      replicate <- readline("Would you like to redo the fitting with \n a different time domain? (y/n)")
      if (replicate == "y") {
        neeet.fit(filename)
      } else {
        return(tibble::tibble(filename, tstart, tfinish, wamb, tav,
                              pav, cav, param_lm, flux_exp, rsqd, sigma,
                              aic.lm, aic.nlm))
        # comment/uncomment to use print statement for including non-linear
        # fitting with aic scores
        # return(c(tstart,tfinish,time,wamb,tav,pav,cav,flux_lm,flux_exp,
        # rsqd,sigma,aic.lm, aic.nlm))
      }
    }
  }

  ##########################
  # If working with multiples of both P and R, do this
  # Do we need to change to a >= for the instance of only one of each?
  if (length( fluxfiles$photo_names) > 1 & length( fluxfiles$resp_names) > 1) {
    stats.df <- purrr::map_df(c(fluxfiles$photo_names, fluxfiles$resp_names), ~ neeet.fit(.))
    # If only resp, do this
  } else if (length(fluxfiles$resp_names) >= 1) {
    stats.df <- purrr::map_df(fluxfiles$resp_names, ~ neeet.fit(.))
    # If only photo, do this
  } else if (length(fluxfiles$photo_names) >= 1) {
    stats.df <- purrr::map_df(fluxfiles$photo_names, ~ neeet.fit(.))
  }
  # rename dataframe
  if ("nee" == param) {
    names.vec <- c(
      "filename", "tstart", "tfinish",
      "camb", "tav", "pav", "nee_lm", "nee_exp",
      "lm_rsqd", "non_linear_sigma", "aic_lm", "aic_nlm"
    )
  } else if ("et" == param) {
    names.vec <- c(
      "filename", "tstart", "tfinish",
      "wamb", "tav", "pav", "cav", "flux_lm", "flux_nlm",
      "lm_rsqd", "non_linear_sigma", "aic_lm", "aic_nlm"
    )
  }

  names(stats.df) <- names.vec
  # return

  return(stats.df)

}

gaussFit <- function(t, y, tm = TRUE, beta = FALSE){
  # 20181025 using 'port' algorithm for nls fit to try to test out 
  # setting lower and upper bounds for fitted values, done mostly
  # to constrain h
  # this could beg the question to not use the fitted value for h
  # but to calculated it separately based on the fitted values
  # of ti and tt
  # set some parameters to avoid recoding
  chlorophyll <- y
  yday <- t
  state <- list()
  state$tm <- tm
  state$beta <- beta
  # set lower and upper limits
  # limits decided on from initial fits using 'port' algorithm
  # note that due to poor fit of h, the ranges were determined
  #   from other bloom fit methods
  lower <- list(B0 = -1,
                h = 0,
                sigma = 0,
                beta = -0.02,
                tm = 30)
  upper <- list(B0 = 5,
                h = 350,
                sigma = 100,
                beta = 0.01,
                tm = 180)
  # do fit
  params <- vector(mode = 'list')
  params[[1]] <- list(B0 = 0.5,
                      h = 50,
                      sigma = 10,
                      beta = -0.002,
                      tm = 100)
  params[[2]] <- list(B0 = 0.5,
                      h = 50,
                      sigma = 2,
                      beta = -0.002,
                      tm = 100)
  params[[3]] <- list(B0 = 0.5,
                      h = 10,
                      sigma = 2,
                      beta = -0.001,
                      tm = 100)
  params[[4]] <- list(B0 = 0.5,
                      h = 10,
                      sigma = 1,
                      beta = -0.001,
                      tm = 100)
  # state$gausscoef
  # 1 - infer tm from data
  # 2 - omit beta t term
  # need if statement for when 1 or 2 is selected, and 1 and 2 are/are not selected
  #{if ("tm" %in% state$gausscoef && length(state$gausscoef) == 1) { # only infer tm is selected
  # 20181022 switch TRUE and FALSE for all if statements to make it work correctly
  # I originally miss-understood the TRUE/FALSE for the slider in the app
  {if (state$tm == FALSE && state$beta == TRUE){
    tm <- yday[which.max(chlorophyll)]
    for (i in 1:length(params)){
      fit <- NULL
      parms <- params[[i]]
      p <- parms[names(parms) != 'tm']
      l <- lower[names(lower) != 'tm']
      u <- upper[names(upper) != 'tm']
      try(fit <- nls(B ~ B0 + beta * t + h / (sqrt(2*pi) * sigma) * exp(- (t - tm)^2 / (2 * sigma^2)),
                     data = list(B = chlorophyll, t = yday, tm = tm),
                     algorithm = 'port', lower = l, upper = u,
                     start = p),
          silent = TRUE)
      if(!is.null(fit)) break
    }
    #} else if ( "beta" %in% state$gausscoef && length(state$gausscoef) == 1){ # only omit beta*t parameter
  } else if (state$tm == TRUE && state$beta == FALSE){
    for (i in 1:length(params)){
      fit <- NULL
      parms <- params[[i]]
      p <- parms[names(parms) != 'beta']
      l <- lower[names(lower) != 'beta']
      u <- upper[names(upper) != 'beta']
      try(fit <- nls(B ~ B0 + h / (sqrt(2*pi) * sigma) * exp(- (t - tm)^2 / (2 * sigma^2)),
                     data = list(B = chlorophyll, t = yday),
                     algorithm = 'port', lower = l, upper = u,
                     start = p),
          silent = TRUE)
      if(!is.null(fit)) break
    }
    #}  else if (length(state$gausscoef) == 2){ # infer tm from data AND omit beta*t parameter
  } else if (state$tm == FALSE && state$beta == FALSE){
    tm <- yday[which.max(chlorophyll)]
    for (i in 1:length(params)){
      fit <- NULL
      parms <- params[[i]]
      p <- parms[names(parms) != 'tm' & names(parms) != 'beta'] 
      l <- lower[names(lower) != 'tm' & names(lower) != 'beta']
      u <- upper[names(upper) != 'tm' & names(upper) != 'beta']
      try(fit <- nls(B ~ B0 + h / (sqrt(2*pi) * sigma) * exp(- (t - tm)^2 / (2 * sigma^2)),
                     data = list(B = chlorophyll, t = yday, tm = tm),
                     algorithm = 'port', lower = l, upper = u,
                     start = p),
          silent = TRUE)
      if(!is.null(fit)) break
    }
    #} else if (is.null(state$gausscoef)) { # apply prescribed gaussian
    #} else {
  } else if (state$tm == TRUE && state$beta == TRUE){
    for (i in 1:length(params)){
      fit <- NULL
      try(fit <- nls(B ~ B0 + beta * t + h / (sqrt(2*pi) * sigma) * exp(- (t - tm)^2 / (2 * sigma^2)),
                     data = list(B = chlorophyll, t = yday),
                     algorithm = 'port', lower = lower, upper = upper,
                     start = params[[i]]),
          silent = TRUE)
      if(!is.null(fit)) break
    }
  }
  }
  # 20181022 switch TRUE and FALSE for all if statements to make it work correctly
  # I originally miss-understood the TRUE/FALSE for the slider in the app
values <- list()  
  if(!is.null(fit)){
    H <- coef(fit)[2] / (sqrt(2 * pi) * coef(fit)[3])
    # B0, h, and sigma are always the 1,2,3rd coef
    values$B0 <- unname(round(coef(fit)[1], 2))
    values$h <- unname(round(coef(fit)[2], 2))
    values$sigma <- unname(round(coef(fit)[3], 2))
    values$H <- unname(H)
    if(state$tm == FALSE && state$beta == TRUE){
      # ti = tm - 1.79*sigma
      ti <- tm - 1.79 * coef(fit)[3]
      # td = 2*(tm - ti) = 3.59*sigma
      td <- 3.59 * coef(fit)[3]            
      values$beta <- unname(round(coef(fit)[4], 3))
      values$tm <- unname(round(tm))
      values$ti <- unname(round(ti))
      values$td <- unname(round(td))
      bkrnd <- values$B0 + (values$beta*(values$ti:(values$ti + values$td)))
      bkrndBm <-  values$B0 + (values$beta*values$tm)
    }
    if(state$tm == TRUE && state$beta == FALSE){
      ti <- coef(fit)[4] - 1.79 * coef(fit)[3]
      td <- 3.59 * coef(fit)[3]   
      values$tm <- unname(round(coef(fit)[4]))
      values$ti <- unname(round(ti))
      values$td <- unname(round(td))
      bkrnd <- bkrndBm <- values$B0
    }
    if(state$tm == TRUE && state$beta == TRUE){
      # beta and tm are coefs 4, 5
      # ti = tm - 1.79 * sigma
      ti <- coef(fit)[5] - 1.79 * coef(fit)[3]
      # td = 3.59 * sigma
      td <- 3.59 * coef(fit)[3] 
      values$beta <- unname(round(coef(fit)[4], 3))
      values$tm <- unname(round(coef(fit)[5]))
      values$ti <- unname(round(ti))
      values$td <- unname(round(td))
      bkrnd <- values$B0 + (values$beta*(values$ti:(values$ti + values$td)))
      bkrndBm <-  values$B0 + (values$beta*values$tm)
    }
    if(state$tm == FALSE && state$beta == FALSE){
      ti <- tm - 1.79 * coef(fit)[3]
      td <- 3.59 * coef(fit)[3]
      values$tm <- unname(round(tm))
      values$ti <- unname(round(ti))
      values$td <- unname(round(td))
      bkrnd <- bkrndBm <- values$B0
    }
    values$tt <- values$ti + values$td
    Bmind <- which.min(abs(values$tm - yday))
    values$Bm <- round(predict(fit)[Bmind], 2)
    # calculate H and h using new method
    tiidx <- which.min(abs(yday - values$ti))
    ttidx <- which.min(abs(yday - values$tt))
    if(state$beta == FALSE){
      bkrnd <- bkrndBm <- values$B0
    }
    if(state$beta == TRUE){
      bkrnd <- values$B0 + (values$beta*(yday[tiidx:ttidx]))
    }
    values$h2 <- sum(diff(yday[tiidx:ttidx]) * (head(chlorophyll[tiidx:ttidx] - bkrnd, -1) + tail(chlorophyll[tiidx:ttidx] - bkrnd, -1))/2)
    values$H2 <- values$Bm - bkrndBm
  }
  if(!is.null(fit) && values$ti < 0){
    fit <- NULL
    values$B0 <- values$h <- values$sigma <- values$tm <- values$ti <- values$td <- values$tt <- values$Bm <- values$h <- values$h2 <- values$H <- values$H2 <- NA
  }
  
  if(is.null(fit)){
    values$B0 <- values$h <- values$sigma <- values$tm <- values$ti <- values$td <- values$tt <- values$Bm <- values$h <- values$h2 <- values$H <- values$H2 <- NA
  }

  return(list(fit = fit, values = values))
}
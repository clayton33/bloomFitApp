library(shiny)
library(shinyWidgets)
library(oce)
library(ocedata)
library(quantreg)
source('../R/HANTS.R')
source('../R/rateOfChange.R')
source('../R/threshold.R')
source('../R/gaussFit.R')
source('00_regionBoxes.R')
data("coastlineWorldFine")

regionlonlim <- list(maritimes = range(unlist(lapply(maritimes, function(k) k$lon))),
                     quebec = range(unlist(lapply(quebec, function(k) k$lon))),
                     newfoundland = range(unlist(lapply(newfoundland, function(k) k$lon))),
                     labradorSea = range(unlist(lapply(labradorSea, function(k) k$lon))))
regionlatlim <- list(maritimes = range(unlist(lapply(maritimes, function(k) k$lat))),
                     quebec = range(unlist(lapply(quebec, function(k) k$lat))),
                     newfoundland = range(unlist(lapply(newfoundland, function(k) k$lat))),
                     labradorSea = range(unlist(lapply(labradorSea, function(k) k$lat))))
allRegions <- list(maritimes = maritimes,
                   quebec = quebec, 
                   newfoundland = newfoundland, 
                   labradorSea = labradorSea)

years <- seq(2003, 2017)
names(years) <- years
ui <- fluidPage(
  # App title
  titlePanel("Satellite Chlorophyll Data Visualization"),
  fluidRow(
    column(2, wellPanel(
      h3("Select data, then click Run"),
      h4("DATA"),
      selectInput(inputId = "satellite",
                  label = h5("1. Choose a Satellite"),
                  choices = c('MODIS 4km' = 'modis')),
      selectInput(inputId = "year",
                  label = h5("2. Choose a year"),
                  choices = years,
                  selected = years[length(years)]),
      selectInput(inputId = "region",
                  label = h5("3a. Choose a region"),
                  choices = c('Maritimes' = 'maritimes',
                              'Quebec' = 'quebec',
                              'Newfoundland' = 'newfoundland',
                              'Labrador Sea' = 'labradorSea'),
                  selected = 'maritimes'),
      uiOutput(outputId = "box"),
      actionButton(inputId = 'run',
                   label = 'Run'),
      sliderInput(inputId = 'yearday',
                  label = 'Year Day',
                  min = 1,
                  max = 365, 
                  value = 1,
                  animate = TRUE),
      h4("DATA PROCESSING"),
      numericInput(inputId = 'percent',
                  label = h5("4. Minimum daily percent data coverage"),
                  value = 10,
                  min = 0,
                  max = 100),
      selectInput(inputId='outlier',
                  label = h5("5. Outlier detection method"),
                  choices = c('None' = 'none',
                              '+/- 2 sd' = 'sd2',
                              '+/- 3 sd' = 'sd3',
                              '1.5 IQR' = 'iqr15'),
                  selected = 'none'),
      selectInput(inputId = 'dailystat',
                  label = h5('6. Daily statistic'),
                  choices = c('Mean' = 'avg',
                              'Median' = 'med'),
                  selected = 'avg'),

      h4("BLOOM FITTING"),
      selectInput(inputId = 'fitmethod',
                  label = h5('7. Fit Method'),
                  choices = list('Shifted Gaussian' = 'gauss',
                              'Rate Of Change' = 'roc',
                              'Threshold' = 'thresh'),
                  selected = 'gauss'),
      conditionalPanel(condition = "input.fitmethod == 'gauss'",
                       switchInput(inputId = 'tm', 
                                   label = 'tm',
                                   value = FALSE),
                       switchInput(inputId = 'beta',
                                   label = '\u03B2t',
                                   value = FALSE)),
      conditionalPanel(condition = "input.fitmethod == 'roc'",
                       selectInput(inputId = 'rocshape',
                                   label = h6('Bloom shape'),
                                   choices = list('Symmetric' = 'symmetric',
                                                  'Asymmetric' = 'asymmetric'),
                                   selected = 'symmetric'),
                       selectInput(inputId = 'rocmethod',
                                   label = h6('Method'),
                                   choices = list('LOESS' = 'loess',
                                                  'NONE' = 'nofit'),
                                   selected = 'loess')),
      conditionalPanel(condition = "input.fitmethod == 'roc' && input.rocmethod == 'loess'",
                       numericInput(inputId = 'loessSpan',
                                    label = h6('Span'),
                                    value = 1/3,
                                    min = 1/24,
                                    max = 1)),
      conditionalPanel(condition = "input.fitmethod == 'thresh'",
                       numericInput(inputId = 'threshold',
                                    label = h6('Threshold value, x*median'),
                                    value = 1.05,
                                    min = 1.00),
                       selectInput(inputId = 'threshshape',
                                   label = h6('Bloom shape'),
                                   choices = list('Symmetric' = 'symmetric',
                                                  'Asymmetric' = 'asymmetric'),
                                   selected = 'symmetric'),
                       selectInput(inputId = 'threshmethod',
                                   label = h6('Method'),
                                   choices = list('LOESS' = 'loess',
                                                  'NONE' = 'none'),
                                   selected = 'loess')),
      conditionalPanel(condition = "input.fitmethod == 'thresh' && input.threshmethod == 'loess'",
                       numericInput(inputId = 'threshSpan',
                                    label = h6('Span'),
                                    value = 1/3,
                                    min = 1/24,
                                    max = 1))      
    ) # closes well panel
      ),  # closes column 
    column(10,
           #verbatimTextOutput(outputId = "value"),
           plotOutput(outputId = 'fullmap',
                      height = '1050px'
                      #width = '750px'
                      ),
           plotOutput(outputId = 'bloomfit',
                      height = '350px',
                      click = 'bloomfit_click'
                      #width = '300px'
                      )
           
           ) #closes column for plots
  )
)


server <- function(input, output, session) {
  state <- reactiveValues()
  output$box <- renderUI({
    if(input$region == 'maritimes'){
      choices <- c('Central Scotian Shelf' = 'css',
                  'Eastern Scotian Shelf' = 'ess',
                  'Western Scotian Shelf' = 'wss',
                  'Georges Bank' = 'gb',
                  'Cabot Strait' = 'cs',
                  'Lurcher Shoal' = 'ls')
    }
    if(input$region == 'quebec'){
      choices <- c('Northwest Gulf of St. Lawrence' = 'nwgsl',
                   'Northeast Gulf of St. Lawrence' = 'negsl',
                   'Magdalen Shallows' = 'ms', 
                   'Cabot Strait' = 'cs')
      
    }
    if(input$region == 'newfoundland'){
      choices <- c('St. Pierre Bank' = 'spb',
                   'Southeast Shoal' = 'ses',
                   'Avalon Channel' = 'ac',
                   'Hibernia' = 'hib',
                   'Flemish Pass' = 'fp',
                   'Northeast Newfoundland Shelf' = 'nens',
                   'St. Anthony Bank' = 'sab',
                   'Hamilton Bank' = 'hb',
                   'Northern Labrador Shelf' = 'nls',
                   'Hudson Strait' = 'hs')
      
    }
    if(input$region == 'labradorSea'){
      choices <- c('North Central Labrador Shelf' = 'ncls',
                   'Greenland Shelf' = 'gs',
                   'Central Labrador Shelf' = 'cls',
                   'Eastern Labrador Shelf' = 'els',
                   'Bravo Station' = 'bra',
                   'Labrador Shelf' = 'las')
      
    }
    selectInput(inputId = 'box', label = h5('3b. Choose a box'),
                choices = choices,
                selected = head(choices, 1))
  })
  observeEvent(input$yearday,{
    state$yday <- input$yearday
    print(state$yday)
  })
  observeEvent(input$bloomfit_click,{
    state$bfclick <- input$bloomfit_click
  })
  # observeEvent(input$bloomfit_click,{
  #   test <- 1 # for debug
  #   state$yday <- nearPoints(dfbloomparms,
  #                            coordinfo = input$bloomfit_click,
  #                            xvar = "x",
  #                            yvar = "y")$x
  # })
  observeEvent(input$outlier, {
    state$outlier <- input$outlier
  })
  observeEvent(input$dailystat,{
    state$dailystat <- input$dailystat
  })
  observeEvent(input$percent, {
    state$percent <- input$percent / 100
  })
  #observeEvent(input$gausscoef,{
  #  state$gausscoef <- input$gausscoef
  #})
  
  # fit method
  observeEvent(input$fitmethod, {
    state$fitmethod <- input$fitmethod
  })

  # gaussian fit conditions
  observeEvent(input$tm,{
    state$tm <- input$tm
  })
  observeEvent(input$beta,{
    state$beta <- input$beta
  })
  # rate of change fit conditions
  observeEvent(input$rocmethod,{
    state$rocmethod <- input$rocmethod
  })
  observeEvent(input$rocshape, {
    state$rocshape <- input$rocshape
  })
    # loess fit conditions
  observeEvent(input$loessSpan,{
    state$loessSpan <- input$loessSpan
  })
  # threshold conditions
  observeEvent(input$threshmethod, {
    state$threshmethod <- input$threshmethod
  })  
  observeEvent(input$threshold, {
    state$threshold <- input$threshold
  })
  observeEvent(input$threshSpan, {
    state$threshSpan <- input$threshSpan
  })
  observeEvent(input$threshshape, {
    state$threshshape <- input$threshshape
  })
  #output$value <- renderPrint({input$gausscoef})
  observeEvent(input$run, {
    #load full map data
    fileall <- paste0('./data/atlantic/', input$satellite, input$year, 'ss.rda')
    load(fileall)
    #load region data
    file <- paste('./data', input$region, input$box, paste0(input$satellite, input$year, input$box,'.rda'), sep = '/')
    load(file)
    
    okallreg <- which(input$region == names(allRegions))
    pltreg <- allRegions[[okallreg]]
    
    #get coordinate limits based on input$region
    okreg <- which(input$region == names(regionlonlim))
    latlim <- regionlatlim[[okreg]]
    lonlim <- regionlonlim[[okreg]]
    
    # to plot box outline
    bxs <- names(pltreg)
    okbx <- which(input$box == bxs)
    
    # define region for use when giving initial conditions for fits
    region <- input$region
    
    #full map plot output
    output$fullmap <- renderPlot({
      
      # 1. Outlier method
      mean <- apply(rchla, 1, mean, na.rm = TRUE)
      sd <- apply(rchla, 1, sd, na.rm = TRUE)  
      median <- apply(rchla, 1, median, na.rm = TRUE)
      iqr <- apply(rchla, 1, IQR, na.rm = TRUE)        
      if(state$outlier == 'sd2'){
        limits <- matrix(nrow = length(sd), ncol = 2)
        limits[,1] <- -1 * 2 * sd + mean
        limits[,2] <- 1 * 2 * sd + mean
      } else if (state$outlier == 'sd3'){
        limits <- matrix(nrow = length(sd), ncol = 2)
        limits[,1] <- -1 * 3 * sd + mean
        limits[,2] <- 1 * 3 * sd + mean
      } else if (state$outlier == 'iqr15'){
        limits <- matrix(nrow = length(iqr), ncol = 2)
        limits[,1] <- -1 * 1.5 * iqr + median
        limits[,2] <- 1 * 1.5 * iqr + median
      }
      
      proj <- '+proj=merc'
      fillcol <- 'lightgrey'
      # #get coordinate limits based on input$region
      # okreg <- which(input$region == names(regionlonlim))
      # latlim <- regionlatlim[[okreg]]
      # lonlim <- regionlonlim[[okreg]]
      m <- c(1,1,2,1,1,2,3,3,3)
      layout(matrix(m, nrow = 3, ncol = 3, byrow = TRUE))
      # map of scotian shelf
      par(mar = c(2,2,2.5,1))
      zlim <- c(0,20) #mg/m^3 typical values found on SS
      cm <- colormap(z = sschla[state$yday,], zlim = zlim)
      mapPlot(coastlineWorldFine,
              grid = TRUE,
              col = fillcol,
              proj = proj,
              longitudelim = lonlim + c(-0, 0),
              latitudelim = latlim + c(-0, 0)
      )
      mapPoints(sslon, sslat, col = cm$zcol, pch = 20) 
      mtext(text = paste(as.POSIXct(time[state$yday], origin = '1970-01-01', tz = 'UTC')),
            side = 3, adj = 1)
      # mapLines(gblon, gblat, col = 'magenta', cex = 1.4)
      # mapLines(csslon, csslat, col = 'magenta', cex = 1.4)
      # mapText(longitude = -67.25, latitude = 41.51, labels = 'GB', col = 'magenta')
      # mapText(longitude = -63, latitude = 43.79, labels = 'CSS', col = 'magenta')
      
      # okallreg <- which(input$region == names(allRegions))
      # pltreg <- allRegions[[okallreg]]
      for (i in 1:length(pltreg)){
        bx <- names(pltreg)[i]
        srd <- pltreg[[i]]
        mapLines(longitude = srd$lon, latitude = srd$lat, 
                 col = 'magenta', cex = 1.4, lwd = 3)
        mapText(longitude = mean(range(srd$lon)), latitude = mean(range(srd$lat)), 
                labels = toupper(bx), col = 'magenta', cex = 2)
      }
      
      mapLines(longitude = pltreg[[okbx]]$lon, latitude = pltreg[[okbx]]$lat, 
               col = 'yellow', cex = 1.4, lwd = 3)
      
      # map of chosen box
      latlimr <- range(rlat)
      lonlimr <- range(rlon)
      par(mar = c(2,2,2.5,1))
      zlim <- c(0,20) #mg/m^3 typical values found on SS
      cmr <- colormap(z = rchla[state$yday,], zlim = zlim)
      drawPalette(colormap = cmr, pos = 4, zlab = expression('Chlorophyll ' * '[' * mg/m^3 * ']'), 
                  cex = 1, cex.axis = 1)
      mapPlot(coastlineWorldFine,
              grid = TRUE,
              col = fillcol,
              proj = proj,
              longitudelim = lonlimr + c(-0.5, 0.5),
              latitudelim = latlimr + c(-0.5, 0.5),
              cex = 4
      )
      mapPoints(rlon, rlat, col = cmr$zcol, pch = 20)
      # bxs <- names(pltreg)
      # ok <- which(input$box == bxs)
      mapLines(longitude = pltreg[[okbx]]$lon, latitude = pltreg[[okbx]]$lat, 
               col = 'magenta', cex = 1.4, lwd = 3)
      
      
      #histogram of chlorophyll in region for chosen year day
      ok <- length(which(!is.na(rchla[state$yday,]))) / length(rchla[1,]) > 0.1
      {if(ok){
        hist(rchla[state$yday,], 
           main = paste('Histogram of Chlorophyll concentration at day',state$yday),
           cex = 4)
        abline(v = mean[state$yday], lty = 1, col = 'black')
        abline(v = median[state$yday], lty = 1, col = 'red')
        {if(state$outlier != 'none'){
          abline(v= limits[state$yday,], lty = 3, col = 'blue')
          legend('topright', lty = c(1,1,3), col = c('black', 'red', 'blue'),
                 legend = c('mean', 'median', ifelse(state$outlier == 'sd2',
                                                     '+/- 2 sd', ifelse(state$outlier == 'sd3',
                                                                        '+/- 3 sd',
                                                                        '+/- 1.5 IQR'))))
        }
        else{
          legend('topright', lty = 1, col = c('black', 'red'),
                 legend = c('mean', 'median'))
        }
        }
      }
        else{
          plot(1:10, 1:10, axes = 'FALSE', col = 'white')
          box()
          text(5,5, labels = "Insufficient amount of data, coverage less than 10%",
               cex = 1.4)
        }
      }
    })
    output$bloomfit <- renderPlot({
      
      # 1. Outlier method
      mean <- apply(rchla, 1, mean, na.rm = TRUE)
      sd <- apply(rchla, 1, sd, na.rm = TRUE)
      median <- apply(rchla, 1, median, na.rm = TRUE)
      iqr <- apply(rchla, 1, IQR, na.rm = TRUE)
      if(state$outlier == 'sd2'){
        limits <- matrix(nrow = length(sd), ncol = 2)
        limits[,1] <- -1 * 2 * sd + mean
        limits[,2] <- 1 * 2 * sd + mean
      } else if (state$outlier == 'sd3'){
        limits <- matrix(nrow = length(sd), ncol = 2)
        limits[,1] <- -1 * 3 * sd + mean
        limits[,2] <- 1 * 3 * sd + mean
      } else if (state$outlier == 'iqr15'){
        limits <- matrix(nrow = length(iqr), ncol = 2)
        limits[,1] <- -1 * 1.5 * iqr + median
        limits[,2] <- 1 * 1.5 * iqr + median
      }
      
      # 2. remove outliers based on method and obtain indicies 
      # where data coverage is greater than defined percentage
     lenok <- vector(mode = 'logical', length = length(rchla[,1]))
      for ( i in 1:length(rchla[,1])){
        d <- rchla[i,]
        ok <- which(!is.na(d))
        if(state$outlier != 'none'){
          ok <- which(d >= limits[i,1] & d <= limits[i,2])
          mean[i] <- mean(d[ok])
          median[i] <- median(d[ok])
        }
        lenok[i] <- length(ok) 
      }      
      ydays <- as.POSIXlt(time, origin = '1970-01-01', tz = 'UTC')$yday + 1
      good <- lenok / length(rchla[1,]) > state$percent & ydays < 200
      goodper <- lenok / length(rchla[1,]) > state$percent
      yday <- ydays[good]
      ydays <- ydays[goodper]
      #good <- yday < 200
      
      if(state$dailystat == 'avg'){
        chlorophyll <- mean[good]
        chlall <- mean[goodper]
      } else if(state$dailystat == 'med'){
        chlorophyll <- median[good]
        chlall <- median[goodper]
      }
      
      # dataframe for nearPoints
      dfbloomparms <- data.frame(y = chlall, x = ydays)
      
      ## 3. Apply fit method
      ## GAUSSIAN
      if(state$fitmethod == 'gauss'){
        {if(region != 'newfoundland' && region != 'labradorSea'){
          # uses data subset before day 200
          gauss <- gaussFit(t = yday, y = chlorophyll,
                            tm = state$tm, beta = state$beta)}
        else{
          # uses data subset before day 200
          gauss <- gaussFit(t = ydays, y = chlall,
                            tm = state$tm, beta = state$beta)}}
        
      } # closes if state$fitmethod = gauss
      
      ## RATE OF CHANGE
      if (state$fitmethod == 'roc'){
        if (state$rocmethod == 'loess'){
          {if(region != 'newfoundland' && region != 'labradorSea'){
              mod <- loess(chlorophyll ~ yday, span = state$loessSpan, degree = 2)
              t <- yday
          }
            else{
              mod <- loess(chlall ~ ydays, span = state$loessSpan, degree = 2)
              t <- ydays
            }
          }
          
          y <- fitted(mod)
        }
        if (state$rocmethod == 'nofit'){
          {if(region != 'newfoundland' && region != 'labradorSea'){
            y <- chlorophyll
            t <- yday
          }
            else{
              y <- chlall
              t <- ydays
            }
          }
          
        }
        rocsym <- rateOfChange(y = y, t = t,
                               yall = chlall, tall = ydays,
                               bloomShape = state$rocshape)
        #rocasym <- rateOfChange(y = y, t = yday,
        #                        bloomShape = 'asymmetric')
      }
      # old code for hants fit for roc method
        # fit <- HANTS(y = chlorophyll, t = yday,
        #             nf = state$nf, nb = 365,
        #             low = state$low, high = state$high,
        #             HiLo = state$HiLo , fet = state$fet,
        #             dod = state$dod, delta = 0.7)$yr
        # # get desired parameters
        # # find initiaion time
        # maxidx <- which.max(fit)
        # ynpm <- fit[1:maxidx]
        # tpm <- yday[1:maxidx]
        # dchladt <- diff(ynpm) / diff(tpm)
        # maxidxdcdt <- which.max(dchladt)
        # ti <- tpm[maxidxdcdt]
        # tm <- yday[maxidx]
        # # duration assuming symmetric
        # td <- 2*(tm - ti)
        # tt <- ti + td # termination day
        # tiidx <- which.min(abs(yday - ti))
        # ttidx <- which.min(abs(yday - tt))
        # # calculate integral - magnitude  
        # hs <- sum(diff(yday[tiidx:ttidx]) * (head(fit[tiidx:ttidx], -1) + tail(fit[tiidx:ttidx], -1))/2)
        # 
        # # calculate integral - magnitude
        # revidx <- which.min(abs(yday - 2*tm))
        # yntm <- rev(fit[maxidx:revidx])
        # ttm <- rev(yday[maxidx:revidx])
        # dcdtrev <- diff(yntm) / diff(ttm) * -1 # multiply by -1 since dt is negative
        # maxttaidx <- which.max(dcdtrev)
        # tta <- ttm[maxttaidx] # termination day asymmetric
        # ttaidx <- which.min(abs(yday - tta))
        # # duration assuming asymmetric  
        # ha <- sum(diff(yday[tiidx:ttaidx]) * (head(fit[tiidx:ttaidx], -1) + tail(fit[tiidx:ttaidx], -1))/2)
      
      
      if(state$fitmethod == 'thresh'){
        if (state$threshmethod == 'loess'){
          {if(region != 'newfoundland' && region != 'labradorSea'){
            mod <- loess(chlorophyll ~ yday, span = state$threshSpan, degree = 2)
            t <- yday
          }
            else{
              mod <- loess(chlall ~ ydays, span = state$threshSpan, degree = 2) 
              t <- ydays
            }
          }
          y <- fitted(mod)
        }
        if (state$threshmethod == 'none'){
          {if(region != 'newfoundland' && region != 'labradorSea'){
            y <- chlorophyll
            t <- yday
          }
            else{
              y <- chlall
              t <- ydays
            }
          }
        }
        threshsym <- threshold(t = t, y = y, 
                               tall = ydays, yall = chlall,
                               threshold = state$threshold, 
                               bloomShape = state$threshshape)
        #threshasym <- threshold(t = yday, y = y, 
        #                       threshold = state$threshold, 
        #                       bloomShape = 'asymmetric')
      }
      
      par(mar = c(3,3.5,2,1))      
      plot(ydays, chlall,
           xlab = '',
           ylab = '',
           xlim = c(0,365))
      mtext('Day number', side = 1, line = 2)
      mtext(ifelse(state$dailystat == 'avg',
                   expression('Daily average Chlorophyll ' * '[' * mg/m^3 * ']'),
                   expression('Daily median Chlorophyll' * '[' * mg/m^3 * ']')), 
            side = 2, line = 2)
      # gaussian fit
      if(state$fitmethod == 'gauss'){
        if(!is.null(gauss$fit)) {
          {if(region != 'newfoundland' && region != 'labradorSea'){
            lines(yday, predict(gauss$fit))}
            else{
            lines(ydays, predict(gauss$fit))  
            }
          }
          abline(v = c(gauss$values$ti, gauss$values$tm, gauss$values$tt))
          mtext(paste(round(gauss$values$ti, 1)), side = 3, at = gauss$values$ti, cex = 0.7)
          mtext(paste(round(gauss$values$tm, 1)), side = 3, at = gauss$values$tm, cex = 0.7)
          mtext(paste(round(gauss$values$tt, 1)), side = 3, at = gauss$values$tt, cex = 0.7)
          values <- vector('expression', length = 6)
          values[1] <- substitute(expression(t[m] == tm), list(tm = round(gauss$values$tm, 1)))[2]
          values[2] <- substitute(expression(t[i] == ti), list(ti = round(gauss$values$ti, 1)))[2]        
          values[3] <- substitute(expression(t[t] == tt), list(tt = round(gauss$values$tt, 1)))[2]
          values[4] <- substitute(expression(h == hs), list(hs = round(gauss$values$h,2)))[2]
          values[5] <- substitute(expression(t[d] == td), list(td = round(gauss$values$td, 1)))[2]
          #values[6] <- substitute(expression(t[ta] == tta), list(tta = round(rocasym$tt, 1)))[2]
          #values[7] <- substitute(expression(h[a] == ha), list(ha = round(rocasym$h, 1)))[2]
          #values[8] <- substitute(expression(t[da] == td), list(td = round(rocasym$td, 1)))[2]
          legend('topleft', legend = values, bty = 'n')
        }
        if(is.null(gauss$fit)) mtext(text = 'unable to fit', side = 3, adj = 0, cex = 2)
      } # closes state$fit gauss
      
      if(state$fitmethod == 'roc'){
        lines(t, y)
        abline(v = c(rocsym$ti, rocsym$tm, rocsym$tt))
        mtext(paste(round(rocsym$ti, 1)), side = 3, at = rocsym$ti, cex = 0.7)
        mtext(paste(round(rocsym$tm, 1)), side = 3, at = rocsym$tm, cex = 0.7)
        mtext(paste(round(rocsym$tt, 1)), side = 3, at = rocsym$tt, cex = 0.7)
        values <- vector('expression', length = 6)
        values[1] <- substitute(expression(t[m] == tm), list(tm = round(rocsym$tm, 1)))[2]
        values[2] <- substitute(expression(t[i] == ti), list(ti = round(rocsym$ti, 1)))[2]        
        values[3] <- substitute(expression(t[t] == tt), list(tt = round(rocsym$tt, 1)))[2]
        values[4] <- substitute(expression(h == hs), list(hs = round(rocsym$h,2)))[2]
        values[5] <- substitute(expression(t[d] == td), list(td = round(rocsym$td, 1)))[2]
        #values[6] <- substitute(expression(t[ta] == tta), list(tta = round(rocasym$tt, 1)))[2]
        #values[7] <- substitute(expression(h[a] == ha), list(ha = round(rocasym$h, 1)))[2]
        #values[8] <- substitute(expression(t[da] == td), list(td = round(rocasym$td, 1)))[2]

        legend('topleft', legend = values, bty = 'n')
      }
      if(state$fitmethod == 'thresh'){
        # no fit/ smoothing applied, so just values
        if(state$threshmethod == 'loess'){
          lines(t, y)
        }
        abline(v = c(threshsym$ti, threshsym$tm, threshsym$tt))
        mtext(paste(round(threshsym$ti, 1)), side = 3, at = threshsym$ti, cex = 0.7)
        mtext(paste(round(threshsym$tm, 1)), side = 3, at = threshsym$tm, cex = 0.7)
        mtext(paste(round(threshsym$tt, 1)), side = 3, at = threshsym$tt, cex = 0.7)
        values <- vector('expression')
        values[1] <- substitute(expression(t[m] == tm), list(tm = round(threshsym$tm, 1)))[2]
        values[2] <- substitute(expression(t[i] == ti), list(ti = round(threshsym$ti, 1)))[2]        
        values[3] <- substitute(expression(t[t] == tt), list(tt = round(threshsym$tt, 1)))[2]
        values[4] <- substitute(expression(h == hs), list(hs = round(threshsym$h,2)))[2]
        values[5] <- substitute(expression(t[d] == td), list(td = round(threshsym$td, 1)))[2]
        #values[6] <- substitute(expression(t[ta] == tta), list(tta = round(threshasym$tt, 1)))[2]
        #values[7] <- substitute(expression(h[a] == ha), list(ha = round(threshasym$h, 1)))[2]
        #values[8] <- substitute(expression(t[da] == td), list(td = round(threshsym$td, 1)))[2]
        
        legend('topleft', legend = values, bty = 'n')
      }
      #put bloomfit click inside observe event for run ?
      # observeEvent(input$bloomfit_click,{
      #   state$bfclick <- input$bloomfit_click
      # })
      #observeEvent(input$bloomfit_click,{
      #observe({
        #test <- 1 # for debug
        npyday <- nearPoints(dfbloomparms,
                                 coordinfo = state$bfclick,
                                 xvar = "x",
                                 yvar = "y")$x
        updateSliderInput(session, inputId = 'yearday', value = npyday)
      #}) # closes observe event for bloom click
    }) # closes bloom fit plot
  }) # closes plotting when 'run' button is clicked
}

# Run the application 
shinyApp(ui = ui, server = server)


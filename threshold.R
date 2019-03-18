threshold <- function(t, y, tall, yall, threshold, bloomShape = 'symmetric'){
  rq <- rq(yall ~ tall, tau = 0.25)
  yok <- !is.na(y)
  y <- y[yok]
  t <- t[yok]
  
  ## threshold method
  # 1. subset before fall bloom, so before day 200
  tok <- t < 200
  y <- y[tok]
  t <- t[tok]
  
  # 2. find the maximum chl value
  maxidx <- which.max(y)
  tm <- t[maxidx]
  Bm <- y[maxidx]
  
  # 3. find median of yearly data, and define threshold as 1.05 * median
  med <- median(y, na.rm = TRUE)
  thresh <- threshold * med
  
  # 4. find where concentration drops below thresh for 14 days
  yt <- y[1:maxidx]
  tt <- t[1:maxidx]
  
  ytr <- rev(yt) - thresh
  # find runlength for above/below threshold
  # TRUE : above threshold
  # FALSE : below threshold
  rle <- rle(ytr > 0) 
  csrle <- cumsum(rle$lengths)
  #if(csrle[1] != 1) csrle <- c(1, csrle)
  dcsrle <- diff(rev(tt)[csrle]) * -1
  # find where values are below threshold for more than 14 days
  dvalues <- rle$values[2:length(rle$values)]
  okth <- which(dcsrle > 14 & dvalues == FALSE)[1] # want the first instance
  # NOTE: do not add 1 to index due to diff, want the time closest to maximum
  # esentially the last day of the 14 consecutive days where values were below thresh
  ti <- t[((maxidx - csrle[okth])+1)]
  
  if(bloomShape == 'symmetric'){
    td <- 2*(tm - ti)
    tt <- ti + td # termination time assuming symmetry  
    # calculate integral if ti and tt exist
    # first for symmetric case
    h <- NA
    if(!is.na(ti) && !is.na(tt)){
      tiidx <- which.min(abs(t - ti))
      ttidx <- which.min(abs(t - tt))
      bkrnd <- predict(rq, newdata = data.frame(tall = t[tiidx:ttidx]))
    # calculate integral - magnitude
      ya <- yall[tall < 200]
    h <- sum(diff(t[tiidx:ttidx]) * (head(ya[tiidx:ttidx] - bkrnd, -1) + tail(ya[tiidx:ttidx] - bkrnd, -1))/2)
    }
  }
  
  if(bloomShape == 'asymmetric'){
    # find termination assuming assymetry
    ya <- y[maxidx:length(y)]
    ta <- t[maxidx:length(y)]
  
    ytar <- ya - thresh
    rler <- rle(ytar > 0)
  
    csrler <- cumsum(rler$lengths)
    #if(csrler[1] != 1) csrler <- c(1, csrler)
    dcsrler <- diff(ta[csrler])
    dvaluesa <- rler$values[2:length(rler$values)]
    oktha <- which(dcsrler > 14 & dvaluesa == FALSE)[1]
    tt <- ta[csrler[oktha]]
    td <- tt - ti
    
    h <- NA
    if(!is.na(ti) && !is.na(tt)){
      tiidx <- which.min(abs(t - ti))
      ttidx <- which.min(abs(t - tt))
      bkrnd <- predict(rq, newdata = data.frame(tall = t[tiidx:ttidx]))
      # calculate integral - magnitude
      ya <- yall[tall < 200]
      h <- sum(diff(t[tiidx:ttidx]) * (head(ya[tiidx:ttidx] - bkrnd, -1) + tail(ya[tiidx:ttidx] - bkrnd, -1))/2)
    }
  }
  H <- unname(Bm - predict(rq, newdata = data.frame(tall = tm)))
  return(list(tm = tm, ti = ti, tt = tt, h = h, Bm = Bm, td = td, H = H))
}

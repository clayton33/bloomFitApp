rateOfChange <- function(y, yall, t, tall, bloomShape = 'symmetric'){
  rq <- rq(yall ~ tall, tau = 0.25)
  yn <- y
  # originally the function did a hants fit
  # but was removed, too lazy to change everything 
  # below to y instead of yn
  # find initiaion time
  # subset prior to day 200
  maxidx <- which.max(yn)
  if(maxidx == 1){
    tm <- NA
    ti <- NA
    tt <- NA
    h <- NA
    Bm <- NA
    td <- NA
    H <- NA
  }
  else{
    Bm <- yn[maxidx]
    ynpm <- yn[1:maxidx]
    tpm <- t[1:maxidx]
    dchladt <- diff(ynpm) / diff(tpm)
    maxidxdcdt <- which.max(dchladt)
    ti <- tpm[maxidxdcdt]
    tm <- t[maxidx]
    tiidx <- which.min(abs(t - ti))
    
    if(bloomShape == 'symmetric'){
      # duration assuming symmetric
      td <- 2*(tm - ti)
      tt <- ti + td # termination day
      ttidx <- which.min(abs(t - tt))
      bkrnd <- predict(rq, newdata = data.frame(tall = t[tiidx:ttidx]))
      # calculate integral - magnitude
      ya <- yall[tall < 200]
      h <- sum(diff(t[tiidx:ttidx]) * (head(ya[tiidx:ttidx] - bkrnd, -1) + tail(ya[tiidx:ttidx] - bkrnd, -1))/2)
      H <- Bm - predict(rq, newdata = data.frame(tall = tm))
    }
    
    if(bloomShape == 'asymmetric'){
      # calculate integral - magnitude
      revidx <- which.min(abs(t - 2*tm))
      if(revidx == maxidx){
        tm <- NA
        ti <- NA
        tt <- NA
        h <- NA
        Bm <- NA
        td <- NA
        H <- NA
      }
      else{
        yntm <- rev(yn[maxidx:revidx])
        ttm <- rev(t[maxidx:revidx])
        dcdtrev <- diff(yntm) / diff(ttm) * -1 # multiply by -1 since dt is negative
        maxttaidx <- which.max(dcdtrev)
        tt <- ttm[maxttaidx] # termination day asymmetric
        td <- tt - ti
        ttaidx <- which.min(abs(t - tt))
        bkrnd <- predict(rq, newdata = data.frame(tall = t[tiidx:ttaidx]))
        # duration assuming asymmetric  
        ya <- yall[tall < 200]
        h <- sum(diff(t[tiidx:ttaidx]) * (head(ya[tiidx:ttaidx] - bkrnd, -1) + tail(ya[tiidx:ttaidx] - bkrnd, -1))/2)
        H <- Bm - predict(rq, newdata = data.frame(tall = tm))
        }
    }
  }
  return(list(yn = yn, t = t, tm = tm, ti = ti, tt = tt, h = h, Bm = Bm, td = td, H = H))
}

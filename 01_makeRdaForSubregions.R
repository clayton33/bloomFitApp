rm(list=ls())
library(ncdf4)
source('00_regionBoxes.R') # source definitions of boxes for each region

# define area limits for subset of pan-canadian grid 
# scotian shelf limits
#sslat <- c(38,50)
#sslon <- c(-72, -52)
# atlantic region limits
sslat <- range(unlist(lapply(c(maritimes, quebec, newfoundland, labradorSea), function(k) k$lat))) + c(-2.5, 2.5)
sslon <- range(unlist(lapply(c(maritimes, quebec, newfoundland, labradorSea), function(k) k$lon))) + c(-2.5, 2.5)

# subset over each subregion
srnames <- list(maritimes = names(maritimes),
                quebec = names(quebec),
                newfoundland = names(newfoundland),
                labradorSea = names(labradorSea))
srranges <- list(maritimes = lapply(maritimes, function(k) lapply(k, range)),
                 quebec = lapply(quebec, function(k) lapply(k, range)),
                 newfoundland = lapply(newfoundland, function(k) lapply(k, range)),
                 labradorSea = lapply(labradorSea, function(k) lapply(k, range)))


# load in CAN 4km grid file
gridfile <- 'CAN_4km.nc'
grid <- nc_open(filename = gridfile)
lat <- ncvar_get(nc = grid, varid = 'lat')
lon <- ncvar_get(nc = grid, varid = 'lon')

srok <- list(maritimes = lapply(srranges$maritimes, function(k) abs(lon) >= abs(k$lon[2]) & abs(lon) <= abs(k$lon[1]) & lat >= k$lat[1] & lat <= k$lat[2]),
             quebec = lapply(srranges$quebec, function(k) abs(lon) >= abs(k$lon[2]) & abs(lon) <= abs(k$lon[1]) & lat >= k$lat[1] & lat <= k$lat[2]),
             newfoundland = lapply(srranges$newfoundland, function(k) abs(lon) >= abs(k$lon[2]) & abs(lon) <= abs(k$lon[1]) & lat >= k$lat[1] & lat <= k$lat[2]),
             labradorSea = lapply(srranges$labradorSea, function(k) abs(lon) >= abs(k$lon[2]) & abs(lon) <= abs(k$lon[1]) & lat >= k$lat[1] & lat <= k$lat[2]))

# gbok <- 
# cssok <- abs(lon) >= abs(csslon[2]) & abs(lon) <= abs(csslon[1]) & lat >= csslat[1] & lat <= csslat[2]
ssok <- abs(lon) >= abs(sslon[2]) & abs(lon) <= abs(sslon[1]) & lat >= sslat[1] & lat <= sslat[2]

# modis subset CAN 4 km grid data
# get directory names
dirs <- list.dirs(path = '../data', full.names = FALSE)
okdirs <- grep(pattern = 'modis-aqua-L3b-rsg-*', x = dirs)
datadir <- dirs[okdirs]
yr <- unlist(lapply(datadir, function(k) strsplit(k, split = '-')[[1]][5]))
for (k in 1:length(datadir)){
  #path <- paste('../', datadir[k], sep = '')
  path <- datadir[k]
  files <- list.files(path = paste0('../data/',path), pattern = '*.nc')

  # define matricies to fill for boxes and full region
  srchla <- list(maritimes = lapply(srok$maritimes, function(k) matrix(data = NA, nrow = length(files), ncol = length(lat[k]))),
                 quebec = lapply(srok$quebec, function(k) matrix(data = NA, nrow = length(files), ncol = length(lat[k]))),
                 newfoundland = lapply(srok$newfoundland, function(k) matrix(data = NA, nrow = length(files), ncol = length(lat[k]))),
                 labradorSea = lapply(srok$labradorSea, function(k) matrix(data = NA, nrow = length(files), ncol = length(lat[k]))))
  sschla <- matrix(data = NA, nrow = length(files), ncol = length(lat[ssok]))

  time <- vector(mode = 'logical', length = length(files))
  for(i in 1:length(files)){
    d <- nc_open(filename = paste('../data', path, files[i], sep='/'))
    t <- as.POSIXct(ncvar_get(nc = d, varid = 'time'), origin = '1970-01-01', tz = 'UTC')
    chla <- ncvar_get(nc = d, varid = 'chlor_a')
    for(j in 1:length(srchla)){ # loop through each region
      for (m in 1:length(srchla[[j]])){ # loop through each box
        srchla[[j]][[m]][i,] <- chla[srok[[j]][[m]]]
      }
    }
    # gbchla[i,] <- chla[gbok]
    # csschla[i,] <- chla[cssok]
    sschla[i,] <- chla[ssok]
    time[i] <- t
    nc_close(d)
  }
  
  boxes <- list(maritimes = names(srchla[[1]]),
                  quebec = names(srchla[[2]]),
                  newfoundland = names(srchla[[3]]),
                  labradorSea = names(srchla[[4]]))
  regions <- names(srchla)
  for(q in 1:length(srchla)){
    for(p in 1:length(srchla[[q]])){
      region <- regions[q]
      box <- boxes[[q]][p]
      rlat <- lat[srok[[q]][[p]]]
      rlon <- lon[srok[[q]][[p]]]
      rchla <- srchla[[q]][[p]]
      savedir <- paste('./data',region, box, '', sep = '/')
      if(!dir.exists(savedir)){
        dir.create(savedir, recursive = TRUE)
      }
      save(time, rlat, rlon, rchla, file = paste0(savedir,'modis',yr[k],box,'.rda'))
    }
  }

  # this is the subset of pan-canadian grid
  # so variable names are different from the regions
  sslat <- lat[ssok]
  sslon <- lon[ssok]
  atlsavedir <- './data/atlantic/'
  if(!dir.exists(atlsavedir)){
    dir.create(atlsavedir, recursive = TRUE)
  }
  save(time, sslat, sslon, sschla, file = paste0(atlsavedir,'modis', yr[k], 'ss.rda'))
}

# maritime boxes
# georges bank
gb <- list(lat = c(41,42,42,41, 41),
           lon = c(-68,-68, -66.5, -66.5, -68))
# central scotian shelf
css <- list(lat = c(43.33, 44.33, 44.33, 43.33, 43.33),
            lon = c(-64, -64, -62, -62, -64))
# eastern scotian shelf
ess <- list(lat = c(44.2, 45.67, 45.67, 44.2, 44.2),
            lon = c(-60, -60, -58, -58, -60))
# western scotian shelf
wss <- list(lat = c(42.5, 43.33, 43.33, 42.5, 42.5),
            lon = c(-65.5, -65.5, -64.5, -64.5, -65.5))
# cabot strait
cs <- list(lat = c(46.9, 48, 48, 46.9, 46.9),
           lon = c(-60.4, -60.4, -59, -59, -60.4))
#lurcher shoal
ls <- list(lat = c(43, 44, 44, 43, 43),
           lon = c(-66.7, -66.7, -66, -66, -66.7))

maritimes <- list(gb = gb,
                   css = css,
                   ess = ess,
                   wss = wss,
                   cs = cs,
                   ls = ls)
# quebec boxes
# northwest gulf of st lawrence
nwgsl <- list(lat = c(49.7, 50.3, 50.3, 49.7, 49.7),
              lon = c(-67.0, -67.0, -64.5, -64.5, -67.0))
# northeast gulf of st lawrence
negsl <- list(lat = c(49, 50, 50, 49, 49),
              lon = c(-61, -61, -58, -58, -61))
# magdalen shallows
ms <- list(lat = c(46.5, 48, 48, 46.5, 46.5),
           lon = c(-64, -64, -61.5, -61.5, -64))
# cabot strait
cs <- list(lat = c(46.9, 48, 48, 46.9, 46.9),
           lon = c(-60.4, -60.4, -59, -59, -60.4))
quebec <- list(nwgsl = nwgsl,
               negsl = negsl,
               ms = ms, 
               cs = cs)

# newfoundland boxes
# st pierre bank
spb <- list(lat = c(45.33, 46.33, 46.33, 45.33, 45.33),
            lon = c(-56, -56, -54, -54, -56))
# southeast shoal
ses <- list(lat = c(44, 46, 46, 44, 44),
            lon = c(-52, -52, -50, -50, -52))
# avalon channel
ac <- list(lat = c(46, 48, 48, 46, 46),
           lon = c(-53, -53, -51.5, -51.5, -53))
# hibernia
hib <- list(lat = c(46, 47, 47, 46, 46),
            lon = c(-51, -51, -48.75, -48.75, -51))
# flemish pass
fp <- list(lat = c(46, 48, 48, 46, 46),
           lon = c(-47.5, -47.5, -46, -46, -47.5))
# northeast newfoundland shelf
nens <- list(lat = c(48.5, 50, 50, 48.5, 48.5),
             lon = c(-53, -53, -51, -51, -53))
# st anthony bank
sab <- list(lat = c(50, 52, 52, 50, 50),
            lon = c(-55, -55, -53, -53, -55))
# hamilton bank
hb <- list(lat = c(53.5, 54.5, 54.5, 53.5, 53.5),
           lon = c(-56, -56, -54, -54, -56))
# northern labrador shelf
nls <- list(lat = c(56.9145, 57.8125, 57.8125 , 56.9145, 56.9145),
            lon = c(-61.1957, -61.1957, -59.54983, -59.54983, -61.1957))
# hudson strait
hs <- list(lat = c(60.5058 , 61.403, 61.4033, 60.5058, 60.5058),
           lon = c(-64.5484, -64.5484, -62.7235, -62.7235 , -64.5484))


newfoundland <- list(spb = spb,
                     ses = ses,
                     ac = ac,
                     hib = hib,
                     fp = fp,
                     nens = nens,
                     sab = sab,
                     hb = hb,
                     nls = nls,
                     hs = hs)

# labrador sea
# north central labrador sea
ncls <- list(lat = c(60, 62.5, 62.5, 60, 60),
             lon = c(-60, -60, -55, -55, -60))
# greenland sea
gs <- list(lat = c(60.1, 60.7, 60.7, 60.1, 60.1),
           lon = c(-48.8, -48.8, -48.1, -48.1, -48.8))
# central labrador sea
cls <- list(lat = c(55.5, 60.1, 60.1, 55.5, 55.5),
            lon = c(-53.7, -53.7, -48.8, -48.8, -53.7))
# eastern labrador shelf
els <- list(lat = c(59, 60.5, 60.5, 59, 59),
            lon = c(-49, -49, -48.3, -48.3, -49))
# bravo station
bra <- list(lat = c(56.627, 58.127, 58.127, 56.627, 56.627),
            lon = c(-53.168, -53.168, -50.415, -50.415, -53.168))
# labrador shelf
las <- list(lat = c(53.6, 55.5, 55.5, 53.6, 53.6),
            lon = c(-55.7, -55.7, -53.7, -53.7, -55.7))

labradorSea <- list(ncls = ncls,
                    gs = gs,
                    cls = cls,
                    els = els,
                    bra = bra,
                    las = las)

# DEPRECIATED BOXES
#estuary - old quebec box
est <- list(lat = c(48, 49.3, 49.3, 48, 48),
            lon = c(-69.5, -69.5, -67.4, -67.4, -69.5))
#bay of fundy - old maritimes box
bof <- list(lat = c(44.5, 45.5, 45.5, 44.5, 44.5),
            lon = c(-66.33, -66.33, -65, -65, -66.33))
#bras d'Or - old maritimes  box
bdo <- list(lat = c(45.81666667, 49.6, 45.9, 45.81666667, 45.81666667),
            lon = c(-60.9, -60.9, -60.75, -60.75, -60.9))
# western bank - old maritimes box
wb <- list(lat = c(43.33, 44, 44, 43.33, 43.33),
           lon = c(-62, -62, -61, -61, -62))
# solas - old maritimes box
sol <- list(lat = c(41, 43, 43, 41, 41),
            lon = c(-60, -60, -58, -58, -60))



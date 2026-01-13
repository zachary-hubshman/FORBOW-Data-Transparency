# egen dimpsych = rowmean(zpleself zplecur haluc delus)
# 
# 
# gen haluc = 0
# replace haluc = 1 if sipshaluc == 1
# replace haluc = 2 if sipshaluc == 2
# replace haluc = 2 if plekshaluccur==1
# replace haluc = 2 if plekshaluccur==2
# 
# gen delus = 0
# replace delus = 1 if pleksdeluscur==1
# replace delus = 1 if sipsdelus==1
# replace delus = 2 if pleksdeluscur==2
# replace delus = 2 if sipsdelus==2t
# 
# 
# sum antplen if group==0
# gen zplecur = (antplen - r(mean)) / r(sd)
# 
# sum fftot if group==0
# gen zpleself = (fftot - r(mean)) / r(sd)
# 
# alpha FFY1 FF3Y FF5Y FF7Y FF9Y FF11Y FF13Y, asis item gen(fftotal)
# gen fftot = fftotal*7
# 
# egen antplen = rowtotal(plecurate*)

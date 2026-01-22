# egen dimsleep = rowmean(zpsqitotal zcshtot zssrtot zsshtot)
# 
# *** correlation between sleep measures
# pwcorr dimsleep zpsqitotal zcshtot zssrtot zpsqidist zpsqilaten zpsqitotal zcshsleepbeh zcshwake zssrtot zsshtot psqidurat psqidistb psqilaten psqidaydys psqihse psqislpqual psqimeds cshbedresistance cshsleepbeh cshwake
# 



#dimsleep needs psqi, csh, ssr, and ssh
#CSHQ - This is redudant also, this is done in CSHQ scripts.
FOR <- FOR %>%   mutate(
  zcshsleepbeh = as.numeric(scale(cshsleepbeh)),
  zcshwake     = as.numeric(scale(cshwake)),
  zcsh = rowMeans(
    cbind(zcshwake, zcshsleepbeh),
    na.rm = TRUE
  )
)
##PSQI
FOR$zpsqitotal <- scale(FOR$psqitotal)

#SSR



##SSH
FOR$zsshtot <- scale(FOR$sshtot)

table(FOR$zsshtot)

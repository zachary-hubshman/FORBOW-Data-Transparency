# gen dimbs = 29*(bssum / bsdenom)
# 
# gen bssum = 0
# gen bsdenom = 0
# local s SPICY_D7 SPICY_D8 SPICY_D9 SPICY_D10 SPICY_D11 SPICY_D12 SPICY_D14 SPICY_D15 SPICY_B1 SPICY_B2 SPICY_B7 SPICY_B3_1 SPICY_B3_2 SPICY_B3_3 SPICY_B3_4 SPICY_B3_5 SPICY_B3_6 SPICY_B3_7 SPICY_B3_8 SPICY_B3_9 SPICY_B3_10 SPICY_B3_11 SPICY_B3_12 SPICY_O1 SPICY_O2 SPICY_O3 SPICY_B4_2 SPICY_B5_1 SPICY_B5_2  
# foreach v of local s {
#   di "`v' symptom"
#   count if antbs==1 & `v'>2 & `v'<7
# replace bssum = bssum + 1 if `v'>2 & `v'<7
# replace bsdenom  = bsdenom + 1 if  `v'<7
# }



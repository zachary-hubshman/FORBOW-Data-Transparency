#Some SSH need to be subctracted by one
sub_SSH <- c("ssh15wake", "ssh19rate", "ssh20goodbad","ssh21reg",
             "ssh24blate","ssh24cmorn","ssh24daft","ssh24ewoke",
             "ssh24flate","ssh24gallnight","ssh24hnoon",
             "ssh24itired","ssh24jalarm","ssh24kdiffic","ssh24lbaddre",
             "ssh24mtobed","ssh24ndanger","ssh25bfall") 

#Reverse coded SSH
reverse_SSH <- c("ssh24ogoodsle", "ssh24asatis")


FOR <- FOR %>%
  mutate(
    across(all_of(c(sub_SSH, reverse_SSH)),
           ~ as.numeric(as.character(.x))))

FOR <- FOR %>%
  mutate(
    # subtract-1 items
    across(all_of(sub_SSH),
           ~ case_when(
             .x == 99           ~ NA_real_,
             .x %in% 1:5       ~ .x - 1,
             TRUE              ~ NA_real_
           )
    ),
    
    # reverse-coded items: 1..5 -> 4..0
    across(all_of(reverse_SSH),
           ~ case_when(
             .x == 99          ~ NA_real_,
             .x %in% 1:5      ~ 5 - .x,
             TRUE             ~ NA_real_
           )
    )
  )

ssh24_sleepwakeproblems <- c("ssh24asatis","ssh24blate","ssh24cmorn",
                             "ssh24daft","ssh24ewoke","ssh24flate",
                             "ssh24gallnight","ssh24hnoon","ssh24itired",
                             "ssh24jalarm","ssh24kdiffic","ssh24lbaddre",
                             "ssh24mtobed","ssh24ndanger","ssh24ogoodsle")

total_items <- c("ssh15wake","ssh19rate","ssh20goodbad",
                 "ssh21reg", ssh24_sleepwakeproblems, "ssh25bfall")
FOR <- FOR %>%
  rowwise() %>%
  mutate(
    sshtotal = sum(c_across(all_of(total_items))),
    sshtot   = sshtotal * 20
  ) %>% 
  ungroup()
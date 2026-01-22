FORredcap <- FORredcap %>% rename(subject_id = ï..subject_id)
FORredcap <- FORredcap %>% filter(subject_id != '800')
FOR <- FOR %>%
  mutate(subject_id = as.character(subject_id)) %>%
  filter(str_detect(subject_id, "^(\\d{10}|\\d{7}[MF][1-3])$"))
invalid_si <- c("9999999888", "9999999998", "8009999123")
FOR <- FOR %>%
  filter(!subject_id %in% invalid_si)


str(FOR$mfq_01)
str(FORredcap$mfq_01)


redcapmfq <- sprintf("mfq_%02d", 1:13)
FORredcap[redcapmfq] <- lapply(FORredcap[redcapmfq], as.numeric)
FOR[redcapmfq] <- lapply(FOR[redcapmfq], as.numeric)
table(FORredcap$mfq_01)
table(FOR$mfq_01)
rm(redcapmfq)

redcapyeti <- paste0('yeti', 1:27)
FORredcap[redcapyeti] <- lapply(FORredcap[redcapyeti], as.numeric)
FOR[redcapyeti] <- lapply(FOR[redcapyeti], as.numeric)
rm(redcapyeti)

str(FOR$time_point)
str(FORredcap$time_point)
FORredcap$time_point <- as.numeric(FORredcap$time_point)
FOR$time_point <- as.numeric(FOR$time_point)

keys <- c("subject_id", "time_point")

FOR <- FOR %>%
  left_join(FORredcap, by = keys, suffix = c(".main", ".rc")) %>%
  mutate(
    across(
      ends_with(".main"),
      ~ coalesce(
        .x,
        cur_data()[[ sub("\\\\.main$", ".rc", cur_column()) ]]
      ),
      .names = "{sub('\\\\.main$', '', .col)}"
    )
  ) %>%
  select(-ends_with(".main"), -ends_with(".rc"))






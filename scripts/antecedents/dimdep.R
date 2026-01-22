FOR <- FOR %>%
  mutate(
    antdep = as.integer(
      coalesce(mfqytot > 8.5, FALSE) |
        coalesce(mfqptot > 8.5, FALSE) |
        coalesce(ccDEP == 1, FALSE) |
        coalesce(antdep == 1, FALSE)
    )
  )

data(denmark)
sjd <- denmark[, c("LRM", "LRY", "IBO", "IDE")]
sjd.vecm <- summary(ca.jo(sjd, constant = TRUE, type = "eigen", K = 2, spec = "longrun", season = 4, ctable = "A3"))
lue.vecm <- summary(cajolst(sjd, season=4))

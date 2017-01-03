"fingers" <-
structure(list(name = structure(as.integer(c(2, 6, 5, 9, 4, 8, 
1, 3, 7)), .Label = c("Char", "Evan", "Hong", "Jenny", "Jing", 
"Koah", "Luton", "Seb", "Yang"), class = "factor"), second = c(7, 
6.5, 7.5, 6.7, 7, 6.9, 7.6, 7.9, 6.3), fourth = c(7.5, 7, 8, 
7.1, 6.5, 6.6, 7.5, 8.1, 6.7), gender = structure(as.integer(c(2, 
1, 1, 1, 1, 2, 1, 2, 1)), .Label = c("F", "M"), class = "factor"), 
    indicator = c(1, 0, 0, 0, 0, 1, 0, 1, 0)), .Names = c("name", 
"second", "fourth", "gender", "indicator"), row.names = c("1", 
"2", "3", "4", "5", "6", "7", "8", "9"), class = "data.frame")
y11.lm <- lm(fourth ~ second, data=fingers)  # one slope, one intercept
summary(y11.lm)
y21.lm <- lm(fourth ~ second+gender, data=fingers) 
# one slopes, two intercepts
summary(y21.lm)
y22.lm <- lm(fourth ~ second*gender, data=fingers) 
# two slopes, two intercepts
summary(y22.lm)
y12.lm <- lm(fourth ~ second*gender -1, data=fingers) 
# two slopes, one intercepts
summary(y12.lm)



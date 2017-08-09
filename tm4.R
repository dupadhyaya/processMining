dates <- c("05/27/84", "07/07/05", "08/17/20")
betterDates <- as.Date(dates, "%m/%d/%y")
betterDates
str(betterDates)

as.Date("01/01/2000")
[1] "0001-01-20"

x =as.Date("2017-07-26","%Y-%m-%d")
str(x)

mytable <- table(iris$Species)
lbls <- paste(names(mytable), "\n", mytable, sep="")
pie(mytable, labels = lbls, 
    main="Pie Chart of Species\n (with sample sizes)")
mytable
str(mytable)

# TM Intiation - Exploratory Analysis
df1= read.csv(file='tminitiation.csv')
head(df1)              
str(df1)

df2= read.csv(file='tminitiation.csv', header=T, sep=",",
              colClasses = c(NA, 'factor',NA,
                             'factor', 'factor'))
str(df2)
head(df2)
df2[[5]] = as.Date(df2[[5]],'%Y-%m-%d')
head(df2)
length(df2)  # no of attributes
dim(df2)  # no of records & attributes
summary(df2)
fivenum(df2$age)
stem(df2$age) # stem & Leaf view
hist(df2$age)  # histogram
hist(df2$age,breaks=7)
hist(df2$age,breaks=c(0,20,30,40,50,60,70,100),
     labels=c(0,20,30,40,50,60,70,100))
hist(df2$age,breaks=7, colors='gray75', freq=T)

density(na.omit(df2$age) )
plot(density(na.omit(df2$age) ))
?density

hist(df2$age, freq=F, col='gray85')
lines(density(na.omit(df2$age)), lty=2)
lines(density(na.omit(df2$age), k = 'rectangular'))

#Test for Normality
shapiro.test(df2$age)
# p value < 0.05 : Significant : Not Normal
qqnorm(df2$age)
qqline(df2$age)
#check alignment along the straight line


#plots
boxplot(df2$age)
boxplot(df2$age, horizontal = T)

summary(df2$age)
pie(na.omit(df2$age))
table(df2$age)

pie(table(df2$gender))
pie(table(df2$initiator))
pie(table(df2$place))

dotchart(df2$age, df2$place)  # not proper

barplot(df2$age)
barplot(table(df2$gender))
barplot(table(df2$initiator))
barplot(table(df2$place), horiz = T)


piedata1 =  tapply(df2$age, df2$initiator, mean)
piedata1
# Mean of Meditator's age for each Trainer - Genderwise
tapply(df2$age, list(df2$initiator, df2$gender), mean)
# Count of Meditators under each Trainer - Genderwise
tapply(df2$age, list(df2$initiator, df2$gender), function(x) length(unique(x)))


data.frame(key=names(piedata1), value=piedata1)
aggregate( df2$age ~ df2$initiator, FUN = mean )

str(piedata2)

pd1$value



# Frequency Distribution
breaks = seq(0, 100, by= 10)    # 10 yrs age sequence 
breaks 
age.cut = cut(df2$age, breaks, right=FALSE)
age.freq = table(age.cut)
cbind(age.freq) 
str(df2)
with(df2, tapply(age, list(initiator, gender,place), FUN=length))

library(moments)
kurtosis(na.omit(df2$age))
skewness(na.omit(df2$age))


#sapply
sapply(df2,class)
library(ggplot2)
qplot(df2$age, geom = 'histogram', binwidth = 2) + xlab('Age')

#fBasics :basicsStats 
#returns data frame with the following entries and row names: 
#nobs, NAs, Minimum, Maximum , 1. Quartile, 3. Quartile, Mean, Median, Sum, SE Mean, LCL Mean, UCL Mean, Variance, Stdev, Skewness, Kurtosis.
library(fBasics)
basicStats(df2$age)

library(mlbench)
#data(df2)
# distribution of class variable
y <- df2
y
cbind(freq=table(y), percentage=prop.table(table(y))*100)

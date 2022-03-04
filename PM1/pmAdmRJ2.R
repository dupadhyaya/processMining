#process Mining

pacman::p_load(readxl, dplyr, ggplot2, reshape2, gridExtra, zoo, xts, lubridate,googlesheets4, processanimateR, bupaR, edeaR)

options(scipen=99)
list.files('C:/Users/du//OneDrive - Amity University/admissions')
floc="E:/AU/admission/MIS/adm19sumDates.csv"

#
fileloc = floc
#all = read_excel(path=fileloc , sheet='master')
aurj = read.csv(file=floc, stringsAsFactors = F)
#
#
data = aurj
head(data)
names(data)
data1 <- data %>% select(all_of(c('campus', 'formNo', 'gender', 'applMode', 'state', 'admStatus', 'city', dateCols)))
dim(data1)
str(data1)
table(data$campus1)
names(data1)
df1 = data
cols1 = c('campus','formNo', 'gender', 'applMode')
cols2 = c('formDate', 'programCode', 'skypeDate','sopDate', 'portfolioDate','admEntryBasis', 'offerDate','schPerc', 'feePayDate','feePaidDate', 'feeWOsch', 'feeAmtDue','feePaid', 'withdrawlDate', 'admStatus', 'city','state','remarks')
(allcols = c(cols1, cols2))
length(allcols)
cbind(allcols, names(data))
dateCols = c('formDate','skypeDate','sopDate', 'portfolioDate','offerDate','feePayDate','feePaidDate', 'withdrawlDate')
names(df1) <- allcols
names(df1) 

#subset cols
names(df1)
dateCols[!dateCols %in% names(df1)]

df2 = df1[,c('campus','formNo', 'gender', 'admStatus', 'city', dateCols)]
names(df2)
head(df2)


colSums(!is.na(data1))
str(df2)
#------
names(data1)
df2Melt = reshape2::melt(data1, id.vars=c('campus','formNo','gender','applMode', 'admStatus','state','city'), value.name='timestamp', variable.name='activity')  #many blank rows
head(df2Melt)
str(df2Melt)
summary(df2Melt$timestamp)

df2Melt$timestamp = as.Date(df2Melt$timestamp,format='%d-%b-%y')
summary(df2Melt)
df2Melt %>% filter(is.na(timestamp()))  
#df2Melt$timestamp = as.POSIXct(df2Melt$timestamp)  #this can be done later
#df2$timestamp = as.numeric(df2$timestamp)
#anytime::anytime(df2$timestamp)
#df2$timestamp = anytime::anytime(df2$timestamp)
str(df2Melt)

dim(df2Melt)
colSums(is.na(df2Melt))

#consider only complete cases--- ie where dates are not empty
df3 <- df2Melt %>% filter(!is.na(timestamp))
head(df3)
#df3=df2Melt
colSums(is.na(df3))
dim(df3)
names(df3)
#save the file
write.csv(df3, 'E:/AU/adata/df3.csv', row.names = F)

write.csv(df3, 'E:/AU/adata/aurjForms.csv', row.names = F)

#prepare for PM----
df3$timestamp = as.POSIXct(df3$timestamp)  #
names(df3)
str(df3)
head(df3)
#simple Event
events1 <- bupaR::simple_eventlog(eventlog = df3,   case_id = 'formNo',  activity_id = 'activity', timestamp = 'timestamp')

events1
str(events1)
activity_frequency(events1)
events1 %>% activity_frequency(level='activity')

events1 %>% activity_frequency(level = "activity") %>% plot()
events1 %>%  process_map(sec=frequency('relative'))

head(events1)
processmapR::process_map(events1)
names(events1)
events1 %>% processmapR::trace_explorer(coverage=1)
events1 %>% processmapR::trace_explorer(coverage=.5) 
events1 %>% processmapR::precedence_matrix() %>% plot()
events1 %>% processmapR::resource_matrix() %>% plot() 
events1 %>%   filter_time_period(interval = ymd(c(20190501, 20190901)), filter_method = "contained") %>% dotted_chart()

#animate-----
table(events1$admStatus)
names(events1)
#since no of records are high, filter to only admission, unwilling and withdrawl cases
events1P <- events1 %>% filter(admStatus == 'Progress')
events1R <- events1 %>% filter(admStatus == 'Rejected')
events1A <- events1 %>% filter(admStatus == 'Admitted')
events1U <- events1 %>% filter(admStatus == 'Unwilling')
events1W <- events1 %>% filter(admStatus == 'Withdrawl')
names(df2)
(admSummary <- df2 %>% dplyr::group_by(admStatus) %>% summarise(count = n()))
ggplot(admSummary, aes(x=admStatus, y=count, fill=admStatus)) + geom_bar(stat='identity') + geom_text(aes(label=count, y=count)) + coord_flip()

#animate_process(events1, duration=20) #animate later with lesser values
whichEvent <- events1P
whichEvent %>% animate_process(sec=frequency('relative'), duration=20)
whichEvent %>% filter_activity_presence("withdrawlDate") %>% animate_process()
animate_process(events, mode = "relative", jitter = 10, repeat_count = 1)

#save as html------
processmapR::trace_explorer(events, coverage=1,.abbreviate = F)
m1 <- processanimateR::animate_process(events1, duration=30)
m1
htmlwidgets::saveWidget(m1, file='E:/AU/PM/adm2019_UP.html')
#use OBS to save video

processmapR::idotted_chart(events1) 

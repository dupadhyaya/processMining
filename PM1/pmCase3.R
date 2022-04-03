#Adm GS Process Mining of Dates

pacman::p_load(readxl, dplyr, ggplot2, reshape2, gridExtra, zoo, xts, lubridate,googlesheets4, tidyverse, forcats)
#Admission Analysis using data with Dates

gs1AURJ = 'https://docs.google.com/spreadsheets/d/1IFEewuvwwLMhRmp2ckvk5CsZdrWGdoXQRsUkNLtP9L0'
gs1=gs1AURJ
(master <- as.data.frame(read_sheet(gs1, sheet='master')))
#(sheets = master$sheet)
sheet_properties(gs1)
(statusDate = Sys.Date())
(analysisDate = Sys.Date())
(campus = 'AURJ')
(mergedData = as.data.frame(read_sheet(gs1, sheet='merge', skip=3)))
dim(mergedData) 
names(mergedData)
str(mergedData)

admData1
df1 = admData1  #or merge
df1 = mergedData
names(df1)
dim(df1)
names(df1)
df1 %>% filter(!is.na(feePaidDate)) %>% filter(formDate > feePaidDate) %>% select(formNo)
df1 %>% filter(!is.na(feePaidDate)) %>% filter(formDate < feePaidDate) %>% select(formNo)
df1 %>% filter(formNo %in% c(8154086))
#df1 %>%  filter(formDate >= feePayDate | formDate >= sopEvalDate | feePayDate > feePaidDate1) %>% select(formNo, formDate, sopEvalDate, feePayDate, feePaidDate1) 

colSums(is.na(df1))
names(df1)
str(df1)
dateCols = c("formDate" ,"skypeDate","offerDate","feePayDate","unwillingDate", "withdrawlDate", "feePaidDate") 
#df1[, dateCols] <- lapply(df1[,dateCols], as.character)
df1 %>% select(dateCols)
df1[,dateCols] = lapply(df1[,dateCols], as.Date)
names(df1)
df2 <- df1 %>% select(c(formNo, campus, state, admStatus, dateCols)) %>% reshape2::melt(id.vars=c('formNo', 'campus','admStatus','state'), value.name='timestamp', variable.name='activity', na.rm=T)  #many blank rows

head(df2)
str(df2)
colListChr = c( 'campus', 'state')
df2[, colListChr] <- lapply(df2[,colListChr], as.character)
df2 <- df2 %>% mutate_at(c('campus','state'), as.character)
summary(df2$timestamp)
colSums(is.na(df2))
sum(complete.cases(df2))
df2[is.na(df2$admStatus), 'admStatus'] = 'NotAvailable'
table(df2$admStatus)
#prepare for PM----
df2$timestamp = as.POSIXct(df2$timestamp)  #
pacman::p_load(readxl, dplyr, ggplot2, reshape2, gridExtra, zoo, xts, lubridate,googlesheets4, processanimateR, bupaR, edeaR)

#simple Event
events1 <- bupaR::simple_eventlog(eventlog = tibble::as_tibble(df2),   case_id = 'formNo',  activity_id = 'activity', timestamp = 'timestamp')
#df2Melt$timestamp = as.Date(df2Melt$timestamp,format='%d-%b-%y')

events1
str(events1)
activity_frequency(events1)
events1 %>% activity_frequency(level='activity')

#eventPlot:PLOT----
events1 %>% activity_frequency(level = "activity") %>% plot() #use this
events1 %>% activity_frequency(level = "trace") %>% plot()
events1 %>% activity_frequency(level = "case") %>% plot()

#processMap : HTML----
events1 %>%  process_map(sec=frequency('absolute'))
events1 %>%  process_map(sec=frequency('relative'), rankdir='TB')
?process_map

head(events1)
processmapR::process_map(events1,rankdir="RL")

#check for anomalies---
df1 %>% filter(formDate > sopEvalDate) %>% select(formNo)
df1 %>% filter(formDate > feePayDate) %>% select(formNo)
df1 %>% filter(offerDate > feePayDate) %>% select(formNo)
df1 %>% filter(feePayDate > feePaidDate1) %>% select(formNo)
df1 %>%  filter(formDate >= feePayDate | feePayDate > feePaidDate) %>% select(formNo, admStatus, state, formDate, feePayDate, feePaidDate) %>% reshape2::melt(id.vars=c('formNo', 'admStatus','state'), value.name='timestamp', variable.name='activity', na.rm=T) %>% mutate(timestamp = as.POSIXct(timestamp)) %>% bupaR::simple_eventlog(case_id = 'formNo',  activity_id = 'activity', timestamp = 'timestamp') %>% process_map()
#-----------------

names(events1)
#traceExplorer:PLOT------
events1 %>% processmapR::trace_explorer(coverage=1)
events1 %>% processmapR::trace_explorer(coverage=.5) 
#precedentMatrix:PLOT
events1 %>% processmapR::precedence_matrix() %>% plot()
events1 %>% processmapR::precedence_matrix(type='relative-case') %>% plot()
events1 %>% processmapR::precedence_matrix(type='relative-antecedent') %>% plot()
events1 %>% processmapR::precedence_matrix(type='relative') %>% plot()
events1 %>% processmapR::precedence_matrix(type='relative-consequent') %>% plot()

#events1 %>% processmapR::resource_matrix() %>% plot() 
#dottedChart : PLOT-------
events1 %>%   filter_time_period(interval = ymd(c(20200101, 20200601)), filter_method = "contained") %>% dotted_chart()

#dotted chart with lines


#animate-----
table(events1$admStatus)
names(events1)
#since no of records are high, filter to only admission, unwilling and withdrawl cases
events1P <- events1 %>% filter(admStatus == 'Progress')
events1P %>% process_map()
events1R <- events1 %>% filter(admStatus == 'Rejected')
events1R %>% process_map()
events1A <- events1 %>% filter(admStatus == 'Admitted')
events1A %>% process_map()
events1U <- events1 %>% filter(admStatus == 'Unwilling')
events1U %>% process_map()
events1W <- events1 %>% filter(admStatus == 'Withdrawl')
events1W %>% process_map() #maybe nil

names(df2)
(admSummary <- df1 %>% dplyr::group_by(admStatus) %>% summarise(count = n()))
ggplot(admSummary, aes(x=admStatus, y=count, fill=admStatus)) + geom_bar(stat='identity') + geom_text(aes(label=count, y=count)) + coord_flip()

#animate_process(events1, duration=20) #animate later with lesser values
events1P
animate_process(events1P)
library(svgPanZoom)
whichEvent <- events1P
whichEvent %>% animate_process(sec=frequency('relative'), duration=20)
#whichEvent %>% filter_activity_presence("withdrawlDate") %>% animate_process()
animate_process(events1, mode = "relative", jitter = 10, repeat_count = 1)

#save as html------
table(events1$admStatus)
df2 %>% group_by(admStatus) %>% summarise(n=n())
processmapR::trace_explorer(events1, coverage=1,.abbreviate = F)
table(events1$admStatus)


head(events1)
str(events1)
animate_process(events1,    legend = "color",   mapping = token_aes(color = token_scale("state", scale = "ordinal", range = RColorBrewer::brewer.pal(8, "Paired"))), duration=20)

events1 %>% filter(state %in% c('UP','DL', 'AP')) %>% animate_process(.,    legend = "color",   mapping = token_aes(color = token_scale("state", scale = "ordinal", range = RColorBrewer::brewer.pal(8, "Paired"))), duration=20)




animateAllDates <- processanimateR::animate_process(events1, sec=frequency('absolute'), duration=60, legend='color', repeat_delay = 2, repeat_count=2, mode='absolute', inital_state='paused', espsilon_time=.3, mapping = token_aes(color= token_scale('admStatus', scale='ordinal', range=c('green','blue','red','yellow','violet'))))
animateAllDates
htmlwidgets::saveWidget(animateAllDates, file=paste0('E:/AU/PM/allDates20',campus,Sys.Date(),'.html'), libdir='E:/AU/PM/libdep', selfcontained=T)
#use OBS to save video

animateOthers <- events1 %>% filter(!admStatus %in% 'Admitted') %>% processanimateR::animate_process(sec=frequency('relative'),repeat_delay = 2, mode='absolute', duration=60, legend='color', repeat_count=2, inital_state='paused', espsilon_time=.4, mapping = token_aes(color= token_scale('admStatus', scale='ordinal', range=c('green','blue','red','yellow','violet'))))
animateOthers
htmlwidgets::saveWidget(animateOthers, file=paste0('E:/AU/PM/otherDates20',campus, Sys.Date(),'.html'), libdir='E:/AU/PM/libdep', selfcontained=T)

?animate_process

processmapR::idotted_chart(events) 
processmonitR::activity_dashboard(events1)
processmonitR::performance_dashboard(events1)
processmonitR::rework_dashboard(events1)

write.csv(merge, paste0('E:/data/backup/merge',campus,Sys.Date(),'.csv'), na='', row.names=F)

library(bupaR)
library(heuristicsmineR)
library(petrinetR)

heuristicsmineR::dependency_matrix(patients) %>% render_dependency_matrix()
heuristicsmineR::dependency_matrix(events1) %>% render_dependency_matrix()


#events1A, events1P, events1R, events1U, events1W
(eName = 'events1A')
(htmlPath = paste0('E:/graphs/',eName,'.html'))
events1P %>% process_map() %>% htmlwidgets::saveWidget(htmlPath, selfcontained = T, libdir = "lib")
list.files('E:/graphs/')
(imgPath  = paste0('E:/graphs/',eName,'.png'))
areaV <- c(top=0, left=0, width=1200, height=1000)
webshot::webshot(htmlPath, imgPath, zoom=1, expand=1)
image1 <- imager::load.image(file = imgPath)
GGevent1A <- ggplot() + ggpubr::background_image(image1) + labs(title=paste0('Process Map : ', eName))
GGevent1A
#-----
#traceExplorer:PLOT------
pEE1 <- plot(events1 %>% processmapR::trace_explorer(coverage=1), title='Coverage=1')
pEE1
pEE2 <- plot(events1 %>% processmapR::trace_explorer(coverage=.5), main='Coverage=50%') 

#precedentMatrix:PLOT
pPM1 <- plot(events1 %>% processmapR::precedence_matrix())
pPM2 <- plot(events1 %>% processmapR::precedence_matrix(type='relative-case'))
pPM3 <- plot(events1 %>% processmapR::precedence_matrix(type='relative-antecedent'))
pPM4 <- plot(events1 %>% processmapR::precedence_matrix(type='relative'))
pMP5 <- plot(events1 %>% processmapR::precedence_matrix(type='relative-consequent'))

#
areaV <- c(top=0, left=0, width=1200, height=1000)
webshot::webshot("E:/graphs/pmapP.html", "E:/graphs/pmapP2.png", cliprect = areaV, zoom=1.5, expand=1.5)
list.files('E:/graphs/')
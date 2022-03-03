#PM simulated Case - Admissions

#libraries
pacman::p_load(gsheet, bupaR, dplyr, edeaR, processmapR, processanimateR, DiagrammeR,lubridate, htmlwidgets, googlesheets4, tidyverse, reshape2) 

#LMS ------
slink1 <- 'https://docs.google.com/spreadsheets/d/1TCYjFz32Y6tlRFG0t0t4B4RVJaHfufZNg-bL0ZgSCpQ'

#sheets_browse(slink1)
adata <- read_sheet(slink1, sheet='admTraces')
head(adata)
names(adata)
adata2 <- adata %>% melt(id.vars= c('formNo','gender', 'leadSource', 'discipline','status'), variable.name ='activity', value.name='activityDate', na.rm=T)
head(adata2)
str(adata2)

aevents1 <- bupaR::simple_eventlog(eventlog = adata2,   case_id = 'formNo',  activity_id = 'activity', timestamp = 'activityDate')
aevents1

#process Map
aevents1 %>% process_map()
aevents1 %>%  processing_time("activity") %>%  plot
aevents1 %>%  throughput_time("log") %>% plot() #start to end
aevents1 %>%  resource_frequency("resource")
aevents1 %>% process_map(edge_width=F)
aevents1 %>% process_map(sec=frequency('relative'), rankdir='LR')
?process_map
aevents1 %>% animate_process(mapping = token_aes(color=token_scale('red')), duration=20)
table(aevents1$leadSource)

aevents1 %>% trace_explorer(coverage=1)
aevents1 %>% precedence_matrix() %>% plot()
aevents1 %>% dotted_chart()


PMV_SA1 <- aevents1 %>% animate_process(duration=30, sec=frequency('relative'), legend='color', mapping = token_aes(color=token_scale('leadSource', scale='ordinal', range=c('red','blue','green','orange','pink','gray'))))
PMV_SA1 
htmlwidgets::saveWidget(widget=PMV_SA1, file='E:/AU/PM/PMV_SA1.html', title='Process Mining Video : Campus Admissions', libdir ='E:/AU/PM/lib', selfcontained = T) 

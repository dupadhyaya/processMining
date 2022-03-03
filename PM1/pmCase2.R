#libraries
pacman::p_load(gsheet, bupaR, dplyr, edeaR, processmapR, processanimateR, DiagrammeR,lubridate, htmlwidgets, googlesheets4, tidyverse, reshape2) 

#LMS ------
slink1 <- 'https://docs.google.com/spreadsheets/d/1TCYjFz32Y6tlRFG0t0t4B4RVJaHfufZNg-bL0ZgSCpQ'

#sheets_browse(slink1)
admLog <- read_sheet(slink1, sheet='admTraces3')
head(admLog)

names(admLog)

aEvent2 <- admLog %>%
  mutate(activity_instance = 1:nrow(.)) %>%
  eventlog(
    case_id = "formNo",
    activity_id = "activity",
    activity_instance_id = "activity_instance",
    lifecycle_id = "status",
    timestamp = "activityDate",
    resource_id = "resource"
  )
aEvent2

aEvent2 %>% process_map()
aEvent2 %>% animate_process(duration=10)
aEvent2 %>% trace_explorer(coverage=1)
aEvent2 %>% precedence_matrix() %>% plot()
aEvent2 %>% dotted_chart()
aEvent2 %>%  resource_frequency("resource") %>% plot()
aEvent2 %>%  resource_specialisation("resource") %>% plot()


aEvent2 %>% activity_presence() %>% plot()  #not clear
aEvent2 %>%  resource_frequency("resource")
aEvent2 %>%  resource_frequency("resource") %>% plot()
aEvent2 %>% resource_specialisation("resource")
aEvent2 %>% resource_specialisation("resource") %>% plot()

aEvent2 %>%  activity_frequency("activity") %>% plot()
aEvent2 %>%  start_activities("resource-activity") %>% plot()
aEvent2 %>%  end_activities("resource-activity") %>% plot()
aEvent2 %>% activity_presence() %>%  plot
aEvent2 %>% activity_presence()
aEvent2 %>%  activity_frequency("activity")
aEvent2 %>%  activity_frequency("activity") %>% plot()
aEvent2 %>% trace_coverage("trace") %>% plot() #??
aEvent2 %>%   trace_length("log") %>%   plot() #ads from 200 to 300 times
aEvent2 %>% process_map(type = frequency("relative_case"))
aEvent2 %>%  process_map(type_nodes = frequency("relative_case"),  type_edges = performance(mean))
aEvent2 %>%  process_map(sec=frequency('relative'), type=frequency('absolute'))
aEvent2 %>% process_map(sec=frequency('absolute'), type=frequency('relative'), fixed_edge_width=T, layout = layout_pm(edge_weight = T, fixed_positions = T))

(dc1a1 <- aEvent2 %>% dotted_chart(x='absolute', y='start_day'))
(dc1a2 <- aEvent2 %>% dotted_chart(x='absolute', y='start_week'))

(dc1b1 <- aEvent2 %>% dotted_chart(x='relative', units='weeks'))
(dc1b2 <- aEvent2 %>% dotted_chart(x='relative',units='days'))
(dc1b3 <- aEvent2 %>% dotted_chart(x='relative',units='days', add_end_events =T))
(dc1c <- aEvent2 %>% dotted_chart(x='relative_day', unit='days'))
(dc1d <- aEvent2 %>% dotted_chart(x='relative_week', unit='weeks'))

dotted_chart(aEvent2, x = "relative", y ="duration", color = NULL, units ="hours")
dotted_chart(aEvent2, x = "absolute", y ="duration", color = NULL, units ="hours")
dotted_chart(aEvent2, x = "relative", y ="start", color = NULL, units ="hours")
dotted_chart(aEvent2, x = "absolute", y ="start", color = NULL, units ="hours")

#activities-----
pm1 <- aEvent2 %>% process_map()
get_activities(pm1)
get_flows(pm1)


# frequency
fa1 <- frequency(value = 'absolute', color_scale='PuBu', color_edges ='dodgerblue4')
fa2 <- frequency(value = 'absolute-case', color_scale='PuBu', color_edges ='dodgerblue4')
fr1 <- frequency(value = 'relative', color_scale='PuBu', color_edges ='dodgerblue4')
fr2 <- frequency(value = 'relative-case', color_scale='PuBu', color_edges ='dodgerblue4')

aEvent2 %>% process_map(type=fa1)
aEvent2 %>% process_map(type=fa2)
aEvent2 %>% process_map(type=fr1)
aEvent2 %>% process_map(type=fr2)
aEvent2 %>% process_map(type = frequency("relative_case", color_scale = "Purples"))
aEvent2  %>% process_map(type_nodes = frequency("relative_case"), type_edges = performance(mean))


aEvent2 %>% trace_explorer(coverage=1)
aEvent2 %>% precedence_matrix()  %>% plot()
aEvent2 %>% trace_explorer( coverage = NULL,n_traces = NULL,type = c("frequent", "infrequent"),  coverage_labels = c("relative", "absolute", "cumulative"),   .abbreviate = T, show_labels = T,   label_size = 3,  scale_fill = scale_fill_discrete(h = c(0, 360) + 15, l = 40), raw_data = F )
aEvent2 %>% trace_explorer(coverage=1)
aEvent2 %>% trace_explorer(n_traces=1)
aEvent2 %>% trace_explorer(type='infrequent', n_traces=1, show_labels=F)

#Mar
aEvent2
aEvent2 %>% filter_time_period(interval=c(as.Date('2020-03-01'), as.Date('2020-03-20')), filter_method =c('start')) %>% trace_explorer(coverage=1,show_labels=T)
aEvent2 %>% filter_time_period(interval=c(as.Date('2020-03-01'), as.Date('2020-03-20')), filter_method =c('trim')) %>% trace_explorer(coverage=1,show_labels=T)
aEvent2 %>% filter_time_period(interval=c(as.Date('2020-03-01'), as.Date('2020-03-20')), filter_method =c('complete')) %>% trace_explorer(coverage=1,show_labels=T)


aEvent2 %>% process_map(type = performance())
aEvent2 %>% process_map(type = performance(FUN=mean))
aEvent2 %>% process_map(type = performance(FUN=max))
aEvent2 %>% process_map(type = performance(units='days'))
aEvent2 %>% process_map(type = performance(FUN=min), units='mins')
aEvent2 %>% process_map(type = performance(FUN=max, color_edges = 'blue'))
aEvent2 %>% process_map(type = performance(FUN=max, color_scale = 'Blues'))
aEvent2 %>% process_map(type = performance(FUN=mean, flow_time='idle_time'))
aEvent2 %>% process_map(type = performance(FUN=mean, flow_time='inter_start_time'))
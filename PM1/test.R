


example_log
# Animate the example process with activities placed in some locations
animate_process(example_log,
                renderer = renderer_leaflet(
                  node_coordinates = data.frame(
                    act = c("A", "B", "C", "D", "ARTIFICIAL_START", "ARTIFICIAL_END"),
                    lat = c(63.443680, 63.426925, 63.409207, 63.422336, 63.450950, 63.419706),
                    lng = c(10.383625, 10.396972, 10.406418, 10.432119, 10.383368, 10.252347),
                    stringsAsFactors = FALSE),
                  edge_coordinates = data.frame(
                    act_from = c("B"),
                    act_to = c("C"),
                    lat = c(63.419207),
                    lng = c(10.386418),
                    stringsAsFactors = FALSE),
                  options = list(center = c(63.412273, 10.399590), zoom = 12)),
                duration = 5, repeat_count = 1)


animate_process(example_log, mapping = token_aes(color = token_scale("red")))

animate_process(edeaR::filter_trace_frequency(bupaR::sample_n(traffic_fines,1000),percentage=0.95),     legend = "color", mode = "relative",  mapping = token_aes(color = token_scale("amount", scale = "linear",  range = c("yellow","red"))))


head(example_log)
animate_process(example_log,    legend = "color",   mapping = token_aes(color = token_scale("res", scale = "ordinal", range = RColorBrewer::brewer.pal(8, "Paired"))), duration=10)


x <- data.frame(case = as.character(rep(c(1,2,3), 2)),
                time = seq(from = as.POSIXct("2018-10-03 05:00:00"),
                           to = as.POSIXct("2018-10-03 06:00:00"),
                           length.out = 6),
                value = rep(c("orange", "green"), 3),
                stringsAsFactors = FALSE)
example_log
x  %>% arrange(case, time)
animate_process(example_log  %>% filter(case %in% c(1,2)),
                mode = "absolute",
                jitter = 10,
                legend = "color",
                mapping = token_aes(color = token_scale(x)), duration = 10)


token_select_decoration("red")

animate_process(edeaR::filter_trace_frequency(bupaR::sample_n(traffic_fines,1000),percentage=0.95),   legend = "color", mode = "relative",           mapping = token_aes(color = token_scale("amount",  scale = "linear",        range = c("yellow","red"))))
animate_process(example_log, renderer = renderer_graphviz())

animate_process(patients,
                mapping = token_aes(shape = "image",
                                    size = token_scale(10),
                                    image = token_scale("https://upload.wikimedia.org/wikipedia/en/5/5f/Pacman.gif")))


# Extract only the lacticacid measurements
lactic <- sepsis %>%
  mutate(lacticacid = as.numeric(lacticacid)) %>%
  filter_activity(c("LacticAcid")) %>%
  as.data.frame() %>%
  select("case" = case_id, 
         "time" =  timestamp, 
         value = lacticacid) # format needs to be 'case,time,value'

lactic

# Remove the measurement events from the sepsis log
sepsisBase <- sepsis %>%
  filter_activity(c("LacticAcid", "CRP", "Leucocytes", "Return ER",
                    "IV Liquid", "IV Antibiotics"), reverse = T) %>%
  filter_trace_frequency(percentage = 0.95)

sepsis
head(lactic)
head(sepsis)
# Animate with the secondary data frame `lactic`
animate_process(sepsisBase, 
                mode = "relative", 
                duration = 300,
                legend = "color", 
                mapping = token_aes(color = token_scale(lactic,   scale = "linear",     range = c("#fff5eb","#7f2704"))))


# Create performance time flags ------------------------------------------------
library(dplyr)            ##pipes
library(tidyr)            ##tidy data, partcularly the crossing() function
library(lubridate)        ##date time manipulation
library(bupaR)            ##buisness process analytics
library(processanimateR)  ##animates process
my_flags <- data.frame(value = c(0,2,4,8,16)) %>%   mutate(day = days(value)) #convert numeric value into days
my_flags
# Create timestamps of flags ----------------------------------------------

my_timeflags <- patients %>% 
  cases %>%
  crossing(my_flags) %>% ##similar to a SQL outer join
  mutate(time = start_timestamp + day) %>% 
  filter(time <= complete_timestamp) %>% 
  select("case" = patient,time,value) ##must be case, time, value
my_timeflags
# Animate process ---------------------------------------------------------

patients %>%
  animate_process(mode ="absolute", repeat_count = 1, duration=100,
                  jitter=10,
                  legend = "color", 
                  mapping = token_aes(
                    color = token_scale(my_timeflags
                                        , scale = "ordinal"
                                        , domain = my_flags$value
                                        , range = rev(RColorBrewer::brewer.pal(5,"Spectral"))              )))

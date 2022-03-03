#admission Dashboard
tz(Sys.time())
Sys.timezone(location = TRUE)
Sys.unsetenv("IST")

pacman::p_load(readxl, dplyr, ggplot2, reshape2, gridExtra, zoo, xts, lubridate)

filelocation="C:\\Users\\du\\OneDrive - Amity University\\ak\\admission_AK_DB.xlsx"

all = read_excel(path=filelocation , sheet='all')

all$date = as.Date(all$date)
df = all
head(df)
str(df)
df$date
sum(is.na(df))

campus = 'AURJ_J'

table(df$category)
catlevels = c('InboundCalls','WalkIns','OnlineDownloads','FormsSold','FormsReceived','SOP','Skype','AdmissionFees')

df$category = factor(df$category, ordered=T, levels=catlevels)
df$admSession = factor(df$admSession, ordered=T, levels=c('2018-19','2019-20'))
head(df)
dfsum1 <- df %>% mutate(mon = format(date, '%b'), yr = format(date, '%Y')) %>% group_by(month=floor_date(date, "month"), admSession, mon, yr, category) %>%  summarize(SUMV=sum(value, na.rm=T))

str(dfsum1)
head(dfsum1)
dfsum1 %>% as.data.frame()
dfsum1$mon = factor(dfsum1$mon, ordered=T, levels = month.abb[c(6:12,1:5)])

gAK1A <- ggplot(dfsum1, aes(x=category,y=SUMV, fill=category)) + geom_bar(stat='identity', position = position_dodge2(.7)) + geom_text(aes(label=SUMV, y = SUMV), position = position_dodge2(.7)) +  labs(title = "Admission Process Summary : 2019 vs 2020 : Monthwise", subtitle = campus)  +  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=30, size=rel(1)) )  + facet_grid(mon ~ admSession, scales='free')   + scale_fill_brewer(palette = "Set1")
gAK1A

gAK1B <- ggplot(dfsum1, aes(x=category, y=SUMV, fill=admSession)) + geom_bar(stat='identity', position = position_dodge2(.7)) + geom_text(aes(label=SUMV, y = SUMV), position = position_dodge2(.7)) +  labs(title = "Admission Process Summary : 2019 vs 2020 : Compare Category - Yearwise", subtitle = campus)  +  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=30, size=rel(1)) )  + facet_grid(mon ~ ., scales='free') + scale_fill_brewer(palette = "Set1", drop=F) 
gAK1B

#total: 2019-20----
summary(df)
(dfsum2 <- df %>% group_by(admSession, category) %>%  summarize(SUMV=sum(value, na.rm=T)))
gAK1C <- ggplot(dfsum2, aes(x=category, y=SUMV, fill=admSession)) + geom_bar(stat='identity', position = position_dodge2(.7)) + geom_text(aes(label=SUMV, y = SUMV), position = position_dodge2(.7)) +  labs(title = "Admission Process Summary : 2019 vs 2020 : Compare Category-Yearwise", subtitle = campus )  +  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=30, size=rel(1)) )  + scale_fill_brewer(palette = "Set1", drop=F) 
gAK1C
names(dfsum1)
dfsum2 <- dfsum1 %>% group_by(category, admSession) %>% mutate(cumSum = cumsum(SUMV))  %>% arrange(admSession, category) %>% as.data.frame()

all %>% group_by(yearmon(date), category)  %>% summarise(sum(value, na.rm=T)) 

all %>% group_by(category, admSession) %>% filter(category =='WalkIns') %>% summarise(sum2 = sum(value, na.rm=T))

head(dfsum2)
gAK1D <- ggplot(dfsum2, aes(x=mon, y=cumSum)) + geom_line(size=2, aes( group=admSession, colour=factor(admSession))) + facet_wrap( category ~ ., scale='free') + geom_text(aes(label=cumSum, y=cumSum)) + labs(title='Cumulative Changes ', subtitle = campus) + theme(plot.title = element_text(hjust = 0.5), legend.position="top") + scale_color_manual(values=c('green','red'))
gAK1D


#save plots-----
graphAK1  = list(gAK1A, gAK1B, gAK1C, gAK1D)
agraphAK1 <- gridExtra::marrangeGrob(graphAK1, nrow=1, ncol=1, top = quote(paste("Admission Dashboard : page", g, "of", npages)))
ggsave(paste0("E:/AU/agraph/admAK1",Sys.Date(), ".pdf"), agraphAK1, width=4, height=2, units="in", scale=3)
#x----



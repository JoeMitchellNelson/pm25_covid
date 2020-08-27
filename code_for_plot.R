require(pacman)

p_load(tidyverse,ggplot2,rnoaa,zoo,extrafont)

# NOTE: while rnoaa provides a highly useful api interface for NOAA data
# it doesn't seem to have access to pm2.5 or other air quality data


#### this file ##########################
# 1) get fonts because arial is boring  #
# 2) read in and format data            #
# 3) make plot                          #
#########################################

#import even more fonts for ggplot
#WARNING: this takes a few minutes, so it's commented out for now
#font_import()
#loadfonts(device="win")

# see a list of fonts. 
#fonts()



# read in daily pm2.5/AQI data for 2017-2020 (downloaded from NOAA)
# for the New York/Newark/Jersey City metropolitan area

dat <- read.csv("~/pm25_covid/aqi_ny.csv") %>% dplyr::select(Date,Site.ID,DAILY_AQI_VALUE,Daily.Mean.PM2.5.Concentration)
dat$Date <- mdy(dat$Date)

dat19 <- read.csv("~/pm25_covid/aqi_ny_19.csv") %>% dplyr::select(Date,Site.ID,DAILY_AQI_VALUE,Daily.Mean.PM2.5.Concentration)
dat19$Date <- mdy(dat19$Date)
dat19 <- dat19 %>% dplyr::filter(Date <= max(dat$Date - ddays(365)))

dat18 <- read.csv("~/pm25_covid/aqi_ny_18.csv") %>% dplyr::select(Date,Site.ID,DAILY_AQI_VALUE,Daily.Mean.PM2.5.Concentration)
dat18$Date <- mdy(dat18$Date)
dat18 <- dat18 %>% dplyr::filter(Date <= max(dat$Date - ddays(365)))

dat17 <- read.csv("~/pm25_covid/aqi_ny_17.csv") %>% dplyr::select(Date,Site.ID,DAILY_AQI_VALUE,Daily.Mean.PM2.5.Concentration)
dat17$Date <- mdy(dat17$Date)
dat17 <- dat17 %>% dplyr::filter(Date <= max(dat$Date - ddays(365)))

dat$rel_date <- format(dat$Date,"%m %d")
dat19$rel_date <- format(dat19$Date,"%m %d")
dat18$rel_date <- format(dat18$Date,"%m %d")
dat17$rel_date <- format(dat17$Date,"%m %d")


dates <- dat %>% dplyr::select(Date,rel_date) %>% unique()

joined <- left_join(dat,dat19,by=c("Site.ID","rel_date"))
joined <- left_join(joined,dat18,by=c("Site.ID","rel_date"))
joined <- left_join(joined,dat17,by=c("Site.ID","rel_date"))


# only keep site-day observations where we have readings for all years
joined <- joined %>% dplyr::filter(!is.na(Daily.Mean.PM2.5.Concentration.y))
joined <- joined %>% dplyr::filter(!is.na(DAILY_AQI_VALUE.y))
joined <- joined %>% dplyr::filter(!is.na(Daily.Mean.PM2.5.Concentration.y.y))
joined <- joined %>% dplyr::filter(!is.na(DAILY_AQI_VALUE.y.y))
joined <- joined %>% dplyr::filter(!is.na(Daily.Mean.PM2.5.Concentration.x.x))
joined <- joined %>% dplyr::filter(!is.na(DAILY_AQI_VALUE.x.x))


joined <- joined %>% group_by(rel_date) %>% summarise(pm2020 = mean(Daily.Mean.PM2.5.Concentration.x,na.rm=T),
                                                      aqi2020 = mean(DAILY_AQI_VALUE.x,na.rm=T),
                                                      
                                                      pm2019 = mean(Daily.Mean.PM2.5.Concentration.y,na.rm=T),
                                                      aqi2019 = mean(DAILY_AQI_VALUE.y,na.rm=T),
                                                      
                                                      pm2018 = mean(Daily.Mean.PM2.5.Concentration.x.x,na.rm=T),
                                                      aqi2018 = mean(DAILY_AQI_VALUE.x.x,na.rm=T),
                                                      
                                                      pm2017 = mean(Daily.Mean.PM2.5.Concentration.y.y,na.rm=T),
                                                      aqi2017 = mean(DAILY_AQI_VALUE.y.y,na.rm=T))

joined <- joined %>% dplyr::mutate(aqi2020ma = zoo::rollmean(aqi2020, k = 28, fill = NA,align="center"),
                                   pm2020ma = zoo::rollmean(pm2020, k = 28, fill = NA,align="center"),
                                   
                                   aqi2019ma = zoo::rollmean(aqi2019, k = 28, fill = NA,align="center"),
                                   pm2019ma = zoo::rollmean(pm2019, k = 28, fill = NA,align="center"),
                                   
                                   aqi2018ma = zoo::rollmean(aqi2018, k = 28, fill = NA,align="center"),
                                   pm2018ma = zoo::rollmean(pm2018, k = 28, fill = NA,align="center"),
                                   
                                   aqi2017ma = zoo::rollmean(aqi2017, k = 28, fill = NA,align="center"),
                                   pm2017ma = zoo::rollmean(pm2017, k = 28, fill = NA,align="center"),)

joined <- left_join(joined,dates)


chosen_font <- "Arimo"

# annotate in ggplot doesn't always play well with fonts
# run this line to enable chosen_font for annotate
windowsFonts(Times=windowsFont(chosen_font))


ggplot(joined) +
  geom_vline(xintercept=ymd("2020-01-01")) +
  geom_hline(yintercept=4) +
  geom_line(aes(x=Date,y=pm2017ma),color="grey80",size=1) +
  geom_line(aes(x=Date,y=pm2018ma),color="grey60",size=1) +
  geom_line(aes(x=Date,y=pm2019ma),color="grey45",size=1) +
  geom_line(aes(x=Date,y=pm2020ma),size=1,color="#4CBB17") +
  
  # geom_vline(xintercept=ymd("2020-03-22")) +
  # geom_vline(xintercept=ymd("2020-03-13")) +
  # geom_vline(xintercept=ymd("2020-02-28")) +
  # geom_vline(xintercept=ymd("2020-06-08")) +
  
  annotate("text",x=ymd("2020-02-01"),y=6.4,label="First US Covid-19\ndeath reported",family=chosen_font,color="#201069") +
  geom_segment(x=ymd("2020-02-01"),y=6.9,xend=ymd("2020-02-26"),yend=7.57,color="#201069",alpha=.2) +
  
  annotate("text",x=ymd("2020-04-10"),y=8.65,label="NY & NJ begin lockdown",family=chosen_font,color="#201069") +
  geom_segment(x=ymd("2020-04-10"),y=8.3,xend=ymd("2020-03-22"),yend=5.5,color="#201069",alpha=.2) +
  #  geom_segment(x=ymd("2020-04-10"),y=8.3,xend=ymd("2020-04-01"),yend=5.2,color="#201069",alpha=.2) +
  
  annotate("text",x=ymd("2020-07-20"),y=5.8,label="NY & NJ begin\nto reopen",family=chosen_font,color="#201069") +
  geom_segment(x=ymd("2020-07-10"),y=5.5,xend=ymd("2020-06-11"),yend=5.1,color="#201069",alpha=.2) +
  
  annotate("text",x=ymd("2020-08-15"),y=8.71,label="2020",color="#4CBB17",fontface="bold",family=chosen_font) +
  annotate("text",x=ymd("2020-08-15"),y=9.67,label="2019",color="grey45",fontface="bold",family=chosen_font) +
  annotate("text",x=ymd("2020-08-15"),y=10.1,label="2018",color="grey60",fontface="bold",family=chosen_font) +
  annotate("text",x=ymd("2020-08-15"),y=9.31,label="2017",color="grey80",fontface="bold",family=chosen_font) +
  
  labs(y="PM2.5 (28-day rolling average)",x="",
       title="Effect of Covid-19 on urban air quality",
       subtitle="PM2.5 in New York-Newark-Jersey City MSA, 2020 compared to previous years",
       caption="Data: NOAA, 26 monitor sites        \n") +
  
  theme_minimal() +
  theme(text=element_text(family=chosen_font),
        panel.background = element_rect(fill="#fff0fc",color=NA),
        plot.background = element_rect(fill="#fff0fc",color=NA)) # sets all fonts except annotate




## code to prepare `surveys` dataset goes here

subs <- read.csv('data-raw/data/Sample_4.5_OEP1.csv')
subs

subs <- subs %>%
  filter(Phase == 2) %>%
  group_by(SightID) %>%
  summarize(Date = stringr::str_pad(Date[1],width=6,side='left',pad='0'),
            SightNo = SightNo[1],
            Phase2_start = stringr::str_pad(Time[1],width=6,side='left',pad='0')) %>%
  mutate(mm=substr(Date,1,2),
         dd=substr(Date,3,4),
         yy=substr(Date,5,6),
         hh=substr(Phase2_start,1,2),
         mins=substr(Phase2_start,3,4),
         ss=substr(Phase2_start,5,6))

subs
subs$Date <- paste0('20',subs$yy,'-',subs$mm,'-',subs$dd)
subs$Phase2_start <- paste0('20',subs$yy,'-',subs$mm,'-',subs$dd,' ',subs$hh,':',subs$mins,':',subs$ss)

subs <- subs[,c(2,3,4)]
subs
subgroup_phases <- subs

usethis::use_data(subgroup_phases, overwrite = TRUE)

#example tidyr::spread and tidyr::gather functions

library(tidyr)
library(lubridate)
library(nycflights13)

fl <- flights

#number of flights scheduled per weekday per carrier

fl1 <- fl %>%
  mutate(date = paste(year, month, day)) %>%
  mutate(date = ymd(date), wkday = wday(date, label = TRUE)) %>%
  group_by(wkday, carrier) %>%
  summarize(n_scheduled = n())
#as cross table
fl1.cross <- fl1 %>%
  spread(wkday, n_scheduled)

#back to tidy format
fl1.tidy <- fl1.cross %>%
  gather(wkday, n_scheduled, Sun:Sat)


sqldf('select * from fl')

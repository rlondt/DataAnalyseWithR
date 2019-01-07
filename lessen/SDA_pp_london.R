library(tidyverse)
#Tijddiagram
#Aantal verkochte woningen per dag in London in 2016

#bron: https://www.gov.uk/government/statistical-data-sets/price-paid-data-downloads
pp2016.london <- read_csv("datafiles/pp2016_london.csv")




pp2016.london.1 <- pp2016.london %>%
  select(ID, DATE) %>%
  group_by(DATE) %>%
  summarize(AANT = n())

ggplot(pp2016.london.1, aes(x=DATE, y=AANT)) +
  geom_line() 

ggplot(pp2016.london.1, aes(x=DATE, y=AANT)) +
  geom_line() +
  scale_x_datetime(date_breaks = "month", date_labels = "%b") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)
        , title = element_text(color = "blue"))

#inzoomen op januari

pp2016.london.2 <- filter(pp2016.london.1, DATE < "2016-02-01")

ggplot(pp2016.london.2, aes(x=DATE, y=AANT)) +
  geom_line() +
  scale_x_datetime(date_breaks = "day", date_labels = "%a-%d") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

#inzoomen op april

pp2016.london.2 <- filter(pp2016.london.1, DATE >= "2016-04-01",DATE < "2016-05-01")

ggplot(pp2016.london.2, aes(x=DATE, y=AANT)) +
  geom_line() +
  scale_x_datetime(date_breaks = "day", date_labels = "%a") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

#inzoomen op september (filter m.b.v. een functie lubridate:: month)
library(lubridate)

pp2016.london.2 <- filter(pp2016.london.1, month(DATE) == 9)
#let op: dubbel =-teken om maand 9 te filteren

ggplot(pp2016.london.2, aes(x=DATE, y=AANT)) +
  geom_line() +
  scale_x_datetime(date_breaks = "day", date_labels = "%a") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))







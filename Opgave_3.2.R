#install.packages('RSocrata')
#install.packages("Hmisc")


library(RSocrata) #eerst eenmalig installeren met install.packages('RSocrata')
library(dplyr)
library(Hmisc)
library(lubridate)
library(scales)
# Formuleer vragen die dit databestand oproept en die met behulp van dit databestand te beantwoorden zijn. Beantwoord deze vragen op basis van (grafische) data analyse.


endpoint.1 <-  "https://opendata.rdw.nl/resource/m9d7-ebf2.json?voertuigsoort=Personenauto&merk=OPEL&$limit=10000"
df.opel <- read.socrata(endpoint.1)
class(df.opel)
describe(df.opel)
colnames(df.opel)

# is de cylinder inhoud afgenomen van de verkochte in de loop van de tijd?

df.opel.prepared <- 
  mutate(df.opel, datum_eerste_afgifte_nederland_date = as.Date(datum_eerste_afgifte_nederland, "%d/%m/%Y")) %>%
  mutate( datum_eerste_toelating_date = as.Date(datum_eerste_toelating, "%d/%m/%Y")) %>%
  mutate( datum_tenaamstelling_date = as.Date(datum_tenaamstelling, "%d/%m/%Y")) %>%
  mutate( cilinderinhoud = as.numeric(cilinderinhoud )) %>%
  mutate( catalogusprijs = as.numeric(catalogusprijs)) %>%
  mutate( massa_ledig_voertuig = as.numeric(massa_ledig_voertuig)) %>%
  mutate( lengte = as.numeric(lengte)) %>%
  mutate( datum_tenaamstelling = floor_date(datum_tenaamstelling_date,"years")) %>%
#  mutate( datum_tenaamstelling = floor_date(datum_eerste_toelating_date,"years")) %>%
#  mutate( datum_tenaamstelling = floor_date(datum_eerste_afgifte_nederland_date,"years")) %>%
  select( datum_tenaamstelling, cilinderinhoud, catalogusprijs, massa_ledig_voertuig, lengte ) %>%
  group_by( datum_tenaamstelling ) %>%
  summarise(avg_cilinderinhoud = mean(cilinderinhoud, na.rm = TRUE)) %>%
  filter(avg_cilinderinhoud != 1)
  
      
ggplot(df.opel.prepared, aes(datum_tenaamstelling, avg_cilinderinhoud)) + 
  geom_bar( stat = "identity") +
  scale_x_date(date_breaks = "1 year",
               labels = date_format("%y"),
               limits = as.Date(c('1989-01-01', '2019-01-01'))
              )


# Kies een ander automerk waarvan verwacht wordt dat dit merk voor bepaalde kenmerken (variabelen) verschilt van het merk Opel. Ga op grond van data-analyse na of deze verwachting juist is.

endpoint.2 <-  "https://opendata.rdw.nl/resource/m9d7-ebf2.json?voertuigsoort=Personenauto&merk=BMW&$limit=10000"
df.bmw <- read.socrata(endpoint.2)

df.bmw.prepared <- 
  mutate(df.bmw, datum_eerste_afgifte_nederland_date = as.Date(datum_eerste_afgifte_nederland, "%d/%m/%Y")) %>%
  mutate( datum_eerste_toelating_date = as.Date(datum_eerste_toelating, "%d/%m/%Y")) %>%
  mutate( datum_tenaamstelling_date = as.Date(datum_tenaamstelling, "%d/%m/%Y")) %>%
  mutate( cilinderinhoud = as.numeric(cilinderinhoud )) %>%
  mutate( catalogusprijs = as.numeric(catalogusprijs)) %>%
  mutate( massa_ledig_voertuig = as.numeric(massa_ledig_voertuig)) %>%
  mutate( lengte = as.numeric(lengte)) %>%
  mutate( datum_tenaamstelling = floor_date(datum_tenaamstelling_date,"years")) %>%
  #  mutate( datum_tenaamstelling = floor_date(datum_eerste_toelating_date,"years")) %>%
  #  mutate( datum_tenaamstelling = floor_date(datum_eerste_afgifte_nederland_date,"years")) %>%
  select( datum_tenaamstelling, cilinderinhoud, catalogusprijs, massa_ledig_voertuig, lengte ) %>%
  group_by( datum_tenaamstelling ) %>%
  summarise(avg_cilinderinhoud = mean(cilinderinhoud, na.rm = TRUE)) %>%
  filter(avg_cilinderinhoud != 1)


ggplot(df.bmw.prepared, aes(datum_tenaamstelling, avg_cilinderinhoud)) + 
  geom_bar( stat = "identity") +
  scale_x_date(date_breaks = "1 year",
               labels = date_format("%y"),
               limits = as.Date(c('1989-01-01', '2019-01-01'))
  )

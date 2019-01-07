#### STANDAARD GRAFIEKEN IN R: BARPLOTS ####

library(readr)   #read_csv() functie
library(ggplot2)
library(tidyverse)

#inlezen bestand
funda <- read_csv("datafiles/2018-10-04_tekoop_funda.csv")

#inspecteren bestand
class(funda)   #soort object
  #tbl_df staat voor tibble data format, aangepast data frame format
head(funda) 
tail(funda)
summary(funda)
str(funda)


##### CATEGORIALE VARIABELE: BAR PLOT #####

#variabele : PLAATS
#grafiek aantal tekoop staande woningen per plaats
ggplot(data = funda, aes(x = PLAATS)) +
  geom_bar()

#zelfde grafiek, andere kleur staven
ggplot(data = funda, aes(x = PLAATS)) +
  geom_bar(fill = "blue")

#ordenen naar aantal tekoop staande woningen
#hiervoor bestaand diverse methoden

#eenvoudigst, maar niet meest handig
#PLAATS omzetten naar ordered factor variabele

#eerst volgorde levels bepalen
sort(table(funda$PLAATS))

funda$PLAATS <- factor(funda$PLAATS,
                          levels = c("Amsterdam", "Den Haag",
                          "Rotterdam", "Utrecht", "Arnhem",
                          "Den Bosch", "Groningen",
                          "Nijmegen", "Zwolle", "Maastricht"))
ggplot(data = funda, aes(x = PLAATS)) +
  geom_bar(fill = "blue")

#betere methode
overz01 <- funda %>%
  group_by(PLAATS) %>%
  summarize(AANTAL_TEKOOP = n()) %>%
  arrange(desc(AANTAL_TEKOOP))

funda$PLAATS <- factor(funda$PLAATS, levels = overz01$PLAATS)

ggplot(data = funda, aes(x = PLAATS)) +
  geom_bar(fill = "purple")


### OPGAVE
# maak een staafdiagram van het aantal te koop staande woningen
# naar de variabele KAMERS
# zet KAMERS daartoe eerst om in een factor variabele

# minder goede manier gesorteerd op aantal kamers
ggplot(data = funda, aes(x = KAMERS)) +
  geom_bar()



sort(table(funda$KAMERS))

funda$KAMERS <- factor(funda$KAMERS,
                       levels = seq(0, 30, by=1))

ggplot(data = funda, aes(x = KAMERS)) +
  geom_bar(fill = "green")


str(funda)

#betere manier gesorteerd op aantal aantal kamers.
funda <- read_csv("datafiles/2018-10-04_tekoop_funda.csv")

overz02 <- funda %>%
  group_by(KAMERS) %>%
  summarize(AANTAL_TEKOOP = n()) %>%
  arrange(desc(AANTAL_TEKOOP))
overz02
#sort(table(overz02$AANTAL_TEKOOP))

ggplot(data = overz02, aes(x = reorder(overz02$KAMERS, -overz02$AANTAL_TEKOOP), y = AANTAL_TEKOOP)) +
  #de tellingen zijn al gedaan
  #daarom wordt het default stat argument voor barplots ("count")
  #overschreven door stat = "identity"
  geom_bar(stat = "identity", fill = "blue")


#funda$KAMERS <- factor(funda$KAMERS, levels = overz02$KAMERS)

ggplot(data = funda, aes(x = KAMERS)) +
  geom_bar(fill = "yellow")

#het is ook mogelijk eerst zelf de telling per categorie te doen
#- zoals in overz01 gedaan is -
#en dan de grafiek te maken

ggplot(data = overz01, aes(x = PLAATS, y = AANTAL_TEKOOP)) +
  #de tellingen zijn al gedaan
  #daarom wordt het default stat argument voor barplots ("count")
  #overschreven door stat = "identity"
  geom_bar(stat = "identity", fill = "blue")


#enkele veel voorkomende aanpassingen aan een grafiek

ggplot(data = funda, aes(x = PLAATS)) +
  geom_bar(fill = "lightblue") +
  
  #titel boven de grafiek
  ggtitle("Op funda.nl te koop staande woningen op 4 oktober 2018") +
  
  #aanpassen bijschriften bij assen
  xlab(NULL) +
  ylab("aantal") +
  
  #ticks op de y-as
  scale_y_continuous(breaks = seq(0, 3000, by = 250)) +
  
  #ander achtergrond theme
  theme_minimal() +
  
  #bijschriften bij x-as draaien
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
  



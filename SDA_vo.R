#Grafische analyse
#categoriale variabele

#Voorbeeld:ginstellingen voor voortgezet onderwijs in Nederland

#website:https://duo.nl/open_onderwijsdata/databestanden/vo/adressen/adressen-vo-1.jsp

#het bestand kan gedownload worden; of vanaf website ingelezen
#url van csv bestand, is een ;-gescheiden bestand
#daarom wordt bestand ingelezen met readr::read_csv2
#read_csv verwacht een ,-gescheiden bestand

library(tidyverse)
library(ggplot2)

url <- "https://duo.nl/open_onderwijsdata/images/01-hoofdvestigingen-vo.csv"
vo <- read_csv2(url)

#enkele standaard checks
class(vo)
  #tbl staat voor 'tibble data format' een iets
  #aangepast data.frame format
str(vo)
head(vo) #eerste 6 rijen
tail(vo) #laatste 6 rijen
summary(vo)

#Aantal zaken vereist aanpassing
#spaties verwijderen uit namen variabelen
#deze kunnen bij analyse problemen geven
#BEVOEGD GEZAG NUMMER is geen numeriek variabele

#make.names() functie maakt namen overenkomstig 
#een conventie voor namen (bijv. geen spaties)
names(vo) <- make.names(names(vo))

#BEVOEGD.GEZAG wijzigen in character variabele
vo$BEVOEGD.GEZAG.NUMMER <- as.character(vo$BEVOEGD.GEZAG.NUMMER)


#barplot aantal scholen per provincie

ggplot(data = vo, aes(x = PROVINCIE)) + 
  geom_bar()

#indien andere volgorde gewenst van de provincies gewenst is
#dan variabele PROVINCIE omzetten naar factor-variabele
#en volgorde levels aangeven

vo$PROVINCIE <- factor(vo$PROVINCIE,
                       levels = c("Groningen", "Friesland", "Drenthe",
                                  "Overijssel", "Gelderland",
                                  "Flevoland", "Utrecht",
                                  "Zuid-Holland", "Noord-Holland",
                                  "Noord-Brabant", "Zeeland",
                                  "Limburg"))
ggplot(data = vo, aes(x = PROVINCIE)) +
  geom_bar(fill = "skyblue")

#bijschrift bij de assen beter leesbaar maken
ggplot(data = vo, aes(x = PROVINCIE)) +
  geom_bar(fill = "skyblue") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

#staven ordenen van hoogste naar laagste

hulpdf <- data.frame(table(vo$PROVINCIE)) %>%
             arrange(-Freq)

vo$PROVINCIE <- factor(vo$PROVINCIE, levels = hulpdf$Var1)

ggplot(data = vo, aes(x = PROVINCIE)) +
  geom_bar(fill = "lightblue") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

#OPGAVE
#(1) Maak een staafdiagram van de aantallen per DENOMINATIE
#(2) Maak een staafdiagram van de aantallen per ONDERWIJSSTRUCTUUR


#Uitsplitsen aantal scholen per denominatie naar PROVINCIE

ggplot(data = vo, aes(x = DENOMINATIE)) +
  geom_bar(fill = "lightblue") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  facet_grid(PROVINCIE~.)

#let op verschil met onderstaande
#waarbij de schaalindeling van de y-as niet voor elke PROVINCIE hetzelfde
#hoeft te zijn

ggplot(data = vo, aes(x = DENOMINATIE)) +
  geom_bar(fill = "lightblue") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  facet_grid(PROVINCIE~., scales = "free_y")

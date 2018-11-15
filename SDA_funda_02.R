#### STANDAARD GRAFIEKEN IN R: HISTOGRAMMEN EN BOXPLOTS####

library(readr)   #read_csv() functie
library(ggplot2)
library(scales)   #nodig om wetenschappelijke notatie om te zetten

#inlezen bestand
funda <- read_csv("datafiles/2018-10-04_tekoop_funda.csv")


##### NUMERIEKE VARIABELE: HISTOGRAM #####

#variabele : VRGPRIJS
#histogram verdeling vraagprijzen
ggplot(data = funda, aes(x = VRGPRIJS)) +
  geom_histogram()

#varieren met aantal klassen
ggplot(data = funda, aes(x = VRGPRIJS)) +
  geom_histogram(fill = "blue", bins = 100) +
  
  #wetenschappelijke notatie bij x-as labels vervangen
  scale_x_continuous(label = comma)


#inzoomen op woningen met vraagprijs onder 1 mln
ggplot(data = funda, aes(x = VRGPRIJS)) +
  geom_histogram(fill = "orange", bins = 30) +
  xlim(c(0, 1000000))

## OPGAVE
#  Bestudeer de verdeling van vraagprijzen onder 1 mln
#  door te varieren met aantal bins
ggplot(data = funda, aes(x = VRGPRIJS)) +
  geom_histogram(fill = "orange", bins = 10) +
  xlim(c(0, 1000000))

ggplot(data = funda, aes(x = VRGPRIJS)) +
  geom_histogram(fill = "orange", bins = 300) +
  xlim(c(0, 1000000))



#Vergelijken vraagprijzen per plaats

ggplot(data = funda, aes(x = VRGPRIJS)) +
  geom_histogram(fill = "blue", bins = 100) +
  facet_grid(PLAATS~.) +
  #wetenschappelijke notatie bij x-as labels vervangen
  scale_x_continuous(label = comma)

#in bovenstaande grafiek wordt voor elke plaats 
#dezelfde schaalverdeling op de y-as gebruikt
#aantallen per plaats verschillen nogal
#optie is om de schaal op de y-as vrij te laten

ggplot(data = funda, aes(x = VRGPRIJS)) +
  geom_histogram(fill = "blue", bins = 50) +
  facet_grid(PLAATS~., scales = 'free_y') +
  #wetenschappelijke notatie bij x-as labels vervangen
  scale_x_continuous(label = comma, limits = c(0, 1000000))



##### NUMERIEKE VARIABELE: BOXPLOT #####

ggplot(data = funda, aes(x = PLAATS, y = VRGPRIJS)) +
  geom_boxplot(fill = "blue")

###OPGAVE genereer boxplots per plaats van woningen
#         met vraagprijs tot 1 mln
#         en draai de plaatsnamen bij de horizontale as 45 graden

ggplot(data = funda, aes(x = PLAATS, y = VRGPRIJS)) +
  ylim(c(0, 1000000))+
  geom_boxplot(fill = "blue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))








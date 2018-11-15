#Maak een nieuw script in R.
#Lees het funda bestand opnieuw in.
library(readr)   #read_csv() functie
library(ggplot2)
library(scales)   #nodig om wetenschappelijke notatie om te zetten

#inlezen bestand
funda <- read_csv("datafiles/2018-10-04_tekoop_funda.csv")

#(i) Maak een scatterplot waarin de oppervlakte wordt uitgezet tegen de vraagprijs.
ggplot(data = funda, aes(x = funda$OPP, y= funda$VRGPRIJS)) +
  geom_point()

#(ii) Gebruik facet_grid() om voor elke van de tien plaatsen dit scatterplot te genereren.
ggplot(data = funda, aes(x = funda$OPP, y= funda$VRGPRIJS)) +
  geom_point() + facet_grid(PLAATS~.)

#(iii) Genereer een scatterplot met op de x-as het aantal kamers en op de y-as de vraagprijs.
ggplot(data = funda, aes(x = funda$KAMERS, y= funda$VRGPRIJS)) +
  geom_point()


#(iv) Genereer een scatterplot met op de x-as het aantal kamers en op de y-as de oppervlakte.
ggplot(data = funda, aes(x = funda$KAMERS, y= funda$OPP)) +
  geom_point()


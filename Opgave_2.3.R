#Maak een nieuw script in R.
#Lees het funda bestand opnieuw in.
library(readr)   #read_csv() functie
library(ggplot2)
library(scales)   #nodig om wetenschappelijke notatie om te zetten

#inlezen bestand
funda <- read_csv("datafiles/2018-10-04_tekoop_funda.csv")

#aantal te koop staande huizen
count(funda)
#minimum vraagprijs
min(funda$VRGPRIJS)
#mediaan vraagprijs
median(funda$VRGPRIJS)
#maximum vraagprijs
max(funda$VRGPRIJS)
#gemiddeld vraagprijs
mean(funda$VRGPRIJS)
#standaarddeviatie vraagprijs
sd(funda$VRGPRIJS)
#percentage van de tekoop staande woningen waarvan de vraagprijs 1 mln euro of meer bedraagt
count(filter(funda, funda$VRGPRIJS >= 1000000))/count(funda)


summary(funda$VRGPRIJS)


ggplot(data = funda, aes(y=funda$VRGPRIJS)) +
  geom_boxplot()

#bar plots
#op basis van reeds getelde aantallen

library(tidyverse)
#aantal ingeschrevenen hbo instellingen

url <- "https://duo.nl/open_onderwijsdata/images/01.ingeschrevenen-hbo-2017.csv"
hbo <- read_csv2(url)

#bestand inspecteren
class(hbo)
str(hbo)
head(hbo)
tail(hbo)

hbo.1 <- hbo[1:3361,] #laatste vier rijen verwijderen
#namen probleem wordt later opgelost in dit script

summary(hbo.1)

hbo.17.1 <- hbo.1 %>%
  select(CROHO = `CROHO ONDERDEEL`,
         MAN2016 = `2016 MAN`, VROUW2016 = `2016 VROUW`) %>%
  gather(SEXE, AANTAL, MAN2016, VROUW2016) %>%
  group_by(CROHO, SEXE) %>%
  summarize(AANT = sum(AANTAL))

hbo.17 <- hbo.1 %>%
  select(PROVINCIE, INSTNAAM = `INSTELLINGSNAAM ACTUEEL`,
         OPLNAAM = `OPLEIDINGSNAAM ACTUEEL`, CROHO = `CROHO ONDERDEEL`,
         MAN2016 = `2016 MAN`, VROUW2016 = `2016 VROUW`) %>%
  #namen aangepast aan gebruikelijke conventies, geen spaties,
  #niet beginnen met cijfer
  mutate(INS2016 = MAN2016 + VROUW2016) %>% #nieuwe variabele, totaal ingeschreven
  group_by(PROVINCIE) %>%
    summarize(TOTAAL = sum(INS2016))


#bar plot naar CROHO en GESLACHT

ggplot(hbo.17.1, aes(x = CROHO, y = AANT, fill = SEXE)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggplot(hbo.17.1, aes(x = CROHO, y = AANT, fill = SEXE)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

#kruistabel met de data
hbo.17.kruistab <- hbo.17.1 %>% 
  spread(SEXE, AANT)


# maatstaf voor samenhang tussen twee categoriale variabelen
# Cramers V
require(questionr)
cramer.v(hbo.17.kruistab[, 2:3])

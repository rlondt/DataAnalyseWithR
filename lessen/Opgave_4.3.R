# OPGAVE 4.3
# In een onderzoek naar verkeersongevallen in Nederland komt de vraag aan de orde of in Den Haag in het weekend 
# relatief minder ongelukken gebeuren dan buiten het weekend. De onderzoeksvraag is o.a. geoperationaliseerd in 
# de te toetsen hypothese: de proportie door de politie geregistreerde ongevallen in Den Haag die in het weekend 
# plaatsvonden is kleiner dan 2/7.
# (i) Waarom wordt getoetst of de proportie kleiner is dan 2/7 (en niet een andere waarde)?
# weekend is 2 van de 7 dagen.


# (ii) Toets de opgestelde hypothese. Maak voor het toetsen van deze hypothese gebruik van het 
#   bestand met geregistreerde ongevallen in 2006 (zie ook handout 3, opgave 3.3)

df_ongevallen <- download_and_unzip(2006)

gemeentes <- distinct(df_ongevallen, GME_NAAM )

df_ongevallen_denhaag <- filter(df_ongevallen, df_ongevallen$GME_NAAM=='S Gravenhage')



df_ongevallen %>%
  filter( GME_NAAM == 'S Gravenhage') %>%
  group_by( DAGTYPE) %>%
  summarise(count = n())

# 1 MA-VR    2355
# 2 ZA-ZO     733


n <- 2355+733 #totaal aantal ongelukken 
p0 <- 2/7 #de kans op juiste voorspelling als H0 juist is (2/7) (aantal ongelukken gelijk)
k <- 733 #aantal ongelukken in het weekend

proptoets <- prop.test(x = k, n = n, p = p0, alternative = "greater")
proptoets$p.value # = 0.00326 H0 is verworpen ==> weekend is gevaarlijker

# (iii) Toets dezelfde hypothese voor een andere regio naar keuze.

distinct(df_ongevallen, PLT_NAAM )


df_ongevallen %>%
  filter( PLT_NAAM == 'REGIO FRIESLAND                         ') %>%
  group_by( DAGTYPE) %>%
  summarise(count = n())

n <- 3253+960 #totaal aantal ongelukken 
p0 <- 2/7     #de kans op juiste voorspelling als H0 juist is (2/7) (aantal ongelukken gelijk)
k <- 960      #aantal ongelukken in het weekend

proptoets <- prop.test(x = k, n = n, p = p0, alternative = "greater")
proptoets$p.value # = 1  H0 is NIET verworpen


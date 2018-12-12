# OPGAVE 4.2
# De toets die in paragraaf 4 is gebruikt, is een zogenaamde proportietoets (of toets op een proportie). 
# In R kan een dergelijke toets worden uitgevoerd met de functie prop.test.
# Neem aan dat de proefpersoon van 10 kaarten er 8 goed voorspelt.
# De vraag is dan of daarmee de bewering dat de proefpersoon voorspellende gaven heeft wordt ondersteund, 
# of dat dit resultaat uit toeval te verklaren is. Dat wil zeggen dat de kans op zo’n resultaat voor een persoon die 
# geen voorspellende gaven heeft groot is, althans groter dan de afgesproken alpha waarde (veelal wordt hiervoor .05 gekozen). 
# De kans dat een persoon zonder voorspellende gaven een vergelijkbaar resultaat behaalt wordt de p-value van de toets genoemd.
# Bij 8 van de 10 goed voorspelde kaarten bedraagt de p-value 0.057; code zie hierna. Omdat dit

n <- 10 #aantal keer dat kaart voorspeld wordt
p0 <- .50 #de kans op juiste voorspelling als H0 juist is
k <- 8 #aantal juist voorspelde kaarten

proptoets <- prop.test(x = k, n = n, p = p0, alternative = "greater")
proptoets$p.value # = 0.057 H0 niet verworpen

# Stel dat het experiment 100 keer herhaald wordt en dat de proefpersoon 60 keer juist voorspelt. 
# Geeft dat statistische ondersteuning voor de bewering dat de kans dat de proefpersoon juist voorspelt groter is dan .50?
#

n <- 100 #aantal keer dat kaart voorspeld wordt
p0 <- .50 #de kans op juiste voorspelling als H0 juist is
k <- 60 #aantal juist voorspelde kaarten

proptoets <- prop.test(x = k, n = n, p = p0, alternative = "greater")
proptoets$p.value # = 0.0287 H0 verworpen



# Stel een proefpersoon beweert dat de kans dat hij de kleur van een kaart goed voorspelt groter is dan .75. 
# In een experiment voorspelt hij van 150 kaarten de kleur van 115 kaarten correct. Ondersteunt dit resultaat zijn bewering. 
# Met andere woorden: is dit een statistisch significant resultaat?
# de claim dat de dobbelsteen oneerlijk is, wordt ondersteund door de data....

n <- 150 #aantal keer dat kaart voorspeld wordt
p0 <- .75 #de kans op juiste voorspelling als H0 juist is
k <- 115 #aantal juist voorspelde kaarten

proptoets <- prop.test(x = k, n = n, p = p0, alternative = "greater")
proptoets$p.value # = 0.353 H0 niet verworpen

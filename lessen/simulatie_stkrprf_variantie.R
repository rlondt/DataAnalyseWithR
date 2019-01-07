#simulatie steekproeftrekking
#berekening variantie

library(ggplot2)

#definieer functies voor berekening variantie van een populatie
#geen standaardfunctie in R hiervoor aanwezig
#var() berekend steekproefvariantie (dus met n-1 in de noemer)

pop.var <- function(x){
  sum((x - mean(x))^2)/length(x)
}


#set.seed i.v.m. reproduceerbaarheid
set.seed(20171123)

#1000000 random getallen genereren met normale verdeling
populatie <- rnorm(n = 1000000, mean = 1000, sd = 10)  #variantie = 10^2 = 100
mean(populatie)
pop.var(populatie)

#steekproef trekken (n = 10)
stkprf.getallen <- sample(1:1000000, size = 10)
#skprf.getallen: de volgnummers van de steekproefelementen
stkprf.pop <- populatie[stkprf.getallen] #de eigenlijke steekproef
pop.var(stkprf.pop) #variantie gebaseerd op formule met n in de noemer
#deze laatste drie regels meerdere keren herhalen
#om meerdere steekproeven te trekken
#dit geeft inzicht in de mogelijke steekproefvarianties
#als de populatie verdeling bekend is
#herhalen kan met een for-next-loop in R
#zie hieronder

#met for next-loop groot aantal steekproeven trekken
#steekproefvarianties in een vector plaatsen
stkprf.var <- NULL #initialisatie; vector om resultaten vast te leggen

for (i in 1:1000) {
  stkprf.getallen <- sample(1:1000000, size = 10) 
  stkprf.pop <- populatie[stkprf.getallen]
  v <- pop.var(stkprf.pop)
  stkprf.var <- c(stkprf.var, v)
}

#steekproefvarianties in dataframe
#om ggplot te kunnen gebruiken
df <- data.frame(STKPRF_VAR = stkprf.var)
ggplot(df, aes(x = STKPRF_VAR)) +
  geom_histogram(fill = "gray", col = "darkgray",
                 binwidth = 5)

#gemiddelde waarden van berekende varianties
mean(stkprf.var)


###zelfde procedure, maar nu met n-1 in de noemer
#van de formule voor de steekproefvariantie


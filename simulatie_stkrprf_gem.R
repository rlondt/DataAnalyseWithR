#simulatie steekproeftrekking
library(ggplot2)
#set.seed i.v.m. reproduceerbaarheid
set.seed(20171123)

#1000000 random getallen genereren met normale verdeling
populatie <- rnorm(n = 1000000, mean = 1000, sd = 50)
populatie <- runif(n = 1000000, min = 0, max = 1000)
mean(populatie)
sd(populatie)

populatie <- arrange(populatie)

# hoofdstelling statistiek
# steekproefverdelingen zijn normaal verdeeld. 
# spreiding wordt wortel de wortel uit het aantal samples kleiner bij sampling

#steekproef trekken (n = 10)
?sample
stkprf.getallen <- sample(1:1000000, size = 10)
#skprf.getallen: de volgnummers van de steekproefelementen
stkprf.pop <- populatie[stkprf.getallen] #de eigenlijke steekproef
mean(stkprf.pop) #het steekproefgemiddelde
#deze laatste drie regels meerdere keren herhalen
#om meerdere steekproeven te trekken
#dit geeft inzicht in de mogelijke steekproefgemiddelden
#als de populatie verdeling bekend is
#herhalen kan met een for-next-loop in R
#zie hieronder

#met for next-loop groot aantal steekproeven trekken
#steekproefgemiddelden in een vector plaatsen
stkprf.mean <- NULL #initialisatie; vector om resultaten vast te leggen

for (i in 1:1000) {
  stkprf.getallen <- sample(1:1000000, size = 10) 
  stkprf.pop <- populatie[stkprf.getallen]
  m <- mean(stkprf.pop)
  stkprf.mean <- c(stkprf.mean, m)
}

#het gemiddelde van alle steekproefgemiddelden
mean(stkprf.mean)
#standaarddeviatie van alle steekproefgemiddelden
sd(stkprf.mean)

#steekproefgemiddelden in dataframe
#om ggplot te kunnen gebruiken
df <- data.frame(STKPRFGEM = stkprf.mean)
ggplot(df, aes(x = STKPRFGEM)) +
  geom_histogram(fill = "gray", col = "darkgray",
                 binwidth = 5)


#grafiek met waarden in parent-populatie
#en grafiek met steekproefgemiddelden

parent <- data.frame(X = "PARENTPOP", VALUE = populatie)
stkprven <- data.frame(X = "STKPRFGEM", VALUE = df$STKPRFGEM)

geheel <- rbind(parent, stkprven)

ggplot(geheel, aes(x=VALUE)) +
  geom_histogram(fill = "gray", col = "darkgray",
                 binwidth = 0.5) +
  facet_grid(X~., scales = "free_y") +
  theme_minimal()


library(tidyverse)
df_kt <- read_csv("datafiles/df_kenteken.csv")
df_br <- read_csv("datafiles/df_brandstof.csv")
df <- left_join(df_kt, df_br)


#Maak een scatterplot met langs de x-as de catalogusprijs en langs de y-as de bruto_bpm (let op, check eerst of deze zijn ingelezen als getal).

class(df$bruto_bpm)
class(df$catalogusprijs)

ggplot(df, aes(catalogusprijs, bruto_bpm)) +
  geom_point()


#Maak de scatter nogmaals, kleur de punten in de scatter op basis van de variabele brandstof_omschrijving.
df.prepared <- mutate(df, brandstof_omschrijving_factor = as.factor(brandstof_omschrijving))

ggplot(df.prepared, aes(catalogusprijs, bruto_bpm, color = df.prepared$brandstof_omschrijving_factor)) +
  geom_point( shape = as.factor(df.prepared$merk ) )



#Genereer een samenvattend overzicht per merk en soort brandstof de gemiddelde bruto_bpm.
# ggplot(df.prepared, aes(catalogusprijs, bruto_bpm, color = df.prepared$brandstof_omschrijving_factor)) +
#   geom_point( shape = df.prepared$brandstof_omschrijving_factor ) +
#   plot_grid ()
df.summ <- group_by(df, merk, brandstof_omschrijving) %>%
summarise(gemiddelde_bpm = mean(bruto_bpm, na.rm = TRUE))


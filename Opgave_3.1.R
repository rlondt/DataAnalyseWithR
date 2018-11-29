# Opgave 3.1

# Gebruik onderstaande code om het bestand NLD_adm1.shp in te lezen, de geometry kolom van dit bestand bevat polygons op provincieniveau.
library(tidyverse)
library(sf)
nld.1 <- st_read("datafiles/NLD_adm/NLD_adm1.shp")
# Bekijk het dataframe, en de informatie in de kolommen.
# 
# Plot een grafiek:
ggplot(data = nld.1) +
       geom_sf()
# 
# Kleur de kaart in:

nld.prov <- filter(nld.1, TYPE_1 == "Provincie")

ggplot(data = nld.prov) +
       geom_sf(data = nld.prov$geometry) + aes(fill = nld.prov$NAME_1)

# 
# Zoek op internet de bevolkingsdichtheid voor elk van de provincies op en voeg deze toe aan het bestand met behulp:
# nld.prov$bev_dh <- c(.., .., ..., ..)
# 
nld.prov$bev_dh <- c(185
                      ,285
                      ,193
                      ,409
                      ,250
                      ,519
                      ,507
                      ,1039
                      ,344
                      ,916
                      ,213
                      ,1282)




nld.prov

# Genereer een barplot van de bevolkingsdichtheden per provincie (zoals figuur 1.)
# 
ggplot(data = nld.prov, aes(x=nld.prov$NAME_1, y=nld.prov$bev_dh)) + 
  geom_bar(stat = "identity")

# Produceer een kaart van Nederland waarbij deze nieuwe variabele bev_dh wordt gebruikt als waarde voor argument fill:
# ggplot(data = nld.prov) +
#   geom_sf(fill = nld.prov$bev_dh) +
#   scale_fill_brewer(palette = "Blues")
ggplot(data = nld.prov) +
  geom_sf(fill = nld.prov$bev_dh) 

# http://oscarperpinan.github.io/bookvis/spatial.html
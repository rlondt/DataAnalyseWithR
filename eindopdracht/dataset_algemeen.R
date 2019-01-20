packages <- c("tidyverse", "ggpubr", "ggplot2", "lubridate", "gridExtra", "NbClust", "cluster", "scales", "sf")
# Installeer packages
for (p in packages) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p, repos = 'http://cran.us.r-project.org')
  }
}
# Laad the packages
for (p in packages){
  suppressPackageStartupMessages(
    library(p, quietly = TRUE, character.only = TRUE ) 
  )
}

#laad data

df_totaal <- read_rds('./datafiles/ongevallen-totaal.rds')

df_summary <- df_totaal %>%
  mutate(jaar_fact = as.factor(year(DATUM))
         , jaar_int = as.integer(year(DATUM))) %>%
  group_by(jaar_fact, jaar_int ) %>%
  summarise( aantal = n_distinct(VKL_NUMMER)
           , aantal_partijen = sum(ANTL_PTJ)
           , aantal_unieke_partijen = n_distinct(PTJ_ID))

df_summary2 <- df_summary %>%
  ungroup()%>%
  summarise(gem_aantal = mean(aantal)
            , sd_aantal = sd(aantal)
            , gem_partijen = mean(aantal_partijen)
            , sd_partijen = sd(aantal_partijen)
            , gem_unieke_partijen = mean(aantal_unieke_partijen)
            , sd_unieke_partijen = sd(aantal_unieke_partijen)
  )

ggplot(data =df_summary, aes(x=jaar_int)) +
  geom_line(stat = "identity", aes(y=aantal, color=as.factor("Aantal"))) + 
  geom_line(stat = "identity", aes(y=aantal_partijen, color=as.factor("Aantal partijen"))) + 
  geom_line(stat = "identity", aes(y=aantal_unieke_partijen, color=as.factor("Aantal unieke partijen")))+
  geom_hline(yintercept = df_summary2$gem_aantal, color="red" , linetype="dashed")+
  geom_hline(yintercept = df_summary2$gem_partijen, color="green" , linetype="dashed")+
  geom_hline(yintercept = df_summary2$gem_unieke_partijen, color="blue" , linetype="dashed")+
  labs(color="Karakteristiek")+  
  xlab(label = "Jaartal")



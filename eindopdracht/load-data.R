source('./eindopdracht/data-preparation.R')

df <- download_bron(c(2005:2017),
                   "192.168.56.128",
                   "1521",
                   "XE",
                   "mba",
                   "mba")

write_rds(df, "./datafiles/ongevallen-totaal.rds")


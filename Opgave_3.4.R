


#downloaden bestand

download_and_unzip <- function(download_url){
  opgave.bestandsnaam <- sapply(str_split(download_url, '/'),tail,1)
  opgave.bestandsnaam_compleet <- paste('./datafiles/',sep = '',opgave.bestandsnaam)
  opgave.uitpakdirectory <- str_replace(sapply(str_split(opgave.bestandsnaam, pattern = '\\.'), head, 1), pattern = '_', replacement = ' - ')
  opgave.uitpakdirectory_compleet <- paste('./datafiles/',sep = '',opgave.uitpakdirectory)
  opgave.ongevallen <- paste('./datafiles/', opgave.uitpakdirectory, sep = '', '/02 TOTNL J-N-J-N/Ongevallengegevens/ongevallen.txt')
  
  if (!file.exists(opgave.bestandsnaam_compleet)){
    download.file(url= download_url, destfile = paste('./datafiles/',sep = '',opgave.bestandsnaam))
  }
  if (!file.exists(opgave.uitpakdirectory_compleet)){
    unzip(opgave.bestandsnaam_compleet, exdir = './datafiles/' )  
  }
  
  opgave.df.ongevallen <- read_delim(file = opgave.ongevallen, delim = ',')
  
  opgave.df.ongevallen <- opgave.df.ongevallen %>%
    mutate(datum = dmy(paste('01', MND_NUMMER, JAAR_VKL, sep = '/')))
    
  
  kolommen <-
  c( "VKL_NUMMER" ,"REGNUMMER"  ,"PVOPGEM"    ,"*DATUM_VKL"  ,"*DAG_CODE"   ,"*MND_NUMMER" ,"*JAAR_VKL"   ,"*TIJDSTIP"   ,"*UUR"        ,"*DDL_ID"     ,"AP3_CODE"   ,"*AP4_CODE"   ,"*AP5_CODE"   ,"*ANTL_SLA"  
   , "*ANTL_DOD"   ,"*ANTL_GZH"   ,"*ANTL_SEH"   ,"*ANTL_GOV"   ,"ANTL_PTJ"   ,"*ANTL_TDT"   ,"*MNE_CODE"   ,"AOL_ID"     ,"NIVEAUKOP"  ,"WSE_ID"     ,"WSE_AN"     ,"BEBKOM"     ,"MAXSNELHD"  ,"WVL_ID"    
   , "WVG_ID"     ,"WVG_AN"     ,"WDK_ID"     ,"WDK_AN"     ,"LGD_ID"     ,"ZAD_ID"     ,"WGD_CODE_1" ,"WGD_CODE_2" ,"BZD_ID_VM1" ,"BZD_ID_VM2" ,"BZD_ID_VM3" ,"BZD_VM_AN"  ,"BZD_ID_IF1" ,"BZD_ID_IF2"
   , "BZD_ID_IF3" ,"BZD_IF_AN"  ,"BZD_ID_TA1" ,"BZD_ID_TA2" ,"BZD_ID_TA3" ,"BZD_TA_AN"  ,"JTE_ID"     ,"WVK_ID"     ,"HECTOMETER" ,"FK_VELD5"   ,"HUISNUMMER" ,"GME_ID"     ,"GME_NAAM"   ,"PVE_CODE"  
   , "PVE_NAAM"   ,"KDD_NAAM"   ,"PLT_NAAM"   ,"DIENSTCODE" ,"DIENSTNAAM" ,"DISTRCODE"  ,"DISTRNAAM"  ,"DAGTYPE"    ,"datum"     
   )
  
  opgave.df.ongevallen <- opgave.df.ongevallen[ , (colnames(opgave.df.ongevallen) %in% kolommen)]
  
  return(opgave.df.ongevallen)
}



df.ongevallen_2017 <- download_and_unzip('https://www.rijkswaterstaat.nl/apps/geoservices/geodata/dmc/bron/01-01-2017_31-12-2017.zip')
df.ongevallen_2006 <- download_and_unzip('https://www.rijkswaterstaat.nl/apps/geoservices/geodata/dmc/bron/01-01-2006_31-12-2006.zip')
# download_and_unzip('https://www.rijkswaterstaat.nl/apps/geoservices/geodata/dmc/bron/01-01-2016_31-12-2016.zip')

# Provincie df.ongevallen_2017$PVE_NAAM


# opgave.directory_naam <- sapply(str_split(opgave.bestandsnaam, '.'),tail,1)
# 
# opgave.bestandsnaam
# str_replace(sapply(str_split(opgave.bestandsnaam, pattern = '\\.'), head, 1), pattern = '_', replacement = ' - ')


kolommen <- colnames(df.ongevallen_2017)


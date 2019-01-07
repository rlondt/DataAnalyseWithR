#install.packages('elastic')
#install.packages('futile.logger')

library(elastic)
library(tidyverse)
library(dplyr)
library(lubridate)
library(futile.logger)



zoekGemeente <- function (p_naam, p_provincie, p_df_gemeentes){ 
  # flog.info(str(p_naam))
  # # flog.info(as.character(p_naam))
  # flog.info(str(p_provincie))
  # # flog.info(as.character(p_provincie))
  
  gemeente <- filter(p_df_gemeentes, NAME_2 == as.character(p_naam))  
  result <- as.character(p_naam)
  if (nrow(gemeente)){
    gemeentes_binnen_provincie <- filter(p_df_gemeentes, NAME_1 == p_provincie) %>%
      select(NAME_2)
    aantal_gemeentes_binnen_provincie <- nrow(gemeentes_binnen_provincie)  
    result <- as.character(gemeentes_binnen_provincie$NAME_2[
      
      
      pmin(aantal_gemeentes_binnen_provincie, floor(runif(1)*aantal_gemeentes_binnen_provincie)+1)])
  }
  # flog.info(result)
  return(result)
}



#downloaden bestand

download_and_unzip <- function(p_jaar, p_push_to_elastic = FALSE){
  opgave.url <- paste ('https://www.rijkswaterstaat.nl/apps/geoservices/geodata/dmc/bron/01-01-',p_jaar, '_31-12-', p_jaar, '.zip', sep = '')
  
  opgave.bestandsnaam <- sapply(str_split(opgave.url, '/'),tail,1)
  opgave.bestandsnaam_compleet <- paste('./datafiles/',sep = '',opgave.bestandsnaam)
  opgave.uitpakdirectory <- str_replace(sapply(str_split(opgave.bestandsnaam, pattern = '\\.'), head, 1), pattern = '_', replacement = ' - ')
  opgave.uitpakdirectory_compleet <- paste('./datafiles/',sep = '',opgave.uitpakdirectory)
  opgave.ongevallen <- paste('./datafiles/', opgave.uitpakdirectory, sep = '', '/02 TOTNL J-N-J-N/Ongevallengegevens/ongevallen.txt')
  opgave.partijen <- paste('./datafiles/', opgave.uitpakdirectory, sep = '', '/02 TOTNL J-N-J-N/Ongevallengegevens/partijen.txt')
  opgave.partijaanvullingen <- paste('./datafiles/', opgave.uitpakdirectory, sep = '', '/02 TOTNL J-N-J-N/Ongevallengegevens/partijaanvullingen.txt')
  
  if (!file.exists(opgave.bestandsnaam_compleet)){
    download.file(url= opgave.url, destfile = paste('./datafiles/',sep = '',opgave.bestandsnaam))
  }
  if (!file.exists(opgave.uitpakdirectory_compleet)){
    unzip(opgave.bestandsnaam_compleet, exdir = './datafiles/' )  
  }
  
  opgave.df.ongevallen <- read_delim(file = opgave.ongevallen, delim = ',')
  flog.info('aantal records %s', count(opgave.df.ongevallen))
  opgave.df.ongevallen <- opgave.df.ongevallen %>%
    mutate(datum = dmy(paste('01', MND_NUMMER, JAAR_VKL, sep = '/')))
  
  
  kolommen <-
    c( "VKL_NUMMER" 
       ,"REGNUMMER"  
       ,"PVOPGEM"    
       ,"*DATUM_VKL"  
       ,"*DAG_CODE"   
       ,"*MND_NUMMER" 
       ,"*JAAR_VKL"   
       ,"*TIJDSTIP"
       ,"UUR" 
       ,"*DDL_ID"
       ,"AP3_CODE"
       ,"*AP4_CODE"   
       ,"*AP5_CODE"
       ,"*ANTL_SLA"  
       ,"*ANTL_DOD"  
       ,"*ANTL_GZH"
       ,"*ANTL_SEH"  
       ,"*ANTL_GOV"  
       ,"ANTL_PTJ" 
       ,"*ANTL_TDT"  
       ,"*MNE_CODE" 
       ,"AOL_ID"     
       ,"NIVEAUKOP" 
       ,"WSE_ID"   
       ,"WSE_AN"   
       ,"BEBKOM"   
       ,"MAXSNELHD"
       ,"WVL_ID"    
       ,"WVG_ID"     
       ,"WVG_AN"     
       ,"WDK_ID"    
       ,"WDK_AN"     
       ,"LGD_ID"     
       ,"ZAD_ID"     
       ,"WGD_CODE_1" 
       ,"WGD_CODE_2" 
       ,"BZD_ID_VM1" 
       ,"BZD_ID_VM2" 
       ,"BZD_ID_VM3"
       ,"BZD_VM_AN"  
       ,"BZD_ID_IF1" 
       ,"BZD_ID_IF2"
       ,"BZD_ID_IF3" 
       ,"BZD_IF_AN" 
       ,"BZD_ID_TA1"
       ,"BZD_ID_TA2"
       ,"BZD_ID_TA3"
       ,"BZD_TA_AN"
       ,"JTE_ID"   
       ,"WVK_ID"   
       ,"HECTOMETER"
       ,"FK_VELD5"   
       ,"HUISNUMMER" 
       ,"GME_ID"     
       ,"GME_NAAM"   
       ,"PVE_CODE"  
       ,"PVE_NAAM"  
       ,"KDD_NAAM" 
       ,"PLT_NAAM" 
       ,"DIENSTCODE"
       ,"DIENSTNAAM"
       ,"DISTRCODE" 
       ,"DISTRNAAM" 
       ,"DAGTYPE"   
       ,"datum"     
    )
  
  opgave.df.ongevallen <- opgave.df.ongevallen[ , (colnames(opgave.df.ongevallen) %in% kolommen)]
  
  opgave.aangrijppunten     <- paste('./datafiles/', opgave.uitpakdirectory, sep = '', '/02 TOTNL J-N-J-N/ReferentiebestandenOngevallen/aangrijppunten.txt');
  opgave.aardongevallen     <- paste('./datafiles/', opgave.uitpakdirectory, sep = '', '/02 TOTNL J-N-J-N/ReferentiebestandenOngevallen/aardongevallen.txt');
  opgave.aflopen3           <- paste('./datafiles/', opgave.uitpakdirectory, sep = '', '/02 TOTNL J-N-J-N/ReferentiebestandenOngevallen/aflopen3.txt');
  opgave.aflopen4           <- paste('./datafiles/', opgave.uitpakdirectory, sep = '', '/02 TOTNL J-N-J-N/ReferentiebestandenOngevallen/aflopen4.txt');
  opgave.aflopen5           <- paste('./datafiles/', opgave.uitpakdirectory, sep = '', '/02 TOTNL J-N-J-N/ReferentiebestandenOngevallen/aflopen5.txt');
  opgave.bewegingen         <- paste('./datafiles/', opgave.uitpakdirectory, sep = '', '/02 TOTNL J-N-J-N/ReferentiebestandenOngevallen/bewegingen.txt');
  opgave.bijzonderheden     <- paste('./datafiles/', opgave.uitpakdirectory, sep = '', '/02 TOTNL J-N-J-N/ReferentiebestandenOngevallen/bijzonderheden.txt');
  opgave.dagdelen           <- paste('./datafiles/', opgave.uitpakdirectory, sep = '', '/02 TOTNL J-N-J-N/ReferentiebestandenOngevallen/dagdelen.txt');
  opgave.dagen              <- paste('./datafiles/', opgave.uitpakdirectory, sep = '', '/02 TOTNL J-N-J-N/ReferentiebestandenOngevallen/dagen.txt');
  #opgave.Definitie          <- paste('./datafiles/', opgave.uitpakdirectory, sep = '', '/02 TOTNL J-N-J-N/ReferentiebestandenOngevallen/Definitie.txt');
  opgave.inrichtingen       <- paste('./datafiles/', opgave.uitpakdirectory, sep = '', '/02 TOTNL J-N-J-N/ReferentiebestandenOngevallen/inrichtingen.txt');
  opgave.leeftijdsklassen   <- paste('./datafiles/', opgave.uitpakdirectory, sep = '', '/02 TOTNL J-N-J-N/ReferentiebestandenOngevallen/leeftijdsklassen.txt');
  opgave.lichtgesteldheden  <- paste('./datafiles/', opgave.uitpakdirectory, sep = '', '/02 TOTNL J-N-J-N/ReferentiebestandenOngevallen/lichtgesteldheden.txt');
  opgave.maanden            <- paste('./datafiles/', opgave.uitpakdirectory, sep = '', '/02 TOTNL J-N-J-N/ReferentiebestandenOngevallen/maanden.txt');
  opgave.manoeuvres         <- paste('./datafiles/', opgave.uitpakdirectory, sep = '', '/02 TOTNL J-N-J-N/ReferentiebestandenOngevallen/manoeuvres.txt');
  opgave.nationaliteiten    <- paste('./datafiles/', opgave.uitpakdirectory, sep = '', '/02 TOTNL J-N-J-N/ReferentiebestandenOngevallen/nationaliteiten.txt');
  opgave.objecttypes        <- paste('./datafiles/', opgave.uitpakdirectory, sep = '', '/02 TOTNL J-N-J-N/ReferentiebestandenOngevallen/objecttypes.txt');
  opgave.toedrachten        <- paste('./datafiles/', opgave.uitpakdirectory, sep = '', '/02 TOTNL J-N-J-N/ReferentiebestandenOngevallen/toedrachten.txt');
  opgave.wegdekken          <- paste('./datafiles/', opgave.uitpakdirectory, sep = '', '/02 TOTNL J-N-J-N/ReferentiebestandenOngevallen/wegdekken.txt');
  opgave.wegsituaties       <- paste('./datafiles/', opgave.uitpakdirectory, sep = '', '/02 TOTNL J-N-J-N/ReferentiebestandenOngevallen/wegsituaties.txt');
  opgave.wegverhardingen    <- paste('./datafiles/', opgave.uitpakdirectory, sep = '', '/02 TOTNL J-N-J-N/ReferentiebestandenOngevallen/wegverhardingen.txt');
  opgave.wegverlichtingen   <- paste('./datafiles/', opgave.uitpakdirectory, sep = '', '/02 TOTNL J-N-J-N/ReferentiebestandenOngevallen/wegverlichtingen.txt');
  opgave.zichtafstanden     <- paste('./datafiles/', opgave.uitpakdirectory, sep = '', '/02 TOTNL J-N-J-N/ReferentiebestandenOngevallen/zichtafstanden.txt');
  opgave.ziekenhuizen       <- paste('./datafiles/', opgave.uitpakdirectory, sep = '', '/02 TOTNL J-N-J-N/ReferentiebestandenOngevallen/ziekenhuizen.txt');
  
  # foute bestanden in 2014 wegpoetsen
  if (p_jaar == 2014){
    # werken met 2015
    p_jaar <- 2015
    opgave.url <- paste ('https://www.rijkswaterstaat.nl/apps/geoservices/geodata/dmc/bron/01-01-',p_jaar, '_31-12-', p_jaar, '.zip', sep = '')
    opgave.bestandsnaam <- sapply(str_split(opgave.url, '/'),tail,1)
    opgave.bestandsnaam_compleet <- paste('./datafiles/',sep = '',opgave.bestandsnaam)
    opgave.uitpakdirectory <- str_replace(sapply(str_split(opgave.bestandsnaam, pattern = '\\.'), head, 1), pattern = '_', replacement = ' - ')
    opgave.uitpakdirectory_compleet <- paste('./datafiles/',sep = '',opgave.uitpakdirectory)
    
    # bepalen vervangers
    opgave.ongevallen         <- paste('./datafiles/', opgave.uitpakdirectory, sep = '', '/02 TOTNL J-N-J-N/Ongevallengegevens/ongevallen.txt')
    opgave.inrichtingen       <- paste('./datafiles/', opgave.uitpakdirectory, sep = '', '/02 TOTNL J-N-J-N/ReferentiebestandenOngevallen/inrichtingen.txt');
    opgave.ziekenhuizen       <- paste('./datafiles/', opgave.uitpakdirectory, sep = '', '/02 TOTNL J-N-J-N/ReferentiebestandenOngevallen/ziekenhuizen.txt');
    opgave.bijzonderheden     <- paste('./datafiles/', opgave.uitpakdirectory, sep = '', '/02 TOTNL J-N-J-N/ReferentiebestandenOngevallen/bijzonderheden.txt');
    opgave.nationaliteiten    <- paste('./datafiles/', opgave.uitpakdirectory, sep = '', '/02 TOTNL J-N-J-N/ReferentiebestandenOngevallen/nationaliteiten.txt');
    opgave.partijaanvullingen <- paste('./datafiles/', opgave.uitpakdirectory, sep = '', '/02 TOTNL J-N-J-N/Ongevallengegevens/partijaanvullingen.txt')
    
    # terugzetten orignele waarden
    p_jaar <- 2014
    opgave.url <- paste ('https://www.rijkswaterstaat.nl/apps/geoservices/geodata/dmc/bron/01-01-',p_jaar, '_31-12-', p_jaar, '.zip', sep = '')
    opgave.bestandsnaam <- sapply(str_split(opgave.url, '/'),tail,1)
    opgave.bestandsnaam_compleet <- paste('./datafiles/',sep = '',opgave.bestandsnaam)
    opgave.uitpakdirectory <- str_replace(sapply(str_split(opgave.bestandsnaam, pattern = '\\.'), head, 1), pattern = '_', replacement = ' - ')
    opgave.uitpakdirectory_compleet <- paste('./datafiles/',sep = '',opgave.uitpakdirectory)
    opgave.ongevallen <- paste('./datafiles/', opgave.uitpakdirectory, sep = '', '/02 TOTNL J-N-J-N/Ongevallengegevens/ongevallen.txt')
  }
  
  
  
  opgave.df.aangrijppunten    <- read_delim(file = opgave.aangrijppunten    , delim = ',')
  opgave.df.aardongevallen    <- read_delim(file = opgave.aardongevallen    , delim = ',')
  opgave.df.aflopen3          <- read_delim(file = opgave.aflopen3          , delim = ',')
  opgave.df.aflopen4          <- read_delim(file = opgave.aflopen4          , delim = ',')
  opgave.df.aflopen5          <- read_delim(file = opgave.aflopen5          , delim = ',')
  opgave.df.bewegingen        <- read_delim(file = opgave.bewegingen        , delim = ',')
  opgave.df.dagdelen          <- read_delim(file = opgave.dagdelen          , delim = ',')
  opgave.df.dagen             <- read_delim(file = opgave.dagen             , delim = ',')
  opgave.df.leeftijdsklassen  <- read_delim(file = opgave.leeftijdsklassen  , delim = ',')
  opgave.df.lichtgesteldheden <- read_delim(file = opgave.lichtgesteldheden , delim = ',')
  opgave.df.maanden           <- read_delim(file = opgave.maanden           , delim = ',')
  opgave.df.manoeuvres        <- read_delim(file = opgave.manoeuvres        , delim = ',')
  opgave.df.objecttypes       <- read_delim(file = opgave.objecttypes       , delim = ',')
  opgave.df.toedrachten       <- read_delim(file = opgave.toedrachten       , delim = ',')
  opgave.df.wegdekken         <- read_delim(file = opgave.wegdekken         , delim = ',')
  opgave.df.wegsituaties      <- read_delim(file = opgave.wegsituaties      , delim = ',')
  opgave.df.wegverhardingen   <- read_delim(file = opgave.wegverhardingen   , delim = ',')
  opgave.df.wegverlichtingen  <- read_delim(file = opgave.wegverlichtingen  , delim = ',')
  opgave.df.zichtafstanden    <- read_delim(file = opgave.zichtafstanden    , delim = ',')
  opgave.df.inrichtingen      <- read_delim(file = opgave.inrichtingen      , delim = ',')
  opgave.df.ziekenhuizen      <- read_delim(file = opgave.ziekenhuizen      , delim = ',')  
  opgave.df.bijzonderheden    <- read_delim(file = opgave.bijzonderheden    , delim = ',')
  opgave.df.nationaliteiten   <- read_delim(file = opgave.nationaliteiten   , delim = ',')
  opgave.df.partijen          <- read_delim(file = opgave.partijen          , delim = ',')
  opgave.df.partijaanvullingen<- read_delim(file = opgave.partijaanvullingen, delim = ',')
  
    
  # opgave.df.ongevallen <- left_join(opgave.df.ongevallen,  opgave.df.aangrijppunten    )
   opgave.df.ongevallen <- left_join(opgave.df.ongevallen,  opgave.df.aardongevallen    )
   opgave.df.ongevallen <- left_join(opgave.df.ongevallen,  opgave.df.aflopen3          )
  # opgave.df.ongevallen <- left_join(opgave.df.ongevallen,  opgave.df.aflopen4          )
  # opgave.df.ongevallen <- left_join(opgave.df.ongevallen,  opgave.df.aflopen5          )
  # opgave.df.ongevallen <- left_join(opgave.df.ongevallen,  opgave.df.bewegingen        )
   opgave.df.ongevallen <- left_join(opgave.df.ongevallen,  opgave.df.bijzonderheden, c("BZD_ID_VM1"="BZD_ID"))

   if (class(opgave.df.ongevallen$BZD_ID_VM2)!='integer'){
     flog.info('BZD_ID_VM2 != integer voor %s ', p_jaar)
     opgave.df.ongevallen$BZD_ID_VM2 <- as.integer(opgave.df.ongevallen$BZD_ID_VM2)
   }
   opgave.df.ongevallen <- left_join(opgave.df.ongevallen,  opgave.df.bijzonderheden, c("BZD_ID_VM2"="BZD_ID"))
   if (class(opgave.df.ongevallen$BZD_ID_VM3)!='integer'){
     flog.info('BZD_ID_VM3 != integer voor %s ', p_jaar)
     opgave.df.ongevallen$BZD_ID_VM3 <- as.integer(opgave.df.ongevallen$BZD_ID_VM3)
   }
   opgave.df.ongevallen <- left_join(opgave.df.ongevallen,  opgave.df.bijzonderheden, c("BZD_ID_VM3"="BZD_ID"))
   opgave.df.ongevallen <- left_join(opgave.df.ongevallen,  opgave.df.bijzonderheden, c("BZD_ID_IF1"="BZD_ID"))

      
   if (class(opgave.df.ongevallen$BZD_ID_IF2)!='integer'){
     flog.info('BZD_ID_IF2 != integer voor %s ', p_jaar)
     opgave.df.ongevallen$BZD_ID_IF2 <- as.integer(opgave.df.ongevallen$BZD_ID_IF2)
   }
   opgave.df.ongevallen <- left_join(opgave.df.ongevallen,  opgave.df.bijzonderheden, c("BZD_ID_IF2"="BZD_ID"))

   if (class(opgave.df.ongevallen$BZD_ID_IF3)!='integer'){
     flog.info('BZD_ID_IF3 != integer voor %s ', p_jaar)
     opgave.df.ongevallen$BZD_ID_IF3 <- as.integer(opgave.df.ongevallen$BZD_ID_IF3)
   }
   opgave.df.ongevallen <- left_join(opgave.df.ongevallen,  opgave.df.bijzonderheden, c("BZD_ID_IF3"="BZD_ID"))
   opgave.df.ongevallen <- left_join(opgave.df.ongevallen,  opgave.df.bijzonderheden, c("BZD_ID_TA1"="BZD_ID"))
   
   if (class(opgave.df.ongevallen$BZD_ID_TA2)!='integer'){
     flog.info('BZD_ID_TA2 != integer voor %s ', p_jaar)
     opgave.df.ongevallen$BZD_ID_TA2 <- as.integer(opgave.df.ongevallen$BZD_ID_TA2)
   }
   opgave.df.ongevallen <- left_join(opgave.df.ongevallen,  opgave.df.bijzonderheden, c("BZD_ID_TA2"="BZD_ID"))
   if (class(opgave.df.ongevallen$BZD_ID_TA3)!='integer'){
     flog.info('BZD_ID_TA3 != integer voor %s ', p_jaar)
     opgave.df.ongevallen$BZD_ID_TA3 <- as.integer(opgave.df.ongevallen$BZD_ID_TA3)
   }
   opgave.df.ongevallen <- left_join(opgave.df.ongevallen,  opgave.df.bijzonderheden, c("BZD_ID_TA3"="BZD_ID"))
  # opgave.df.ongevallen <- left_join(opgave.df.ongevallen,  opgave.df.dagdelen          )
  # opgave.df.ongevallen <- left_join(opgave.df.ongevallen,  opgave.df.dagen             )
  # opgave.df.ongevallen <- left_join(opgave.df.ongevallen,  opgave.df.Definitie         )
  # opgave.df.ongevallen <- left_join(opgave.df.ongevallen,  opgave.df.inrichtingen      )
  # opgave.df.ongevallen <- left_join(opgave.df.ongevallen,  opgave.df.leeftijdsklassen  )
  # 2011 - as.integer(str(opgave.df.ongevallen$LGD_ID))
   
  
  if (class(opgave.df.ongevallen$LGD_ID)!='integer'){
    flog.info('LDG_ID != integer voor %s ', p_jaar)
    opgave.df.ongevallen$LGD_ID <- as.integer(opgave.df.ongevallen$LGD_ID)
  }
  opgave.df.ongevallen <- left_join(opgave.df.ongevallen,  opgave.df.lichtgesteldheden ) 
   
  # opgave.df.ongevallen <- left_join(opgave.df.ongevallen,  opgave.df.maanden           )
  # opgave.df.ongevallen <- left_join(opgave.df.ongevallen,  opgave.df.manoeuvres        )
  # opgave.df.ongevallen <- left_join(opgave.df.ongevallen,  opgave.df.nationaliteiten   )
  # opgave.df.ongevallen <- left_join(opgave.df.ongevallen,  opgave.df.objecttypes       )
  # opgave.df.ongevallen <- left_join(opgave.df.ongevallen,  opgave.df.toedrachten       )
   opgave.df.ongevallen <- left_join(opgave.df.ongevallen,  opgave.df.wegdekken         )
   opgave.df.ongevallen <- left_join(opgave.df.ongevallen,  opgave.df.wegsituaties      )
   opgave.df.ongevallen <- left_join(opgave.df.ongevallen,  opgave.df.wegverhardingen   )
   opgave.df.ongevallen <- left_join(opgave.df.ongevallen,  opgave.df.wegverlichtingen  )
  # opgave.df.zichtafstanden$ZAD_ID <- as.integer(opgave.df.zichtafstanden$ZAD_ID)
  # opgave.df.ongevallen <- left_join(opgave.df.ongevallen,  opgave.df.zichtafstanden    )
  # opgave.df.ongevallen <- left_join(opgave.df.ongevallen,  opgave.df.ziekenhuizen      )
  
   if(p_push_to_elastic){
     connect("192.168.56.128", 9200)
     docs_bulk(opgave.df.ongevallen, index=paste( 'ongevallen-', p_jaar, sep=''))
   }

   # kolommen die alleen in 2016 en 2017 gebruikt worden verwijderen
   kolommen <- c( "DIENSTCODE", "DIENSTNAAM", "DISTRCODE",  "DISTRNAAM")
   
   opgave.df.ongevallen <- opgave.df.ongevallen[ , !(colnames(opgave.df.ongevallen) %in% kolommen)]   
   
   opgave.df.ongevallen <- opgave.df.ongevallen %>%
     unite("bijzonderheden_ids" , c( BZD_ID_VM1      
                                     , BZD_ID_VM2      
                                     , BZD_ID_VM3      
                                     , BZD_ID_IF1      
                                     , BZD_ID_IF2      
                                     , BZD_ID_IF3      
                                     , BZD_ID_TA1      
                                     , BZD_ID_TA2      
                                     , BZD_ID_TA3      
     ), sep = ",", remove = TRUE)
   
   
   
   opgave.df.ongevallen <- opgave.df.ongevallen %>%
     unite("bijzonderheden_oms" , c( BZD_OMS,
                                     BZD_OMS.x       
                                     , BZD_OMS.x.x     
                                     , BZD_OMS.y       
                                     , BZD_OMS.y.y     
                                     , BZD_OMS.x.x.x   
                                     , BZD_OMS.y.y.y   
                                     , BZD_OMS.x.x.x.x 
                                     , BZD_OMS.y.y.y.y 
     ), sep = ",", remove = TRUE)
   
   opgave.df.partijen <- left_join(opgave.df.partijen, opgave.df.partijaanvullingen)
   opgave.df.partijen <- left_join(opgave.df.partijen, opgave.df.leeftijdsklassen)
   opgave.df.ongevallen <- left_join(opgave.df.ongevallen, opgave.df.partijen)
   
   kolommen <- c(
  #   "VKL_NUMMER"      
  # , "REGNUMMER"       
    "PVOPGEM"         
   , "UUR"             
   , "AP3_CODE"        
   , "ANTL_PTJ"        
   #, "AOL_ID"          
   , "NIVEAUKOP"       
   , "WSE_ID"          
   , "WSE_AN"          
   , "BEBKOM"          
   , "MAXSNELHD"       
   #, "WVL_ID"          
   #, "WVG_ID"          
   , "WVG_AN"          
   #, "WDK_ID"          
   , "WDK_AN"          
   #, "LGD_ID"          
   #, "ZAD_ID"          
   , "WGD_CODE_1"      
   #, "WGD_CODE_2"      
   , "bijzonderheden_ids"
   , "bijzonderheden_oms"
   #, "BZD_ID_VM1"      
   #, "BZD_ID_VM2"      
   #, "BZD_ID_VM3"      
   #, "BZD_ID_IF1"      
   #, "BZD_ID_IF2"      
   #, "BZD_ID_IF3"      
   #, "BZD_ID_TA1"      
   #, "BZD_ID_TA2"      
   #, "BZD_ID_TA3"      
   , "BZD_VM_AN"       
   , "BZD_TA_AN"       
   , "BZD_IF_AN"       
   , "JTE_ID"          
   #, "WVK_ID"          
   #, "HECTOMETER"      
   , "FK_VELD5"        
   #, "HUISNUMMER"      
   #, "GME_ID"          
   , "GME_NAAM"        
   #, "PVE_CODE"        
   , "PVE_NAAM"        
   #, "KDD_NAAM"        
   #, "PLT_NAAM"        
   , "DAGTYPE"         
   , "datum"           
   , "AOL_OMS"         
   , "AP3_OMS"         
   #, "BZD_OMS"         
   #, "BZD_OMS.x"       
   #, "BZD_OMS.x.x"     
   #, "BZD_OMS.y"       
   #, "BZD_OMS.y.y"     
   #, "BZD_OMS.x.x.x"   
   #, "BZD_OMS.y.y.y"   
   #, "BZD_OMS.x.x.x.x" 
   #, "BZD_OMS.y.y.y.y" 
   #, "BZD_TYPE.x.x.x.x"
   #, "BZD_TYPE.x.x.x"  
   #, "BZD_TYPE.y.y"    
   #, "BZD_TYPE.y"      
   #, "BZD_TYPE.x.x"    
   #, "BZD_TYPE.x"      
   #, "BZD_TYPE"        
   #, "BZD_TYPE.y.y.y"  
   #, "BZD_TYPE.y.y.y.y"
   , "LGD_OMS"         
   , "WDK_OMS"         
   , "WSE_OMS"         
   , "WVG_OMS"         
   , "WVL_OMS"         
   #, "PTJ_ID"          
   #, "NUMMER"          
   , "DOORRIJDER"      
   #, "OTE_ID"          
   , "OTE_AN"          
   #, "NTT_CODE_V"      
   , "VTGVERZ"         
   , "SCHADE"          
   , "GETRAANH"        
   , "GEVSTOF"         
   , "VTGVERL"         
   , "ANTL_PAS"        
   #, "GEBDAT"          
   #, "GEBJAAR"         
   #, "LEEFTIJD"        
   #, "LKE_ID"          
   #, "NTT_CODE_B"      
   , "GESLACHT"        
   #, "BLAASTEST"       
   #, "ART8"            
   #, "MEDICGEBR"       
   #, "RIJBEWGEL"       
   , "RIJBEWCAT"       
   #, "RIJBEWBEG"     minimaal gevuld  
   #, "BROMFCERT"      minimaal gevuld 
   #, "UITGPOS1"        
   #, "UITGPOS2"        
   , "UITGPOS_AN"      
   , "VOORGBEW"        
   , "AGT_TYPE"        
   , "AGT_ID_1"        
   #, "AGT_ID_2"        
   , "BWG_ID_1"        
   , "BWG_ID_2"        
   #, "BWG_AN"          
   #, "TDT_ID_1"        
   #, "TDT_ID_2"        
   #, "TDT_ID_3"        
   #, "TDT_AN"          
   , "IRG_CODE"        
   #, "EERTOEDAT"
   , "MASSALEEG"       
   , "BREEDTE"         
   , "LENGTE"          
   , "APKGEK"          
   #, "VERZEK"   minimaal gevuld       
   , "LKE_OMS"         
   #, "g"     
   , "datum_eerste_toelating"
   )
   
   mutate(opgave.df.ongevallen, datum_eerte_toelating = ymd(EERTOEDAT))
   
   opgave.df.ongevallen <- opgave.df.ongevallen[ , (colnames(opgave.df.ongevallen) %in% kolommen)]   
   
   #
   # Toevoegen gemeentenaam
   
   
   
   opgave.df.ongevallen <- opgave.df.ongevallen %>% # Use by_group party_df
     rowwise() %>%
     mutate(
       gemeentenaam = zoekGemeente(GME_NAAM, PVE_NAAM, nld.2)
     )
   
   
   return(opgave.df.ongevallen)
}




vZoekGemeente <- Vectorize(zoekGemeente, vectorize.args = c("p_naam", "p_provincie"))



# df.ongevallen_2006 <- download_and_unzip(2006);
# df.ongevallen_2007 <- download_and_unzip(2007);
# df.ongevallen_2008 <- download_and_unzip(2008);
# df.ongevallen_2009 <- download_and_unzip(2009);
# df.ongevallen_2010 <- download_and_unzip(2010);
# df.ongevallen_2011 <- download_and_unzip(2011);
# df.ongevallen_2012 <- download_and_unzip(2012);
# df.ongevallen_2013 <- download_and_unzip(2013);
# df.ongevallen_2014 <- download_and_unzip(2014);
# df.ongevallen_2015 <- download_and_unzip(2015);
# df.ongevallen_2016 <- download_and_unzip(2016);
# df.ongevallen_2017 <- download_and_unzip(2017);
# 
# 
# # colnames(df.ongevallen_2017)
# #
# # df.ongevallen <- rbind(df.ongevallen_2006, df.ongevallen_2007)
# # df.ongevallen <- rbind(df.ongevallen, df.ongevallen_2008)
# # df.ongevallen <- rbind(df.ongevallen, df.ongevallen_2009)
# # df.ongevallen <- rbind(df.ongevallen, df.ongevallen_2010)
# # df.ongevallen <- rbind(df.ongevallen, df.ongevallen_2011)
# # df.ongevallen <- rbind(df.ongevallen, df.ongevallen_2012)
# # df.ongevallen <- rbind(df.ongevallen, df.ongevallen_2013)
# # df.ongevallen <- rbind(df.ongevallen, df.ongevallen_2014)
# # df.ongevallen <- rbind(df.ongevallen, df.ongevallen_2015)
# # df.ongevallen <- rbind(df.ongevallen, df.ongevallen_2016)
# # df.ongevallen <- rbind(df.ongevallen, df.ongevallen_2017)
# #
# 
# #samenvoegen data alle jaren
# df.ongevallen_list <- list( df.ongevallen_2006
#                           , df.ongevallen_2007
#                           , df.ongevallen_2008
#                           , df.ongevallen_2009
#                           , df.ongevallen_2010
#                           , df.ongevallen_2011
#                           , df.ongevallen_2012
#                           , df.ongevallen_2013
#                           , df.ongevallen_2014
#                           , df.ongevallen_2015
#                           , df.ongevallen_2016
#                           , df.ongevallen_2017
#                           )
# DFlist <- Map(cbind, df.ongevallen_list, g = seq_along(df.ongevallen_list)); df.ongevallen_totaal <- do.call(rbind, DFlist)
# 
# 
# #
# saveRDS(df.ongevallen_totaal, "./datafiles/ongevallen-totaal.rds")
# 
# write_csv(df.ongevallen_totaal, "./datafiles/ongevallen-totaal.csv")
# 
# df.ongevallen_totaal <- readRDS("./datafiles/ongevallen-totaal.rds")
# 
# # 
# # 
# connect("192.168.56.128", 9200)
# docs_bulk(df.ongevallen_totaal, index='ongevallen-totaal')




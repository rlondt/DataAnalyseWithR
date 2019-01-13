#install.packages("RJDBC",dep=TRUE)
#install.packages('elastic')
#install.packages('futile.logger')


packages <- c("elastic"
              ,"tidyverse"
              ,"dplyr"
              ,"sf"
              ,"futile.logger"
              ,"lubridate"
              ,"RJDBC"
              ,"shiny")

## Installeer packages
for (p in packages) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p, repos = 'http://cran.us.r-project.org')
  }
}

## Laad the packages
for (p in packages){
  suppressPackageStartupMessages(
    library(p, quietly = TRUE, character.only = TRUE ) 
  )
}


# Onderstaande functie alleen gebruiken als je tijd over hebt. Performed bar slecht door column-oriented ipv row-oriented
#
# Bij haast database update doen..
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


download_jaar <- function(p_jaar, p_push_to_elastic = FALSE){
  opgave.url <- paste ('https://www.rijkswaterstaat.nl/apps/geoservices/geodata/dmc/bron/01-01-',p_jaar, '_31-12-', p_jaar, '.zip', sep = '')
  
  # initialisatie bestandslocaties
  opgave.bestandsnaam <- sapply(str_split(opgave.url, '/'),tail,1)
  opgave.bestandsnaam_compleet <- paste('./datafiles/',sep = '',opgave.bestandsnaam)
  opgave.uitpakdirectory <- str_replace(sapply(str_split(opgave.bestandsnaam, pattern = '\\.'), head, 1), pattern = '_', replacement = ' - ')
  opgave.uitpakdirectory_compleet <- paste('./datafiles/',sep = '',opgave.uitpakdirectory)
  opgave.ongevallen <- paste('./datafiles/', opgave.uitpakdirectory, sep = '', '/02 TOTNL J-N-J-N/Ongevallengegevens/ongevallen.txt')
  opgave.partijen <- paste('./datafiles/', opgave.uitpakdirectory, sep = '', '/02 TOTNL J-N-J-N/Ongevallengegevens/partijen.txt')
  opgave.partijaanvullingen <- paste('./datafiles/', opgave.uitpakdirectory, sep = '', '/02 TOTNL J-N-J-N/Ongevallengegevens/partijaanvullingen.txt')
  
  # Alleen downloaden indien bestand niet aanwezig
  if (!file.exists(opgave.bestandsnaam_compleet)){
    download.file(url= opgave.url, destfile = paste('./datafiles/',sep = '',opgave.bestandsnaam))
  }
  
  # Alleen uitpakken indien bestand niet aanwezig
  if (!file.exists(opgave.uitpakdirectory_compleet)){
    unzip(opgave.bestandsnaam_compleet, exdir = './datafiles/' )  
  }
  
  # inlezen initiele bestand
  opgave.df.ongevallen <- read_delim(file = opgave.ongevallen, delim = ',')
  flog.info('aantal records %s', count(opgave.df.ongevallen))
  
  # toevoegen van datum
  opgave.df.ongevallen <- opgave.df.ongevallen %>%
    mutate(DATUM = dmy(paste('01', MND_NUMMER, JAAR_VKL, sep = '/')))
  
  # toevoegen van initiele kolommen van bijzonderheden
  opgave.df.ongevallen <- opgave.df.ongevallen %>%
    mutate( BIJZ_100 = 0
            , BIJZ_110 = 0
            , BIJZ_120 = 0
            , BIJZ_130 = 0
            , BIJZ_140 = 0
            , BIJZ_150 = 0
            , BIJZ_160 = 0
            , BIJZ_170 = 0
            , BIJZ_180 = 0
            , BIJZ_200 = 0
            , BIJZ_210 = 0
            , BIJZ_220 = 0
            , BIJZ_230 = 0
            , BIJZ_240 = 0
            , BIJZ_250 = 0
            , BIJZ_260 = 0
            , BIJZ_270 = 0
            , BIJZ_280 = 0
            , BIJZ_290 = 0
            , BIJZ_300 = 0
            , BIJZ_310 = 0
            , BIJZ_320 = 0
            , BIJZ_330 = 0
            , GEMEENTENAAM = ''
    )
  
  # inlezen locaties stambestanden
  opgave.aangrijppunten     <- paste('./datafiles/', opgave.uitpakdirectory, sep = '', '/02 TOTNL J-N-J-N/ReferentiebestandenOngevallen/aangrijppunten.txt');
  opgave.aardongevallen     <- paste('./datafiles/', opgave.uitpakdirectory, sep = '', '/02 TOTNL J-N-J-N/ReferentiebestandenOngevallen/aardongevallen.txt');
  opgave.aflopen3           <- paste('./datafiles/', opgave.uitpakdirectory, sep = '', '/02 TOTNL J-N-J-N/ReferentiebestandenOngevallen/aflopen3.txt');
  opgave.aflopen4           <- paste('./datafiles/', opgave.uitpakdirectory, sep = '', '/02 TOTNL J-N-J-N/ReferentiebestandenOngevallen/aflopen4.txt');
  opgave.aflopen5           <- paste('./datafiles/', opgave.uitpakdirectory, sep = '', '/02 TOTNL J-N-J-N/ReferentiebestandenOngevallen/aflopen5.txt');
  opgave.bewegingen         <- paste('./datafiles/', opgave.uitpakdirectory, sep = '', '/02 TOTNL J-N-J-N/ReferentiebestandenOngevallen/bewegingen.txt');
  opgave.bijzonderheden     <- paste('./datafiles/', opgave.uitpakdirectory, sep = '', '/02 TOTNL J-N-J-N/ReferentiebestandenOngevallen/bijzonderheden.txt');
  opgave.dagdelen           <- paste('./datafiles/', opgave.uitpakdirectory, sep = '', '/02 TOTNL J-N-J-N/ReferentiebestandenOngevallen/dagdelen.txt');
  opgave.dagen              <- paste('./datafiles/', opgave.uitpakdirectory, sep = '', '/02 TOTNL J-N-J-N/ReferentiebestandenOngevallen/dagen.txt');
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
  
  # foute stambestanden in 2014 wegpoetsen met 2015
  if (p_jaar == 2014){
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
  
  
  
  # inlezen stambestanden
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
  
  
  # samenvoegen stambestanden met data-bestand
  opgave.df.ongevallen <- left_join(opgave.df.ongevallen,  opgave.df.aardongevallen    )
  opgave.df.ongevallen <- left_join(opgave.df.ongevallen,  opgave.df.aflopen3          )
  
  if (class(opgave.df.ongevallen$LGD_ID)!='integer'){
    flog.info('LDG_ID != integer voor %s ', p_jaar)
    opgave.df.ongevallen$LGD_ID <- as.integer(opgave.df.ongevallen$LGD_ID)
  }
  opgave.df.ongevallen <- left_join(opgave.df.ongevallen,  opgave.df.lichtgesteldheden ) 
  opgave.df.ongevallen <- left_join(opgave.df.ongevallen,  opgave.df.wegdekken         )
  opgave.df.ongevallen <- left_join(opgave.df.ongevallen,  opgave.df.wegsituaties      )
  opgave.df.ongevallen <- left_join(opgave.df.ongevallen,  opgave.df.wegverhardingen   )
  opgave.df.ongevallen <- left_join(opgave.df.ongevallen,  opgave.df.wegverlichtingen  )
  
  
  # kolommen die alleen in 2016 en 2017 gebruikt worden verwijderen
  kolommen <- c( "DIENSTCODE", "DIENSTNAAM", "DISTRCODE",  "DISTRNAAM")
  opgave.df.ongevallen <- opgave.df.ongevallen[ , !(colnames(opgave.df.ongevallen) %in% kolommen)]   
  
  #partijen als stamdata prepareren
  opgave.df.partijen <- left_join(opgave.df.partijen, opgave.df.partijaanvullingen)
  opgave.df.partijen <- left_join(opgave.df.partijen, opgave.df.leeftijdsklassen)
  
  #partijen als stamdata joinen
  opgave.df.ongevallen <- left_join(opgave.df.ongevallen, opgave.df.partijen)
  
  # datum eerste toelating toevoegen als datum
  opgave.df.ongevallen <- opgave.df.ongevallen %>%
    mutate(DATUM_EERSTE_TOELATING = ymd(EERTOEDAT))
  
  # relevante kolommen selecteren
  kolommen <- c( 
    "PVOPGEM"   
    # sleutel  
    , "VKL_NUMMER"
    , "REGNUMMER"  
    , "PTJ_ID"
    # datum
    , "DATUM"  
    , "DATUM_EERSTE_TOELATING"           
    # factoren
    , "UUR"               # uur van de dag
    #, "AP3_CODE"          # soort schade /UMS materiaal
    , "PVE_NAAM"          # provincie
    , "DAGTYPE"           # weekend/week
    , "BEBKOM"            # bebouwde kom
    , "MAXSNELHD"         # maximum snelheid
    , "WVG_AN"            # wegvlak
    , "WDK_AN"            # wegdek
    , "WGD_CODE_1"        #
    , "AOL_OMS"           # partijen
    , "AP3_OMS"           # type schade
    , "LGD_OMS"           # lichtomstandigheden
    , "WDK_OMS"           # wegdek_omstandigheden
    , "WSE_OMS"           # weg_inrichting
    , "WVG_OMS"           # soort-wegdek
    , "WVL_OMS"           # wegverlichting
    , "DOORRIJDER"        # doorrijder
    , "OTE_AN"            # weghalen te weinig ingevuld
    , "VTGVERZ"           # voertuig verzekerd J/N
    , "SCHADE"            # Schade J/N
    , "GETRAANH"          # ?? weinig 
    , "GEVSTOF"           # gevaarlijke stoffen
    , "VTGVERL"           # voertuigverlichting
    , "ANTL_PAS"          # fout
    , "GESLACHT"          # geslacht
    , "RIJBEWCAT"         # categorie rijbewijzen
    , "UITGPOS_AN"        # uitgpositie
    , "VOORGBEW"          # 1-11
    , "AGT_TYPE"          # A/V
    , "IRG_CODE"          
    , "APKGEK"            # APK gekeurd
    , "LKE_OMS"           # Leeftijds klassificatie
    # Numerieke data
    , "ANTL_PTJ"       
    , "MASSALEEG"         
    , "BREEDTE"           
    , "LENGTE"            
    # bijzonderheden
    , "BZD_ID_VM1"
    , "BZD_ID_VM2"
    , "BZD_ID_VM3"
    , "BZD_ID_IF1"
    , "BZD_ID_IF2"
    , "BZD_ID_IF3"
    , "BZD_ID_TA1"
    , "BZD_ID_TA2"
    , "BZD_ID_TA3"
    , "BIJZ_100"
    , "BIJZ_110"
    , "BIJZ_120"
    , "BIJZ_130"
    , "BIJZ_140"
    , "BIJZ_150"
    , "BIJZ_160"
    , "BIJZ_170"
    , "BIJZ_180"
    , "BIJZ_200"
    , "BIJZ_210"
    , "BIJZ_220"
    , "BIJZ_230"
    , "BIJZ_240"
    , "BIJZ_250"
    , "BIJZ_260"
    , "BIJZ_270"
    , "BIJZ_280"
    , "BIJZ_290"
    , "BIJZ_300"
    , "BIJZ_310"
    , "BIJZ_320"
    , "BIJZ_330"
    # Onduidelijk
    , "BZD_VM_AN"         
    , "BZD_IF_AN"         
    , "BZD_TA_AN"         
    , "AGT_ID_1"          
    , "BWG_ID_1"          
    , "BWG_ID_2"          
    , "NIVEAUKOP"         
    # Locatie
    , "GME_NAAM"
    , "PVE_NAAM"
    , "GEMEENTENAAM"      # Gemeente
  )  
  
  opgave.df.ongevallen <- opgave.df.ongevallen %>%
    mutate(
      WDK_AN = case_when(
                          WDK_AN == "BESMEURD"       ~ "Besmeurd",
                          WDK_AN == "Overige"        ~ "Overig",
                          WDK_AN == "OVERIG"         ~ "Overig",
                          WDK_AN == "OVERIGE"        ~ "Overig",
                          WDK_AN == "overig"         ~ "Overig",
                          WDK_AN == "overige"        ~ "Overig",
                          WDK_AN == "Overige"        ~ "Overig",
                          WDK_AN == "zand"           ~ "Zand",
                          is.na(WDK_AN)              ~ "onbekend",
                          TRUE                       ~  WDK_AN)
      , OTE_AN = case_when(
        OTE_AN == "BOMEN"        ~ "BOOM",
        OTE_AN == "politievrtg"  ~ "POLITIEVOERTUIG",
        OTE_AN == "POLITIEAUTO"  ~ "POLITIEVOERTUIG",
        OTE_AN == "Onbekend"     ~ "ONBEKEND",
        OTE_AN == "POLITIEAUTO"  ~ "POLITIEVOERTUIG",
        OTE_AN == "OVERIGE"      ~ "OVERIG",
        OTE_AN == "rolstoel"     ~ "ROLSTOEL",
        is.na(OTE_AN)              ~ "ONBEKEND",
        TRUE                       ~  OTE_AN)
      , UITGPOS_AN = case_when(
        UITGPOS_AN == "RAILS" ~ "Rails",
        UITGPOS_AN == "busstation" ~ "Busstation",
        UITGPOS_AN == "BUSSTATION" ~ "Busstation",
        UITGPOS_AN == "tramhalte"  ~ "Tramhalte",
        UITGPOS_AN == "Van de weg"  ~ "van de weg",
        UITGPOS_AN == "voetpad"  ~ "Voetpad",
        UITGPOS_AN == "VOETPAD"  ~ "Voetpad",
        TRUE                       ~  UITGPOS_AN)
    ) 
    
    
  
  opgave.df.ongevallen <- opgave.df.ongevallen[ , (colnames(opgave.df.ongevallen) %in% kolommen)]   
  
  # factoren opslaan als factoren
  opgave.df.ongevallen <- data_as_factor(opgave.df.ongevallen)
  
  opgave.df.ongevallen <- opgave.df.ongevallen %>%
    replace_na((list(BZD_ID_VM1     =-1
                     , BZD_ID_VM2    =-1
                     , BZD_ID_VM3    =-1
                     , BZD_ID_IF1    =-1
                     , BZD_ID_IF2    =-1
                     , BZD_ID_IF3    =-1
                     , BZD_ID_TA1    =-1
                     , BZD_ID_TA2    =-1
                     , AGT_ID_1      =-1
                     , BWG_ID_1      =-1
                     , BWG_ID_2      =-1
                     , ANTL_PTJ      =-1
                     , MASSALEEG     =-1
                     , BREEDTE       =-1
                     , LENGTE        =-1
                     , BZD_ID_TA3    =-1)))
  

  #
  # Toevoegen gemeentenaam als je tijd teveel hebt...
  # opgave.df.ongevallen <- opgave.df.ongevallen %>% # Use by_group party_df
  #   rowwise() %>%
  #   mutate(
  #     gemeentenaam = zoekGemeente(GME_NAAM, PVE_NAAM, nld.2)
  #   )
  # 
  
  # naar elastic search
  if(p_push_to_elastic){
    connect("192.168.56.128", 9200)
    docs_bulk(opgave.df.ongevallen, index=paste( 'ongevallen-', p_jaar, sep=''))
  }
  
  return(opgave.df.ongevallen)
}


download_bron <- function(p_jaartallen, p_db_host, p_db_port, p_db_sid, p_db_user, p_db_password){
  # optie voor jdbc driver (vergroten beschikbare geheugen)
  options(java.parameters = "-Xmx12g")
  
  # Connectie naar database toevoegen
  drv <- JDBC("oracle.jdbc.OracleDriver", "/home/rstudio/jdbc/ojdbc8.jar", identifier.quote = "'")
  conn <- dbConnect(drv, paste("jdbc:oracle:thin:@", p_db_host, ":", p_db_port, "/", p_db_sid , sep = ""), p_db_user, p_db_password)
  
  
  i<-1
  for (jaartal in p_jaartallen){
    l_rds_bestand <-paste('./datafiles/ongevallen-',sep = '',jaartal, '.rds')
    df <- download_jaar(jaartal)
    flog.debug(str(df))
    write_rds(df, path = l_rds_bestand)
    if (i==1){
      i <- 2
      dbWriteTable(conn, "MBA_ONGEVALLEN", df, overwrite=TRUE)
    } else {
      dbWriteTable(conn, "MBA_ONGEVALLEN", df, append=TRUE, overwrite=FALSE)
    }
  }
  

  # tellen bijzonderheden (flattening) 
  dbSendUpdate(conn, "update mba_ongevallen
set bijz_100 = case when 100 in (bzd_id_vm1, bzd_id_vm2, bzd_id_vm3, bzd_id_if1, bzd_id_if2, bzd_id_if3, bzd_id_ta1, bzd_id_ta2, bzd_id_ta3) then 1 else 0 end
,  bijz_110 = case when 110 in (bzd_id_vm1, bzd_id_vm2, bzd_id_vm3, bzd_id_if1, bzd_id_if2, bzd_id_if3, bzd_id_ta1, bzd_id_ta2, bzd_id_ta3) then 1 else 0 end
,  bijz_120 = case when 120 in (bzd_id_vm1, bzd_id_vm2, bzd_id_vm3, bzd_id_if1, bzd_id_if2, bzd_id_if3, bzd_id_ta1, bzd_id_ta2, bzd_id_ta3) then 1 else 0 end
,  bijz_130 = case when 130 in (bzd_id_vm1, bzd_id_vm2, bzd_id_vm3, bzd_id_if1, bzd_id_if2, bzd_id_if3, bzd_id_ta1, bzd_id_ta2, bzd_id_ta3) then 1 else 0 end
,  bijz_140 = case when 140 in (bzd_id_vm1, bzd_id_vm2, bzd_id_vm3, bzd_id_if1, bzd_id_if2, bzd_id_if3, bzd_id_ta1, bzd_id_ta2, bzd_id_ta3) then 1 else 0 end
,  bijz_150 = case when 150 in (bzd_id_vm1, bzd_id_vm2, bzd_id_vm3, bzd_id_if1, bzd_id_if2, bzd_id_if3, bzd_id_ta1, bzd_id_ta2, bzd_id_ta3) then 1 else 0 end
,  bijz_160 = case when 160 in (bzd_id_vm1, bzd_id_vm2, bzd_id_vm3, bzd_id_if1, bzd_id_if2, bzd_id_if3, bzd_id_ta1, bzd_id_ta2, bzd_id_ta3) then 1 else 0 end
,  bijz_170 = case when 170 in (bzd_id_vm1, bzd_id_vm2, bzd_id_vm3, bzd_id_if1, bzd_id_if2, bzd_id_if3, bzd_id_ta1, bzd_id_ta2, bzd_id_ta3) then 1 else 0 end
,  bijz_180 = case when 180 in (bzd_id_vm1, bzd_id_vm2, bzd_id_vm3, bzd_id_if1, bzd_id_if2, bzd_id_if3, bzd_id_ta1, bzd_id_ta2, bzd_id_ta3) then 1 else 0 end
,  bijz_200 = case when 200 in (bzd_id_vm1, bzd_id_vm2, bzd_id_vm3, bzd_id_if1, bzd_id_if2, bzd_id_if3, bzd_id_ta1, bzd_id_ta2, bzd_id_ta3) then 1 else 0 end
,  bijz_210 = case when 210 in (bzd_id_vm1, bzd_id_vm2, bzd_id_vm3, bzd_id_if1, bzd_id_if2, bzd_id_if3, bzd_id_ta1, bzd_id_ta2, bzd_id_ta3) then 1 else 0 end
,  bijz_220 = case when 220 in (bzd_id_vm1, bzd_id_vm2, bzd_id_vm3, bzd_id_if1, bzd_id_if2, bzd_id_if3, bzd_id_ta1, bzd_id_ta2, bzd_id_ta3) then 1 else 0 end
,  bijz_230 = case when 230 in (bzd_id_vm1, bzd_id_vm2, bzd_id_vm3, bzd_id_if1, bzd_id_if2, bzd_id_if3, bzd_id_ta1, bzd_id_ta2, bzd_id_ta3) then 1 else 0 end
,  bijz_240 = case when 240 in (bzd_id_vm1, bzd_id_vm2, bzd_id_vm3, bzd_id_if1, bzd_id_if2, bzd_id_if3, bzd_id_ta1, bzd_id_ta2, bzd_id_ta3) then 1 else 0 end
,  bijz_250 = case when 250 in (bzd_id_vm1, bzd_id_vm2, bzd_id_vm3, bzd_id_if1, bzd_id_if2, bzd_id_if3, bzd_id_ta1, bzd_id_ta2, bzd_id_ta3) then 1 else 0 end
,  bijz_260 = case when 260 in (bzd_id_vm1, bzd_id_vm2, bzd_id_vm3, bzd_id_if1, bzd_id_if2, bzd_id_if3, bzd_id_ta1, bzd_id_ta2, bzd_id_ta3) then 1 else 0 end
,  bijz_270 = case when 270 in (bzd_id_vm1, bzd_id_vm2, bzd_id_vm3, bzd_id_if1, bzd_id_if2, bzd_id_if3, bzd_id_ta1, bzd_id_ta2, bzd_id_ta3) then 1 else 0 end
,  bijz_280 = case when 280 in (bzd_id_vm1, bzd_id_vm2, bzd_id_vm3, bzd_id_if1, bzd_id_if2, bzd_id_if3, bzd_id_ta1, bzd_id_ta2, bzd_id_ta3) then 1 else 0 end
,  bijz_300 = case when 300 in (bzd_id_vm1, bzd_id_vm2, bzd_id_vm3, bzd_id_if1, bzd_id_if2, bzd_id_if3, bzd_id_ta1, bzd_id_ta2, bzd_id_ta3) then 1 else 0 end
,  bijz_310 = case when 310 in (bzd_id_vm1, bzd_id_vm2, bzd_id_vm3, bzd_id_if1, bzd_id_if2, bzd_id_if3, bzd_id_ta1, bzd_id_ta2, bzd_id_ta3) then 1 else 0 end
,  bijz_320 = case when 320 in (bzd_id_vm1, bzd_id_vm2, bzd_id_vm3, bzd_id_if1, bzd_id_if2, bzd_id_if3, bzd_id_ta1, bzd_id_ta2, bzd_id_ta3) then 1 else 0 end
,  bijz_330 = case when 330 in (bzd_id_vm1, bzd_id_vm2, bzd_id_vm3, bzd_id_if1, bzd_id_if2, bzd_id_if3, bzd_id_ta1, bzd_id_ta2, bzd_id_ta3) then 1 else 0 end
")
  
  # bijwerken gemeentenaam 
  nld.2 <- st_read("datafiles/NLD_adm/NLD_adm2.shp")
  nld.2 <- filter(nld.2, TYPE_2 == 'Gemeente')
  
  dbWriteTable(conn, "MBA_GEMEENTES", st_set_geometry(nld.2, NULL), overwrite=TRUE)
  
  # eerst de gemeentes die wel gevonden kunnen worden
  dbSendUpdate(conn,"update mba_ongevallen 
    set gemeentenaam = gme_naam
    where (GME_NAAM, PVE_NAAM) in (select name_2, name_1 from mba_gemeentes)")

  # index aanmaken om het bijwerken te versnellen  
  try(dbExecute(conn, "create index mba_1 on mba_ongevallen(vkl_nummer)"))
  
  # ontbrekende gemeentenamen toewijzen door te loopen over bestaande gemeentenamen 
  dbSendUpdate(conn, "declare 
    cursor c_ongevallen
    is
      select distinct vkl_nummer, pve_naam 
      from   mba_ongevallen
      where  gemeentenaam is null
      order by pve_naam
    ;
    TYPE gemeentenamen_t IS TABLE OF mba_gemeentes.name_2%TYPE
                         INDEX BY PLS_INTEGER; 
    l_gemeentenaam_t     gemeentenamen_t;
    l_pve_naam varchar2(200):='Dummy';
    l_teller   number;
    l_mod      number;
  begin
    for r_ongevallen in c_ongevallen
    loop
      if l_pve_naam != r_ongevallen.pve_naam
      then
        select name_2 bulk collect into l_gemeentenaam_t
        from   mba_gemeentes
        where  name_1 = r_ongevallen.pve_naam;
        l_pve_naam := r_ongevallen.pve_naam;
        l_teller := 1;
      end if;
      l_mod := l_gemeentenaam_t.count;
      update mba_ongevallen 
      set    gemeentenaam = l_gemeentenaam_t(mod(l_teller, l_mod)+1)
      where  vkl_nummer = r_ongevallen.vkl_nummer;
      l_teller := l_teller +1;
  end loop;
  end;
  ")
  
  
  # uitlezen bewerkte dataset in dataframe
  df <- dbReadTable(conn, "MBA_ONGEVALLEN")
  
  # factoren als factoren
  df <- data_as_factor(df)
  
  # NA's weer terugzetten
  df <- df %>%
    replace(.==-1, NA)
  
  # Datums weer date maken
  df <- mutate(df, DATUM = ymd(DATUM))
  df <- mutate(df, DATUM_EERSTE_TOELATING = ymd(DATUM_EERSTE_TOELATING))
  
  return (df)
}


data_as_factor <- function(df){
  factoren <- c(
    "UUR"               # uur van de dag
    #, "AP3_CODE"          # soort schade /UMS materiaal
    , "PVE_NAAM"          # provincie
    , "DAGTYPE"           # weekend/week
    , "BEBKOM"            # bebouwde kom
    , "MAXSNELHD"         # maximum snelheid
    , "WVG_AN"            # wegvlak
    , "WDK_AN"            # wegdek
    , "WGD_CODE_1"        #
    , "AOL_OMS"           # partijen
    , "AP3_OMS"           # type schade
    , "LGD_OMS"           # lichtomstandigheden
    , "WDK_OMS"           # wegdek_omstandigheden
    , "WSE_OMS"           # weg_inrichting
    , "WVG_OMS"           # soort-wegdek
    , "WVL_OMS"           # wegverlichting
    , "DOORRIJDER"        # doorrijder
    , "OTE_AN"            # weghalen te weinig ingevuld
    , "VTGVERZ"           # voertuig verzekerd J/N
    , "SCHADE"            # Schade J/N
    , "GETRAANH"          # ?? weinig 
    , "GEVSTOF"           # gevaarlijke stoffen
    , "VTGVERL"           # voertuigverlichting
    , "ANTL_PAS"          # fout
    , "GESLACHT"          # geslacht
    , "RIJBEWCAT"         # categorie rijbewijzen
    , "UITGPOS_AN"        # uitgpositie
    , "VOORGBEW"          # 1-11
    , "AGT_TYPE"          # A/V
    , "IRG_CODE"          
    , "APKGEK"            # APK gekeurd
    , "LKE_OMS"           # Leeftijds klassificatie
    , "GME_NAAM"
    , "PVE_NAAM"
    , "GEMEENTENAAM"      # Gemeente
    , "NIVEAUKOP"
  )
  
  df <- as.data.frame(df)
  for (l_factor in factoren){
    flog.debug(str(l_factor))
    df[,l_factor] = as.factor(df[,l_factor])
  }
  
  return(df)
}
# Data-analyse met R

## Introductie

Deze repository is gemaakt als onderdeel van de cursus Data-analyse met R. Het bevat de gebruikte scripts die ondersteunend zijn geweest voor het onderzoek. 
In de rest van deze README zullen alle gebruikte bestanden en directories die voor het paper gebruikt zijn worden toegelicht.
Van belang hierbij is dat de scripts gedraaid dienen te worden in een omgeving waar een Oracle Database beschikbaar is en dat het script <a href="./eindopdracht/load-data.R">eindopdracht/load-data.R</a> voor de script die rapportage doen gedraaid moet hebben.

## Bestanden / directories 
* directory <a href="./datafiles">datafiles</a>; deze directory bevat alle data-bestanden die gedownload en uitgepakt worden 
* directory <a href="./eindopdracht">eindopdracht</a>; deze directory bevat alle scripts die voor de eindopdracht gemaakt zijn
* <a href="./eindopdracht/data-preparation.R">eindopdracht/data-preparation.R</a>; dit script bevat alle functies om de data te downloaden en te bewerken
    * data_as_factoren; een functie die de verschillende kolommen uit het bestand omzet als factor
    * download_bron; downloaden van het totaalbestand parameters zijn onder andere een vector voor de jaartallen die gedownload dienen te worden en een connectie naar een Oracle-Database waar verdere verwerking plaats vindt.
    * download_jaar; deze functie zorgt voor het downloaden, uitpakken en koppelen van de verschillende bestanden van de BRON-dataset
    * zoekGemeente; deze functie zoekt op basis van een provincie en een gemeente of deze gemeente ook echt bestaat indien deze bestaat wordt deze teruggegeven; indien de gemeente niet bestaat wordt een willekeurige andere gemeente teruggegeven.
* <a href="./eindopdracht/load-data.R">eindopdracht/load-data.R</a>; dit script roept de functies van <a href="./eindopdracht/data-preparation.R">eindopdracht/data-preparation.R</a> aan en zorgt dus voor het prepareren van de data. Dit script zal eerst gedraaid moeten hebben voordat er over wordt gegaan tot rapportage. Het zorgt namelijk voor het aanmaken van het <a href="./datafiles/ongevallen-totaal.rds">ongevallen-totaal.rds</a> bestand
* <a href="./eindopdracht/ShinyApp.R">eindopdracht/ShinyApp.R</a>; dit script bevat de ShyniApp. Dit is een webapplicatie die de data eenvoudig kan ontsluiten. Hiervoor dient wel eerst het load-data.R script gedraaid te hebben.
* <a href="./eindopdracht/dataset_algemeen.R">eindopdracht/dataset_algemeen.R</a>; dit script bevat de tabellen en afbeeldingen die in  het hoofdstuk "Gebruikte Data" in het paper terecht zijn gekomen.
* <a href="./eindopdracht/Presentatie-Totaal.Rpres">eindopdracht/Presentatie-Totaal.Rpres</a>; dit script bevat de eind-presentatie van behorende bij deze opdracht als ook de code die hoort bij het genereren van de afbeeldingen in het hoofdstuk "Data-analyse"
* <a href="./eindopdracht/Presentatie-Totaal.md">eindopdracht/Presentatie-Totaal.md</a>; dit bestand bevat de presentatie in markdown formaat. 
* directory <a href="./eindopdracht/Presentatie-Totaal-figure">eindopdracht/Presentatie-Totaal-figure</a> bevat alle afbeeldingen die in het paper en de presentatie zijn verwerkt



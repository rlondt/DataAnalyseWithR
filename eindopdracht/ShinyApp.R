#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(tidyverse)
library(lubridate)

if(!exists("df_totaal")){
  df_totaal <- read_rds('../datafiles/ongevallen-totaal.rds')
}

choiceVec <- c(
  "uur van de dag" = "UUR"               # uur van de dag
  , "provincie" = "PVE_NAAM"          # provincie
  , "weekend/week" = "DAGTYPE"           # weekend/week
  , "bebouwde kom" = "BEBKOM"            # bebouwde kom
  , "maximum snelheid" = "MAXSNELHD"         # maximum snelheid
  , "wegvlak" = "WVG_AN"            # wegvlak
  , "wegdek" = "WDK_AN"            # wegdek
  , "partijen" = "AOL_OMS"           # partijen
  , "type schade" = "AP3_OMS"           # type schade
  , "lichtomstandigheden" = "LGD_OMS"           # lichtomstandigheden
  , "wegdek_omstandigheden" = "WDK_OMS"           # wegdek_omstandigheden
  , "weg_inrichting" = "WSE_OMS"           # weg_inrichting
  , "soort-wegdek" = "WVG_OMS"           # soort-wegdek
  , "wegverlichting" = "WVL_OMS"           # wegverlichting
  , "doorrijder" = "DOORRIJDER"        # doorrijder
  , "weghalen te weinig ingevuld" = "OTE_AN"            # weghalen te weinig ingevuld
  , "voertuig verzekerd J/N" = "VTGVERZ"           # voertuig verzekerd J/N
  , "Schade J/N" = "SCHADE"            # Schade J/N
  , "?? weinig " = "GETRAANH"          # ?? weinig 
  , "gevaarlijke stoffen" = "GEVSTOF"           # gevaarlijke stoffen
  , "voertuigverlichting" = "VTGVERL"           # voertuigverlichting
  #, "fout" = "ANTL_PAS"          # fout
  , "geslacht" = "GESLACHT"          # geslacht
  , "categorie rijbewijzen" = "RIJBEWCAT"         # categorie rijbewijzen
  , "uitgpositie" = "UITGPOS_AN"        # uitgpositie
  , "1-11??" = "VOORGBEW"          # 1-11
  , "Achter/Voor??" = "AGT_TYPE"          # A/V
  , "APK gekeurd?" = "APKGEK"            # APK gekeurd
  , "Leeftijds klassificatie" = "LKE_OMS"           # Leeftijds klassificatie
  , "WGD_CODE_1"        #
  , "IRG_CODE"          
  , "NIVEAUKOP"
  #, "AP3_CODE"          # soort schade /UMS materiaal
)

set.seed(2019)
df_training <- df_totaal %>%
  ungroup() %>%
 # filter(BEBKOM=="BI" & MAXSNELHD == "30") %>%
  filter(as.integer(year(DATUM)) >= 2012) %>%
  group_by(year(DATUM)) %>%
  sample_frac(.4)

dfFiltered <- df_training



# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Verkeersongevallen Analyse Data"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(width = "120px",
        # actionButton("go", "Go"),
        # sliderInput("jaarRange",
        #             "Selecteer jaartallen:",
        #             min = 2006,
        #             max = 2017,
        #             value = c(2013, 2017)
        # ),
        selectInput("factor",
                     label="Kies een factor",
                     choices = choiceVec)
        
      ),
      mainPanel(width = 120,
                fillPage(
                  plotOutput("distPlot")
                )
        # textOutput("to1"),
        #tableOutput("voaTabel")
      )
    )
  )


# Define server logic required to draw a histogram
server <- function(input, output) {

    filterData <- eventReactive(input$go, {
      dfFiltered <- df_training %>%
        filter(
          year(DATUM) >= input$jaarRange[1] && year(DATUM) <= input$jaarRange[2]
        )
      return(dfFiltered)
    })
      
      
    output$distPlot <- renderCachedPlot({
      df <- dfFiltered
      
      ggplot(df, aes(x=df$DATUM, colour=df[[input$factor]])) + 
      geom_line(stat = "count"
        #col=df$NIVEAUKOP, 
        # fill="green", 
        #alpha = .2
        ) + 
      labs(title=paste("Histogram aantallen ongevallen - ", names(choiceVec)[choiceVec == input$factor] )) +
      labs(color=names(choiceVec)[choiceVec == input$factor]) +
      labs(x="Jaartal", y="Aantal * 0.4")+
        facet_wrap(~PVE_NAAM)
    },
    cacheKeyExpr = { list(input$factor) }
    )
  
  # output$to1 <- renderText({
  #   paste("Jaartallen ", input$jaarRange[1], "-", input$jaarRange[2])
  # })
  
  # output$voaTabel <- renderTable({
  #   filterData()[1:100,]
  #   
  # })
}

# Run the application 
shinyApp(ui = ui, server = server)


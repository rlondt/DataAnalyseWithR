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

set.seed(2019)
df_training <- df_totaal %>%
  filter(BEBKOM=="BI" & MAXSNELHD == "30") %>%
  group_by(year(DATUM)) %>%
  sample_frac(.4)

dfFiltered <- df_training



# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Verkeersongevallen Analyse Data"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        actionButton("go", "Go"),
        sliderInput("jaarRange",
                    "Selecteer jaartallen:",
                    min = 2006,
                    max = 2017,
                    value = c(2013, 2017)
        ),
        selectInput("factor",
                     label="Kies een factor",
                     choices = c("NIVEAUKOP"
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
                     ))
        
      ),
      mainPanel(
        textOutput("to1"),
        plotOutput("distPlot"),
        tableOutput("voaTabel")
        
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
      
      
    output$distPlot <- renderPlot({
      df <- filterData()
      
      ggplot(df, aes(x=df$DATUM, colour=df[[input$factor]])) + 
      geom_line(stat = "count",
        #col=df$NIVEAUKOP, 
        # fill="green", 
        alpha = .2) + 
      labs(title=paste("Histogram aantallen ongevallen - ",input$factor )) +
      labs(x="Datum", y="Count")+
        facet_wrap(~PVE_NAAM)
    })
  
  output$to1 <- renderText({
    paste("Jaartallen ", input$jaarRange[1], "-", input$jaarRange[2])
  })
  
  output$voaTabel <- renderTable({
    filterData()[1:100,]
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


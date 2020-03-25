############################################################################################
# OpenITI NgramReader+ @DEV_VERSION@ v.2020.1 ##############################################
############################################################################################

############################################################################################
# 0.0 Required libraries ###################################################################
############################################################################################

library(shiny)
library(shinythemes)
library(data.table)
library(plotly)
require(markdown)
library(stringr)
library(tidyverse)

############################################################################################
# 0.1 Loading Data #########################################################################
############################################################################################

#setwd("/Users/romanovienna/_ADHFANIS_Data/NGRAMS/OpenITI_NgramReaderPlus_DEV_GITHUB/") # for debugging

ngramsNew <- readRDS("./data/ngrams_1x5_step25.rds")
allFreqsNew <- ngramsNew %>% select(-ngram,-freq) %>% colSums()
periods <- as.numeric(names(allFreqsNew))

ngrams <- ngramsNew
allFreqs <- allFreqsNew

############################################################################################
# 0.2 Main functions #######################################################################
############################################################################################

filterData <- function(freqTable, searchVars){
  tempData = freqTable[1,]
  tempData = tempData[-1,]
  for (v in searchVars){
    if (v != ""){
      item = v
      item = gsub("^#", "", item)
      item = gsub(" ?#", "|", item)
      item = paste0("^(", item, ")$")
      subSet = subset(freqTable, grepl(item, ngram))
      tempData = rbind(tempData, subSet)}
  }
  return(tempData)
}

#filterData(freqTable, searchVars)
spanVar = 0.2

wordFreqSmooth <- function(freqTable, relVector, item, spanVar, periods){
  item = gsub("^#", "", item)
  item = gsub(" ?#", "|", item)
  item = paste0("^(", item, ")$")
  tempData = subset(freqTable, grepl(item, ngram))
  tempData = subset(tempData, select = -c(ngram, freq))
  tempData = colSums(tempData)
  tempData = as.integer(as.vector(tempData))
  tempData = tempData/relVector
  smoothingmainVar = tempData
  smoothingmainVar = predict(loess(tempData~periods, span=spanVar))
  smoothingmainVar[smoothingmainVar < 0] = 0
  #smoothingmainVar = prepend(smoothingmainVar, 0, before=1)
  return(smoothingmainVar)
}

wordFreq <- function(freqTable, relVector, item, periods){
  #periods = c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600, 650, 700, 750, 800, 850, 900, 950, 1000, 1050, 1100, 1150, 1200, 1250, 1300, 1350, 1400)
  item = gsub("^#", "", item)
  item = gsub(" ?#", "|", item)
  item = paste0("^(", item, ")$")
  tempData = subset(freqTable, grepl(item, ngram))
  tempData = subset(tempData, select = -c(ngram, freq))
  tempData = colSums(tempData)
  tempData = as.integer(as.vector(tempData))
  tempData = tempData/relVector
  return(tempData)
}

#periods = c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600, 650, 700, 750, 800, 850, 900, 950, 1000, 1050, 1100, 1150, 1200, 1250, 1300, 1350, 1400)
#xlabVarAH = c(1, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400)
#xlabVarAH = c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600, 650, 700, 750, 800, 850, 900, 950, 1000, 1050, 1100, 1150, 1200, 1250, 1300, 1350, 1400)
periodsCE = as.integer((periods-periods/33)+622)

datesCE = paste0(periodsCE, "")
datesAH = paste0(periods, "/")
dateLabels = paste0(periods, periodsCE)

############################################################################################
# 0.3 Plot.ly Visual Elements ##############################################################
############################################################################################

# XAXIS ELEMENTS

xAxisFont <- list(
  #family = "Brill, Baskerville, Courier New",
  family = "Courier New",
  size = 12,
  color = 'black'
)

ahAxis <- list(
  tickfont = list(color = "black"),
  #ticktext = xlabVarAH, #dateLabels, # paste0(as.character(periods),"ah"),
  #tickvals = periods,
  showticklabels = TRUE,
  side = "bottom",
  title = paste0("<b>Generated with:</b> OpenITI NgramReader+ @DEV_VERSION@ v.2020-DEV (Maxim Romanov) <b>Data:</b> OpenITI Corpus (ver. 2019.1.1) <b>Date:</b> ",
                 format(Sys.time(), "%b %d, %Y")),#https://maximromanov.shinyapps.io/natharat/
  range = c(0, periods[length(periods)]+periods[2]),
  titlefont = xAxisFont
)

# LEGEND ELEMENTS

legendItem <- function(searchExp){
  temp = str_replace_all(searchExp, "^#", "")
  temp = str_split(temp, " ?#")
  reg = as.vector(temp[[1]])
  nameVar = reg[1]
  reg = reg[2:length(reg)]
  reg = paste0(reg, collapse="|")
  final = paste0("<b>", nameVar, "</b> (RE: ", reg,")")
  return(final)
}

legendFont <- list(
  #family = "Brill, Baskerville, Courier New",
  family = "Courier New",
  size = 16,
  color = 'black'
)

# SAVE CONFIG for SVG Files
saveConfig <- list(
  format = "svg", # "svg", "png"
  filename = paste0("ngramReader_OpenITI_",format(Sys.time(), "D%Y%m%dT%H%M%S")),
  width = 1100,
  height = 500
)

# logoConfig <- list(
#   list(source = "./OpenITI_Logo.png",
#        xref = "", yref = "",
#        x = 0, y = 0,
#        sizex = 0.1, sizey = 0.1,
#        opacity = 0.8,
#        layer = "below"
#   )
# )

logoConfig <- list()

legendConfig <- list(x=0.7, y=1, font = legendFont)
#legendConfig <- list(x=0, y=1, orientation="h", font = legendFont)

############################################################################################
# 1.0 User Interface #######################################################################
############################################################################################

ui <- shinyUI(
  navbarPage(
    #theme = shinytheme("united"), # spacelab+, simplex, readable+, lumen, journal, united,
    theme = "united.min.css",
    div(img(src = "RM_inverse.png", height = "25"),
        "OpenITI NgramReader+ @DEV_VERSION@ v.2020-DEV"), # "Naṯarāt al-ʿUṣūr min al-Muʿaṣṣarāt al-Manṯūr"
    windowTitle = "OpenITI NgramReader+ @DEV_VERSION@ v.2020-DEV",
    tabPanel(title = "Ngram Graphs",
             
             sidebarPanel(
               includeMarkdown("./www/development_version.md"),br(),
               includeMarkdown("./www/intro_short.md"),br(),
               textInput(inputId="text1", label = "Line 1", value = "#ḥaddaṯanā #[wf]?Hdvn[Ay]"),
               textInput(inputId="text2", label = "Line 2", value = "#aḫbaranā #[wf]?Axbrn[Ay]"),
               textInput(inputId="text3", label = "Line 3", value = "#anbaʾanā #[wf]?AnbAn[Ay]"),
               textInput(inputId="text4", label = "Line 4", value = ""),
               textInput(inputId="text5", label = "Line 5", value = ""),
               actionButton(inputId="go", label="Generate graph...")
               
             ),
             mainPanel(
               h3("Graphs of Relative and Absolute Frequencies of Ngrams"),
               #includeMarkdown("./www/BuckwalterSimplified.txt"), br(),
               plotlyOutput("plotly1"),br(),
               plotlyOutput("plotly1A"),br(),
               plotlyOutput("plotly2"),br(),
               plotlyOutput("plotly2A"),br(),
               #h4("Results Data Table"),
               #dataTableOutput('resultsTable'),              
               includeMarkdown("./www/citation.md"), br()
             )
             
             # h3("Results Data Table"),
             # dataTableOutput('resultsTable')
    ),

    tabPanel(title = "Ngram Data Table",
             h3("Data Table of Ngram Results"),
             dataTableOutput('resultsTable')
    ),
        
    tabPanel(title = "About",
             sidebarPanel(
               #h4("Transliteration scheme"),
               includeMarkdown("./www/development_version.md"),br(),
               includeMarkdown("./www/citation.md")
             ),
             mainPanel(
               includeMarkdown("./www/intro.md")
             )
    )
  )
)



############################################################################################
# 2.0 Server ###############################################################################
############################################################################################

server <- shinyServer(
  function(input, output) {
    
    searchVector = eventReactive(input$go,
                                 {
                                   c(input$text1, input$text2, input$text3, input$text4, input$text5)
                                 }
    )
    
    filteredData = eventReactive(input$go,
                                 {
                                   filteredData <- filterData(ngrams, searchVector())
                                 }
    )
    
    
    output$resultsTable <- renderDataTable({filteredData()})
    
    # You can access the value of the widget with input$text, e.g.
    #output$value <- renderPrint({ c(input$text1, input$text2, input$text3, input$text4, input$text5) })
        
    # Plotly Graph with interactivity: relative numbers
    output$plotly1 <- renderPlotly({
      #searchVector = c(input$text1, input$text2, input$text3, input$text4, input$text5)
      div = allFreqs/100

      # generating plot_ly
      perAxis <- list(
        tickfont = list(color = "black"), showticklabels = TRUE, side = "left",
        title = "relative frequencies (%)"
      )
      
      plotlyGraph = plot_ly(type="scatter", mode="markers+lines") %>%
        config(toImageButtonOptions = saveConfig)
      
      # generating data
      counter = 0
      searchVectorNew = c()
      for (n in searchVector()){
        if(n != ""){
          counter = counter+1
          searchVectorNew = c(searchVectorNew,n)

          # actual data
          lineA = wordFreq(filteredData(), div, n, periods)
          lineA[lineA == 0] <- NA
 
          plotlyGraph <- add_trace(plotlyGraph, y=lineA, x=periods,
                                   type="scatter", mode="markers+lines", connectgaps = F,
                                   name=legendItem(n),
                                   hoverinfo = 'text',
                                   text = ~paste0(#'Frequency: ', lineA, '<br>', # for some reason the same values added to all lines...
                                     'Date:', periods,
                                     '/', as.integer((periods-periods/33)+622), "CE")
                                   )
        }
      }
      
      plotlyGraph %>%
        layout(
          legend = legendConfig,
          xaxis  = ahAxis, yaxis  = perAxis,
          images = logoConfig
        )
      
    })
    
    # Plotly Graph with interactivity: relative numbers (with LOWESS CURVE)
    output$plotly1A <- renderPlotly({
      #searchVector = c(input$text1, input$text2, input$text3, input$text4, input$text5)
      div = allFreqs/100
      
      # generating plot_ly
      perAxis <- list(
        tickfont = list(color = "black"), showticklabels = TRUE, side = "left",
        title = paste0("relative frequencies (%) with LOESS smoothing (", spanVar, ")")
      )
      
      plotlyGraph = plot_ly(type="scatter", mode="markers+lines") %>%
        config(toImageButtonOptions = saveConfig)
      
      # generating data
      counter = 0
      searchVectorNew = c()
      for (n in searchVector()){
        if(n != ""){
          counter = counter+1
          searchVectorNew = c(searchVectorNew,n)
          
          # actual data
          #lineA = wordFreq(filteredData(), div, n, periods)
          lineA = wordFreqSmooth(filteredData(), div, n, spanVar, periods)
          lineA = prepend(lineA, 0, before=1)
          lineA[lineA == 0] <- NA
          
          plotlyGraph <- add_trace(plotlyGraph, y=lineA, x=periods,
                                   type="scatter", mode="markers+lines", connectgaps = F,
                                   name=legendItem(n),
                                   hoverinfo = 'text',
                                   text = ~paste0(#'Frequency: ', lineA, '<br>', # for some reason the same values added to all lines...
                                     'Date:', periods,
                                     '/', as.integer((periods-periods/33)+622), "CE")
          )
        }
      }
      
      plotlyGraph %>%
        layout(
          legend = legendConfig,
          xaxis  = ahAxis, yaxis  = perAxis,
          images = logoConfig
        )
      
    })
    
    # Plotly Graph with interactivity: absolute numbers
    output$plotly2 <- renderPlotly({
      div = 1
      #searchVector = c(input$text1, input$text2, input$text3, input$text4, input$text5)
      
      # generating plot_ly
      perAxis <- list(
        tickfont = list(color = "black"), showticklabels = TRUE, side = "left",
        title = "absolute frequencies"
      )
      
      plotlyGraph = plot_ly(type="scatter", mode="markers+lines") %>%
        config(toImageButtonOptions = saveConfig)
      
      counter = 0
      searchVectorNew = c()
      for (n in searchVector()){
        if(n != ""){
          counter = counter+1
          searchVectorNew = c(searchVectorNew,n)
          
          # actual data
          lineA = wordFreq(filteredData(), div, n, periods)
          lineA[lineA == 0] <- NA
          
          plotlyGraph <- add_trace(plotlyGraph, y=lineA, x=periods,
                                   type="scatter", mode="markers+lines", connectgaps = F,
                                   name=legendItem(n),
                                   hoverinfo = 'text',
                                   text = ~paste0(#'Frequency: ', lineA, '<br>', # for some reason the same values added to all lines...
                                     'Date:', periods,
                                     '/', as.integer((periods-periods/33)+622), "CE")
                                   )         
          
        }
      }
      
      plotlyGraph %>%
        layout(
          legend = legendConfig,
          xaxis  = ahAxis, yaxis  = perAxis,
          images = logoConfig
        )
      
    })

    # Plotly Graph with interactivity: absolute numbers (with LOWESS CURVE)
    output$plotly2A <- renderPlotly({
      #searchVector = c(input$text1, input$text2, input$text3, input$text4, input$text5)
      div = 1
      
      # generating plot_ly
      perAxis <- list(
        tickfont = list(color = "black"), showticklabels = TRUE, side = "left",
        title = paste0("absolute frequencies with LOESS smoothing (", spanVar, ")")
      )
      
      plotlyGraph = plot_ly(type="scatter", mode="markers+lines") %>%
        config(toImageButtonOptions = saveConfig)
      
      # generating data
      counter = 0
      searchVectorNew = c()
      for (n in searchVector()){
        if(n != ""){
          counter = counter+1
          searchVectorNew = c(searchVectorNew,n)
          
          # actual data
          lineA = wordFreqSmooth(filteredData(), div, n, spanVar, periods)
          lineA[lineA == 0] <- NA
          
          plotlyGraph <- add_trace(plotlyGraph, y=lineA, x=periods,
                                   type="scatter", mode="markers+lines", connectgaps = F,
                                   name=legendItem(n),
                                   hoverinfo = 'text',
                                   text = ~paste0(#'Frequency: ', lineA, '<br>', # for some reason the same values added to all lines...
                                     'Date:', periods,
                                     '/', as.integer((periods-periods/33)+622), "CE")
          )
        }
      }
      
      plotlyGraph %>%
        layout(
          legend = legendConfig,
          xaxis  = ahAxis, yaxis  = perAxis,
          images = logoConfig
        )
      
    })
    
        
  }
)

############################################################################################
# 3.0 Start Your Engines! ##################################################################
############################################################################################

shinyApp(ui = ui, server = server)
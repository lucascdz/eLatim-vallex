library(shiny)
library(stringr)
library(readtext)
library(fst)
library(DT)
library(data.table)

BaseList <- readRDS('./data/VallencyDataList.rds')

#### UI ####

ui <- fluidPage(
   titlePanel("eLatim-dependencias-v.0.1.funcs"),
   # actionButton("stop","stop preview"),

   tabsetPanel(

      #tabPanel("Home",
      #         includeMarkdown("Home.Rmd")
      #),

      tabPanel("Dictionary",
               sidebarLayout(
                  sidebarPanel(
                     width=3,
                     code('NÚCLEO'),
                     #selectInput("searchBy", "busca por lema", choices = "selecione"),
                     textInput(
                        inputId = "headwordText",
                        label = "busca no lemário",
                        value = "dico"),
                     #actionBttn("go","go"),
                     selectInput(inputId = "headword",
                                 label = "lema verbal",
                                 choices = "",
                                 selected = ""),
                     hr(),
                     code('DEPENDENTE #1'),
                     selectInput("arg1func", "função sintática", choices = "selecione"),
                     selectInput("arg1feat", "tipo de sintagma", choices = "selecione"),
                     selectInput("arg1form", "forma da palavra", choices = "selecione")#,
                     #hr(),
                     #code('ARGUMENTO #2'),
                     #selectInput("arg2funct", "função", choices = "selecione"),
                     #selectInput("arg2choice", "nível de análise", choices = c('forma de ocorrência','tipo de sintagma','traço semântico')),
                     #selectInput("arg2result", "resultados", choices = "selecione"),
                     #selectInput("deprel_value", "morfologia", choices = "selecione")
                  ),
                  mainPanel(width=9,
                            tags$br(),
                            tags$br(),
                            tags$br(),
                            htmlOutput('title_examples'),
                            DT::dataTableOutput('examples'),
                            tags$br(),
                            tags$br(),
                            tags$br(),
                            tags$br(),
                  )
               )
      ),
   )
)

#### SERVER ####

server <- function(input, output,session) {

   #### select headword ####
   lemmaChoices <- names(BaseList)
   updateSelectInput(session, "headword", choices=lemmaChoices )
   ## BLOCK:END ##


   #### search headword by text input ####
   observeEvent(
      c(input$headwordText),{
         CHOICES <- names(BaseList)[str_detect(names(BaseList),input$headwordText)]
         CHOICES <- CHOICES[order(CHOICES)]
         updateSelectInput(session, "headword", choices=CHOICES )
      })
   ## BLOCK:END ##


   #### set input for arg1func ####
   observeEvent(
      c(input$headword),{
         funcsVec <- unique(gsub('ARG_(\\w+)_\\w+','\\1',colnames(BaseList[[input$headword]])[str_detect(colnames(BaseList[[input$headword]]),'ARG')]))
         funcsVec <- c('Qualquer',funcsVec)
         updateSelectInput(session, "arg1func", choices=funcsVec )
      })

   ## BLOCK:END ##

   #### OUTPUT ####
   observeEvent(
      c(input$headword,input$arg1func),{
         if (identical(input$arg1func,'Qualquer')){
            OutputDF <- BaseList[[input$headword]]
         } else {
            OutputDF <- BaseList[[input$headword]][BaseList[[input$headword]][[paste0('ARG_',input$arg1func,'_form')]]!='',]
         }
         output$examples <- DT::renderDataTable(data.table(OutputDF))
      })

   ## BLOCK:END ##

   observeEvent(input$stop,{
      stopApp()
   })
}
shinyApp(ui = ui, server = server)


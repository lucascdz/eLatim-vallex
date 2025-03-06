library(shiny)
library(stringr)
library(readtext)
library(fst)
library(DT)
library(data.table)
SafeEncodeUTF8 <- function(string){
   paste(utf8ToInt(string), collapse = "-")
}

#### UI ####

ui <- fluidPage(
   titlePanel("eLatim-dependencias-v.0.1.forms"),
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
                        value = "fac"),
                     #actionBttn("go","go"),
                     selectInput(inputId = "headword",
                                 label = "lema verbal",
                                 choices = "",
                                 selected = ""),
                     hr(),
                     code('DEPENDENTE #1'),
                     selectInput("arg1form", "forma", choices = "selecione"),
                     selectInput("arg1morph", "morfologia", choices = "selecione"),
                     selectInput("arg1funct", "função", choices = "selecione")#,
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

#BaseDF <- readRDS("./data/VallencyDataFrame.rds")
#BaseDF[is.na(BaseDF)] <- ''
#BaseList <- split(BaseDF,BaseDF$HEAD_lemma)
# clear empty columns
#for (j in seq_along(BaseList)){
#   BaseList[[j]] <- BaseList[[j]][,-which(
#      unlist(lapply(seq_along(BaseList[[j]]),
#                    function(i)
#                       identical(
#                          unique(BaseList[[j]][,i]),
#                          ''))))]
#}
#saveRDS(BaseList,'./data/VallencyDataList.rds')

server <- function(input, output,session) {

   BaseList <- readRDS('./data/VallencyDataList.rds')

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

   # 'arg1morph'
   # 'arg1funct'




   #### set input for arg1form ####
   observeEvent(
      c(input$headword),{
         FormsDF <- BaseList[[input$headword]][ , str_detect(colnames(BaseList[[input$headword]]),'_form$') ]
         FormsVec <- unique(unlist(lapply(seq_along(FormsDF), function(i) FormsDF[[i]][order(FormsDF[[i]])])))
         #FormsVec <- FormsVec[order(FormsVec)]
         FormsVec[FormsVec==''] <- 'Qualquer'
         updateSelectInput(session, "arg1form", choices=FormsVec )
      })

   ## BLOCK:END ##

   #### OUTPUT ####
   observeEvent(
      c(input$headword,input$arg1form),{
         if (identical(input$arg1form,'Qualquer')){
            OutputDF <- BaseList[[input$headword]]
         } else {
            formsList <- lapply(seq_along(BaseList[[input$headword]][[1]]), function(i)
               unlist(c(BaseList[[input$headword]][i,str_detect(colnames(BaseList[[input$headword]]),'_form')])))
            result_lines <- unlist(lapply(seq_along(formsList), function(i) input$arg1form %in% formsList[[i]]))
            OutputDF <- BaseList[[input$headword]][result_lines,]
         }
         output$examples <- DT::renderDataTable(data.table(OutputDF))
      })

   ## BLOCK:END ##

   observeEvent(input$stop,{
      stopApp()
   })
}
shinyApp(ui = ui, server = server)


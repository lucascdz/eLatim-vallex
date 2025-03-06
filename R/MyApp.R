library(data.table)
library(shiny)

DataFrame <- read.csv('./data/VallencyDataframeLT.tsv',sep='\t')
lemmas <- unique(DataFrame$lema_regente)
lemmas <- lemmas[order(lemmas)]
lemmas[lemmas==''] <- '--'

ui <- fluidPage(
   
   titlePanel("eLatim-vallex v.0.5",windowTitle = "eLatim-vallex v.0.5"),
   
   tabPanel("eLatim-vallex",
            sidebarLayout(
               sidebarPanel(
                  width=3,
                  textInput("headwordText", "busca", value = "", placeholder = 'digite algumas letras'),
                  hr(),
                  code('REGENTE'),
                  selectInput("headword", "lema", choices = lemmas),
                  hr(),
                  code('DEPENDENTE'),
                  selectInput("sintagma", "sintagma", choices = '--'),
                  selectInput("traco", "traço", choices = '--'),
                  selectInput("funcao", "função", choices = '--'),
                  actionButton('reset','reiniciar')
                  #checkboxInput()
                  #sliderInput()
                  #numericInput()
                  #
               ),
               mainPanel(width=9,
                         tags$br(),
                         tags$br(),
                         tags$br(),
                         htmlOutput('Exemplos'),
                         DT::dataTableOutput('examples'),
                         tags$br(),
                         tags$br(),
                         tags$br(),
                         tags$br(),
               )
            )
   )
)   


server <- function(input, output,session) {
   
   # reset button ####
   observeEvent(input$reset, {
      updateSelectInput(session, "headwordText", selected = '')
      updateSelectInput(session, "headword", choices = lemmas)
      updateSelectInput(session, "sintagma", choices = '--')
      updateSelectInput(session, "funcao", choices = '--')
      updateSelectInput(session, "traco", choices = '--')
   })
   
   
   # search headword ####
   observeEvent(
      c(input$headwordText),{
         if(!is.null(input$headwordText) && nchar(input$headwordText)>0){
            lemmaChoices <- lemmas[str_detect(lemmas,input$headwordText)]
            lemmaChoices <- c(lemmaChoices,'--')
            updateSelectInput(session, "headword", choices=lemmaChoices)
         } else {
            lemmaChoices <- lemmas
            updateSelectInput(session, "headword", choices=lemmaChoices)
         }
      })
   
   
   
   
   # filter choices ####
   observeEvent(
      c(input$headword,input$sintagma,input$funcao,input$traco),{
         if(input$headword!='--'){
            df <- DataFrame[DataFrame$lema_regente==input$headword,]
            
            sintagmas <- unique(df$sintagma)
            sintagmas <- sintagmas[order(sintagmas)]
            sintagmas <- c('TODOS',sintagmas,'--')
            updateSelectInput(session, 'sintagma', choices=sintagmas)
            #
            funcoes <- unique(df$funcao)
            funcoes <- funcoes[order(funcoes)]
            funcoes <- c('TODOS',funcoes,'--')
            updateSelectInput(session, 'funcao', choices=funcoes)
            #
            tracos <- unique(df$traco)
            tracos <- tracos[order(tracos)]
            tracos[tracos==''] <- 'TODOS'
            tracos <- c(tracos,'--')
            updateSelectInput(session, 'traco', choices=tracos)
            #
         } else {
            #ok
         }
      })
   
   # output ####
   observeEvent(
      c(input$headword,input$sintagmas,input$tracos,input$funcoes),{
         df <- DataFrame[DataFrame$lema_regente==input$headword,3:5]
         df <- df[!duplicated(df),]
         output$examples <- DT::renderDataTable(data.table(df))
      })
   
   
   
}

shinyApp(ui = ui, server = server)



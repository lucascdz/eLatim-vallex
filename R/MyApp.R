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
      updateSelectInput(session, 'sintagma', choices='TODOS')
      updateSelectInput(session, 'traco', choices='TODOS')
      updateSelectInput(session, 'funcao', choices='TODOS')
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
   
   # set head lemma ####
   observeEvent(
      c(input$headword),{
         if(input$headword!='--'){
            updateSelectInput(session, 'sintagma', choices='TODOS')
            updateSelectInput(session, 'traco', choices='TODOS')
            updateSelectInput(session, 'funcao', choices='TODOS')
         }
      })
   

   # filter choices ####
   observeEvent(
      c(input$headword,input$sintagma,input$traco,input$funcao),{
         
         if(input$sintagma=='TODOS' && input$traco=='TODOS' && input$funcao=='TODOS'){
            #
            df <- DataFrame[DataFrame$lema_regente==input$headword,]
            #
            sintagmas <- unique(df$sintagma)
            sintagmas <- sintagmas[order(sintagmas)]
            sintagmas <- c('TODOS',sintagmas)
            updateSelectInput(session, 'sintagma', choices=sintagmas)
            #
            tracos <- unique(df$traco)
            tracos <- tracos[order(tracos)]
            tracos <- c('TODOS',tracos)
            updateSelectInput(session, 'traco', choices=tracos)
            #
            funcoes <- unique(df$funcao)
            funcoes <- funcoes[order(funcoes)]
            funcoes <- c('TODOS',funcoes)
            updateSelectInput(session, 'funcao', choices=funcoes)
            
         }
         
         if(!input$sintagma %in% c('TODOS','--') && input$traco=='TODOS' && input$funcao=='TODOS'){
            #
            df <- DataFrame[DataFrame$lema_regente==input$headword 
                            & DataFrame$sintagma==input$sintagma,]
            #
            updateSelectInput(session, 'sintagma', selected = input$sintagma)
            #
            tracos <- unique(df$traco)
            tracos <- tracos[order(tracos)]
            tracos <- c('TODOS',tracos)
            updateSelectInput(session, 'traco', choices=tracos)
            #
            funcoes <- unique(df$funcao)
            funcoes <- funcoes[order(funcoes)]
            funcoes <- c('TODOS',funcoes)
            updateSelectInput(session, 'funcao', choices=funcoes)
            
         }
         
         if(input$sintagma=='TODOS' && !input$traco %in% c('TODOS','--') && input$funcao=='TODOS'){
            df <- DataFrame[DataFrame$lema_regente==input$headword 
                            & DataFrame$traco==input$traco,]
            #
            sintagmas <- unique(df$sintagma)
            sintagmas <- sintagmas[order(sintagmas)]
            sintagmas <- c('TODOS',sintagmas)
            updateSelectInput(session, 'sintagma', choices=sintagmas)
            #
            updateSelectInput(session, 'traco', selected = input$traco)
            #
            funcoes <- unique(df$funcao)
            funcoes <- funcoes[order(funcoes)]
            funcoes <- c('TODOS',funcoes)
            updateSelectInput(session, 'funcao', choices=funcoes)
            
         }
         
         if(input$sintagma=='TODOS' && input$traco=='TODOS' && !input$funcao %in% c('TODOS','--')){
            df <- DataFrame[DataFrame$lema_regente==input$headword 
                            & DataFrame$funcao==input$funcao,]
            #
            sintagmas <- unique(df$sintagma)
            sintagmas <- sintagmas[order(sintagmas)]
            sintagmas <- c('TODOS',sintagmas)
            updateSelectInput(session, 'sintagma', choices=sintagmas)
            #
            tracos <- unique(df$traco)
            tracos <- tracos[order(tracos)]
            tracos <- c('TODOS',tracos)
            updateSelectInput(session, 'traco', choices=tracos)
            #
            updateSelectInput(session, 'funcao', selected = input$funcao)
            
         }
         
         if(!input$sintagma %in% c('TODOS','--') && !input$traco %in% c('TODOS','--') && input$funcao=='TODOS'){
            df <- DataFrame[DataFrame$lema_regente==input$headword 
                            & DataFrame$sintagma==input$sintagma
                            & DataFrame$traco==input$traco,]
            #
            updateSelectInput(session, 'sintagma', selected = input$sintagma)
            #
            updateSelectInput(session, 'traco', selected = input$traco)
            #
            funcoes <- unique(df$funcao)
            funcoes <- funcoes[order(funcoes)]
            funcoes <- c('TODOS',funcoes)
            updateSelectInput(session, 'funcao', choices=funcoes)
            
         }
         
         if(!input$sintagma %in% c('TODOS','--') && input$traco=='TODOS' && !input$funcao %in% c('TODOS','--')){
            df <- DataFrame[DataFrame$lema_regente==input$headword 
                            & DataFrame$sintagma==input$sintagma
                            & DataFrame$funcao==input$funcao,]
            #
            updateSelectInput(session, 'sintagma', selected = input$sintagma)
            #
            tracos <- unique(df$traco)
            tracos <- tracos[order(tracos)]
            tracos <- c('TODOS',tracos)
            updateSelectInput(session, 'traco', choices=tracos)
            #
            updateSelectInput(session, 'funcao', selected = input$funcao)
         }
         
         if(input$sintagma=='TODOS' && !input$traco %in% c('TODOS','--') && !input$funcao %in% c('TODOS','--')){      
            df <- DataFrame[DataFrame$lema_regente==input$headword 
                            & DataFrame$traco==input$traco
                            & DataFrame$funcao==input$funcao,]
            #
            sintagmas <- unique(df$sintagma)
            sintagmas <- sintagmas[order(sintagmas)]
            sintagmas <- c('TODOS',sintagmas)
            updateSelectInput(session, 'sintagma', choices=sintagmas)
            #
            updateSelectInput(session, 'traco', selected = input$traco)
            #
            updateSelectInput(session, 'funcao', selected = input$funcao)
            
         }
         
      })
   
   
   
   
   
   # OUTPUT ####
   observeEvent(
      c(input$headword,input$sintagma,input$traco,input$funcao),{
         df <- DataFrame[DataFrame$lema_regente==input$headword,3:5]
         df <- df[!duplicated(df),]
         output$examples <- DT::renderDataTable(data.table(df))
      })
   
   
   
}

shinyApp(ui = ui, server = server)



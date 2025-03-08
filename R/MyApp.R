library(data.table)
library(shiny)

#df_path <- './data/DataFrame.tsv'
MyApp <- function(df_path){
   
   DataFrame <- read.csv(df_path,sep='\t')
   lemmas <- unique(DataFrame$reg_lemma)
   lemmas <- lemmas[order(lemmas)]
   #lemmas[lemmas==''] <- '--'
   
   ui <- fluidPage(
      
      titlePanel("eLatim-vallex v.0.5",windowTitle = "eLatim-vallex v.0.5"),
      
      tabPanel("eLatim-vallex",
               sidebarLayout(
                  sidebarPanel(
                     width=3,
                     p('Seja bem vindo!',br(),br(),'Escolha abaixo o elemento regente',em('(head)'),'da estrutura sintática.',br()),
                     hr(),
                     code('REGENTE'),
                     #textInput("headwordText", "", value = "", placeholder = 'digite algumas letras'),
                     selectInput("headword", "lema", choices = lemmas, selected = 'lego'),
                     textOutput('reg_pos',inline = T),
                     br(),
                     textOutput('reg_cat',inline = T),
                     br(),
                     textOutput('reg_stem',inline = T),
                     hr(),
                     code('DEPENDENTE'),
                     selectInput("sintagma", "sintagma", choices = 'TODOS'),
                     selectInput("traco", "traço", choices = 'TODOS'),
                     selectInput("funcao", "função", choices = 'TODOS'),
                     hr(),
                     p(em('para garantir a precisão dos resultados, clique no botão abaixo antes de iniciar uma nova combinação'),br()),
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
         updateSelectInput(session, 'sintagma', selected='TODOS')
         updateSelectInput(session, 'traco', selected='TODOS')
         updateSelectInput(session, 'funcao', selected='TODOS')
      })
      
      
      # search headword (DEPRECATED: 'selectInput()' allows text input also) ####
      #observeEvent(
      #   c(input$headwordText),{
      #      if(!is.null(input$headwordText) && nchar(input$headwordText)>0){
      #         lemmaChoices <- lemmas[str_detect(lemmas,input$headwordText)]
      #         #lemmaChoices <- c(lemmaChoices,'--')
      #         updateSelectInput(session, "headword", choices=lemmaChoices)
      #      }
      #   })
      
      # get head class  ####
      observeEvent(
         c(input$headword),{
            updateSelectInput(session, 'sintagma', selectInput='TODOS')
            updateSelectInput(session, 'traco', selectInput='TODOS')
            updateSelectInput(session, 'funcao', selectInput='TODOS')
            #
            output$reg_pos <- renderText(DataFrame$reg_pos[DataFrame$reg_lemma==input$headword][1])
            output$reg_cat <- renderText(DataFrame$reg_cat[DataFrame$reg_lemma==input$headword][1])
            output$reg_stem <- renderText(DataFrame$reg_stem[DataFrame$reg_lemma==input$headword][1])
         })
      
      
      # filter choices ####
      observeEvent(
         c(input$headword,input$sintagma,input$traco,input$funcao),{
            
            if(input$sintagma=='TODOS' && input$traco=='TODOS' && input$funcao=='TODOS'){
               #
               df <- DataFrame[DataFrame$reg_lemma==input$headword,]
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
               df <- DataFrame[DataFrame$reg_lemma==input$headword 
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
               df <- DataFrame[DataFrame$reg_lemma==input$headword 
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
               df <- DataFrame[DataFrame$reg_lemma==input$headword 
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
               df <- DataFrame[DataFrame$reg_lemma==input$headword 
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
               df <- DataFrame[DataFrame$reg_lemma==input$headword 
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
               df <- DataFrame[DataFrame$reg_lemma==input$headword 
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
            
            if(input$sintagma=='TODOS' && input$traco=='TODOS' && input$funcao=='TODOS'){
               df <- DataFrame[DataFrame$reg_lemma==input$headword,3:6]
               df <- df[!duplicated(df),]
               output$examples <- DT::renderDataTable(data.table(df), options = list(pageLength = 50))
            }
            
            if(!input$sintagma %in% c('TODOS','--') && input$traco=='TODOS' && input$funcao=='TODOS'){
               df <- DataFrame[DataFrame$reg_lemma==input$headword & 
                                  DataFrame$sintagma==input$sintagma,1:6]
               df <- df[!duplicated(df),]
               output$examples <- DT::renderDataTable(data.table(df), options = list(pageLength = 50))
            }
            
            if(input$sintagma=='TODOS' && !input$traco %in% c('TODOS','--') && input$funcao=='TODOS'){
               
               df <- DataFrame[DataFrame$reg_lemma==input$headword & 
                                  DataFrame$traco==input$traco,1:6]
               df <- df[!duplicated(df),]
               output$examples <- DT::renderDataTable(data.table(df), options = list(pageLength = 50))
               
            }
            
            if(input$sintagma=='TODOS' && input$traco=='TODOS' && !input$funcao %in% c('TODOS','--')){
               
               df <- DataFrame[DataFrame$reg_lemma==input$headword & 
                                  DataFrame$funcao==input$funcao,1:6]
               df <- df[!duplicated(df),]
               output$examples <- DT::renderDataTable(data.table(df), options = list(pageLength = 50))
               
            }
            
            if(!input$sintagma %in% c('TODOS','--') && !input$traco %in% c('TODOS','--') && input$funcao=='TODOS'){
               
               df <- DataFrame[DataFrame$reg_lemma==input$headword & 
                                  DataFrame$sintagma==input$sintagma & 
                                  DataFrame$traco==input$traco,1:6]
               df <- df[!duplicated(df),]
               output$examples <- DT::renderDataTable(data.table(df), options = list(pageLength = 50))
               
            }
            
            if(!input$sintagma %in% c('TODOS','--') && input$traco=='TODOS' && !input$funcao %in% c('TODOS','--')){
               
               df <- DataFrame[DataFrame$reg_lemma==input$headword & 
                                  DataFrame$sintagma==input$sintagma & 
                                  DataFrame$funcao==input$funcao,1:6]
               df <- df[!duplicated(df),]
               output$examples <- DT::renderDataTable(data.table(df), options = list(pageLength = 50))
               
            }
            if(input$sintagma=='TODOS' && !input$traco %in% c('TODOS','--') && !input$funcao %in% c('TODOS','--')){
               
               df <- DataFrame[DataFrame$reg_lemma==input$headword & 
                                  DataFrame$traco==input$traco & 
                                  DataFrame$funcao==input$funcao,1:6]
               df <- df[!duplicated(df),]
               output$examples <- DT::renderDataTable(data.table(df), options = list(pageLength = 50))
               
            }
            if(!input$sintagma %in% c('TODOS','--') && !input$traco %in% c('TODOS','--') && !input$funcao %in% c('TODOS','--')){
               
               df <- DataFrame[DataFrame$reg_lemma==input$headword & 
                                  DataFrame$sintagma==input$sintagma & 
                                  DataFrame$traco==input$traco & 
                                  DataFrame$funcao==input$funcao,1:6]
               df <- df[!duplicated(df),]
               output$examples <- DT::renderDataTable(data.table(df), options = list(pageLength = 50))
               
            }
            
         })
      
      
      
   }
   
   shinyApp(ui = ui, server = server)
   
   
}


library(shiny)

{
   DataFrame <- read.csv('./data/VallencyDataframeLT.tsv',sep='\t')
   #
   lemmas <- unique(DataFrame$lemmaHEAD)
   lemmas <- lemmas[order(lemmas)]
   lemmas[lemmas==''] <- '--'
   #
   feats <- unique(DataFrame$arg_feat)
   feats <- feats[order(feats)]
   feats <- c('--',feats)
   #
   forms <- unique(DataFrame$arg_form)
   forms <- forms[order(forms)]
   forms <- c('--',forms)
   #
   funcs <- unique(DataFrame$arg_func)
   funcs <- funcs[order(funcs)]
   funcs <- c('--',funcs)
}

ui <- fluidPage(
   
   titlePanel("eLatim-vallex v.0.5",windowTitle = "eLatim-vallex v.0.5"),
   
   tabPanel("Dictionary",
            sidebarLayout(
               sidebarPanel(
                  width=3,
                  textInput("headwordText", "busca", value = "", placeholder = 'digite algumas letras'),
                  hr(),
                  code('REGENTE'),
                  selectInput("headword", "lema", choices = lemmas),
                  hr(),
                  code('DEPENDENTE'),
                  selectInput("arg_feat", "forma do sintagma", choices = feats),
                  selectInput("arg_form", "forma da palavra", choices = forms),
                  selectInput("arg_func", "relação de dependência", choices = funcs),
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
                         htmlOutput('title_examples'),
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
      updateSelectInput(session, "headword", choices = lemmas, selected = '--')
      updateSelectInput(session, "arg_feat", choices = feats, selected = '--')
      updateSelectInput(session, "arg_form", choices = forms, selected = '--')
      updateSelectInput(session, "arg_func", choices = funcs, selected = '--')
   })
   
   
   # search headword ####
   observeEvent(
      c(input$headwordText),{
         if(!is.null(input$headwordText) && nchar(input$headwordText)>0){
            lemmaChoices <- lemmas[str_detect(lemmas,input$headwordText)]
            lemmaChoices <- c(lemmaChoices,'--')
         } else {
            lemmaChoices <- lemmas
         }
         updateSelectInput(session, "headword", choices=lemmaChoices)
      })
   
   
   
   
   # filter choices ####
   observeEvent(
      c(input$headword,input$arg_func,input$arg_feat,input$arg_form),{
         
         if(input$headword!='--' & input$arg_func=='--' & input$arg_feat=='--' & input$arg_form=='--'){
            
            df <- DataFrame[DataFrame$lemmaHEAD==input$headword,]
            #
            #lemmaChoices <- lemmas[lemmas %in% df$lemmaHEAD]
            #lemmaChoices <- c('--',lemmaChoices)
            #updateSelectInput(session, "headword", choices=lemmaChoices)
            #
            featChoices <- feats[feats %in% df$arg_feat]
            featChoices <- c('--',featChoices)
            updateSelectInput(session, "arg_feat", choices=featChoices)
            #
            formChoices <- forms[forms %in% df$arg_form]
            formChoices <- c('--',formChoices)
            updateSelectInput(session, "arg_form", choices=formChoices)
            #
            funcChoices <- funcs[funcs %in% df$arg_func]
            funcChoices <- c('--',funcChoices)
            updateSelectInput(session, "arg_func", choices=funcChoices) 
            
         }
         
      })
   
   
}

shinyApp(ui = ui, server = server)



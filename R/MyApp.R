library(shiny)



DataFrame <- read.csv('./data/VallencyDataframeLT.tsv',sep='\t')
List <- readRDS('./data/VallencyListLT.rds')

ui <- fluidPage(
   
   titlePanel("eLatim-vallex v.0.5",windowTitle = "eLatim-vallex v.0.5"),
   
   tabPanel("Dictionary",
            sidebarLayout(
               sidebarPanel(
                  width=3,
                  code('REGENTE'),
                  #selectInput("searchBy", "busca por lema", choices = "selecione"),
                  textInput("headwordText", "busca", value = "", placeholder = 'digite algumas letras'),
                  #actionBttn("go","go"),
                  selectizeInput("headword", "lema", choices = "selecione"),
                  hr(),
                  code('DEPENDENTE'),
                  selectInput("arg_feat", "forma do sintagma", choices = "selecione"),
                  selectInput("arg_form", "forma da palavra", choices = "selecione"),
                  selectInput("arg_func", "relação de dependência", choices = "selecione")
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
}

shinyApp(ui = ui, server = server)



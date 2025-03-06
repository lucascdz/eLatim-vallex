
df <- DataFrame[DataFrame$lema_regente==input$headword 
                & DataFrame$sintagma==input$sintagma,]


# FILTERED
updateSelectInput(session, 'sintagma', selected = input$sintagma)
#
updateSelectInput(session, 'traco', selected = input$traco)
#
updateSelectInput(session, 'funcao', selected = input$funcao)





# set 'TODOS' on changing selection ####
## NEED to create 'if' possibilities as for filtering

observeEvent(
   c(input$sintagma),{
      if(!input$sintagma %in% c('TODOS','--')){
         #updateSelectInput(session, 'sintagma', selected='TODOS')
         updateSelectInput(session, 'traco', selected='TODOS')
         updateSelectInput(session, 'funcao', selected='TODOS')
      }
   })
observeEvent(
   c(input$traco),{
      if(!input$traco %in% c('TODOS','--')){
         updateSelectInput(session, 'sintagma', selected='TODOS')
         #updateSelectInput(session, 'traco', selected='TODOS')
         updateSelectInput(session, 'funcao', selected='TODOS')
      }
   })
observeEvent(
   c(input$funcao),{
      if(!input$funcao %in% c('TODOS','--')){
         updateSelectInput(session, 'sintagma', selected='TODOS')
         updateSelectInput(session, 'traco', selected='TODOS')
         #updateSelectInput(session, 'funcao', selected='TODOS')
      }
   })



Matrix2e4df <-  read.csv('/Users/lucascdz/Desktop/matrix2e4.csv')

Matrix2e4df$headword[Matrix2e4df$headword==0] <- "input$headword=='--'"
Matrix2e4df$headword[Matrix2e4df$headword==1] <- "input$headword!='--'"
Matrix2e4df$arg_func[Matrix2e4df$arg_func==0] <- "input$arg_func=='--'"
Matrix2e4df$arg_func[Matrix2e4df$arg_func==1] <- "input$arg_func!='--'"
Matrix2e4df$arg_feat[Matrix2e4df$arg_feat==0] <- "input$arg_feat=='--'"
Matrix2e4df$arg_feat[Matrix2e4df$arg_feat==1] <- "input$arg_feat!='--'"
Matrix2e4df$arg_form[Matrix2e4df$arg_form==0] <- "input$arg_form=='--'"
Matrix2e4df$arg_form[Matrix2e4df$arg_form==1] <- "input$arg_form!='--'"

matrixVec <- unlist(lapply(seq_along(Matrix2e4df$headword), function(i) str_flatten(Matrix2e4df[i,],collapse = ' & ')))
write(matrixVec,'~/Desktop/temp.txt')







Matrix2e3df <-  read.csv('/Users/lucascdz/Desktop/matrix2e3.csv')

Matrix2e3df$sintagma[Matrix2e3df$sintagma==0] <- "input$sintagma=='TODOS'"
Matrix2e3df$sintagma[Matrix2e3df$sintagma==1] <- "!input$sintagma %in% c('TODOS','--')"
Matrix2e3df$traco[Matrix2e3df$traco==0] <- "input$traco=='TODOS'"
Matrix2e3df$traco[Matrix2e3df$traco==1] <- "!input$traco %in% c('TODOS','--')"
Matrix2e3df$funcao[Matrix2e3df$funcao==0] <- "input$funcao=='TODOS'"
Matrix2e3df$funcao[Matrix2e3df$funcao==1] <- "!input$funcao %in% c('TODOS','--')"

matrixVec <- unlist(lapply(seq_along(Matrix2e3df[,1]), function(i) str_flatten(Matrix2e3df[i,],collapse = ' && ')))
write(matrixVec,'~/Desktop/temp.txt')










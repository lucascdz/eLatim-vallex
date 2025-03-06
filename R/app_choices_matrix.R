
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


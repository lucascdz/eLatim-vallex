# eLatim-vallex

corpusRdsPath <- './data/CorpusUDTrad.rds'
source('./R/GetDataFrame.R')
GetDataFrame(corpusRdsPath)

df_path <- './data/DataFrame.tsv'
source('./R/MyApp.R')
MyApp(df_path)


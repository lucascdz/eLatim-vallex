# eLatim-vallex

corpusRdsPath <- './data/CorpusUDTrad.rds'
source('./R/CorpusUD_getData_longtable.R')
GetVallencyData(corpusRdsPath)

df_path <- './data/VallencyDataframeLT.tsv'
source('./R/MyApp.R')
MyApp(df_path)


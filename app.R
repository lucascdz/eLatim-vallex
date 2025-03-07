# eLatim-vallex

## check required packages
requirements <- c('shiny','readtext','tidyverse','DT')
install.packages(requirements[!requirements %in% as.data.frame(installed.packages(),stringsAsFactors = F)[['Package']]])

## run app
source('./R/MyApp.R')
while(!'MyApp' %in% ls()){
   Sys.sleep(5000)
}
runApp(MyApp('./data/DataFrame.tsv'),launch.browser = T)


# For updating data sources, uncomment these lines:
{
   #corpusRdsPath <- './data/CorpusUDTrad.rds'
   #source('./R/GetDataFrame.R')
   #GetDataFrame(corpusRdsPath)
}

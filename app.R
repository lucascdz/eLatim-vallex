# eLatim-vallex

## install required packages
requirements <- c('shiny','readtext','tidyverse','DT')
install.packages(requirements[!requirements %in% as.data.frame(installed.packages(),stringsAsFactors = F)[['Package']]])
## run app
source('./R/MyApp.R')
MyApp('./data/DataFrame.tsv')


# For updating data sources, uncomment these lines:
{
   #corpusRdsPath <- './data/CorpusUDTrad.rds'
   #source('./R/GetDataFrame.R')
   #GetDataFrame(corpusRdsPath)
}

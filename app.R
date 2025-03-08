# eLatim-vallex

## check required packages
requirements <- c('shiny','tidyverse','DT','data.table','readtext')
install.packages(requirements[!requirements %in% as.data.frame(installed.packages(),stringsAsFactors = F)[['Package']]])

## load app
source('./R/MyApp.R')
DataFrame <- read.csv('./data/DataFrame.tsv',sep='\t')

# run app
# MyApp(DataFrame)


# For updating data sources, uncomment these lines:
{
   #corpusRdsPath <- './data/CorpusUDTrad.rds'
   #source('./R/GetDataFrame.R')
   #GetDataFrame(corpusRdsPath)
}

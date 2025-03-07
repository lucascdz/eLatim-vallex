library(readtext)
library(tidyverse)

# argument
# FilePath <- '/Users/lucascdz/Rprojects/eLatim-vallex/data/conllu/la_proiel-ud-train.conllu'

GetConlluMetadata <- function(FilePath){

   Corpus <- readtext(FilePath, verbosity = 0)
   sent_ids <- unlist(str_extract_all(Corpus$text, '# sent_id =.*\n')) %>%
      gsub('# sent_id = (.*)\n',paste0('\\1'),.)
   texts <- unlist(str_extract_all(Corpus$text, '# text =.*\n')) %>%
      gsub('# text = (.*)\n','\\1',.)

   CorpusMetaDF <- data.frame(
      sent_id=sent_ids,
      sentenca=texts,
      stringsAsFactors = F)

   return(CorpusMetaDF)
}

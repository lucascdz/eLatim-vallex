# start from result of the following script the SPARQL
# PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> PREFIX lila: <http://lila-erc.eu/ontologies/lila/> SELECT ?lila_id ?pos ?fl_cat WHERE { ?lila_id lila:hasPOS ?pos_id ; lila:hasInflectionType ?fl_cat_id . ?pos_id rdfs:label ?pos . ?fl_cat_id rdfs:label ?fl_cat .}

#subset <- lilaidSubset
GetLilaPostags <- function(lilaPostagsPath,subset){
   
   LilaPostagsDF <- read.csv(lilaPostagsPath)
   #
   LilaPostagsDF$lila_id <- gsub('http://lila-erc.eu/data/id/lemma/','lilaLemma:',LilaPostagsDF$lila_id) %>% 
      gsub('http://lila-erc.eu/data/id/hypolemma/','lilaIpoLemma:',.)
   LilaPostagsDF <- LilaPostagsDF[LilaPostagsDF$lila_id %in% subset,]
   #
   LilaPostagsDF$reg_pos[LilaPostagsDF$pos=="adjective"] <- 'adjetivo'
   LilaPostagsDF$reg_pos[LilaPostagsDF$pos=="adverb"] <- 'advérbio'
   LilaPostagsDF$reg_pos[LilaPostagsDF$pos=="interjection"] <- 'interjeição'
   LilaPostagsDF$reg_pos[LilaPostagsDF$pos=="common noun"] <- 'substantivo'
   LilaPostagsDF$reg_pos[LilaPostagsDF$pos=="other"] <- ''
   LilaPostagsDF$reg_pos[LilaPostagsDF$pos=="proper noun"] <- 'nome próprio'
   LilaPostagsDF$reg_pos[LilaPostagsDF$pos=="verb"] <- 'verbo'
   LilaPostagsDF$reg_pos[LilaPostagsDF$pos=="coordinating conjunction"] <- 'conjunção coordenativa'
   LilaPostagsDF$reg_pos[LilaPostagsDF$pos=="adposition"] <- 'preposição'
   LilaPostagsDF$reg_pos[LilaPostagsDF$pos=="numeral"] <- 'numeral'
   LilaPostagsDF$reg_pos[LilaPostagsDF$pos=="subordinating conjunction"] <- 'conjunção subordenativa'
   LilaPostagsDF$reg_pos[LilaPostagsDF$pos=="particle"] <- 'partícula'
   LilaPostagsDF$reg_pos[LilaPostagsDF$pos=="pronoun"] <- 'pronome'
   LilaPostagsDF$reg_pos[LilaPostagsDF$pos=="determiner"] <- 'determinante'
   #
   LilaPostagsDF$fl_cat <- gsub('^([a-z\\/]+ [a-z]+).*','\\1',LilaPostagsDF$fl_cat)
   LilaPostagsDF$reg_cat[LilaPostagsDF$fl_cat=="e/i conjugation"] <- '5ª conjugação (mista)'
   LilaPostagsDF$reg_cat[LilaPostagsDF$fl_cat=="fifth declension"] <- '5ª declinação'
   LilaPostagsDF$reg_cat[LilaPostagsDF$fl_cat=="first class"] <- '1ª classe'
   LilaPostagsDF$reg_cat[LilaPostagsDF$fl_cat=="first conjugation"] <- '1ª conjugação'
   LilaPostagsDF$reg_cat[LilaPostagsDF$fl_cat=="first declension"] <- '1ª declinação'
   LilaPostagsDF$reg_cat[LilaPostagsDF$fl_cat=="fourth conjugation"] <- '4ª conjugação'
   LilaPostagsDF$reg_cat[LilaPostagsDF$fl_cat=="fourth declension"] <- '4ª declinação'
   LilaPostagsDF$reg_cat[LilaPostagsDF$fl_cat=="invariable"] <- 'invariável'
   LilaPostagsDF$reg_cat[LilaPostagsDF$fl_cat=="irregular conjugation"] <- 'irregular'
   LilaPostagsDF$reg_cat[LilaPostagsDF$fl_cat=="perfectum impersonal"] <- 'perfectivo'
   LilaPostagsDF$reg_cat[LilaPostagsDF$fl_cat=="perfectum verb"] <- 'perfectivo'
   LilaPostagsDF$reg_cat[LilaPostagsDF$fl_cat=="pronominal"] <- ''
   LilaPostagsDF$reg_cat[LilaPostagsDF$fl_cat=="second class"] <- '2ª classe'
   LilaPostagsDF$reg_cat[LilaPostagsDF$fl_cat=="second conjugation"] <- '2ª conjugação'
   LilaPostagsDF$reg_cat[LilaPostagsDF$fl_cat=="second declension"] <- '2ª declinação'
   LilaPostagsDF$reg_cat[LilaPostagsDF$fl_cat=="third conjugation"] <- '3ª conjugação'
   LilaPostagsDF$reg_cat[LilaPostagsDF$fl_cat=="third declension"] <- '3ª declinação'
   LilaPostagsDF$reg_cat[LilaPostagsDF$fl_cat=="uninflected noun"] <- ''
   LilaPostagsDF$reg_cat[LilaPostagsDF$fl_cat=="uninflected verb"] <- ''
   #
   LilaPostagsDF$reg_stem[LilaPostagsDF$fl_cat=="e/i conjugation"] <- 'tema em -Ĭ- (breve)'
   LilaPostagsDF$reg_stem[LilaPostagsDF$fl_cat=="fifth declension"] <- 'tema em -E-'
   LilaPostagsDF$reg_stem[LilaPostagsDF$fl_cat=="first class"] <- 'tema em -A-/-O-'
   LilaPostagsDF$reg_stem[LilaPostagsDF$fl_cat=="first conjugation"] <- 'tema em -A-'
   LilaPostagsDF$reg_stem[LilaPostagsDF$fl_cat=="first declension"] <- 'tema em -A-'
   LilaPostagsDF$reg_stem[LilaPostagsDF$fl_cat=="fourth conjugation"] <- 'tema em -Ī- (longo)'
   LilaPostagsDF$reg_stem[LilaPostagsDF$fl_cat=="fourth declension"] <- 'tema em -U-'
   LilaPostagsDF$reg_stem[LilaPostagsDF$fl_cat=="invariable"] <- ''
   LilaPostagsDF$reg_stem[LilaPostagsDF$fl_cat=="irregular conjugation"] <- ''
   LilaPostagsDF$reg_stem[LilaPostagsDF$fl_cat=="perfectum impersonal"] <- ''
   LilaPostagsDF$reg_stem[LilaPostagsDF$fl_cat=="perfectum verb"] <- ''
   LilaPostagsDF$reg_stem[LilaPostagsDF$fl_cat=="pronominal"] <- ''
   LilaPostagsDF$reg_stem[LilaPostagsDF$fl_cat=="second class"] <- 'tema em -I- ou atemático'
   LilaPostagsDF$reg_stem[LilaPostagsDF$fl_cat=="second conjugation"] <- 'tema em -E-'
   LilaPostagsDF$reg_stem[LilaPostagsDF$fl_cat=="second declension"] <- 'tema em -O-'
   LilaPostagsDF$reg_stem[LilaPostagsDF$fl_cat=="third conjugation"] <- 'atemático'
   LilaPostagsDF$reg_stem[LilaPostagsDF$fl_cat=="third declension"] <- 'tema em -I- ou atemático'
   LilaPostagsDF$reg_stem[LilaPostagsDF$fl_cat=="uninflected noun"] <- ''
   LilaPostagsDF$reg_stem[LilaPostagsDF$fl_cat=="uninflected verb"] <- ''
   
   # clear columns
   LilaPostagsDF <- LilaPostagsDF[,colnames(LilaPostagsDF) %in% c('lila_id',colnames(LilaPostagsDF)[str_detect(colnames(LilaPostagsDF),'^reg')])]
   
   return(LilaPostagsDF)
}
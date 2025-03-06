library(tidyverse)

#dictFolder <- '/Users/lucascdz/FILES/atomiclab/dictionaries/'
#tracosMarquezFilepath <- paste0(dictFolder,'MarquesCruz_substantivos_tracos.tsv')
#linkedTurtleFilepath <- paste0(dictFolder,'MarquesCruz_substantivos_tracos_LINKED.ttl')

GetSemFeatures <- function(tracosMarquezFilepath,linkedTurtleFilepath){

   ####
   # get MarquezCruz original list
   # EXTRACTED FROM: http://repositorios.fdi.ucm.es/DiccionarioDidacticoLatin/view/paginas/view_paginas.php?id=1
   TracosDF <- read.csv(tracosMarquezFilepath,sep='\t')
   TracosDF$key <- seq(1,length(TracosDF$lemma))

   # get MarquezCruz linked list
   # LINKED THROUGH: https://lila-erc.eu/LiLaTextLinker/
   LinkLines <- readLines(linkedTurtleFilepath)
   lila_ids <- LinkLines[str_detect(LinkLines,'lilaOntology:hasLemma')] %>% gsub('^\\s+lilaOntology:hasLemma (.*);$','\\1',.)
   lemmas <- LinkLines[str_detect(LinkLines,'powla:hasStringValue')] %>% gsub('^\\s+powla:hasStringValue \\"(.*)\\";$','\\1',.)
   LinksDF <- data.frame(
      lemma_check=lemmas,
      lila_id=lila_ids,
      key=seq(1,length(lemmas)),
      stringsAsFactors = F)

   # join tables #
   SemFeatsDF <- left_join(TracosDF,LinksDF)
   identical(SemFeatsDF$lemma,SemFeatsDF$lemma_check)

   # translate semantic traces
   SemFeatsDF$traco <- NA
   SemFeatsDF$traco[SemFeatsDF$animacy=='lugar'] <- 'lugar'
   SemFeatsDF$traco[SemFeatsDF$animacy=='+animado' & SemFeatsDF$definiteness=='+humano'] <- 'humano'
   SemFeatsDF$traco[SemFeatsDF$animacy=='+animado' & SemFeatsDF$definiteness=='-humano'] <- 'animado'
   SemFeatsDF$traco[SemFeatsDF$animacy=='-animado' & SemFeatsDF$definiteness=='+definido'] <- 'concreto'
   SemFeatsDF$traco[SemFeatsDF$animacy=='-animado' & SemFeatsDF$definiteness=='-definido'] <- 'abstrato'


   # clear duplicates
   dupsDF <- as.data.frame(table(SemFeatsDF$lila_id),stringsAsFactors = F)
   dupIds <- dupsDF$Var1[dupsDF$Freq>1]
   #write(dupIds,'~/Desktop/temp.txt')
   SemFeatsDF$lila_id[SemFeatsDF$lemma=='auris' & SemFeatsDF$lila_id=='lilaLemma:90802'] <- 'lilaLemma:90784'
   SemFeatsDF$lila_id[SemFeatsDF$lemma=='copiae' & SemFeatsDF$lila_id=='lilaLemma:96517'] <- 'lilaLemma:157528'
   SemFeatsDF$lila_id[SemFeatsDF$lemma=='liber' & SemFeatsDF$lila_id=='lilaLemma:110381' & SemFeatsDF$traco=='concreto'] <- 'lilaLemma:110382'
   SemFeatsDF$lila_id[SemFeatsDF$lemma=='litus' & SemFeatsDF$lila_id=='lilaLemma:110639'] <- 'lilaLemma:110686'
   SemFeatsDF$lila_id[SemFeatsDF$lemma=='malum' & SemFeatsDF$lila_id=='lilaLemma:111418'] <- 'lilaLemma:111419'
   SemFeatsDF$lila_id[SemFeatsDF$lemma=='malus' & SemFeatsDF$lila_id=='lilaLemma:111418'] <- 'lilaLemma:111423'
   SemFeatsDF$lila_id[SemFeatsDF$lemma=='opera' & SemFeatsDF$lila_id=='lilaLemma:115170'] <- 'lilaLemma:115016'
   SemFeatsDF$lila_id[SemFeatsDF$lemma=='pedes' & SemFeatsDF$lila_id=='lilaLemma:117499'] <- 'lilaLemma:116387'
   SemFeatsDF$lila_id[SemFeatsDF$lemma=='tela' & SemFeatsDF$lila_id=='lilaLemma:127750'] <- 'lilaLemma:127736'
   SemFeatsDF$lila_id[SemFeatsDF$lemma=='vitium' & SemFeatsDF$lila_id=='lilaLemma:130430'] <- 'lilaLemma:130432'
   SemFeatsDF <- SemFeatsDF[SemFeatsDF$id!='1211',]
   SemFeatsDF <- SemFeatsDF[SemFeatsDF$id!='1128',]

   # clear table
   SemFeatsDF <- SemFeatsDF[,colnames(SemFeatsDF) %in% c('lila_id','traco')]

   return(SemFeatsDF)

}





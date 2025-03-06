library(tidyverse)

GetVallencyData <- function(corpusRdsPath){
   # load ####
   CorpusDF <- readRDS(corpusRdsPath)
   CorpusDF[CorpusDF==''] <- NA
   CorpusDF <- CorpusDF[CorpusDF$DEPREL!='punct',]
   CorpusDF <- CorpusDF[CorpusDF$UPOS!='PUNCT',]
   
   CorpusDF$tokenID <- unlist(lapply(seq_along(CorpusDF$ID), function(i) str_flatten(c(CorpusDF$sent_id[i],'____',CorpusDF$ID[i]))))
   CorpusDF$tokenHEAD <- unlist(lapply(seq_along(CorpusDF$ID), function(i) str_flatten(c(CorpusDF$sent_id[i],'____',CorpusDF$HEAD[i]))))
   CorpusDF$lila_id <- unlist(lapply(seq_along(CorpusDF$tokenID), function(i) str_extract(CorpusDF$MISC[i],'LId=\\w+:\\w+') %>% gsub('LId=','',.)))
   CorpusDF$loc.cit <- unlist(lapply(seq_along(CorpusDF$ID), function(i) str_extract(CorpusDF$MISC[i],'Ref=[^|]*'))) %>% gsub('Ref=','',.)
   
   
   # get features ####
   # get case for Sprep
   SprepDF <- CorpusDF[CorpusDF$DEPREL=='case',]
   SprepDF <- SprepDF[SprepDF$UPOS=='ADP',]
   SprepDF <- SprepDF[,colnames(SprepDF) %in% c('FORM','LEMMA','tokenHEAD')]
   colnames(SprepDF) <-  c('withPrep','lemmaPrep','tokenID')
   SprepDF$withPrep <- gsub('(.*)','\\L\\1',SprepDF$withPrep,perl = T)
   CorpusDF <- left_join(CorpusDF,SprepDF)
   
   # get sconj (from 'mark')
   SconjDF <- CorpusDF[CorpusDF$DEPREL=='mark',]
   SconjDF <- SconjDF[,colnames(SconjDF) %in% c('FORM','LEMMA','tokenHEAD')]
   colnames(SconjDF) <-  c('withConj','lemmaConj','tokenID')
   SconjDF$withConj <- gsub('(.*)','\\L\\1',SconjDF$withConj,perl = T)
   CorpusDF <- left_join(CorpusDF,SconjDF)
   
   # get Feats
   CorpusDF$FEATS_case <- gsub('Case=','',str_extract(CorpusDF$FEATS,'Case=\\w+')) %>% gsub('(.*)','\\L\\1',.,perl = T)
   CorpusDF$FEATS_mood <- gsub('Mood=','',str_extract(CorpusDF$FEATS,'Mood=\\w+')) %>% gsub('(.*)','\\L\\1',.,perl = T)
   CorpusDF$FEATS_verbForm <- gsub('VerbForm=','',str_extract(CorpusDF$FEATS,'VerbForm=\\w+')) %>% gsub('(.*)','\\L\\1',.,perl = T)
   
   # check doubles: View(CorpusDF[!is.na(CorpusDF$withConj) & !is.na(CorpusDF$withPrep),])
   
   
   # get sintagmas ####
   CorpusDF$sintagma <- NA
   # Sor: Mood only
   CorpusDF$sintagma[is.na(CorpusDF$withConj) & !is.na(CorpusDF$FEATS_mood)] <- unlist(lapply(
      which(is.na(CorpusDF$withConj) & !is.na(CorpusDF$FEATS_mood)), function(x) paste0('Or(',CorpusDF$FEATS_mood[x],')')))
   # Sor: Conj+Mood
   CorpusDF$sintagma[!is.na(CorpusDF$withConj) & !is.na(CorpusDF$FEATS_mood)] <- unlist(lapply(
      which(!is.na(CorpusDF$withConj) & !is.na(CorpusDF$FEATS_mood)), function(x) paste0('Or(',CorpusDF$lemmaConj[x],'+',CorpusDF$FEATS_mood[x],')')))
   # Sor: Conj+VerbForm
   CorpusDF$sintagma[!is.na(CorpusDF$withConj) & is.na(CorpusDF$FEATS_mood)] <- unlist(lapply(
      which(!is.na(CorpusDF$withConj) & is.na(CorpusDF$FEATS_mood)), function(x) paste0('Or(',CorpusDF$lemmaConj[x],'+',CorpusDF$FEATS_verbForm[x],')')))
   # Sor: VerbForm only
   CorpusDF$sintagma[is.na(CorpusDF$withConj) & is.na(CorpusDF$FEATS_mood) & !is.na(CorpusDF$FEATS_verbForm)] <- unlist(lapply(
      which(is.na(CorpusDF$withConj) & is.na(CorpusDF$FEATS_mood) & !is.na(CorpusDF$FEATS_verbForm)), function(x) paste0('Or(',CorpusDF$FEATS_verbForm[x],')')))
   # Sor: withPrep
   CorpusDF$sintagma[!is.na(CorpusDF$withPrep) & !is.na(CorpusDF$FEATS_verbForm)] <- unlist(lapply(
      which(!is.na(CorpusDF$withPrep) & !is.na(CorpusDF$FEATS_verbForm)), function(x) paste0('Or(',CorpusDF$lemmaPrep[x],'+',CorpusDF$FEATS_verbForm[x],')')))
   # Sprep
   CorpusDF$sintagma[!is.na(CorpusDF$withPrep) & is.na(CorpusDF$FEATS_verbForm)] <- unlist(lapply(
      which(!is.na(CorpusDF$withPrep) & is.na(CorpusDF$FEATS_verbForm)), function(x) paste0('Sprep(',CorpusDF$lemmaPrep[x],')')))
   # N
   CorpusDF$sintagma[is.na(CorpusDF$withPrep) & is.na(CorpusDF$FEATS_verbForm) & !is.na(CorpusDF$FEATS_case)] <- unlist(lapply(
      which(is.na(CorpusDF$withPrep) & is.na(CorpusDF$FEATS_verbForm) & !is.na(CorpusDF$FEATS_case)), function(x) paste0('N(',CorpusDF$FEATS_case[x],')')))
   # Sadv
   CorpusDF$sintagma[CorpusDF$DEPREL=='advmod'] <- 'Adv'
   CorpusDF$sintagma[CorpusDF$UPOS=='ADV'] <- 'Adv'
   # Sadj
   CorpusDF$sintagma[CorpusDF$DEPREL=='amod'] <- 'Adj'
   CorpusDF$sintagma[CorpusDF$DEPREL=='det'] <- 'Adj'
   # others
   CorpusDF$sintagma[is.na(CorpusDF$sintagma) & CorpusDF$DEPREL=='obj' & !CorpusDF$UPOS %in% c('X','CCONJ')] <- 'N(Acc)'
   
   # get dependency relations
   CorpusDF$funcao <- NA
   CorpusDF$funcao[!is.na(CorpusDF$sintagma)] <- CorpusDF$DEPREL[!is.na(CorpusDF$sintagma)]
   
   # get forms
   CorpusDF$dependente <- NA
   CorpusDF$dependente[!is.na(CorpusDF$sintagma)] <- CorpusDF$FORM[!is.na(CorpusDF$sintagma)]
   CorpusDF$dependente[!is.na(CorpusDF$withPrep)] <- unlist(lapply(
      which(!is.na(CorpusDF$withPrep)), function(x) paste0(c(CorpusDF$withPrep[x],CorpusDF$dependente[x]),collapse = ' ')))
   CorpusDF$dependente[!is.na(CorpusDF$withConj)] <- unlist(lapply(
      which(!is.na(CorpusDF$withConj)), function(x) paste0(c(CorpusDF$withConj[x],CorpusDF$dependente[x]),collapse = ' ')))
   
   
   # get semantic feature
   source('./R/CorpusUD_getSemFeats.R')
   tracosMarquezFilepath <- './data/MarquesCruz_substantivos_tracos.tsv'
   linkedTurtleFilepath <- './data/MarquesCruz_substantivos_tracos_LINKED.ttl'
   SemFeatsDF <- GetSemFeatures(tracosMarquezFilepath,linkedTurtleFilepath)
   CorpusDF <- left_join(CorpusDF,SemFeatsDF)
   CorpusDF <- CorpusDF[!is.na(CorpusDF$sintagma),]
   
   # get Headed Arguments DF ####
   ArgumentsDF <- CorpusDF[CorpusDF$DEPREL!='root',colnames(CorpusDF) %in% c('sintagma','dependente','funcao','traco','tokenHEAD','sent_id')]
   # load subset
   lilaidSubset <- read.csv('./data/DCC_Latin_Core_Vocabulary_LINKED.tsv',sep='\t')
   lilaidSubset <- lilaidSubset$ontolex.canonicalForm
   tokenidsubset <- CorpusDF[,colnames(CorpusDF) %in% c('tokenID','lila_id','nTokens')]
   tokenidsubset <- tokenidsubset[tokenidsubset$lila_id %in% lilaidSubset,colnames(tokenidsubset)=='tokenID']
   # apply subset to heads
   ArgumentsDF <- ArgumentsDF[ArgumentsDF$tokenHEAD %in% tokenidsubset,]
   # get head forms and lila_ids
   HeadFormsDF <- CorpusDF[,colnames(CorpusDF) %in% c('FORM','tokenID','lila_id')]
   HeadFormsDF <- HeadFormsDF[!duplicated(HeadFormsDF),]
   colnames(HeadFormsDF) <- c('regente','tokenHEAD','lila_id')
   # join
   ArgumentsDF <- left_join(ArgumentsDF,HeadFormsDF)
   # get keys
   keysDF <- read.csv('./data/lila_id_numkeys.tsv',sep='\t')
   colnames(keysDF)[2] <- 'reg_lemma'
   ArgumentsDF <- left_join(ArgumentsDF,keysDF)
   # get lila postags
   source('./R/GetLilaPostags.R')
   lilaPostagsPath <- './data/lila_postags.csv'
   LilaPostagsDF <- GetLilaPostags(lilaPostagsPath,lilaidSubset)
   colnames(LilaPostagsDF)
   ArgumentsDF <- left_join(ArgumentsDF,LilaPostagsDF)
   
   
   # get Sent data and metadata ####
   SentencesDF <- CorpusDF[,colnames(CorpusDF) %in% c('sent_id','loc.cit')]
   SentencesDF <- SentencesDF[!duplicated(SentencesDF),]
   
   ## get sentence latin texts ##
   source('./R/GetConlluMetadata.R')
   conlluDir <- './data/conllu/'
   sentsList <- lapply(seq_along(dir(conlluDir)), function(i) GetConlluMetadata(paste0(conlluDir,dir(conlluDir)[i])))
   sentsDF <- do.call(rbind,sentsList)
   sentsDF$sent_id[str_detect(sentsDF$sent_id,'^\\d+$')] <- gsub('(.*)','proiel_\\1',sentsDF$sent_id[str_detect(sentsDF$sent_id,'^\\d+$')])
   # join
   SentencesDF <- left_join(SentencesDF,sentsDF)
   
   # get translations
   TranslatinumCorptDF <- read.csv('./data/translatinumCor-pt.tsv',sep='\t')
   SentencesDF <- left_join(SentencesDF,TranslatinumCorptDF)
   
   # get 'nTokens' by sentence
   nTokensDF <- as.data.frame(table(CorpusDF$sent_id),stringsAsFactors = F)
   colnames(nTokensDF) <- c('sent_id','nTokens')
   SentencesDF <- left_join(SentencesDF,nTokensDF)
   SentencesDF <- SentencesDF[order(SentencesDF$nTokens),]
   SentencesDF <- SentencesDF[,colnames(SentencesDF)!='nTokens']
   rownames(SentencesDF) <- NULL
   
   
   # GET VALENCY ####
   VallencyDF <- right_join(SentencesDF,ArgumentsDF)
   VallencyDF <- VallencyDF[,!colnames(VallencyDF) %in% c('lila_id','tokenHEAD')]
   VallencyDF$sintagma[str_detect(VallencyDF$sintagma,'\\+NA')] <- gsub('\\+NA','+',VallencyDF$sintagma[str_detect(VallencyDF$sintagma,'\\+NA')])
   # order columns
   columnOrder <- c(
      which(colnames(VallencyDF)=='regente'),
      which(colnames(VallencyDF)=='dependente'),
      which(colnames(VallencyDF)=='loc.cit'),
      which(colnames(VallencyDF)=='sentenca'),
      which(colnames(VallencyDF)=='traducao'),
      which(colnames(VallencyDF)=='sintagma'),
      which(colnames(VallencyDF)=='funcao'),
      which(colnames(VallencyDF)=='traco'),
      which(colnames(VallencyDF)=='reg_lemma'),
      which(colnames(VallencyDF)=='reg_pos'),
      which(colnames(VallencyDF)=='reg_cat'),
      which(colnames(VallencyDF)=='reg_stem'),
      which(colnames(VallencyDF)=='sent_id')
   )
   VallencyDF <- VallencyDF[,columnOrder]
   VallencyDF[is.na(VallencyDF)] <- ''
   
   # save ##
   write_tsv(VallencyDF,'./data/VallencyDataframeLT.tsv')
   
}

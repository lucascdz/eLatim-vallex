library(tidyverse)


# load ####
CorpusDF <- readRDS('./data/CorpusUDTrad.rds')
CorpusDF[CorpusDF==''] <- NA
CorpusDF <- CorpusDF[CorpusDF$DEPREL!='punct',]
CorpusDF <- CorpusDF[CorpusDF$UPOS!='PUNCT',]

CorpusDF$tokenID <- unlist(lapply(seq_along(CorpusDF$ID), function(i) str_flatten(c(CorpusDF$sent_id[i],'____',CorpusDF$ID[i]))))
CorpusDF$tokenHEAD <- unlist(lapply(seq_along(CorpusDF$ID), function(i) str_flatten(c(CorpusDF$sent_id[i],'____',CorpusDF$HEAD[i]))))
CorpusDF$lila_id <- unlist(lapply(seq_along(CorpusDF$tokenID), function(i) str_extract(CorpusDF$MISC[i],'LId=\\w+:\\w+') %>% gsub('LId=','',.)))
CorpusDF$text_ref <- unlist(lapply(seq_along(CorpusDF$ID), function(i) str_extract(CorpusDF$MISC[i],'Ref=[^|]*'))) %>% gsub('Ref=','',.)


# get features ####
# get case for SPrep
SprepDF <- CorpusDF[CorpusDF$DEPREL=='case',]
SprepDF <- SprepDF[SprepDF$UPOS=='ADP',]
SprepDF <- SprepDF[,colnames(SprepDF) %in% c('LEMMA','tokenHEAD')]
colnames(SprepDF) <-  c('withPrep','tokenID')
#SprepDF$withPrep <- gsub('(.*)','\\1_',SprepDF$withPrep)
CorpusDF <- left_join(CorpusDF,SprepDF)

# get sconj (from 'mark')
SconjDF <- CorpusDF[CorpusDF$DEPREL=='mark',]
SconjDF <- SconjDF[,colnames(SconjDF) %in% c('LEMMA','tokenHEAD')]
colnames(SconjDF) <-  c('withConj','tokenID')
#SconjDF$withConj <- gsub('(.*)','\\1_',SconjDF$withConj)
CorpusDF <- left_join(CorpusDF,SconjDF)

# get Feats
CorpusDF$FEATS_case <- gsub('Case=','',str_extract(CorpusDF$FEATS,'Case=\\w+'))
CorpusDF$FEATS_mood <- gsub('Mood=','',str_extract(CorpusDF$FEATS,'Mood=\\w+'))
CorpusDF$FEATS_verbForm <- gsub('VerbForm=','',str_extract(CorpusDF$FEATS,'VerbForm=\\w+'))

# check doubles: View(CorpusDF[!is.na(CorpusDF$withConj) & !is.na(CorpusDF$withPrep),])

# get sintagmas ####
CorpusDF$arg_feat <- NA
# SOr: Mood only
CorpusDF$arg_feat[is.na(CorpusDF$withConj) & !is.na(CorpusDF$FEATS_mood)] <- unlist(lapply(
   which(is.na(CorpusDF$withConj) & !is.na(CorpusDF$FEATS_mood)), function(x) paste0('SO_',CorpusDF$FEATS_mood[x])))
# SOr: Conj+Mood
CorpusDF$arg_feat[!is.na(CorpusDF$withConj) & !is.na(CorpusDF$FEATS_mood)] <- unlist(lapply(
   which(!is.na(CorpusDF$withConj) & !is.na(CorpusDF$FEATS_mood)), function(x) paste0('SO_',CorpusDF$withConj[x],'_',CorpusDF$FEATS_mood[x])))
# SOr: Conj+VerbForm
CorpusDF$arg_feat[!is.na(CorpusDF$withConj) & is.na(CorpusDF$FEATS_mood)] <- unlist(lapply(
   which(!is.na(CorpusDF$withConj) & is.na(CorpusDF$FEATS_mood)), function(x) paste0('SO_',CorpusDF$withConj[x],'_',CorpusDF$FEATS_verbForm[x])))
# SOr: VerbForm only
CorpusDF$arg_feat[is.na(CorpusDF$withConj) & is.na(CorpusDF$FEATS_mood) & !is.na(CorpusDF$FEATS_verbForm)] <- unlist(lapply(
   which(is.na(CorpusDF$withConj) & is.na(CorpusDF$FEATS_mood) & !is.na(CorpusDF$FEATS_verbForm)), function(x) paste0('SO_',CorpusDF$FEATS_verbForm[x])))
# Sor: withPrep
CorpusDF$arg_feat[!is.na(CorpusDF$withPrep) & !is.na(CorpusDF$FEATS_verbForm)] <- unlist(lapply(
   which(!is.na(CorpusDF$withPrep) & !is.na(CorpusDF$FEATS_verbForm)), function(x) paste0('SO_',CorpusDF$withPrep[x],'_',CorpusDF$FEATS_verbForm[x])))
# SPrep
CorpusDF$arg_feat[!is.na(CorpusDF$withPrep) & is.na(CorpusDF$FEATS_verbForm)] <- unlist(lapply(
   which(!is.na(CorpusDF$withPrep) & is.na(CorpusDF$FEATS_verbForm)), function(x) paste0('SPrep_',CorpusDF$withPrep[x])))
# SN
CorpusDF$arg_feat[is.na(CorpusDF$withPrep) & is.na(CorpusDF$FEATS_verbForm) & !is.na(CorpusDF$FEATS_case)] <- unlist(lapply(
   which(is.na(CorpusDF$withPrep) & is.na(CorpusDF$FEATS_verbForm) & !is.na(CorpusDF$FEATS_case)), function(x) paste0('SN_',CorpusDF$FEATS_case[x])))
# Sadv
CorpusDF$arg_feat[CorpusDF$DEPREL=='advmod'] <- 'Sadv'
CorpusDF$arg_feat[CorpusDF$UPOS=='ADV'] <- 'Sadv'
# others
CorpusDF$arg_feat[is.na(CorpusDF$arg_feat) & CorpusDF$DEPREL=='obj' & !CorpusDF$UPOS %in% c('X','CCONJ')] <- 'SN_Acc'

# get dependency relations
CorpusDF$arg_func <- NA
CorpusDF$arg_func[!is.na(CorpusDF$arg_feat)] <- CorpusDF$DEPREL[!is.na(CorpusDF$arg_feat)]

# get forms
CorpusDF$arg_form <- NA
CorpusDF$arg_form[!is.na(CorpusDF$arg_feat)] <- CorpusDF$FORM[!is.na(CorpusDF$arg_feat)]

# get semantic feature
source('./R/CorpusUD_getSemFeats.R')
tracosMarquezFilepath <- './data/MarquesCruz_substantivos_tracos.tsv'
linkedTurtleFilepath <- './data/MarquesCruz_substantivos_tracos_LINKED.ttl'
SemFeatsDF <- GetSemFeatures(tracosMarquezFilepath,linkedTurtleFilepath)
CorpusDF <- left_join(CorpusDF,SemFeatsDF)
CorpusDF <- CorpusDF[!is.na(CorpusDF$arg_feat),]

# get Headed Arguments DF ####
ArgumentsDF <- CorpusDF[CorpusDF$DEPREL!='root',colnames(CorpusDF) %in% c('arg_feat','arg_form','arg_func','traco','tokenHEAD','sent_id')]
# load subset
lilaidSubset <- read.csv('./data/DCC_Latin_Core_Vocabulary_LINKED.tsv',sep='\t')
lilaidSubset <- lilaidSubset$ontolex.canonicalForm
tokenidsubset <- CorpusDF[,colnames(CorpusDF) %in% c('tokenID','lila_id')]
tokenidsubset <- tokenidsubset[tokenidsubset$lila_id %in% lilaidSubset,colnames(tokenidsubset)=='tokenID']
# apply subset to heads
ArgumentsDF <- ArgumentsDF[ArgumentsDF$tokenHEAD %in% tokenidsubset,]
# get head forms and upos
HeadFormsDF <- CorpusDF[,colnames(CorpusDF) %in% c('FORM','UPOS','tokenID','lila_id')]
HeadFormsDF <- HeadFormsDF[!duplicated(HeadFormsDF),]
colnames(HeadFormsDF) <- c('formHEAD','uposHEAD','tokenHEAD','lila_id')
# join
ArgumentsDF <- left_join(ArgumentsDF,HeadFormsDF)
# get keys
keysDF <- read.csv('./data/lila_id_numkeys.tsv',sep='\t')
colnames(keysDF)[2] <- 'lemmaHEAD'
ArgumentsDF <- left_join(ArgumentsDF,keysDF)


# get Sent data and metadata ####
SentencesDF <- CorpusDF[,colnames(CorpusDF) %in% c('sent_id','text_ref')]
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
#SentencesDF <- SentencesDF[,colnames(SentencesDF)!='nTokens']
rownames(SentencesDF) <- NULL


# GET VALENCY ####
VallencyDF <- right_join(SentencesDF,ArgumentsDF)
VallencyDF <- VallencyDF[,!colnames(VallencyDF) %in% c('lila_id','tokenHEAD')]
VallencyDF[is.na(VallencyDF)] <- ''
VallencyList <- split(VallencyDF,VallencyDF$lemmaHEAD)

# save ##
write_tsv(VallencyDF,'./data/VallencyDataframeLT.tsv')
saveRDS(VallencyList,'./data/VallencyListLT.rds')


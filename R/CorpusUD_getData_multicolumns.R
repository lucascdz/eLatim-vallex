library(tidyverse)


# load ####
CorpusDF <- readRDS('./data/CorpusUDTrad.rds')
CorpusDF[CorpusDF==''] <- NA
CorpusDF <- CorpusDF[CorpusDF$DEPREL!='punct',]
CorpusDF <- CorpusDF[CorpusDF$UPOS!='PUNCT',]

CorpusDF$tokenID <- unlist(lapply(seq_along(CorpusDF$ID), function(i) str_flatten(c(CorpusDF$sent_id[i],'____',CorpusDF$ID[i]))))
CorpusDF$tokenHEAD <- unlist(lapply(seq_along(CorpusDF$ID), function(i) str_flatten(c(CorpusDF$sent_id[i],'____',CorpusDF$HEAD[i]))))

# filter ####
# define subset
headSubset <- read.csv('./data/DCC_Latin_Core_Vocabulary_LINKED.tsv',sep='\t')
headSubset <- headSubset$ontolex.canonicalForm
# get tokenHEAD lila_id
tokensDF <- CorpusDF[,colnames(CorpusDF) %in% c('tokenID','MISC')]
tokensDF$MISC <- unlist(lapply(seq_along(tokensDF$tokenID), function(i) str_extract(tokensDF$MISC[i],'LId=\\w+:\\w+') %>% gsub('LId=','',.)))
colnames(tokensDF) <- c('lilaHEAD','tokenHEAD')
# join
headSubsetDF <- left_join(CorpusDF,tokensDF)
headSubsetDF <- headSubsetDF[headSubsetDF$lilaHEAD %in% headSubset,]
sentSubset <- unique(headSubsetDF$sent_id)
# filter
CorpusDF <- CorpusDF[CorpusDF$sent_id %in% sentSubset,]

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

# get 'Feature' column
CorpusDF$FEATS_case <- gsub('Case=','',str_extract(CorpusDF$FEATS,'Case=\\w+'))
CorpusDF$FEATS_mood <- gsub('Mood=','',str_extract(CorpusDF$FEATS,'Mood=\\w+'))
CorpusDF$FEATS_verbForm <- gsub('VerbForm=','',str_extract(CorpusDF$FEATS,'VerbForm=\\w+'))

# check doubles: View(CorpusDF[!is.na(CorpusDF$withConj) & !is.na(CorpusDF$withPrep),])

# get sintagmas ####
CorpusDF$sintagma <- NA
# SOr: Mood only
CorpusDF$sintagma[is.na(CorpusDF$withConj) & !is.na(CorpusDF$FEATS_mood)] <- unlist(lapply(
   which(is.na(CorpusDF$withConj) & !is.na(CorpusDF$FEATS_mood)), function(x) paste0('SO_',CorpusDF$FEATS_mood[x])))
# SOr: Conj+Mood
CorpusDF$sintagma[!is.na(CorpusDF$withConj) & !is.na(CorpusDF$FEATS_mood)] <- unlist(lapply(
   which(!is.na(CorpusDF$withConj) & !is.na(CorpusDF$FEATS_mood)), function(x) paste0('SO_',CorpusDF$withConj[x],'_',CorpusDF$FEATS_mood[x])))
# SOr: Conj+VerbForm
CorpusDF$sintagma[!is.na(CorpusDF$withConj) & is.na(CorpusDF$FEATS_mood)] <- unlist(lapply(
   which(!is.na(CorpusDF$withConj) & is.na(CorpusDF$FEATS_mood)), function(x) paste0('SO_',CorpusDF$withConj[x],'_',CorpusDF$FEATS_verbForm[x])))
# SOr: VerbForm only
CorpusDF$sintagma[is.na(CorpusDF$withConj) & is.na(CorpusDF$FEATS_mood) & !is.na(CorpusDF$FEATS_verbForm)] <- unlist(lapply(
   which(is.na(CorpusDF$withConj) & is.na(CorpusDF$FEATS_mood) & !is.na(CorpusDF$FEATS_verbForm)), function(x) paste0('SO_',CorpusDF$FEATS_verbForm[x])))
# Sor: withPrep
CorpusDF$sintagma[!is.na(CorpusDF$withPrep) & !is.na(CorpusDF$FEATS_verbForm)] <- unlist(lapply(
   which(!is.na(CorpusDF$withPrep) & !is.na(CorpusDF$FEATS_verbForm)), function(x) paste0('SO_',CorpusDF$withPrep[x],'_',CorpusDF$FEATS_verbForm[x])))
# SPrep
CorpusDF$sintagma[!is.na(CorpusDF$withPrep) & is.na(CorpusDF$FEATS_verbForm)] <- unlist(lapply(
   which(!is.na(CorpusDF$withPrep) & is.na(CorpusDF$FEATS_verbForm)), function(x) paste0('SPrep_',CorpusDF$withPrep[x])))
# SN
CorpusDF$sintagma[is.na(CorpusDF$withPrep) & is.na(CorpusDF$FEATS_verbForm) & !is.na(CorpusDF$FEATS_case)] <- unlist(lapply(
   which(is.na(CorpusDF$withPrep) & is.na(CorpusDF$FEATS_verbForm) & !is.na(CorpusDF$FEATS_case)), function(x) paste0('SN_',CorpusDF$FEATS_case[x])))
# Sadv
CorpusDF$sintagma[CorpusDF$DEPREL=='advmod'] <- 'Sadv'
CorpusDF$sintagma[CorpusDF$UPOS=='ADV'] <- 'Sadv'
# others
CorpusDF$sintagma[is.na(CorpusDF$sintagma) & CorpusDF$DEPREL=='obj' & !CorpusDF$UPOS %in% c('X','CCONJ')] <- 'SN_Acc'



CorpusDF$sintagma[!is.na(CorpusDF$withPrep)] <- CorpusDF$withPrep[!is.na(CorpusDF$withPrep)] %>% 
   gsub('(.*)','SPrep\\(\\1\\)',.)




CorpusDF$FEATS_verbForm[CorpusDF$FEATS_verbForm=='Fin'] <- NA

CorpusDF$Feature <- unlist(lapply(seq_along(CorpusDF$ID), function(i) str_flatten(c(CorpusDF$withPrep[i],CorpusDF$withConj[i],CorpusDF$FEATS_mood[i],CorpusDF$FEATS_verbForm[i],CorpusDF$FEATS_case[i]),na.rm = T)))
# get citation refs
CorpusDF$Ref <- unlist(lapply(seq_along(CorpusDF$ID), function(i) str_extract(CorpusDF$MISC[i],'Ref=[^|]*'))) %>% gsub('Ref=','',.)


#### GET SEMANTIC FEATURES (TRACES) ####
source('./R/CorpusUD_getSemFeats.R')
dictFolder <- '/Users/lucascdz/FILES/atomiclab/dictionaries/'
tracosMarquezFilepath <- paste0(dictFolder,'MarquesCruz_substantivos_tracos.tsv')
linkedTurtleFilepath <- paste0(dictFolder,'MarquesCruz_substantivos_tracos_LINKED.ttl')
SemFeatsDF <- GetSemFeatures(tracosMarquezFilepath,linkedTurtleFilepath)

CorpusDF$lila_id <- unlist(lapply(seq_along(CorpusDF$ID), function(i) str_extract(CorpusDF$MISC[i],'LId=\\w+:\\w+'))) %>% gsub('^LId=','',.)
CorpusDF <- left_join(CorpusDF,SemFeatsDF)


#### GET VERBS VALLENCY ####
# split CORPUS by HEADER #
CorpusList <- split(CorpusDF,CorpusDF$tokenHEAD)

## get tokens UPOS tagged as VERB and AUX
Verbs <- CorpusDF$tokenID[CorpusDF$UPOS %in% c('VERB','AUX')]
VerbsList <- CorpusList[names(CorpusList) %in% Verbs]
VerbsListNames <- names(VerbsList)
# remove non-verbal dependency relations
VerbsList <- lapply(seq_along(VerbsList), function(i) VerbsList[[i]][VerbsList[[i]]$DEPREL %in% c("nsubj","obj","iobj","obl","obl:agent","xcomp","ccomp","csubj","csubj:pass","nsubj:pass","advmod"),])

#### get vallency by Forms ####
ArgumentsByFormDF <- data.frame(
   tokenID=VerbsListNames,
   ARG_nsubj_form=unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$FORM[VerbsList[[i]]$DEPREL=='nsubj'],collapse = ' | '))),
   ARG_obj_form=unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$FORM[VerbsList[[i]]$DEPREL=='obj'],collapse = ' | '))),
   ARG_iobj_form=unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$FORM[VerbsList[[i]]$DEPREL=='iobj'],collapse = ' | '))),
   ARG_obl_form=unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$FORM[VerbsList[[i]]$DEPREL=='obl'],collapse = ' | '))),
   ARG_oblagent_form=unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$FORM[VerbsList[[i]]$DEPREL=='obl:agent'],collapse = ' | '))),
   ARG_xcomp_form=unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$FORM[VerbsList[[i]]$DEPREL=='xcomp'],collapse = ' | '))),
   ARG_ccomp_form=unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$FORM[VerbsList[[i]]$DEPREL=='ccomp'],collapse = ' | '))),
   ARG_csubj_form=unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$FORM[VerbsList[[i]]$DEPREL=='csubj'],collapse = ' | '))),
   ARG_csubjpass_form=unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$FORM[VerbsList[[i]]$DEPREL=='csubj:pass'],collapse = ' | '))),
   ARG_nsubjpass_form=unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$FORM[VerbsList[[i]]$DEPREL=='nsubj:pass'],collapse = ' | '))),
   ARG_advmod_form=unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$FORM[VerbsList[[i]]$DEPREL=='advmod'],collapse = ' | '))),
   stringsAsFactors = F)

#### get vallency by Feats ####
ArgumentsByFeatDF <- data.frame(
   tokenID=VerbsListNames,
   ARG_nsubj_feat=unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$Feature[VerbsList[[i]]$DEPREL=='nsubj'],collapse = ' | '))) %>%
      gsub(' \\| $','',.) %>%
      gsub('^ \\| ','',.),
   ARG_obj_feat=unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$Feature[VerbsList[[i]]$DEPREL=='obj'],collapse = ' | '))) %>%
      gsub(' \\| $','',.) %>%
      gsub('^ \\| ','',.),
   ARG_iobj_feat=unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$Feature[VerbsList[[i]]$DEPREL=='iobj'],collapse = ' | '))) %>%
      gsub(' \\| $','',.) %>%
      gsub('^ \\| ','',.),
   ARG_obl_feat=unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$Feature[VerbsList[[i]]$DEPREL=='obl'],collapse = ' | '))) %>%
      gsub(' \\| $','',.) %>%
      gsub('^ \\| ','',.),
   ARG_oblagent_feat=unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$Feature[VerbsList[[i]]$DEPREL=='obl:agent'],collapse = ' | '))) %>%
      gsub(' \\| $','',.) %>%
      gsub('^ \\| ','',.),
   ARG_xcomp_feat=unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$Feature[VerbsList[[i]]$DEPREL=='xcomp'],collapse = ' | '))) %>%
      gsub(' \\| $','',.) %>%
      gsub('^ \\| ','',.),
   ARG_ccomp_feat=unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$Feature[VerbsList[[i]]$DEPREL=='ccomp'],collapse = ' | '))) %>%
      gsub(' \\| $','',.) %>%
      gsub('^ \\| ','',.),
   ARG_csubj_feat=unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$Feature[VerbsList[[i]]$DEPREL=='csubj'],collapse = ' | '))) %>%
      gsub(' \\| $','',.) %>%
      gsub('^ \\| ','',.),
   ARG_csubjpass_feat=unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$Feature[VerbsList[[i]]$DEPREL=='csubj:pass'],collapse = ' | '))) %>%
      gsub(' \\| $','',.) %>%
      gsub('^ \\| ','',.),
   ARG_nsubjpass_feat=unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$Feature[VerbsList[[i]]$DEPREL=='nsubj:pass'],collapse = ' | '))) %>%
      gsub(' \\| $','',.) %>%
      gsub('^ \\| ','',.),
   ARG_advmod_feat=unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$Feature[VerbsList[[i]]$DEPREL=='advmod'],collapse = ' | '))) %>%
      gsub(' \\| $','',.) %>%
      gsub('^ \\| ','',.),
   stringsAsFactors = F)


#### get vallency by Semantic Feature ####
ArgumentsBySemDF <- data.frame(
   tokenID=VerbsListNames,
   ARG_nsubj_sem=unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$semFeat[VerbsList[[i]]$DEPREL=='nsubj'],collapse = ' | '))),
   ARG_obj_sem=unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$semFeat[VerbsList[[i]]$DEPREL=='obj'],collapse = ' | '))),
   ARG_iobj_sem=unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$semFeat[VerbsList[[i]]$DEPREL=='iobj'],collapse = ' | '))),
   ARG_obl_sem=unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$semFeat[VerbsList[[i]]$DEPREL=='obl'],collapse = ' | '))),
   ARG_oblagent_sem=unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$semFeat[VerbsList[[i]]$DEPREL=='obl:agent'],collapse = ' | '))),
   ARG_xcomp_sem=unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$semFeat[VerbsList[[i]]$DEPREL=='xcomp'],collapse = ' | '))),
   ARG_ccomp_sem=unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$semFeat[VerbsList[[i]]$DEPREL=='ccomp'],collapse = ' | '))),
   ARG_csubj_sem=unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$semFeat[VerbsList[[i]]$DEPREL=='csubj'],collapse = ' | '))),
   ARG_csubjpass_sem=unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$semFeat[VerbsList[[i]]$DEPREL=='csubj:pass'],collapse = ' | '))),
   ARG_nsubjpass_sem=unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$semFeat[VerbsList[[i]]$DEPREL=='nsubj:pass'],collapse = ' | '))),
   ARG_advmod_sem=unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$semFeat[VerbsList[[i]]$DEPREL=='advmod'],collapse = ' | '))),
   stringsAsFactors = F)
ArgumentsBySemDF[is.na(ArgumentsBySemDF)] <- ''

## get ArgumentsDF ##
ArgumentsDF <- full_join(ArgumentsByFeatDF,ArgumentsByFormDF)
ArgumentsDF <- full_join(ArgumentsDF,ArgumentsBySemDF)
ArgumentsDF <- ArgumentsDF[,order(colnames(ArgumentsDF))]



## GET Header Features ##
HeaderFeatsDF <- data.frame(
   tokenID=VerbsListNames,
   stringsAsFactors = F)
HeaderFeatsDF <- left_join(HeaderFeatsDF,CorpusDF[,colnames(CorpusDF) %in% c('tokenID','DEPREL','FEATS','MISC')])
HeaderFeatsDF$HEAD_aspect <- unlist(lapply(seq_along(HeaderFeatsDF$tokenID), function(i) str_extract(HeaderFeatsDF$FEATS[i],'Aspect=\\w+'))) %>% gsub('Aspect=','',.)
HeaderFeatsDF$HEAD_mood <- unlist(lapply(seq_along(HeaderFeatsDF$tokenID), function(i) str_extract(HeaderFeatsDF$FEATS[i],'Mood=\\w+'))) %>% gsub('Mood=','',.)
HeaderFeatsDF$HEAD_tense <- unlist(lapply(seq_along(HeaderFeatsDF$tokenID), function(i) str_extract(HeaderFeatsDF$FEATS[i],'Tense=\\w+'))) %>% gsub('Tense=','',.)
HeaderFeatsDF$HEAD_voice <- unlist(lapply(seq_along(HeaderFeatsDF$tokenID), function(i) str_extract(HeaderFeatsDF$FEATS[i],'Voice=\\w+'))) %>% gsub('Voice=','',.)
HeaderFeatsDF$HEAD_deprel <- unlist(lapply(seq_along(HeaderFeatsDF$tokenID), function(i) HeaderFeatsDF$DEPREL[i]))
HeaderFeatsDF$lila_id <- unlist(lapply(seq_along(HeaderFeatsDF$tokenID), function(i) str_extract(HeaderFeatsDF$MISC[i],'LId=\\w+:\\d+'))) %>% gsub('LId=','',.)
## read keys
keysDF <- read.csv('./data/lila_id_numkeys.tsv',sep='\t')
colnames(keysDF)[2] <- 'HEAD_lemma'
HeaderFeatsDF <- left_join(HeaderFeatsDF,keysDF)
HeaderFeatsDF <- HeaderFeatsDF[,-which(colnames(HeaderFeatsDF) %in% c('FEATS','MISC','DEPREL','lila_id'))]
HeaderFeatsDF <- HeaderFeatsDF[,order(colnames(HeaderFeatsDF))]
HeaderFeatsDF <- HeaderFeatsDF[,c(which(colnames(HeaderFeatsDF)=='HEAD_lemma'),which(colnames(HeaderFeatsDF)!='HEAD_lemma'))]


#### GET SENT DATA ####

## get refs ##
RefList <- split(CorpusDF,CorpusDF$sent_id)
RefDF <- data.frame(
   sent_id=names(RefList),
   SENT_ref=unlist(lapply(seq_along(RefList), function(i) str_flatten(unique(RefList[[i]]$Ref),na.rm = T,collapse = ', '))),
   stringsAsFactors = F)

## get sentence latin texts ##
source('./R/GetConlluMetadata.R')
PerseusUDconlluDir <- '/Users/lucascdz/FILES/atomiclab/corpora/Perseus_UD/conllu/'
PerseusUDsentsList <- lapply(seq_along(dir(PerseusUDconlluDir)), function(i) GetConlluMetadata(paste0(PerseusUDconlluDir,dir(PerseusUDconlluDir)[i])))
PerseusUDsentsDF <- do.call(rbind,PerseusUDsentsList)
ProielUDconlluDir <- '/Users/lucascdz/FILES/atomiclab/corpora/Proiel_UD/conllu_la/'
ProielUDsentsList <- lapply(seq_along(dir(ProielUDconlluDir)), function(i) GetConlluMetadata(paste0(ProielUDconlluDir,dir(ProielUDconlluDir)[i]),'proiel('))
ProielUDsentsDF <- do.call(rbind,ProielUDsentsList)
# bind
CorpusUDsentsDF <- rbind(PerseusUDsentsDF,ProielUDsentsDF)
colnames(CorpusUDsentsDF)[2] <- 'SENT_text_la'

#### get translation texts and IDs ####
CorpusJddDF <- read.csv('./data/translatinumCor_pt.tsv',sep='\t')
colnames(CorpusJddDF)[2:3] <- c('SENT_text_pt','SENT_translator')
CorpusJddDF$SENT_translator[CorpusJddDF$SENT_translator=='*'] <- "José Dejalma Dezotti (outra edição)"
CorpusJddDF$SENT_translator[CorpusJddDF$SENT_translator!='*'] <- "José Dejalma Dezotti"

# get 'nTokens' by sentence
nTokensList <- split(CorpusDF,CorpusDF$sent_id)
nTokensDF <- data.frame(
   sent_id=names(nTokensList),
   nTokens=unlist(lapply(seq_along(nTokensList), function(i) nrow(nTokensList[[i]]))),
   stringsAsFactors = F)

## CREATE Sentence Data DF ##
SentenceDF <- left_join(RefDF,CorpusUDsentsDF)
SentenceDF <- left_join(SentenceDF,CorpusJddDF)
SentenceDF <- left_join(SentenceDF,nTokensDF)
SentenceDF <- SentenceDF[order(SentenceDF$nTokens),]
rownames(SentenceDF) <- NULL


#### GET VALENCY DF ####
VallencyDF <- left_join(HeaderFeatsDF,ArgumentsDF)
VallencyDF$sent_id <- gsub('____\\d+','',VallencyDF$tokenID)
VallencyDF <- right_join(SentenceDF,VallencyDF)
VallencyDF <- VallencyDF[,which(str_detect(colnames(VallencyDF),'SENT_|HEAD_|ARG('))]
## SAVE RDS ##
saveRDS(VallencyDF,'./data/VallencyDataFrame.rds')


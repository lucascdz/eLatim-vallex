library(tidyverse)

#FullCorpusDF <- readRDS('/Users/lucascdz/FILES/doutorado/atomiclab/R.projects/LexicographR/data/CorpusData_noSplit_newKeys.rds')
#HeadedCorpusDF <- FullCorpusDF[FullCorpusDF$HEAD!='_',]
#HeadedCorpusDF <- HeadedCorpusDF[HeadedCorpusDF$DEPREL!='_',]
#HeadedCorpusDF <- HeadedCorpusDF[HeadedCorpusDF$UPOS!='PUNCT',]
#HeadedCorpusDF[HeadedCorpusDF==''] <- NA
#HeadedCorpusDF$tokenID <- unlist(lapply(seq_along(HeadedCorpusDF$ID), function(i) str_flatten(c(HeadedCorpusDF$sent_id[i],'____',HeadedCorpusDF$ID[i]))))
#HeadedCorpusDF$tokenHEAD <- unlist(lapply(seq_along(HeadedCorpusDF$ID), function(i) str_flatten(c(HeadedCorpusDF$sent_id[i],'____',HeadedCorpusDF$HEAD[i]))))
#saveRDS(HeadedCorpusDF,'./data/HeadTaggedCorpus_NoSplit.rds')

BaseDF <- readRDS('./data/HeadTaggedCorpus_NoSplit.rds')

# get prepositions (from 'case')
SprepDF <- BaseDF[BaseDF$DEPREL=='case',]
SprepDF <- SprepDF[SprepDF$UPOS=='ADP',]
SprepDF <- SprepDF[,colnames(SprepDF) %in% c('LEMMA','tokenHEAD')]
colnames(SprepDF) <-  c('withPrep','tokenID')
SprepDF$withPrep <- gsub('(.*)','\\1_',SprepDF$withPrep)
BaseDF <- left_join(BaseDF,SprepDF)

# get sconj (from 'mark')
SconjDF <- BaseDF[BaseDF$DEPREL=='mark',]
SconjDF <- SconjDF[,colnames(SconjDF) %in% c('LEMMA','tokenHEAD')]
colnames(SconjDF) <-  c('withConj','tokenID')
SconjDF$withConj <- gsub('(.*)','\\1_',SconjDF$withConj)
BaseDF <- left_join(BaseDF,SconjDF)

# get 'Feature' column
BaseDF$FEATS_case <- gsub('Case=','',str_extract(BaseDF$FEATS,'Case=\\w+'))
BaseDF$FEATS_mood <- gsub('Mood=','',str_extract(BaseDF$FEATS,'Mood=\\w+'))
BaseDF$FEATS_verbForm <- gsub('VerbForm=','',str_extract(BaseDF$FEATS,'VerbForm=\\w+'))
BaseDF$FEATS_verbForm[BaseDF$FEATS_verbForm=='Fin'] <- NA
BaseDF$Feature <- unlist(lapply(seq_along(BaseDF$ID), function(i) str_flatten(c(BaseDF$withPrep[i],BaseDF$withConj[i],BaseDF$FEATS_mood[i],BaseDF$FEATS_verbForm[i],BaseDF$FEATS_case[i]),na.rm = T)))

# create BASE list
BaseDF <- BaseDF[,colnames(BaseDF) %in% c('FORM','UPOS','DEPREL','MISC','tokenID','tokenHEAD','Feature')]
BaseList <- split(BaseDF,BaseDF$tokenHEAD)

## process VERB and AUX upostagged tokens
Verbs <- BaseDF$tokenID[BaseDF$UPOS %in% c('VERB','AUX')]
VerbsList <- BaseList[names(BaseList) %in% Verbs]
VerbsListNames <- names(VerbsList)
# remove non-verbal dependency relations
VerbsList <- lapply(seq_along(VerbsList), function(i) VerbsList[[i]][VerbsList[[i]]$DEPREL %in% c("nsubj","obj","iobj","obl","obl:agent","xcomp","ccomp","csubj","csubj:pass","nsubj:pass","advmod"),])

# individual column values by FORM
verbs_nsubj_Form <- unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$FORM[VerbsList[[i]]$DEPREL=='nsubj'],collapse = ' | ')))
verbs_obj_Form <- unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$FORM[VerbsList[[i]]$DEPREL=='obj'],collapse = ' | ')))
verbs_iobj_Form <- unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$FORM[VerbsList[[i]]$DEPREL=='iobj'],collapse = ' | ')))
verbs_obl_Form <- unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$FORM[VerbsList[[i]]$DEPREL=='obl'],collapse = ' | ')))
verbs_obl_agent_Form <- unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$FORM[VerbsList[[i]]$DEPREL=='obl:agent'],collapse = ' | ')))
verbs_xcomp_Form <- unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$FORM[VerbsList[[i]]$DEPREL=='xcomp'],collapse = ' | ')))
verbs_ccomp_Form <- unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$FORM[VerbsList[[i]]$DEPREL=='ccomp'],collapse = ' | ')))
verbs_csubj_Form <- unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$FORM[VerbsList[[i]]$DEPREL=='csubj'],collapse = ' | ')))
verbs_csubj_pass_Form <- unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$FORM[VerbsList[[i]]$DEPREL=='csubj:pass'],collapse = ' | ')))
verbs_nsubj_pass_Form <- unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$FORM[VerbsList[[i]]$DEPREL=='nsubj:pass'],collapse = ' | ')))
verbs_advmod_Form <- unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$FORM[VerbsList[[i]]$DEPREL=='advmod'],collapse = ' | ')))

VerbsVallencyByFormDF <- data.frame(tokenID=VerbsListNames,
                      nsubj=verbs_nsubj_Form,
                      obj=verbs_obj_Form,
                      iobj=verbs_iobj_Form,
                      obl=verbs_obl_Form,
                      obl_agent=verbs_obl_agent_Form,
                      xcomp=verbs_xcomp_Form,
                      ccomp=verbs_ccomp_Form,
                      csubj=verbs_csubj_Form,
                      csubj_pass=verbs_csubj_pass_Form,
                      nsubj_pass=verbs_nsubj_pass_Form,
                      advmod=verbs_advmod_Form,
                      stringsAsFactors = F)

VerbsVallencyByFormDF <- left_join(VerbsVallencyByFormDF,BaseDF)

# individual column values by FEATURE
verbs_nsubj_Feat <- unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$Feature[VerbsList[[i]]$DEPREL=='nsubj'],collapse = ' | '))) %>%
   gsub(' \\| $','',.) %>%
   gsub('^ \\| ','',.)
verbs_obj_Feat <- unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$Feature[VerbsList[[i]]$DEPREL=='obj'],collapse = ' | '))) %>%
   gsub(' \\| $','',.) %>%
   gsub('^ \\| ','',.)
verbs_iobj_Feat <- unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$Feature[VerbsList[[i]]$DEPREL=='iobj'],collapse = ' | '))) %>%
   gsub(' \\| $','',.) %>%
   gsub('^ \\| ','',.)
verbs_obl_Feat <- unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$Feature[VerbsList[[i]]$DEPREL=='obl'],collapse = ' | '))) %>%
   gsub(' \\| $','',.) %>%
   gsub('^ \\| ','',.)
verbs_obl_agent_Feat <- unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$Feature[VerbsList[[i]]$DEPREL=='obl:agent'],collapse = ' | '))) %>%
   gsub(' \\| $','',.) %>%
   gsub('^ \\| ','',.)
verbs_xcomp_Feat <- unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$Feature[VerbsList[[i]]$DEPREL=='xcomp'],collapse = ' | '))) %>%
   gsub(' \\| $','',.) %>%
   gsub('^ \\| ','',.)
verbs_ccomp_Feat <- unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$Feature[VerbsList[[i]]$DEPREL=='ccomp'],collapse = ' | '))) %>%
   gsub(' \\| $','',.) %>%
   gsub('^ \\| ','',.)
verbs_csubj_Feat <- unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$Feature[VerbsList[[i]]$DEPREL=='csubj'],collapse = ' | '))) %>%
   gsub(' \\| $','',.) %>%
   gsub('^ \\| ','',.)
verbs_csubj_pass_Feat <- unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$Feature[VerbsList[[i]]$DEPREL=='csubj:pass'],collapse = ' | '))) %>%
   gsub(' \\| $','',.) %>%
   gsub('^ \\| ','',.)
verbs_nsubj_pass_Feat <- unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$Feature[VerbsList[[i]]$DEPREL=='nsubj:pass'],collapse = ' | '))) %>%
   gsub(' \\| $','',.) %>%
   gsub('^ \\| ','',.)
verbs_advmod_Feat <- unlist(lapply(seq_along(VerbsList), function(i) str_flatten(VerbsList[[i]]$Feature[VerbsList[[i]]$DEPREL=='advmod'],collapse = ' | '))) %>%
   gsub(' \\| $','',.) %>%
   gsub('^ \\| ','',.)

VerbsVallencyByFeatDF <- data.frame(tokenID=VerbsListNames,
                                    nsubj=verbs_nsubj_Feat,
                                    obj=verbs_obj_Feat,
                                    iobj=verbs_iobj_Feat,
                                    obl=verbs_obl_Feat,
                                    obl_agent=verbs_obl_agent_Feat,
                                    xcomp=verbs_xcomp_Feat,
                                    ccomp=verbs_ccomp_Feat,
                                    csubj=verbs_csubj_Feat,
                                    csubj_pass=verbs_csubj_pass_Feat,
                                    nsubj_pass=verbs_nsubj_pass_Feat,
                                    advmod=verbs_advmod_Feat,
                                    stringsAsFactors = F)


write_tsv(VerbsVallencyByFeatDF,'./data/corpusdata_vallency_tokenID.tsv')



### bold-face token attempts
#SentTextDF <- BaseDF[,colnames(BaseDF) %in% c('FORM','tokenID')]
#SentTextDF$sent_id <- gsub('(.*)____\\d+$','\\1',SentTextDF$tokenID)
#SentTextDF$ID <- gsub('.*____(\\d+)$','\\1',SentTextDF$tokenID)

#SentTextDF <- left_join(SentTextDF[,colnames(SentTextDF) %in% c('tokenID','sent_id')],SentTextDF[,colnames(SentTextDF) %in% c('FORM','ID','sent_id')])
#SentTextDF$FORM[gsub('.*___(\\d+)$','\\1',SentTextDF$tokenID)]
#VerbsVallencyByFeatDF <- left_join(VerbsVallencyByFeatDF,BaseDF)



#BaseDF$args <- ''
#BaseDF$args[BaseDF$DEPREL %in% c("nsubj","obl:agent","csubj")] <- 'argType0'
#BaseDF$args[BaseDF$DEPREL %in% c("obj","xcomp","ccomp","csubj:pass","nsubj:pass")] <- 'argType1'
#BaseDF$args[BaseDF$DEPREL %in% c("iobj","obl")] <- 'argType2'
#BaseDF$args[BaseDF$DEPREL %in% c("advmod","advcl")] <- 'argType3'


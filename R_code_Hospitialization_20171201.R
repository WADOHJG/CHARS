library(dplyr)
library(tidyr)
#rm(list=ls())

###############################################
#####transfer ICD9/10 to CCS categories #######
#####note: a separate file for 2015 data ######
###############################################
year=2014; # year=2016

if (year<2015) {
  ccs <- read.csv('V:/Staff/JXG4303/CHARS/CCS_HCUP/CCS_ICD9/Single_Level_CCS_2015/$dxref 2015.csv', stringsAsFactors = F, skip=1)[-1,]
  icd <- ccs %>% 
    dplyr::select(1:4) %>% 
    rename(icd9=X.ICD.9.CM.CODE., category=X.CCS.CATEGORY.,
           desc_hcup=X.CCS.CATEGORY.DESCRIPTION., desc_icd=X.ICD.9.CM.CODE.DESCRIPTION.)
  icd$diag <- gsub("'", "", icd$icd9)
  icd$diag <- sub("\\s+$", "", icd$diag)    #remove trailing whitespace
  icd$category1 <- as.numeric(gsub("'", "", icd$category))
}

if (year>2015) {
  ccs <- read.csv('V:/Staff/JXG4303/CHARS/CCS_HCUP/CCS_ICD10/ccs_201801/ccs_dx_icd10cm_2018_1/ccs_dx_icd10cm_2018_1.csv', stringsAsFactors = F)
  icd <- ccs %>% 
    dplyr::select(1:4) %>% 
    rename(icd10=X.ICD.10.CM.CODE., category=X.CCS.CATEGORY.,
           desc_hcup=X.CCS.CATEGORY.DESCRIPTION., desc_icd=X.ICD.10.CM.CODE.DESCRIPTION.)
  icd$diag <- gsub("'", "", icd$icd10)
  icd$diag <- sub("\\s+$", "", icd$diag)
  icd$category1 <- as.numeric(gsub("'", "", icd$category))
}


#hospitalization
tmp.inp <- eval(parse(text=paste0("data.frame(haven::read_sas('Y:/Datasets/Restricted/CHARS/CHARS/SAS_REDACTED/chr_r", year, ".sas7bdat'))")))
tab.cnt.inp <- table(tmp.inp$DIAGCNT)
hcup_chars.inp<- tmp.inp %>% dplyr::select(SEQ_NO_ENC, STATERES, COUNTYRES, AGE, SEX, 
                                    HISPANIC, RACE_WHT, RACE_BLK, RACE_AMI, RACE_ASI, RACE_HAW, 
                                    PAYER1, PAYER2, PAYER3)
tmp.inp1 <- tmp.inp %>% dplyr::select(SEQ_NO_ENC, contains("DIAG"))

for (j in 1:25) {
  a <- eval(parse(text=paste0("dplyr::select(tmp.inp1, SEQ_NO_ENC, DIAG", j, ") %>% 
                              left_join(icd[,c('diag', 'category1')], by=c('DIAG", j, "'='diag')) %>% 
                              rename(diag", j, "_ccs=category1)")))
  hcup_chars.inp <- full_join(hcup_chars.inp, a, by='SEQ_NO_ENC')
}


#observation
tmp.obs <- eval(parse(text=paste0("data.frame(haven::read_sas('Y:/Datasets/Restricted/CHARS/CHARS/SAS_REDACTED/Observation/chro_r", year, ".sas7bdat'))")))
tab.cnt.obs <- table(tmp.obs$DIAGCNT)
hcup_chars.obs<- tmp.obs %>% dplyr::select(SEQ_NO_ENC, STATERES, COUNTYRES, AGE, SEX, 
                                    HISPANIC, RACE_WHT, RACE_BLK, RACE_AMI, RACE_ASI, RACE_HAW, 
                                    PAYER1, PAYER2, PAYER3)
tmp.obs1 <- tmp.obs %>% dplyr::select(SEQ_NO_ENC, contains("DIAG"))

for (j in 1:25) {
  a <- eval(parse(text=paste0("dplyr::select(tmp.obs1, SEQ_NO_ENC, DIAG", j, ") %>% 
                              left_join(icd[,c('diag', 'category1')], by=c('DIAG", j, "'='diag')) %>% 
                              rename(diag", j, "_ccs=category1)")))
  hcup_chars.obs <- full_join(hcup_chars.obs, a, by='SEQ_NO_ENC')
}

write.csv(tab.cnt.inp, file=paste0('V:\\Staff\\JXG4303\\CHARS\\results\\CHARS_201801CCS_', year, '_number_diagnosis_hospitalization.csv'), row.names=F)
write.csv(hcup_chars.inp, file=paste0('V:\\Staff\\JXG4303\\CHARS\\results\\CHARS_201801CCS_', year, '_hospitalization.csv'), row.names=F)
write.table(hcup_chars.inp, file=paste0('V:\\Staff\\JXG4303\\CHARS\\results\\CHARS_201801CCS_', year, '_hospitalization.txt'), row.names=F)
write.csv(tab.cnt.obs, file=paste0('V:\\Staff\\JXG4303\\CHARS\\results\\CHARS_201801CCS_', year, '_number_diagnosis_observation.csv'), row.names=F)
write.csv(hcup_chars.obs, file=paste0('V:\\Staff\\JXG4303\\CHARS\\results\\CHARS_201801CCS_', year, '_observation.csv'), row.names=F)
write.table(hcup_chars.obs, file=paste0('V:\\Staff\\JXG4303\\CHARS\\results\\CHARS_201801CCS_', year, '_observation.txt'), row.names=F)



###########################################################################################
###########################################################################################
## label creating ##

lb <- read.csv('V:/Staff/JXG4303/CHARS/CCS_HCUP/CCS_ICD9/Single_Level_CCS_2015/dxlabel 2015.csv', skip=4, header=F, stringsAsFactors = F)
names(lb) <- c('category', 'label')
lb$label2 <- lb$label
lb$label2[lb$category %in% 177:196] <- 'Pregnancy, childbirth'
lb$label2[lb$category %in% 218:224] <- 'Newborns, neonates'
lb$label3 <- lb$label2
lb$label3[lb$category %in% 49:50] <- 'Diabetes'
lb$category2 <- lb$category
lb$category2[lb$category %in% 177:196] <- '177-196'
lb$category2[lb$category %in% 218:224] <- '218-224'

lb_meps <- xlsx::read.xlsx('V:/Staff/JXG4303/CHARS/CCS_HCUP/CCS_grouping_MEPS_AHRQ_2007.xlsx', sheetIndex = 1, stringsAsFactors=FALSE)
lb_meps$v1 <- sub(' - ', ':', lb_meps$CCS.Codes)
lb$meps <- NA
for (i in 1:dim(lb_meps)[1]) {
  lb$meps[lb$category %in% eval(parse(text=paste0('c(', noquote(lb_meps$v1[i]),')')))] <- lb_meps$Condition.Category[i]
}
lb$meps[is.na(lb$meps)==1] <- 'External causes of injury and poisoning'   #########################

###########################################################################################
year=2014:2016
ccs_version <- c('', '201801', '201801')
type <- c('hospitalization', 'observation', 'both')

#state  
for (i in 1:length(year)) {
  
  for (j in 1:length(type)) {
    
    if (type[j]=='hospitalization') {dat <- read.csv(paste0('V:/Staff/JXG4303/CHARS/results/CHARS_', ccs_version[i], 'CCS_', year[i], '_hospitalization.csv'), stringsAsFactors=F)}
    if (type[j]=='observation') {dat <- read.csv(paste0('V:/Staff/JXG4303/CHARS/results/CHARS_', ccs_version[i], 'CCS_', year[i], '_observation.csv'), stringsAsFactors=F)}
    if (type[j]=='both') {
      dat1 <- read.csv(paste0('V:/Staff/JXG4303/CHARS/results/CHARS_', ccs_version[i], 'CCS_', year[i], '_hospitalization.csv'), stringsAsFactors=F)
      dat2 <- read.csv(paste0('V:/Staff/JXG4303/CHARS/results/CHARS_', ccs_version[i], 'CCS_', year[i], '_observation.csv'), stringsAsFactors=F)
      dat <- rbind(dat1, dat2)
    }
    dat <- filter(dat, STATERES=='WA')   #WA residents only
    
    ############CCS comb_preg_newborn_diabetes#####################################################
    res <- matrix(NA, dim(lb)[1], 4, dimnames=list(c(lb$label), c('category','Primary_diagnosis', 'First_nine_diagnoses', 'All_25_diagnoses')))
    res[,1] <- lb$category
    
    for (k in 1:dim(lb)[1]) {
      ct <- matrix(NA, dim(dat)[1], 25)
      for (u in 1:25) {
        ct[,u] <- eval(parse(text=paste0('dat$diag', u, '_ccs==', lb$category[k])))
      }
      tmp25 <- apply(ct, 1, function(x) sum(x, na.rm=T))
      res[k, 4] <- sum(tmp25>0)
      tmp9 <- apply(ct[, 1:9], 1, function(x) sum(x, na.rm=T))
      res[k, 3] <- sum(tmp9>0)
      res[k, 2] <- sum(ct[,1], na.rm=T)
    }
    
    xlsx::write.xlsx(res, file=paste0('V:/Staff/JXG4303/CHARS/results/reports/All WA residents only/causes_hospitalization_', year[i], '-', 
                                      type[j],'.xlsx'), row.names=T, sheetName = 'ccs_original')

    res1 <- data.frame(res)
    res1$label <- rownames(res)
    res1$label2 <- res1$label
    res1$label2[res1$category %in% 177:196] <- 'Pregnancy, childbirth'
    res1$label2[res1$category %in% 218:224] <- 'Newborns, neonates'
    res.final <- data.frame(res1 %>% group_by (label2) %>% summarize(Primary_diagnosis=sum(Primary_diagnosis),
                                                                     First_nine_diagnoses=sum(First_nine_diagnoses),
                                                                     All_25_diagnoses=sum(All_25_diagnoses)))
    xlsx::write.xlsx(res.final, file=paste0('V:/Staff/JXG4303/CHARS/results/reports/All WA residents only/causes_hospitalization_', year[i], '-', 
                                            type[j],'.xlsx'), row.names=F, sheetName = 'ccs_comb_preg_newborn', append=T)
    
    res1$label3 <- res1$label2
    res1$label3[res1$category %in% 49:50] <- 'Diabetes'
    res.final2 <- data.frame(res1 %>% group_by (label3) %>% summarize(Primary_diagnosis=sum(Primary_diagnosis),
                                                                      First_nine_diagnoses=sum(First_nine_diagnoses),
                                                                      All_25_diagnoses=sum(All_25_diagnoses)))
    xlsx::write.xlsx(res.final2, file=paste0('V:/Staff/JXG4303/CHARS/results/reports/All WA residents only/causes_hospitalization_', year[i], '-', 
                                             type[j],'.xlsx'), row.names=F, sheetName = 'ccs_comb_preg_newborn_diabetes', append=T)
    
    ###########MEPS-CCS#######################################################################
    res <- matrix(NA, dim(lb_meps)[1], 3, dimnames=list(c(lb_meps$Condition.Category), c('Primary_diagnosis', 'First_nine_diagnoses', 'All_25_diagnoses')))
    for (k in 1:dim(lb_meps)[1]) {
      cause <- paste0('c(', noquote(lb_meps$v1[k]),')')
      ct <- matrix(NA, dim(dat)[1], 25)
      for (u in 1:25) {
        ct[,u] <- eval(parse(text=paste0('dat$diag', u, '_ccs %in% ', cause)))
      }
      tmp25 <- apply(ct, 1, function(x) sum(x, na.rm=T))
      res[k, 3] <- sum(tmp25>0)
      tmp9 <- apply(ct[, 1:9], 1, function(x) sum(x, na.rm=T))
      res[k, 2] <- sum(tmp9>0)
      res[k, 1] <- sum(ct[,1], na.rm=T)
    }
    xlsx::write.xlsx(res, file=paste0('V:/Staff/JXG4303/CHARS/results/reports/All WA residents only/causes_hospitalization_', year[i], '-', 
                                      type[j],'.xlsx'), row.names=T, sheetName = 'MEPS_ccs', append=T)
  }
}


#######################################################################################
year=2014:2016
type <- 'both' #c('hospitalization', 'observation', 'both')
ccs_version <- c('', '201801', '201801')

#ACH, County
countyname <- read.csv('V:/Staff/JXG4303/county_code_ACH_2017.csv', stringsAsFactors = F)[-1,]
countyname$no <- 1:39

ach <- unique(countyname$ACH)

#for (i in 1:length(ach)) {dir.create(paste0('V:/Staff/JXG4303/CHARS/results/reports/ACH/', ach[i]))}
for (i in 1:length(year)) {
  for (j in 1:length(type)) {
    if (type[j]=='hospitalization') {dat <- read.csv(paste0('V:/Staff/JXG4303/CHARS/results/CHARS_', ccs_version[i], 'CCS_', year[i], '_hospitalization.csv'), stringsAsFactors=F)}
    if (type[j]=='observation') {dat <- read.csv(paste0('V:/Staff/JXG4303/CHARS/results/CHARS_', ccs_version[i], 'CCS_', year[i], '_observation.csv'), stringsAsFactors=F)}
    if (type[j]=='both') {
      dat1 <- read.csv(paste0('V:/Staff/JXG4303/CHARS/results/CHARS_', ccs_version[i], 'CCS_', year[i], '_hospitalization.csv'), stringsAsFactors=F)
      dat2 <- read.csv(paste0('V:/Staff/JXG4303/CHARS/results/CHARS_', ccs_version[i], 'CCS_', year[i], '_observation.csv'), stringsAsFactors=F)
      dat <- rbind(dat1, dat2)
    }
    dat <- dat %>% filter(STATERES=='WA')  %>%  left_join(countyname, by=c('COUNTYRES'='no'))
    
    for (m in 1:length(ach)) {
      dat.tmp=filter(dat, dat$ACH==ach[m])
      
      if (ach[m] %in% c("King County", "Pierce County")) {ach.county=ach[m]}
      else {ach.county=unique(c(ach[m], unique(dat.tmp$name)))}

      for (w in 1:length(ach.county)) {
        if (w==1) {dat.tmp1=dat.tmp}  ##ACH
        else {dat.tmp1=subset(dat.tmp, name==ach.county[w])} ##County
        
        ############CCS comb_preg_newborn_diabetes#####################################################
        res <- matrix(NA, dim(lb)[1], 4, dimnames=list(c(lb$label), c('category','Primary_diagnosis', 'First_nine_diagnoses', 'All_25_diagnoses')))
        res[,1] <- lb$category
        
        for (k in 1:dim(lb)[1]) {
          ct <- matrix(NA, dim(dat.tmp1)[1], 25)
          for (u in 1:25) {
            ct[,u] <- eval(parse(text=paste0('dat.tmp1$diag', u, '_ccs==', lb$category[k])))
          }
          tmp25 <- apply(ct, 1, function(x) sum(x, na.rm=T))
          res[k, 4] <- sum(tmp25>0)
          tmp9 <- apply(ct[, 1:9], 1, function(x) sum(x, na.rm=T))
          res[k, 3] <- sum(tmp9>0)
          res[k, 2] <- sum(ct[,1], na.rm=T)
        }
        
        xlsx::write.xlsx(res, file=paste0('V:/Staff/JXG4303/CHARS/results/reports/ACH/', ach[m], '/causes_hospitalization_', year[i], '-', 
                                          type[j], '_', ach.county[w], '.xlsx'), row.names=T, sheetName = paste('ccs_original', ach.county[w]))
        res1 <- data.frame(res)
        res1$label <- rownames(res)
        res1$label2 <- res1$label
        res1$label2[res1$category %in% 177:196] <- 'Pregnancy, childbirth'
        res1$label2[res1$category %in% 218:224] <- 'Newborns, neonates'
        res.final <- data.frame(res1 %>% group_by (label2) %>% summarize(Primary_diagnosis=sum(Primary_diagnosis),
                                                                         First_nine_diagnoses=sum(First_nine_diagnoses),
                                                                         All_25_diagnoses=sum(All_25_diagnoses)))
        xlsx::write.xlsx(res.final, file=paste0('V:/Staff/JXG4303/CHARS/results/reports/ACH/', ach[m], '/causes_hospitalization_', year[i], '-', 
                                                type[j], '_', ach.county[w],'.xlsx'), row.names=F, sheetName = 'ccs_comb_preg_newborn', append=T)
        
        res1$label3 <- res1$label2
        res1$label3[res1$category %in% 49:50] <- 'Diabetes'
        res.final2 <- data.frame(res1 %>% group_by (label3) %>% summarize(Primary_diagnosis=sum(Primary_diagnosis),
                                                                          First_nine_diagnoses=sum(First_nine_diagnoses),
                                                                          All_25_diagnoses=sum(All_25_diagnoses)))
        xlsx::write.xlsx(res.final2, file=paste0('V:/Staff/JXG4303/CHARS/results/reports/ACH/', ach[m], '/causes_hospitalization_', year[i], '-', 
                                                 type[j], '_', ach.county[w],'.xlsx'), row.names=F, sheetName = 'ccs_comb_preg_newborn_diabetes', append=T)
        
        ###########MEPS-CCS#######################################################################
        res <- matrix(NA, dim(lb_meps)[1], 3, dimnames=list(c(lb_meps$Condition.Category), c('Primary_diagnosis', 'First_nine_diagnoses', 'All_25_diagnoses')))
        for (k in 1:dim(lb_meps)[1]) {
          cause <- paste0('c(', noquote(lb_meps$v1[k]),')')
          ct <- matrix(NA, dim(dat.tmp1)[1], 25)
          for (u in 1:25) {
            ct[,u] <- eval(parse(text=paste0('dat.tmp1$diag', u, '_ccs %in% ', cause)))
          }
          tmp25 <- apply(ct, 1, function(x) sum(x, na.rm=T))
          res[k, 3] <- sum(tmp25>0)
          tmp9 <- apply(ct[, 1:9], 1, function(x) sum(x, na.rm=T))
          res[k, 2] <- sum(tmp9>0)
          res[k, 1] <- sum(ct[,1], na.rm=T)
        }
        xlsx::write.xlsx(res, file=paste0('V:/Staff/JXG4303/CHARS/results/reports/ACH/', ach[m], '/causes_hospitalization_', year[i], '-', 
                                          type[j], '_', ach.county[w],'.xlsx'), row.names=T, sheetName = 'MEPS_ccs', append=T)
      }
    }
  }
}

#################################
##format the ACH/County results##
#################################
#for (i in 1:length(ach)) {dir.create(paste0('V:/Staff/JXG4303/CHARS/results/reports/sent/', ach[i]))}

year=2016:2014
type='both'
diag <- c('Primary_diagnosis',	'All_25_diagnoses')  #drop 'First_nine_diagnoses' due to the results similar to 'All_25_diagnoses'

#label for the combined CCS preg & newborn category
lb_c <- lb %>% dplyr::select(category2, label2) %>% distinct

#ACH, County
countyname <- read.csv('V:/Staff/JXG4303/county_code_ACH_2017.csv', stringsAsFactors = F)[-1,]
countyname$no <- 1:39

ach <- unique(countyname$ACH)

for (u in 1:length(diag)) {
  
  for (m in 1:length(ach)) {
    
    if (ach[m] %in% c("King County", "Pierce County")) {ach.county=ach[m]
    } else {
      ach.county=unique(c(ach[m], unique(countyname$name[countyname$ACH==ach[m]])))}
    
    for (w in 1:length(ach.county)) {
      
      for (i in 1:length(year)){
        ccso <- xlsx::read.xlsx(file=paste0('V:/Staff/JXG4303/CHARS/results/reports/ACH/', ach[m], '/causes_hospitalization_', year[i], '-', 
                                            type, '_', ach.county[w], '.xlsx'), sheetIndex = 1, stringsAsFactors=F)
        ccso <- dplyr::filter(ccso, category!=259)   ##"Residual codes; unclassified"
        
        ccsc <- xlsx::read.xlsx(file=paste0('V:/Staff/JXG4303/CHARS/results/reports/ACH/', ach[m], '/causes_hospitalization_', year[i], '-', 
                                            type, '_', ach.county[w],'.xlsx'), sheetName = 'ccs_comb_preg_newborn', stringsAsFactors=F)
        ccsc <- full_join(ccsc, lb_c, by='label2') %>%  filter(label2!="Residual codes; unclassified")
        
        meps <- xlsx::read.xlsx(file=paste0('V:/Staff/JXG4303/CHARS/results/reports/ACH/', ach[m], '/causes_hospitalization_', year[i], '-', 
                                            type, '_', ach.county[w],'.xlsx'), sheetName = "MEPS_ccs", stringsAsFactors=F)
        names(meps)[1] <- 'Condition.Category'
        meps <- left_join(meps, lb_meps[, 1:2], by='Condition.Category')
        meps <- meps[meps[,1]!='Residual codes',]
        
        if (i==1) {dato=data.frame(Diagnosis=ccso[,1], CCS_category=ccso[,'category'])
        datc=data.frame(Diagnosis=ccsc[,1], CCS_category=ccsc[, 'category2'])
        datm=data.frame(Diagnosis=meps[,1], CCS_category=meps[, 'CCS.Codes'])}
        
        ro <- rank(-1*ccso[,diag[u]], ties.method = 'min')
        rc <- rank(-1*ccsc[,diag[u]], ties.method = 'min')
        rm <- rank(-1*meps[,diag[u]], ties.method = 'min')
        
        tmpo <- data.frame(ccso[, diag[u]], ro)
        names(tmpo) <- c(paste0('N_of_Stays_on_', diag[u], '_', year[i]), paste0('Rank_', year[i]))
        dato <- cbind(dato, tmpo)
        if (i==length(year)) {dato=dato[order(dato[, paste0('N_of_Stays_on_', diag[u], '_', year[1])], decreasing=T),]}
        
        tmpc <- data.frame(ccsc[, diag[u]], rc)
        names(tmpc) <- c(paste0('N_of_Stays_on_', diag[u], '_', year[i]), paste0('Rank_', year[i]))
        datc <- cbind(datc, tmpc)
        if (i==length(year)) {datc=datc[order(datc[, paste0('N_of_Stays_on_', diag[u], '_', year[1])], decreasing=T),]}
        
        tmpm <- data.frame(meps[, diag[u]], rm)
        names(tmpm) <- c(paste0('N_of_Stays_on_', diag[u], '_', year[i]), paste0('Rank_', year[i]))
        datm <- cbind(datm, tmpm)
        if (i==length(year)) {datm=datm[order(datm[, paste0('N_of_Stays_on_', diag[u], '_', year[1])], decreasing=T),]}
      }
      
      if (w==1) {
        xlsx::write.xlsx(dato, file=paste0('V:/Staff/JXG4303/CHARS/results/reports/sent/', ach[m], '/hospitalization_', ach[m], '_', diag[u], '_ccs.xlsx'),
                         row.names=F, sheetName=ach.county[w])
        xlsx::write.xlsx(datc, file=paste0('V:/Staff/JXG4303/CHARS/results/reports/sent/', ach[m], '/hospitalization_', ach[m], '_', diag[u], '_ccs_comb.xlsx'),
                         row.names=F, sheetName=ach.county[w])
        xlsx::write.xlsx(datm, file=paste0('V:/Staff/JXG4303/CHARS/results/reports/sent/', ach[m], '/hospitalization_', ach[m], '_', diag[u], '_MEPS.xlsx'),
                         row.names=F, sheetName=ach.county[w])
      } else {
        xlsx::write.xlsx(dato, file=paste0('V:/Staff/JXG4303/CHARS/results/reports/sent/', ach[m], '/hospitalization_', ach[m], '_', diag[u], '_ccs.xlsx'),
                         row.names=F, sheetName=ach.county[w], append=T)
        xlsx::write.xlsx(datc, file=paste0('V:/Staff/JXG4303/CHARS/results/reports/sent/', ach[m], '/hospitalization_', ach[m], '_', diag[u], '_ccs_comb.xlsx'),
                         row.names=F, sheetName=ach.county[w], append=T)
        xlsx::write.xlsx(datm, file=paste0('V:/Staff/JXG4303/CHARS/results/reports/sent/', ach[m], '/hospitalization_', ach[m], '_', diag[u], '_MEPS.xlsx'),
                         row.names=F, sheetName=ach.county[w], append=T)
      }
    } 
  }    
}



#############################################################################################
##check the primary diagnosis distribution among patients with non-primary diabetes diagnosis (i.e. in diagnosis 2-25) 

##need to get the 'dat' from the cod above

k=5

cause <- paste0('c(', noquote(lb_meps$v1[k]),')')
ct <- matrix(NA, dim(dat)[1], 25)
for (u in 1:25) {
  ct[,u] <- eval(parse(text=paste0('dat$diag', u, '_ccs %in% ', cause)))
}
tmp24 <- apply(ct[,-1], 1, function(x) sum(x, na.rm=T))
dat$diabetes_2  <- tmp24>0
dattmp <- select(dat, SEQ_NO_ENC, diag1_ccs, diabetes_2)

tmp <- dattmp %>% filter(diabetes_2==TRUE) %>% left_join(lb, by=c('diag1_ccs'='category'))

a <- data.frame(table(tmp$meps))
b <- data.frame(table(tmp$label3))

xlsx::write.xlsx(a, file=paste0('V:/Staff/JXG4303/CHARS/results/reports/All WA residents only/distribution of primary cause among patients with non-primary diabetes diagnosis_', year[i], '-', type[j],'.xlsx'), 
          sheetName="MEPS", row.names=F)
xlsx::write.xlsx(b, file=paste0('V:/Staff/JXG4303/CHARS/results/reports/All WA residents only/distribution of primary cause among patients with non-primary diabetes diagnosis_', year[i], '-', type[j],'.xlsx'), 
          sheetName="CCS",row.names=F, append = T)











##change the variable name

year=2014:2016
type <- c('hospitalization', 'observation', 'both')

#ACH, County
countyname <- read.csv('V:/Staff/JXG4303/county_code_ACH_2017.csv', stringsAsFactors = F)[-1,]
countyname$no <- 1:39

ach <- unique(countyname$ACH)

for (j in 1:length(type)) {
  for (m in 1:length(ach)) {
    
    if (ach[m] %in% c("King County", "Pierce County")) {ach.county=ach[m]
    } else {
      ach.county=unique(c(ach[m], unique(countyname$name[countyname$ACH==ach[m]])))}
    
    for (w in 1:length(ach.county)) {
      
      for (i in 1:length(year)){
        ccso <- xlsx::read.xlsx(file=paste0('V:/Staff/JXG4303/CHARS/results/reports/tmp/ACH/', ach[m], '/causes_hospitalization_', year[i], '-', 
                                            type[j], '_', ach.county[w], '.xlsx'), sheetIndex = 1)
        
        ccsc <- xlsx::read.xlsx(file=paste0('V:/Staff/JXG4303/CHARS/results/reports/tmp/ACH/', ach[m], '/causes_hospitalization_', year[i], '-', 
                                            type[j], '_', ach.county[w],'.xlsx'), sheetIndex = 2)
        
        meps <- xlsx::read.xlsx(file=paste0('V:/Staff/JXG4303/CHARS/results/reports/tmp/ACH/', ach[m], '/causes_hospitalization_', year[i], '-', 
                                            type[j], '_', ach.county[w],'.xlsx'), sheetIndex = 3)
        
        
        names(ccso) <- c('Diagnosis', 'category','Primary_diagnosis', 'First_nine_diagnoses', 'All_25_diagnoses')
        names(ccsc) <- c('Diagnosis','Primary_diagnosis', 'First_nine_diagnoses', 'All_25_diagnoses')
        names(meps) <- c('Diagnosis', 'Primary_diagnosis', 'First_nine_diagnoses', 'All_25_diagnoses')
        
        xlsx::write.xlsx(ccso, file=paste0('V:/Staff/JXG4303/CHARS/results/reports/tmp/ACH/', ach[m], '/causes_hospitalization_', year[i], '-', 
                                           type[j], '_', ach.county[w], '.xlsx'), row.names=T, sheetName = paste('ccs_original', ach.county[w]))
        
        xlsx::write.xlsx(ccsc, file=paste0('V:/Staff/JXG4303/CHARS/results/reports/tmp/ACH/', ach[m], '/causes_hospitalization_', year[i], '-', 
                                           type[j], '_', ach.county[w],'.xlsx'), row.names=F, sheetName = 'ccs_comb_preg_newborn_diabetes', append=T)
        
        xlsx::write.xlsx(meps, file=paste0('V:/Staff/JXG4303/CHARS/results/reports/tmp/ACH/', ach[m], '/causes_hospitalization_', year[i], '-', 
                                           type[j], '_', ach.county[w],'.xlsx'), row.names=T, sheetName = 'MEPS_ccs', append=T)
      }
    }
  }
}



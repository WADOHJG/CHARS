library(dplyr)
library(tidyr)

year=2015
  
  ccs9 <- read.csv('V:/Staff/JXG4303/CHARS/CCS_HCUP/CCS_ICD9/Single_Level_CCS_2015/$dxref 2015.csv', stringsAsFactors = F, skip=1)[-1,]
  icd9 <- ccs9 %>% 
    select(1:4) %>% 
    rename(icd=X.ICD.9.CM.CODE., category=X.CCS.CATEGORY.,
           desc_hcup=X.CCS.CATEGORY.DESCRIPTION., desc_icd=X.ICD.9.CM.CODE.DESCRIPTION.)
  icd9$diag <- gsub("'", "", icd9$icd)
  icd9$diag <- sub("\\s+$", "", icd9$diag)    #remove trailing whitespace
  icd9$category1 <- as.numeric(gsub("'", "", icd9$category))
  
  ccs10 <- read.csv('V:/Staff/JXG4303/CHARS/CCS_HCUP/CCS_ICD10/ccs_dx_icd10cm_2017/ccs_dx_icd10cm_2017.csv', stringsAsFactors = F)
  icd10 <- ccs10 %>% 
    select(1:4) %>% 
    rename(icd=X.ICD.10.CM.CODE., category=X.CCS.CATEGORY.,
           desc_hcup=X.CCS.CATEGORY.DESCRIPTION., desc_icd=X.ICD.10.CM.CODE.DESCRIPTION.)
  icd10$diag <- gsub("'", "", icd10$icd)
  icd10$diag <- sub("\\s+$", "", icd10$diag)
  icd10$category1 <- as.numeric(gsub("'", "", icd10$category))


#################
#hospitalization#
#################
tmp.inp <- eval(parse(text=paste0("data.frame(haven::read_sas('Y:/Datasets/Restricted/CHARS/CHARS/SAS_REDACTED/chr_r", year, ".sas7bdat'))")))
tab.cnt.inp <- table(tmp.inp$DIAGCNT)
hcup_chars.inp<- tmp.inp %>% select(SEQ_NO_ENC, STATERES, COUNTYRES, AGE, SEX, 
                                    HISPANIC, RACE_WHT, RACE_BLK, RACE_AMI, RACE_ASI, RACE_HAW, 
                                    PAYER1, PAYER2, PAYER3)
tmp.inp1 <- tmp.inp %>% select(SEQ_NO_ENC, DIS_DATE, contains("DIAG"))

#missing discharge date
tmp.inp1_nodate <- tmp.inp1[is.na(tmp.inp1$DIS_DATE)==1,]

###inpute the missing discharge dates
#icd nunbers exit in both icd9 and icd10
dup <- c(icd9$diag, icd10$diag)[duplicated(c(icd9$diag, icd10$diag))]

tmp.inp1_nodate$ind9 <- 0
tmp.inp1_nodate$ind10 <- 0
tmp.inp1_nodate$ind910 <- 0

for (i in 1:dim(tmp.inp1_nodate)[1]) {
  for (j in 1:tmp.inp1_nodate$DIAGCNT[i])  {
    if (eval(parse(text=paste0("tmp.inp1_nodate$DIAG", j, "[i] %in% dup")))) {tmp.inp1_nodate$ind910[i]=tmp.inp1_nodate$ind910[i]+1}
    if (eval(parse(text=paste0("tmp.inp1_nodate$DIAG", j, "[i] %in% icd9$diag")))) {tmp.inp1_nodate$ind9[i]=tmp.inp1_nodate$ind9[i]+1}
    if (eval(parse(text=paste0("tmp.inp1_nodate$DIAG", j, "[i] %in% icd10$diag")))) {tmp.inp1_nodate$ind10[i]=tmp.inp1_nodate$ind10[i]+1}
  }
}
#table(tmp.inp1_nodate$ind10, tmp.inp1_nodate$ind9, tmp.inp1_nodate$ind910)
tmp.inp1_nodate$DIS_DATE[tmp.inp1_nodate$ind910==1 & tmp.inp1_nodate$ind9>1 & tmp.inp1_nodate$ind10==1] <- '2015-09-30'
tmp.inp1_nodate$DIS_DATE[tmp.inp1_nodate$ind910==1 & tmp.inp1_nodate$ind9==1 & tmp.inp1_nodate$ind10>1] <- '2015-10-01'
tmp.inp1_nodate$DIS_DATE[tmp.inp1_nodate$ind910==0 & tmp.inp1_nodate$ind9>0 & tmp.inp1_nodate$ind10==0] <- '2015-09-30'
tmp.inp1_nodate$DIS_DATE[tmp.inp1_nodate$ind910==0 & tmp.inp1_nodate$ind9==0 & tmp.inp1_nodate$ind10>0] <- '2015-10-01'

tmp.inp1_nodate_impute <- select(tmp.inp1_nodate[is.na(tmp.inp1_nodate$DIS_DATE)==0,], SEQ_NO_ENC, DIS_DATE, contains("DIAG"))
tmp.inp1.impute <- rbind(tmp.inp1[is.na(tmp.inp1$DIS_DATE)==0,], tmp.inp1_nodate_impute)
####

tmp.inp1_9 <- tmp.inp1.impute[tmp.inp1.impute$DIS_DATE<'2015-10-01', ]
tmp.inp1_10 <- tmp.inp1.impute[tmp.inp1.impute$DIS_DATE>='2015-10-01', ]

hcup_chars.inp_9 <- hcup_chars.inp[hcup_chars.inp$SEQ_NO_ENC %in% tmp.inp1_9$SEQ_NO_ENC,]
hcup_chars.inp_10 <- hcup_chars.inp[hcup_chars.inp$SEQ_NO_ENC %in% tmp.inp1_10$SEQ_NO_ENC,]

for (j in 1:25) {
  a <- eval(parse(text=paste0("select(tmp.inp1_9, SEQ_NO_ENC, DIAG", j, ") %>% 
                              left_join(icd9[,c('diag', 'category1')], by=c('DIAG", j, "'='diag')) %>% 
                              rename(diag", j, "_ccs=category1)")))
  hcup_chars.inp_9 <- full_join(hcup_chars.inp_9, a, by='SEQ_NO_ENC')
}

for (j in 1:25) {
  a <- eval(parse(text=paste0("select(tmp.inp1_10, SEQ_NO_ENC, DIAG", j, ") %>% 
                              left_join(icd10[,c('diag', 'category1')], by=c('DIAG", j, "'='diag')) %>% 
                              rename(diag", j, "_ccs=category1)")))
  hcup_chars.inp_10 <- full_join(hcup_chars.inp_10, a, by='SEQ_NO_ENC')
}

hcup_chars.inp_impute <- rbind(hcup_chars.inp_9, hcup_chars.inp_10)

write.csv(tab.cnt.inp, file=paste0('V:\\Staff\\JXG4303\\CHARS\\results\\CHARS_CCS_', year, '_number_diagnosis_hospitalization.csv'), row.names=F)
write.csv(hcup_chars.inp_impute, file=paste0('V:\\Staff\\JXG4303\\CHARS\\results\\CHARS_CCS_', year, '_hospitalization.csv'), row.names=F)
#write.table(hcup_chars.inp_impute, file=paste0('V:\\Staff\\JXG4303\\CHARS\\results\\CHARS_CCS_', year, '_hospitalization.txt'), row.names=F)

#############
#observation#
#############
tmp.obs <- eval(parse(text=paste0("data.frame(haven::read_sas('Y:/Datasets/Restricted/CHARS/CHARS/SAS_REDACTED/Observation/chro_r", year, ".sas7bdat'))")))
tab.cnt.obs <- table(tmp.obs$DIAGCNT)
hcup_chars.obs<- tmp.obs %>% select(SEQ_NO_ENC, STATERES, COUNTYRES, AGE, SEX, 
                                    HISPANIC, RACE_WHT, RACE_BLK, RACE_AMI, RACE_ASI, RACE_HAW, 
                                    PAYER1, PAYER2, PAYER3)
tmp.obs1 <- tmp.obs %>% select(SEQ_NO_ENC, DIS_DATE, contains("DIAG"))

#no missing discharge date
tmp.obs1_nodate <- tmp.obs1[is.na(tmp.obs1$DIS_DATE)==1,]

tmp.obs1_9 <- tmp.obs1[tmp.obs1$DIS_DATE<'2015-10-01', ]
tmp.obs1_10 <- tmp.obs1[tmp.obs1$DIS_DATE>='2015-10-01', ]

hcup_chars.obs_9 <- hcup_chars.obs[hcup_chars.obs$SEQ_NO_ENC %in% tmp.obs1_9$SEQ_NO_ENC,]
hcup_chars.obs_10 <- hcup_chars.obs[hcup_chars.obs$SEQ_NO_ENC %in% tmp.obs1_10$SEQ_NO_ENC,]

for (j in 1:25) {
  a <- eval(parse(text=paste0("select(tmp.obs1_9, SEQ_NO_ENC, DIAG", j, ") %>% 
                              left_join(icd9[,c('diag', 'category1')], by=c('DIAG", j, "'='diag')) %>% 
                              rename(diag", j, "_ccs=category1)")))
  hcup_chars.obs_9 <- full_join(hcup_chars.obs_9, a, by='SEQ_NO_ENC')
}

for (j in 1:25) {
  a <- eval(parse(text=paste0("select(tmp.obs1_10, SEQ_NO_ENC, DIAG", j, ") %>% 
                              left_join(icd10[,c('diag', 'category1')], by=c('DIAG", j, "'='diag')) %>% 
                              rename(diag", j, "_ccs=category1)")))
  hcup_chars.obs_10 <- full_join(hcup_chars.obs_10, a, by='SEQ_NO_ENC')
}

hcup_chars.obs_final <- rbind(hcup_chars.obs_9, hcup_chars.obs_10)

write.csv(tab.cnt.obs, file=paste0('V:\\Staff\\JXG4303\\CHARS\\results\\CHARS_CCS_', year, '_number_diagnosis_observation.csv'), row.names=F)
write.csv(hcup_chars.obs_final, file=paste0('V:\\Staff\\JXG4303\\CHARS\\results\\CHARS_CCS_', year, '_observation.csv'), row.names=F)
#write.table(hcup_chars.obs_final, file=paste0('V:\\Staff\\JXG4303\\CHARS\\results\\CHARS_CCS_', year, '_observation.txt'), row.names=F)






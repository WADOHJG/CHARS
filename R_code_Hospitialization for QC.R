library(dplyr)
########################################
#create label file and geographic file##
########################################
lb <- read.csv('V:/Staff/JXG4303/CHARS/CCS_HCUP/CCS_ICD9/Single_Level_CCS_2015/dxlabel 2015.csv', skip=4, header=F, stringsAsFactors = F)
names(lb) <- c('category', 'label')
lb$label2 <- lb$label
lb$label2[lb$category %in% 177:196] <- 'Pregnancy, childbirth'
lb$label2[lb$category %in% 218:224] <- 'Newborns, neonates'
lb$label3 <- lb$label2
lb$label3[lb$category %in% 49:50] <- 'Diabetes'

meps <- xlsx::read.xlsx('V:/Staff/JXG4303/CHARS/CCS_HCUP/CCS_grouping_MEPS_AHRQ_2007.xlsx', sheetIndex = 1, stringsAsFactors=FALSE)
meps$v1 <- sub(' - ', ':', meps$CCS.Codes)
lb$meps <- NA
for (i in 1:dim(meps)[1]) {
  lb$meps[lb$category %in% eval(parse(text=paste0('c(', noquote(meps$v1[i]),')')))] <- meps$Condition.Category[i]
}
lb$meps[is.na(lb$meps)==1] <- 'External causes of injury and poisoning'   #########################


countyname <- read.csv('V:/Staff/JXG4303/county_code_ACH_2017.csv', stringsAsFactors = F)[-1,]
countyname$no <- 1:39
ach <- unique(countyname$ACH)

####################################################################
####################################################################
type= 'both'    # 'hospitalization', 'observation', or 'both'
year= 2016    # 2014-2016

if (type=='hospitalization') {dat <- read.csv(paste0('V:/Staff/JXG4303/CHARS/results/CHARS_CCS_', year, '_hospitalization.csv'), stringsAsFactors=F)}
if (type=='observation') {dat <- read.csv(paste0('V:/Staff/JXG4303/CHARS/results/CHARS_CCS_', year, '_observation.csv'), stringsAsFactors=F)}
if (type=='both') {
  dat1 <- read.csv(paste0('V:/Staff/JXG4303/CHARS/results/CHARS_CCS_', year, '_hospitalization.csv'), stringsAsFactors=F)
  dat2 <- read.csv(paste0('V:/Staff/JXG4303/CHARS/results/CHARS_CCS_', year, '_observation.csv'), stringsAsFactors=F)
  dat <- rbind(dat1, dat2)
}
dat <- filter(dat, STATERES=='WA')   #WA residents only

#select the diagnosis of interest
#from CCS (285)
k=57; diagnosis= k; label=lb$label[k]    
#or from MEPS (61) 
k=10; diagnosis= paste0('c(', noquote(meps$v1[k]),')'); label=meps$Condition.Category[k]   

#############
#state level#
#############
tmp <- matrix(NA, dim(dat)[1], 25)
for (i in 1:25) {tmp[,i] <- select(dat, ends_with('_ccs'))[,i] %in% eval(parse(text=diagnosis))}

diag1 <- sum(tmp[,1])
diag9 <- sum(apply(tmp[, 1:9]==TRUE, 1, sum)>0)
diag25 <- sum(apply(tmp==TRUE, 1, sum)>0)
cat(paste(year, type, ',', label), "\n",
    paste('1st diagnosis=', diag1, ', 9 diagnoses=', diag9, ', 25 diagnoses=', diag25))

########
#county#
########
county='King'
dat1 <- dat %>% left_join(countyname, by=c('COUNTYRES'='no')) %>%
  filter(name==county)
tmp <- matrix(NA, dim(dat1)[1], 25)
for (i in 1:25) {tmp[,i] <- select(dat1, ends_with('_ccs'))[,i] %in% eval(parse(text=diagnosis))}
diag1 <- sum(tmp[,1])
diag9 <- sum(apply(tmp[, 1:9]==TRUE, 1, sum)>0)
diag25 <- sum(apply(tmp==TRUE, 1, sum)>0)
cat(paste(year, type, ',', label, ',', county, 'County'), "\n",
    paste('1st diagnosis=', diag1, ', 9 diagnoses=', diag9, ', 25 diagnoses=', diag25))

#####
#ACH#
#####
ach='Olympic Community of Health'
dat1 <- dat %>% left_join(countyname, by=c('COUNTYRES'='no')) %>%
  filter(ACH==ach)

tmp <- matrix(NA, dim(dat1)[1], 25)
for (i in 1:25) {tmp[,i] <- select(dat1, ends_with('_ccs'))[,i] %in% eval(parse(text=diagnosis))}
diag1 <- sum(tmp[,1])
diag9 <- sum(apply(tmp[, 1:9]==TRUE, 1, sum)>0)
diag25 <- sum(apply(tmp==TRUE, 1, sum)>0)
cat(paste(year, type, ',', label, ', ACH:', ach), "\n",
    paste('1st diagnosis=', diag1, ', 9 diagnoses=', diag9, ', 25 diagnoses=', diag25))





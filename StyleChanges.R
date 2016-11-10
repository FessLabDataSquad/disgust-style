# This is the code that produced the results of Study 2 reported in
#        Sparks, Fessler, Chan, Ashokkumar & Holbrook
#        "Disgust as a mechanism for decision making under risk"
# Contact Adam Sparks (adspar AT gmail) with questions.
# There may be many questions because 
# I'm an R noob and haven't yet learned 
# programming efficiency and elegance





#import
#setwd()
study1<-read.csv("data_forR_study1.csv",na.strings=c(""))
study2<-read.csv("data_forR_study2.csv",na.strings=c(""))
study2.items<-read.csv("itemsstudy2.csv",na.strings=c(""), row.names=1, header=FALSE)
names(study2.items) <-  "item"


### study1 cleaning ###

#check variable classes, update as needed
#sapply(study1, class)
study1$sex.code<- as.factor (study1$sex.code)
study1$sex.orient <- as.factor (study1$sex.orient)

#create variable for completion time
library(lubridate)
study1$time<-as.numeric(difftime(mdy_hms(study1$end), mdy_hms(study1$start))) 

#time cutoff analysis
mean(study1$time)
MIN_ACCEPTABLE_TIME = 5
MAX_ACCEPTABLE_TIME = 45 #Not sure if you want to name it "max acceptable time" since you end up keeping times greater than this
length(subset(study1$time, study1$time<MIN_ACCEPTABLE_TIME))
length(subset(study1$time, study1$time>MAX_ACCEPTABLE_TIME)) 

#cut point: keep >5 minutes
study1<-subset(study1, study1$time>MIN_ACCEPTABLE_TIME) #sample now n=988 

#attention check #1 (phone)
length (subset (study1$check_low, study1$check_low!=1)) #what is 1?
length (subset (study1$check_low, study1$check_low==1)) #what is 1?
length (subset (study1$check_low, study1$check_low>2)) #what is 2?

# keep 1s and 2s (should I keep 2s?)
study1<- subset (study1, study1$check_low==1 | study1$check_low==2) 

#attention check #2 (alphabet)
LETTERS_IN_ALPHABET = 26
EXTRA_LETTER = LETTERS_IN_ALPHABET + 1
length (subset (study1$check_alphabet, study1$check_alphabet==LETTERS_IN_ALPHABET))
length (subset (study1$check_alphabet, study1$check_alphabet==EXTRA_LETTER)) 
length (subset (study1$check_alphabet, study1$check_alphabet>EXTRA_LETTER)) 
length (subset (study1$check_alphabet, study1$check_alphabet<LETTERS_IN_ALPHABET)) 

#eliminate all but 26 and 27
#27 was common enough that it seems like a counting mistake, rather than attention failure
study1<- subset (study1, study1$check_alphabet==LETTERS_IN_ALPHABET | study1$check_alphabet==EXTRA_LETTER)

#calculate height, ignoring half-inches for now
FEET_TO_INCHES = 12
study1$height1<-(study1$feet1)*FEET_TO_INCHES+(study1$inches1)#+(study1$halfinches1-1)*0.5   
study1$height2<-(study1$feet2)*FEET_TO_INCHES+(study1$inches2)#+(study1$halfinches2-1)*0.5  
study1$height <- pmax (study1$height1, study1$height2, na.rm=TRUE)

#reverse-code such that higher is more harm avoidant
SCALE_MAX = 5
CONVERSION_FACTOR = SCALE_MAX + 1  #Not sure if you want to name it "conversion factor" because this is really vague but I couldn't think of anything else! 
study1$seatbelt <- CONVERSION_FACTOR-study1$seatbelt  
study1$jaywalk <- CONVERSION_FACTOR-study1$jaywalk


### study 2 cleaning ###

#calculate height
SCALE_TO_FEET = 2
SCALE_TO_INCHES = 1
study2$height<-(study2$feet.code+SCALE_TO_FEET)*FEET_TO_INCHES+(study2$inches.code-SCALE_TO_INCHES) 

#recode the only participant to choose "other"
#sex labels 1=M, 2=F, 3=other
PARTICIPANT_NUMBER = 81
study2$sex.code[PARTICIPANT_NUMBER]<-NA 
study2$sex.code<- as.factor (study2$sex.code)

#reverse code scale items
study2$ha5 <- CONVERSION_FACTOR-study2$ha5
study2$ha9 <- CONVERSION_FACTOR-study2$ha9
study2$ha11 <- CONVERSION_FACTOR-study2$ha11
study2$ha11a <- CONVERSION_FACTOR-study2$ha11a

#create variable for completion time
study2$time<-as.numeric(difftime(mdy_hm(study2$end), mdy_hm(study2$start))) 

#removes 25 participants who finished in less than 5 minutes 
study2<-subset(study2, study2$time>=MIN_ACCEPTABLE_TIME)  #not sure if this is right?

#honestresponse -- there are no 3s, so don't filter out anyone
#parent labels 1=yes, 2=no
#polical orientation: high is conservative
#computertype 1=desktop, 2=laptop/notebook, 3=phone



### Scales ###

library(psych)

# both studies
TDDS.sexual    <- c("t.sex1", "t.sex2", "t.sex3", "t.sex4", "t.sex5", "t.sex6", "t.sex7")
TDDS.pathogen  <- c("t.path1", "t.path2", "t.path3", "t.path4", "t.path5", "t.path6", "t.path7")
TDDS.moral     <- c("t.mo1", "t.mo2", "t.mo3", "t.mo4", "t.mo5", "t.mo6", "t.mo7")
TDDS.all       <- c(TDDS.sexual, TDDS.pathogen, TDDS.moral)

DOSPERT.social       <- c("d.s1",  "d.s2",  "d.s3",  "d.s4",  "d.s5",  "d.s6")
DOSPERT.recreational <- c("d.r1",  "d.r2",  "d.r3",  "d.r4",  "d.r5",  "d.r6")
DOSPERT.financial    <- c("d.f1",  "d.f2",  "d.f3",  "d.f4",  "d.f5",  "d.f6")
DOSPERT.healthsafety <- c("d.hs1", "d.hs2", "d.hs3", "d.hs4", "d.hs5", "d.hs6")
#DOSPERT.harmavoid    <- c("d.hs3",  "d.hs4",    "d.hs5",  "d.hs6") #harm avoiding DOSERT items 
DOSPERT.ethical      <- c("d.e1",  "d.e2",  "d.e3",  "d.e4",  "d.e5",  "d.e6")
DOSPERT.all          <- c(DOSPERT.social, DOSPERT.recreational, DOSPERT.financial, DOSPERT.healthsafety, DOSPERT.ethical)

#Study 1
harm.avoidance1  <- c("seatbelt","lockdoor","jaywalk")
allscaleitems1  <- c(TDDS.all, DOSPERT.all, harm.avoidance1)

#make key and score the scales
key.all1<-make.keys(study1,list(
  TDDS.sexual=     TDDS.sexual, 
  TDDS.moral=   TDDS.moral, 
  TDDS.pathogen=TDDS.pathogen,
  TDDS.all= TDDS.all,
  d.social      = DOSPERT.social, 
  d.recreational= DOSPERT.recreational,
  d.financial   = DOSPERT.financial,
  d.healthsafety= DOSPERT.healthsafety,
  d.ethical     = DOSPERT.ethical, 
  DOSPERT       = DOSPERT.all,
  #  d.harmavoid   = DOSPERT.harmavoid,
  harm.avoid1    = harm.avoidance1))

scores1 <- scoreItems(key.all1[allscaleitems1,], study1[,allscaleitems1], missing=TRUE, impute="none")

#save scores as study1 variables
a <- as.data.frame(cbind(scores1$scores, ID=study1$ID))
study1 <- merge(study1, a, by="ID")
rm(a)

### FINAL study1 data FILE ###
### export it for sharing ###
### dim: 941 x 93
write.csv (study1, "finaldata_study1.csv")


#Study 2
harm.avoidance2  <- c("ha1","ha2","ha3","ha4","ha5","ha6","ha7","ha8","ha9","ha10","ha11")
Curtis.relevant <- c("c.a.r","c.b.r","c.c.r","c.d.r","c.e.r","c.f.r","c.g.r", "c.gummaggot")
Curtis.relevant.limited <- c("c.a.r","c.b.r","c.c.r","c.d.r","c.e.r","c.f.r","c.g.r")
Curtis.irrelevant<-c("c.a.i","c.b.i","c.c.i", "c.d.i","c.e.i","c.f.i","c.g.i") #add the other non-paired?
# we don't use irrelevant for many analysis though
allscaleitems2  <- c(TDDS.all, DOSPERT.all, Curtis.relevant, Curtis.irrelevant, harm.avoidance2)

#make key and score the scales
key.all2<-make.keys(study2,list(
  TDDS.sexual=     TDDS.sexual, 
  TDDS.moral=   TDDS.moral, 
  TDDS.pathogen=TDDS.pathogen,
  TDDS.all= TDDS.all,
  Curtis = Curtis.relevant,
  #  Curtis.limited=Curtis.relevant.limited,
  #  c.irrelevant = Curtis.irrelevant,
  d.social      = DOSPERT.social, 
  d.recreational= DOSPERT.recreational,
  d.financial   = DOSPERT.financial,
  d.healthsafety= DOSPERT.healthsafety,
  d.ethical     = DOSPERT.ethical, 
  DOSPERT       = DOSPERT.all,
  #  d.harmavoid   = DOSPERT.harmavoid,
  harm.avoid2    = harm.avoidance2))

scores2 <- scoreItems(key.all2[allscaleitems2,], study2[,allscaleitems2], missing=TRUE, impute="none")

#save scores as variables
a <- as.data.frame(cbind(scores2$scores, ID=study2$ID))
study2 <- merge(study2, a, by="ID")
rm(a)

### FINAL study2 data FILE
### export it for sharing
### dim: 473 x 115
write.csv (study2, "finaldata_study2.csv")


## SCALE VALIDATION ##

#Study 2
#Curtis Disgust item paired comparisons
tests.curtis.items <- list()
tests.curtis.items[["pair a"]] <- t.test(study2$c.a.r, study2$c.a.i, paired=TRUE, alternative = "greater", na.action = na.omit)
tests.curtis.items[["pair b"]] <- t.test(study2$c.b.r, study2$c.b.i, paired=TRUE, alternative = "greater", na.action = na.omit)
tests.curtis.items[["pair c"]] <- t.test(study2$c.c.r, study2$c.c.i, paired=TRUE, alternative = "greater", na.action = na.omit)
tests.curtis.items[["pair d"]] <- t.test(study2$c.d.r, study2$c.d.i, paired=TRUE, alternative = "greater", na.action = na.omit)
tests.curtis.items[["pair e"]] <- t.test(study2$c.e.r, study2$c.e.i, paired=TRUE, alternative = "greater", na.action = na.omit)
tests.curtis.items[["pair f"]] <- t.test(study2$c.f.r, study2$c.f.i, paired=TRUE, alternative = "greater", na.action = na.omit)
tests.curtis.items[["pair g"]] <- t.test(study2$c.g.r, study2$c.g.i, paired=TRUE, alternative = "greater", na.action = na.omit)

Curtis.paired.comparisons <-t(sapply(tests.curtis.items, function(x) {
  c(x$estimate,
    ci.lower = x$conf.int[1],
    ci.upper = x$conf.int[2],
    p.value = x$p.value,
    x$parameter)
}))

HIGH_PRECISION = 3
print(Curtis.paired.comparisons, digits=HIGH_PRECISION)
########################
#####   TABLE   ########
########################

# reported in supplement
#Pathogen-relevant rated more disgusting than paired stimuli
write.table(Curtis.paired.comparisons, row.names=TRUE, file="CurtisPairedComparisonTable.csv", sep=",") 

#Harm Avoidance
#driver's license doesn't matter for seatbelt so can ignore ha1a
t.test(study2$ha1 ~ study2$ha1a, na.action=na.omit)


########################
#####   TABLE   ########
########################
#make a table with the alpha values
# Table 2 in ms
alphatable <- merge( t(scores1$alpha), t(scores2$alpha), by="row.names"   , all=TRUE  )
colnames(alphatable)<- c("(Sub)scale", "Study 1", "Study 2")
print(alphatable, digits=HIGH_PRECISION, row.names = FALSE, right=F)  #how many sig figs
write.table(alphatable, row.names=FALSE, file="AlphaTable.csv", sep=",") 



### Analysis of Hypotheses ###

###########################################################################################
#Females, compared to males, will show more disgust and harm avoidance and less risk taking
###########################################################################################

# DISGUST + HARM AVOIDANCE SEX DIFF
# study 1 
#raw effect and p values based on Welsh's t
measure.disgustharm1 <- list ("TDDS.all", "TDDS.sexual", "TDDS.moral", "TDDS.pathogen", "harm.avoid1")
tests.sex.disgustharm1 <-  lapply (measure.disgustharm1, function (x) 
{t.test (study1[,x] ~ study1$sex.code, alternative="less", na.action = na.omit)}) 

sex.differences.disgustharm1 <- as.data.frame(
  t(sapply(tests.sex.disgustharm1, function(x) {
    c( x$estimate[2]-x$estimate[1],
       x$p.value,
       x$parameter)
  })))
colnames(sex.differences.disgustharm1)<- c("raw effect", "p.value","df")
row.names(sex.differences.disgustharm1) <- measure.disgustharm1
print(sex.differences.disgustharm1, digits=HIGH_PRECISION )

# calculate d
library(effsize)
d.sex.disgustharm1 <-  lapply (measure.disgustharm1, function (x) 
HIGH_CONFIDENCE = 0.95
{cohen.d (study1[,x] ~ study1$sex.code, na.rm=T,conf.level=HIGH_CONFIDENCE)})

d.sex.differences.disgustharm1 <- as.data.frame(
  t(sapply(d.sex.disgustharm1, function(x) {
    c(-x$estimate, #these are reversed to be consistent with meta-analysis effect direction
      -x$conf.int[2] , 
      -x$conf.int[1])
  })))

colnames(d.sex.differences.disgustharm1)<- c("d", "-95%CI" , "+95%CI")
row.names(d.sex.differences.disgustharm1) <- measure.disgustharm1
print(d.sex.differences.disgustharm1, digits=HIGH_PRECISION)

#summarize study 1 disgust & harm avoid
sex.disgustharm1 <- merge(d.sex.differences.disgustharm1,  sex.differences.disgustharm1, by="row.names", all=TRUE  )
print(sex.disgustharm1, digits=HIGH_PRECISION)


# study 2 disgust & harm avoidance raw effect and p values based on Welsh's t
measures.disgustharm2 <- list ("TDDS.all", "TDDS.sexual", "TDDS.moral", "TDDS.pathogen", "Curtis", "harm.avoid2")
t.tests.sex.disgustharm2 <-  lapply (measures.disgustharm2, function (x) 
{t.test (study2[,x] ~ study2$sex.code, alternative="less", na.action = na.omit)}) 

t.sex.differences.disgustharm2 <- as.data.frame(
  t(sapply(t.tests.sex.disgustharm2, function(x) {c(
    x$estimate[2]-x$estimate[1],
    x$p.value,
    x$parameter)
  })))
colnames(t.sex.differences.disgustharm2)<- c("raw effect" ,"p.value","df")
row.names(t.sex.differences.disgustharm2) <- measures.disgustharm2
print(t.sex.differences.disgustharm2, digits=HIGH_PRECISION)

#  calculate d
d.sex.disgustharm2 <-  lapply (measures.disgustharm2, function (x) 
{cohen.d (study2[,x], study2$sex.code,  na.rm=T,conf.level=HIGH_CONFIDENCE)})

d.sex.differences.disgustharm2 <- as.data.frame(
  t(sapply(d.sex.disgustharm2, function(x) {
    c(-x$estimate, #these are reversed to be consistent with meta-analysis effect direction
      -x$conf.int[2] , 
      -x$conf.int[1])
  })))

colnames(d.sex.differences.disgustharm2)<- c("d", "-95%CI" , "+95%CI")
row.names(d.sex.differences.disgustharm2) <- measures.disgustharm2
print(d.sex.differences.disgustharm2, digits=HIGH_PRECISION)

#summarize study 2 disgust
sex.disgustharm2 <- merge(d.sex.differences.disgustharm2, t.sex.differences.disgustharm2,  by="row.names", all=TRUE  )
print(sex.disgustharm2, digits=HIGH_PRECISION)

########################
#| | | TABLE  | | | | |#
########################
DisgustSexDifferences <- merge(x=sex.disgustharm1 , y=sex.disgustharm2,
                               by="Row.names",all=T, suffixes=c(".study1", ".study2"))
print(DisgustSexDifferences, digits=HIGH_PRECISION)
#write.table(DisgustSexDifferences, file="DisgustSex.csv", sep=",", row.names=F) 

#simpler table for MS Table 3
SexDiffs.DigustHarm.MS<-merge(d.sex.differences.disgustharm1,  d.sex.differences.disgustharm2, by="row.names", all=TRUE, suffixes=c(".study1", ".study2")  )
print(SexDiffs.DigustHarm.MS, digits=HIGH_PRECISION)
write.table(SexDiffs.DigustHarm.MS, file="DisgustHarmSex.csv", sep=",", row.names=F)


# RISK TAKING SEX DIFF

#study 1 risk taking
# raw effect and p
measures.risk1 <-list("DOSPERT","d.social","d.recreational","d.financial","d.healthsafety","d.ethical","wager")
tests.sex.risk1 <-  lapply (measures.risk1, function (x) {t.test (study1[,x] ~ study1$sex.code, alternative="greater", na.action = na.omit)}) 
sex.differences.risk1 <- as.data.frame(
  t(sapply(tests.sex.risk1, function(x) {
    c(x$estimate[1] - x$estimate[2], 
      x$p.value,
      x$parameter)
  })))
colnames(sex.differences.risk1)<- c("raw.effect", "p.value","df")
row.names(sex.differences.risk1) <- measures.risk1
print(as.data.frame(sex.differences.risk1), digits=HIGH_PRECISION)

# d and 95% CIs
d.sex.risk1 <-  lapply (measures.risk1, function (x) 
{cohen.d (study1[,x] ~ study1$sex.code, na.rm=T,conf.level=HIGH_CONFIDENCE)}) 

d.sex.differences.risk1 <- as.data.frame(
  t(sapply(d.sex.risk1, function(x) {
    c(x$estimate, #these are reversed to be consistent with meta-analysis effect direction
      x$conf.int[1] , 
      x$conf.int[2])
  })))

colnames(d.sex.differences.risk1)<- c("d", "-95%CI" , "+95%CI")
row.names(d.sex.differences.risk1) <- measures.risk1
print(d.sex.differences.risk1, digits=HIGH_PRECISION)

#summarize study 1 risk
sex.risk1 <- merge(d.sex.differences.risk1,  sex.differences.risk1, by="row.names", all=TRUE  )
print(sex.risk1, digits=HIGH_PRECISION)


#study 2 risk taking
# raw effect and p
measures.risk2 <- list ("DOSPERT","d.social","d.recreational","d.financial","d.healthsafety","d.ethical")
tests.sex.risk2 <-  lapply (measures.risk2, function (x) {t.test (study2[,x] ~ study2$sex.code, alternative="greater", na.action = na.omit)}) 
sex.differences.risk2 <- as.data.frame(
  t(sapply(tests.sex.risk2, function(x) {
    c(x$estimate[1] - x$estimate[2], 
      x$p.value,
      x$parameter)
  })))
colnames(sex.differences.risk2)<- c("raw.effect", "p.value", "df")
row.names(sex.differences.risk2) <- measures.risk2
print(as.data.frame(sex.differences.risk2), digits=HIGH_PRECISION)

# d and 95% CIs
d.sex.risk2 <-  lapply (measures.risk2, function (x) 
{cohen.d (study2[,x] ~ study2$sex.code, na.rm=T,conf.level=HIGH_CONFIDENCE)}) 

d.sex.differences.risk2 <- as.data.frame(
  t(sapply(d.sex.risk2, function(x) {
    c(x$estimate, #these are reversed to be consistent with meta-analysis effect direction
      x$conf.int[1] , 
      x$conf.int[2])
  })))

colnames(d.sex.differences.risk2)<- c("d", "-95%CI" , "+95%CI")
row.names(d.sex.differences.risk2) <- measures.risk2
print(d.sex.differences.risk2, digits=HIGH_PRECISION)

#summarize study 2 risk
sex.risk2 <- merge(d.sex.differences.risk2,  sex.differences.risk2, by="row.names", all=TRUE  )
print(sex.risk2, digits=HIGH_PRECISION)

########################
#####   TABLE   ########
########################
RiskSexDifferences <- merge(sex.risk1 , sex.risk2,
                            by="Row.names",all=T, suffixes=c(".study1", ".study2"))

row.order <- c(5,4,3,1,2,6,7)

print(RiskSexDifferences[order(row.order),], digits=HIGH_PRECISION)
#write.table(RiskSexDifferences[order(row.order),], file="RiskSex.csv", sep=",", row.names=F) 


# simpler table for MS -- Table 3
RiskSexDiff.MS <- merge(d.sex.differences.risk1,  d.sex.differences.risk2, by="row.names", all=TRUE, suffixes=c(".study1", ".study2")  )
print(RiskSexDiff.MS, digits=HIGH_PRECISION)
write.table(RiskSexDiff.MS, file="RiskSex.csv", sep=",", row.names=F) 

# manually combine those last 2 tables for manuscript





# HARM AVOIDANCE ITEM-BY-ITEM SEX DIFF
#report in supplement
#study 1 harm avoidance raw effect & p
tests.sex.ha1 <-  lapply (harm.avoidance1, function (x) {t.test (study1[,x] ~ study1$sex.code, 
                                                                 alternative="less", 
                                                                 na.action = na.omit)}) 
sex.differences.ha1 <- as.data.frame(
  t(sapply(tests.sex.ha1, function(x) {
    c(x$estimate[2] - x$estimate[1],
      x$p.value,
      x$parameter)
  })))
colnames(sex.differences.ha1)<- c("raw effect", "p.value", "df")
row.names(sex.differences.ha1) <- harm.avoidance1
print (sex.differences.ha1, digits=HIGH_PRECISION, row.names = TRUE, right=F)

#study 1 d, 95% CI
d.sex.ha1 <-  lapply (harm.avoidance1, function (x) 
{cohen.d (study1[,x] ~ study1$sex.code, na.rm=T,conf.level=HIGH_CONFIDENCE)}) 

d.sex.differences.ha1 <- as.data.frame(
  t(sapply(d.sex.ha1, function(x) {
    c(-x$estimate, #these are reversed to be consistent with meta-analysis effect direction
      -x$conf.int[2] , 
      -x$conf.int[1])
  })))

colnames(d.sex.differences.ha1)<- c("d", "-95%CI" , "+95%CI")
row.names(d.sex.differences.ha1) <- harm.avoidance1
print(d.sex.differences.ha1, digits=HIGH_PRECISION)

#summarize study 1 harm avoidance
sex.ha1 <- merge(d.sex.differences.ha1,  sex.differences.ha1, by="row.names", all=TRUE  )
print(sex.ha1, digits=HIGH_PRECISION)
########################
#####   TABLE   ########
########################
write.table(sex.ha1, file="HarmSex1.csv", sep=",", row.names=F) 

#study 2 harm avoidance
tests.sex.ha2 <-  lapply (harm.avoidance2, function (x) {t.test (study2[,x] ~ study2$sex.code, alternative="less", na.action = na.omit)}) 
sex.differences.ha2 <- as.data.frame(
  t(sapply(tests.sex.ha2, function(x) {
    c(x$estimate[2] - x$estimate[1],
      x$p.value,
      x$parameter)
  })))
colnames(sex.differences.ha2)<- c("raw effect", "p.value", "df")
row.names(sex.differences.ha2) <- harm.avoidance2
print (sex.differences.ha2, digits=HIGH_PRECISION, row.names = T, right=F)

#study 2 d, 95% CI
d.sex.ha2 <-  lapply (harm.avoidance2, function (x) 
{cohen.d (study2[,x] ~ study2$sex.code, na.rm=T,conf.level=HIGH_CONFIDENCE)}) 

d.sex.differences.ha2 <- as.data.frame(
  t(sapply(d.sex.ha2, function(x) {
    c(-x$estimate, #these are reversed to be consistent with meta-analysis effect direction
      -x$conf.int[2] , 
      -x$conf.int[1])
  })))

colnames(d.sex.differences.ha2)<- c("d", "-95%CI" , "+95%CI")
row.names(d.sex.differences.ha2) <- harm.avoidance2
print(d.sex.differences.ha2, digits=HIGH_PRECISION)

#summarize study 2 harm avoidance
sex.ha2 <- merge(d.sex.differences.ha2,  sex.differences.ha2, by="row.names" , all=TRUE  )
sex.ha2<-merge(study2.items, sex.ha2, by.x="row.names", by.y="Row.names")
print(sex.ha2, digits=HIGH_PRECISION)
########################
#####   TABLE   ########
########################
write.table(sex.ha2, file="HarmSex2.csv", sep=",", row.names=F) 

# men more flu shots??
library(Rmisc)
summarySE(data = study2, measurevar= "ha7", groupvars = "sex.code",
          na.rm = TRUE, conf.interval = HIGH_CONFIDENCE)
#yes, men more flu shots. pain of needle?



## ok that's all for sex diffs, onto the correlations ##

###################################
# Risk & Disgust & Harm Avoidance #
#      (RDH)                      #
###################################

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## the code below is repetitive
## if i were good at this i'd 
## redo it simpler with some loop thing
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### STUDY 1 OVERVIEW ###

#define RDH data set (will recycle "RDH")
RDH <- study1

#select relevant (sub)scales for the key and for the item list

#key
temp.key <- make.keys (RDH, list(
  #  Curtis = Curtis.relevant, 
  TDDS = TDDS.all
  #  ,t.sex=     TDDS.sexual 
  #  ,t.moral=   TDDS.moral 
  #  ,t.pathogen=TDDS.pathogen
  ,DOSPERT = DOSPERT.all
  #  ,d.social      = DOSPERT.social 
  #  ,d.recreational= DOSPERT.recreational
  #  ,d.financial   = DOSPERT.financial
  #  ,d.healthsafety= DOSPERT.healthsafety
  #  ,d.ethical     = DOSPERT.ethical
  ,harm.avoid1    = harm.avoidance1
))

#item list
temp.items <- c(
  TDDS.all
  ,DOSPERT.all
  #  ,Curtis.relevant
  , harm.avoidance1
)

#calculate and display/save scores
temp.scores <- scoreItems (temp.key[temp.items,], RDH [,temp.items], missing=T, impute="none")
print(temp.scores$corrected, digits=2) #shows calculated rs

ITERATIONS = 300 #Another un-creative name that you can change if you want 
temp.effects <- cor.ci(RDH[,temp.items], keys=temp.key[temp.items,], n.iter = ITERATIONS, plot=FALSE) #saves bootstrapped effect & ci   #300 = number of iterations 

Study1.RDH.Overview <- temp.effects
#Study1.RDH.Details <- temp.effects
#Study2.RDH.Overview <- temp.effects
#Study2.RDH.Details <- temp.effects
temp.effects


### STUDY 1 DETAILS ###

#define RDH data set
RDH <- study1

#select relevant (sub)scales for the key and for the item list

#key
temp.key <- make.keys (RDH, list(
  #  Curtis = Curtis.relevant, 
  TDDS = TDDS.all
  ,t.sex=     TDDS.sexual 
  ,t.moral=   TDDS.moral 
  ,t.pathogen=TDDS.pathogen
  ,DOSPERT = DOSPERT.all
  ,d.social      = DOSPERT.social 
  ,d.recreational= DOSPERT.recreational
  ,d.financial   = DOSPERT.financial
  ,d.healthsafety= DOSPERT.healthsafety
  ,d.ethical     = DOSPERT.ethical
  ,harm.avoid1    = harm.avoidance1
))

#item list
temp.items <- c(
  TDDS.all
  ,DOSPERT.all
  #  ,Curtis.relevant
  ,harm.avoidance1
)

#calculate and display/save scores
temp.scores <- scoreItems (temp.key[temp.items,], RDH [,temp.items], missing=T, impute="none")
print(temp.scores$corrected, digits=2) #shows calculated rs

temp.effects <- cor.ci(RDH[,temp.items], keys=temp.key[temp.items,], n.iter = ITERATIONS, plot=FALSE) #saves bootstrapped effect & ci

#Study1.RDH.Overview <- temp.effects
Study1.RDH.Details <- temp.effects
#Study2.RDH.Overview <- temp.effects
#Study2.RDH.Details <- temp.effects
temp.effects

## repeat for each sex ##

# Males #

#define RDH data set
RDH <- subset(study1, study1$sex.code==1) #what is 1?

#calculate and display/save scores
LOW_PRECISION = 2
temp.scores <- scoreItems (temp.key[temp.items,], RDH [,temp.items], missing=T, impute="none")
print(temp.scores$corrected, digits=LOW_PRECISION) #shows calculated rs 

temp.effects <- cor.ci(RDH[,temp.items], keys=temp.key[temp.items,], n.iter = ITERATIONS, plot=FALSE) #saves bootstrapped effect & ci

#Study1.RDH.Overview <- temp.effects
Study1.RDH.Details.m <- temp.effects
#Study2.RDH.Overview <- temp.effects
#Study2.RDH.Details <- temp.effects
temp.effects

# Females #

#define RDH data set
RDH <- subset(study1, study1$sex.code==2) #women 

#calculate and display/save scores
temp.scores <- scoreItems (temp.key[temp.items,], RDH [,temp.items], missing=T, impute="none")
print(temp.scores$corrected, digits=LOW_PRECISION) #shows calculated rs   #digits = sig figs / decimal places

temp.effects <- cor.ci(RDH[,temp.items], keys=temp.key[temp.items,], n.iter = ITERATIONS, plot=FALSE) #saves bootstrapped effect & ci

#Study1.RDH.Overview <- temp.effects
Study1.RDH.Details.f <- temp.effects
#Study2.RDH.Overview <- temp.effects
#Study2.RDH.Details <- temp.effects
temp.effects



### STUDY 2 OVERVIEW ###

#define RDH data set
RDH <- study2

#select relevant (sub)scales for the key and for the item list

#key
temp.key <- make.keys (RDH, list(
  Curtis = Curtis.relevant, 
  TDDS = TDDS.all
  #  ,t.sex=     TDDS.sexual 
  #  ,t.moral=   TDDS.moral 
  #  ,t.pathogen=TDDS.pathogen
  ,DOSPERT = DOSPERT.all
  #  ,d.social      = DOSPERT.social 
  #  ,d.recreational= DOSPERT.recreational
  #  ,d.financial   = DOSPERT.financial
  #  ,d.healthsafety= DOSPERT.healthsafety
  #  ,d.ethical     = DOSPERT.ethical
  ,harm.avoid2    = harm.avoidance2
))

#item list
temp.items <- c(
  TDDS.all
  ,DOSPERT.all
  ,Curtis.relevant
  ,harm.avoidance2
)

#calculate and display/save scores
temp.scores <- scoreItems (temp.key[temp.items,], RDH [,temp.items], missing=T, impute="none")
print(temp.scores$corrected, digits=LOW_PRECISION) #shows calculated rs

temp.effects <- cor.ci(RDH[,temp.items], keys=temp.key[temp.items,], n.iter = ITERATIONS, plot=FALSE) #saves bootstrapped effect & ci

#Study1.RDH.Overview <- temp.effects
#Study1.RDH.Details <- temp.effects
Study2.RDH.Overview <- temp.effects
#Study2.RDH.Details <- temp.effects
print(temp.effects)


### STUDY 2 DETAILS ###

#define RDH data set
RDH <- study2

#select relevant (sub)scales for the key and for the item list

#key
temp.key <- make.keys (RDH, list(
  Curtis = Curtis.relevant, 
  TDDS = TDDS.all
  ,t.sex=     TDDS.sexual 
  ,t.moral=   TDDS.moral 
  ,t.pathogen=TDDS.pathogen
  ,DOSPERT = DOSPERT.all
  ,d.social      = DOSPERT.social 
  ,d.recreational= DOSPERT.recreational
  ,d.financial   = DOSPERT.financial
  ,d.healthsafety= DOSPERT.healthsafety
  ,d.ethical     = DOSPERT.ethical
  ,harm.avoid2    = harm.avoidance2
))

#item list
temp.items <- c(
  TDDS.all
  ,DOSPERT.all
  ,Curtis.relevant
  ,harm.avoidance2
)

#calculate and display/save scores
temp.scores <- scoreItems (temp.key[temp.items,], RDH [,temp.items], missing=T, impute="none")
print(temp.scores$corrected, digits=LOW_PRECISION) #shows calculated rs

temp.effects <- cor.ci(RDH[,temp.items], keys=temp.key[temp.items,], n.iter = ITERATIONS, plot=FALSE) #saves bootstrapped effect & ci

#Study1.RDH.Overview <- temp.effects
#Study1.RDH.Details <- temp.effects
#Study2.RDH.Overview <- temp.effects
Study2.RDH.Details <- temp.effects
print(temp.effects)


## males only ##

#define RDH data set
RDH <- subset( study2, study2$sex.code==1)

#calculate and display/save scores
temp.scores <- scoreItems (temp.key[temp.items,], RDH [,temp.items], missing=T, impute="none")
print(temp.scores$corrected, digits=LOW_PRECISION) #shows calculated rs

temp.effects <- cor.ci(RDH[,temp.items], keys=temp.key[temp.items,], n.iter = ITERATIONS, plot=FALSE) #saves bootstrapped effect & ci

#Study1.RDH.Overview <- temp.effects
#Study1.RDH.Details <- temp.effects
#Study2.RDH.Overview <- temp.effects
Study2.RDH.Details.m <- temp.effects
print(temp.effects)

## females only ##

#define RDH data set
RDH <- subset( study2, study2$sex.code==2)

#calculate and display/save scores
temp.scores <- scoreItems (temp.key[temp.items,], RDH [,temp.items], missing=T, impute="none")
print(temp.scores$corrected, digits=LOW_PRECISION) #shows calculated rs

temp.effects <- cor.ci(RDH[,temp.items], keys=temp.key[temp.items,], n.iter = ITERATIONS, plot=FALSE) #saves bootstrapped effect & ci

#Study1.RDH.Overview <- temp.effects
#Study1.RDH.Details <- temp.effects
#Study2.RDH.Overview <- temp.effects
Study2.RDH.Details.f <- temp.effects
print(temp.effects)

#####################################################################
# the objects below contain the correlation results reported in the ms
# i have simply plucked them out by hand for Table 4 & supplement
#####################################################################

Study1.RDH.Overview
Study1.RDH.Details
Study1.RDH.Details.f
Study1.RDH.Details.m

Study2.RDH.Overview
Study2.RDH.Details
Study2.RDH.Details.f
Study2.RDH.Details.m

#these are just the CIs for easy finding if anyone wants them
write.table(Study1.RDH.Details$ci, file="Study1allcorrelations.csv", sep=",", row.names=T) 
write.table(Study2.RDH.Details$ci, file="Study2allcorrelations.csv", sep=",", row.names=T) 

write.table(Study1.RDH.Details.f$ci, file="Study1allcorrelationsFemale.csv", sep=",", row.names=T) 
write.table(Study2.RDH.Details.f$ci, file="Study2allcorrelationsFemale.csv", sep=",", row.names=T) 

write.table(Study1.RDH.Details.m$ci, file="Study1allcorrelationsMale.csv", sep=",", row.names=T) 
write.table(Study2.RDH.Details.m$ci, file="Study2allcorrelationsMale.csv", sep=",", row.names=T) 


### Plots of correlations ###
# don't plan to include in paper, but nice to see

library(ggplot2)
library(reshape2)
# ALL DISGUST MEASURES
#study 1 risk-disgust
plot.study1<- melt(study1[,c("DOSPERT","TDDS.all","TDDS.sexual","TDDS.moral","TDDS.pathogen")], id="DOSPERT")
ggplot(plot.study1, aes(x=DOSPERT, y=value, col=variable)) + geom_point(shape=1) + geom_smooth(method=lm)  +ylab("Disgust") #what is 1
#study 2 risk-disgust
plot.study2<- melt(study2[,c("DOSPERT","TDDS.all","TDDS.sexual","TDDS.moral","TDDS.pathogen", "Curtis")], id="DOSPERT")
ggplot(plot.study2, aes(x=DOSPERT, y=value, col=variable)) + geom_point(shape=1) + geom_smooth(method=lm)  +ylab("Disgust")

#study 1 harm-disgust
plot.study1<- melt(study1[,c("harm.avoid1","TDDS.all","TDDS.sexual","TDDS.moral","TDDS.pathogen")], id="harm.avoid1")
ggplot(plot.study1, aes(x=harm.avoid1, y=value, col=variable)) + geom_point(shape=1) + geom_smooth(method=lm)  +ylab("Disgust")
#study 2 harm-disgust
plot.study2<- melt(study2[,c("harm.avoid2","TDDS.all","TDDS.sexual","TDDS.moral","TDDS.pathogen")], id="harm.avoid2")
ggplot(plot.study2, aes(x=harm.avoid2, y=value, col=variable)) + geom_point(shape=1) + geom_smooth(method=lm)  +ylab("Disgust")











#now moving on to...

###############################################
### HARM AVOIDANCE ITEMS & (RISK & DISGUST) ###
###############################################

#general task here is item-by-item correlations of the
# harm items with the various scale means
# since the Harm Avoidance scale has low alpha

#study1

#DOSPERT
harm.DOSPERT.rs <-  lapply (harm.avoidance1, function (x) {cor.test (study1[,x], study1$DOSPERT, alternative="less", use="complete.obs")}) 
harm.DOSPERT.table <- as.data.frame(
  t(sapply(harm.DOSPERT.rs, function(x) {
    c(x$parameter,
      x$estimate,
      x$p.value
    )
  })))
colnames(harm.DOSPERT.table)<- c("df", "DOSPERT r", "DOSPERT p")
row.names(harm.DOSPERT.table) <- harm.avoidance1
#harm.DOSPERT.table<-merge(harm.DOSPERT.table, items, by="row.names")
print(harm.DOSPERT.table, digits=HIGH_PRECISION, row.names = T, right=F)

#add TDDSs#
#TDDS Pathogen
harm.t.pathogen.rs <-  lapply (harm.avoidance1, function (x) {cor.test (study1[,x], study1$TDDS.pathogen, alternative="greater", use="complete.obs")}) 
harm.t.pathogen.table <- as.data.frame(
  t(sapply(harm.t.pathogen.rs, function(x) {
    c(x$estimate,
      x$p.value)
  })))
colnames(harm.t.pathogen.table)<- c("T path r", "T path p")
row.names(harm.t.pathogen.table) <- harm.avoidance1
print (harm.t.pathogen.table, digits=HIGH_PRECISION, row.names = T, right=F)

Harm.Risk.Disgust.Table <- merge(harm.DOSPERT.table, harm.t.pathogen.table, by="row.names")
#harm.t.pathogen.table<-merge(harm.t.pathogen.table, items, by="row.names")

#TDDS Sexual
harm.t.sex.rs <-  lapply (harm.avoidance1, function (x) {cor.test (study1[,x], study1$TDDS.sex, alternative="greater", use="complete.obs")}) 
harm.t.sex.table <- as.data.frame(
  t(sapply(harm.t.sex.rs, function(x) {
    c(x$estimate,
      x$p.value)
  })))
colnames(harm.t.sex.table)<- c("T sex r", "T sex p")
row.names(harm.t.sex.table) <- harm.avoidance1
#harm.t.sex.table<-merge(harm.t.sex.table, items, by="row.names")
#harm.t.sex.table <- harm.t.sex.table[c(4,2,3)]
print (harm.t.sex.table, digits=HIGH_PRECISION, row.names = T, right=F)

Harm.Risk.Disgust.Table <- merge(Harm.Risk.Disgust.Table, harm.t.sex.table, by.x="Row.names", by.y="row.names")

#TDDS Moral
harm.t.moral.rs <-  lapply (harm.avoidance1, function (x) {cor.test (study1[,x], study1$TDDS.moral, alternative="greater", use="complete.obs")}) 
harm.t.moral.table <- as.data.frame(
  t(sapply(harm.t.moral.rs, function(x) {
    c(x$estimate,
      x$p.value)
  })))
colnames(harm.t.moral.table)<- c("T moral r", "T moral p")
row.names(harm.t.moral.table) <- harm.avoidance1
#harm.t.moral.table<-merge(harm.t.moral.table, items, by="row.names")
#harm.t.moral.table <- harm.t.moral.table[c(4,2,3)]
print (harm.t.moral.table, digits=HIGH_PRECISION, row.names = T, right=F)

Harm.Risk.Disgust.Table <- merge(Harm.Risk.Disgust.Table, harm.t.moral.table, by.x="Row.names", by.y="row.names")

print(Harm.Risk.Disgust.Table, digits=LOW_PRECISION, row.names = F, left=T)

write.csv (print(Harm.Risk.Disgust.Table, digits=LOW_PRECISION, row.names = F, left=T), "HarmRiskDisgust1.csv")


#study2

#DOSPERT
harm.DOSPERT.rs <-  lapply (harm.avoidance2, function (x) {cor.test (study2[,x], study2$DOSPERT, alternative="less", use="complete.obs")}) 
harm.DOSPERT.table <- as.data.frame(
  t(sapply(harm.DOSPERT.rs, function(x) {
    c(x$parameter,
      x$estimate,
      x$p.value
    )
  })))
colnames(harm.DOSPERT.table)<- c("df", "DOSPERT r", "DOSPERT p")
row.names(harm.DOSPERT.table) <- harm.avoidance2
harm.DOSPERT.table<-merge(harm.DOSPERT.table, study2.items, by="row.names")
print(harm.DOSPERT.table, digits=HIGH_PRECISION, row.names = T, right=F)

#add TDDSs#
#TDDS Pathogen
harm.t.pathogen.rs <-  lapply (harm.avoidance2, function (x) {cor.test (study2[,x], study2$TDDS.pathogen, alternative="greater", use="complete.obs")}) 
harm.t.pathogen.table <- as.data.frame(
  t(sapply(harm.t.pathogen.rs, function(x) {
    c(x$estimate,
      x$p.value)
  })))
colnames(harm.t.pathogen.table)<- c("T path r", "T path p")
row.names(harm.t.pathogen.table) <- harm.avoidance2
print (harm.t.pathogen.table, digits=HIGH_PRECISION, row.names = T, right=F)

Harm.Risk.Disgust.Table <- merge(harm.DOSPERT.table, harm.t.pathogen.table, by.x="Row.names", by.y="row.names")
#harm.t.pathogen.table<-merge(harm.t.pathogen.table, items, by="row.names")

#TDDS Sexual
harm.t.sex.rs <-  lapply (harm.avoidance2, function (x) {cor.test (study2[,x], study2$TDDS.sex, alternative="greater", use="complete.obs")}) 
harm.t.sex.table <- as.data.frame(
  t(sapply(harm.t.sex.rs, function(x) {
    c(x$estimate,
      x$p.value)
  })))
colnames(harm.t.sex.table)<- c("T sex r", "T sex p")
row.names(harm.t.sex.table) <- harm.avoidance2
#harm.t.sex.table<-merge(harm.t.sex.table, items, by="row.names")
#harm.t.sex.table <- harm.t.sex.table[c(4,2,3)]
print (harm.t.sex.table, digits=HIGH_PRECISION, row.names = T, right=F)

Harm.Risk.Disgust.Table <- merge(Harm.Risk.Disgust.Table, harm.t.sex.table, by.x="Row.names", by.y="row.names")

#TDDS Moral
harm.t.moral.rs <-  lapply (harm.avoidance2, function (x) {cor.test (study2[,x], study2$TDDS.moral, alternative="greater", use="complete.obs")}) 
harm.t.moral.table <- as.data.frame(
  t(sapply(harm.t.moral.rs, function(x) {
    c(x$estimate,
      x$p.value)
  })))
colnames(harm.t.moral.table)<- c("T moral r", "T moral p")
row.names(harm.t.moral.table) <- harm.avoidance2
#harm.t.moral.table<-merge(harm.t.moral.table, items, by="row.names")
#harm.t.moral.table <- harm.t.moral.table[c(4,2,3)]
print (harm.t.moral.table, digits=HIGH_PRECISION, row.names = T, right=F)

Harm.Risk.Disgust.Table <- merge(Harm.Risk.Disgust.Table, harm.t.moral.table, by.x="Row.names", by.y="row.names")

print(Harm.Risk.Disgust.Table, digits=LOW_PRECISION, row.names = F, left=T)

#add Curtis

#TDDS Moral
harm.curtis.rs <-  lapply (harm.avoidance2, function (x) {cor.test (study2[,x], study2$Curtis, alternative="greater", use="complete.obs")}) 
harm.curtis.table <- as.data.frame(
  t(sapply(harm.curtis.rs, function(x) {
    c(x$estimate,
      x$p.value)
  })))
colnames(harm.curtis.table)<- c("Curtis r", "Curtis p")
row.names(harm.curtis.table) <- harm.avoidance2
print (harm.curtis.table, digits=HIGH_PRECISION, row.names = T, right=F)

Harm.Risk.Disgust.Table <- merge(Harm.Risk.Disgust.Table, harm.curtis.table, by.x="Row.names", by.y="row.names")

print(Harm.Risk.Disgust.Table, digits=LOW_PRECISION, row.names = F, left=T)

write.csv (print(Harm.Risk.Disgust.Table, digits=LOW_PRECISION, row.names = F, left=T), "HarmRiskDisgust2.csv")









# DISGUST SEX DIFFS TO INCLUDE IN META-ANALYSIS
# are these redundant with the above? (I did them at different times)
# study 1 
#raw effect and p values based on Welsh's t
measures.disgust1 <- list ("TDDS.all", "TDDS.sexual", "TDDS.moral", "TDDS.pathogen")
tests.sex.disgustharm1 <-  lapply (measures.disgust1, function (x) 
{t.test (study1[,x] ~ study1$sex.code, alternative="less", na.action = na.omit)}) 

sex.differences.disgust1 <- as.data.frame(
  t(sapply(tests.sex.disgustharm1, function(x) {
    c( x$estimate[2]-x$estimate[1],
       x$p.value,
       x$parameter)
  })))
colnames(sex.differences.disgust1)<- c("raw effect", "p.value","df")
row.names(sex.differences.disgust1) <- measures.disgust1
print(sex.differences.disgust1, digits=HIGH_PRECISION)

# calculate d
library(effsize)
d.sex.disgustharm1 <-  lapply (measures.disgust1, function (x) 
{cohen.d (study1[,x] ~ study1$sex.code, na.rm=T,conf.level=HIGH_CONFIDENCE)}) 

d.sex.differences.disgust1 <- as.data.frame(
  t(sapply(d.sex.disgustharm1, function(x) {
    c(-x$estimate, #these are reversed to be consistent with meta-analysis effect direction
      -x$conf.int[2] , 
      -x$conf.int[1])
  })))

colnames(d.sex.differences.disgust1)<- c("d", "-95%CI" , "+95%CI")
row.names(d.sex.differences.disgust1) <- measures.disgust1
print(d.sex.differences.disgust1, digits=HIGH_PRECISION)

#summarize study 1 disgust
sex.disgustharm1 <- merge(d.sex.differences.disgust1,  sex.differences.disgust1, by="row.names", all=TRUE  )
print(sex.disgustharm1, digits=HIGH_PRECISION)


# study 2 disgust raw effect and p values based on Welsh's t
measures.disgust2 <- list ("TDDS.all", "TDDS.sexual", "TDDS.moral", "TDDS.pathogen", "Curtis")
t.tests.sex.disgust2 <-  lapply (measures.disgust2, function (x) 
{t.test (study2[,x] ~ study2$sex.code, alternative="less", na.action = na.omit)}) 

t.sex.differences.disgust2 <- as.data.frame(
  t(sapply(t.tests.sex.disgust2, function(x) {c(
    x$estimate[2]-x$estimate[1],
    x$p.value,
    x$parameter)
  })))
colnames(t.sex.differences.disgust2)<- c("raw effect" ,"p.value","df")
row.names(t.sex.differences.disgust2) <- measures.disgust2
print(t.sex.differences.disgust2, digits=HIGH_PRECISION)

#  calculate d
d.sex.disgust2 <-  lapply (measures.disgust2, function (x) 
{cohen.d (study2[,x], study2$sex.code,  na.rm=T,conf.level=HIGH_CONFIDENCE)})

d.sex.differences.disgust2 <- as.data.frame(
  t(sapply(d.sex.disgust2, function(x) {
    c(-x$estimate, #these are reversed to be consistent with meta-analysis effect direction
      -x$conf.int[2] , 
      -x$conf.int[1])
  })))

colnames(d.sex.differences.disgust2)<- c("d", "-95%CI" , "+95%CI")
row.names(d.sex.differences.disgust2) <- measures.disgust2
print(d.sex.differences.disgust2, digits=HIGH_PRECISION)

#summarize study 2 disgust
sex.disgust2 <- merge(d.sex.differences.disgust2, t.sex.differences.disgust2,  by="row.names", all=TRUE  )
print(sex.disgust2, digits=HIGH_PRECISION)

########################
#| | | TABLE  | | | | |#
########################
DisgustSexDifferences <- merge(x=sex.disgustharm1 , y=sex.disgust2,
                               by="Row.names",all=T, suffixes=c(".study1", ".study2"))
print(DisgustSexDifferences, digits=HIGH_PRECISION)
write.table(DisgustSexDifferences, file="DisgustSex95.csv", sep=",", row.names=F) 


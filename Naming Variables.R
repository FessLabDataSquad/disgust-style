# This is the code that produced the results of Study 2 reported in
#        Sparks, Fessler, Chan, Ashokkumar & Holbrook
#        "Disgust as a mechanism for decision making under risk"
# Contact Adam Sparks (adspar AT gmail) with questions.
# There may be many questions because 
# I'm an R noob and haven't yet learned 
# programming efficiency and elegance





#import
#setwd()
study1 = read.csv("data_forR_study1.csv",na.strings=c(""))  #Are read.csv and na.strings standard commands in R?
study2 = read.csv("data_forR_study2.csv",na.strings=c(""))
study2_items = read.csv("itemsstudy2.csv",na.strings=c(""), row_names=1, header=FALSE) #Is row names a variable? 
names(study2_items) =  "item"


### study1 cleaning ###

#check variable classes, update as needed
#sapply(study1, class)
study1$sex_code = as.factor (study1$sex_code) #Is as.factor() a standard command in R? What about as.numeric()? 
study1$sex_orient = as.factor (study1$sex_orient)

#create variable for completion time
library(lubridate)
study1$time = as.numeric(difftime(mdy_hms(study1$end), mdy_hms(study1$start))) 

#time cutoff analysis
mean(study1$time)
MIN_ACCEPTABLE_TIME = 5
MAX_ACCEPTABLE_TIME = 45 #Not sure if you want to name it "max acceptable time" since you end up keeping times greater than this
length(subset(study1$time, study1$time<MIN_ACCEPTABLE_TIME))
length(subset(study1$time, study1$time>MAX_ACCEPTABLE_TIME)) 

#cut point: keep >5 minutes
study1 = subset(study1, study1$time>MIN_ACCEPTABLE_TIME) #sample now n=988 

#attention check #1 (phone)
length (subset (study1$check_low, study1$check_low!=1)) #what is 1?
length (subset (study1$check_low, study1$check_low==1)) #what is 1?
length (subset (study1$check_low, study1$check_low>2)) #what is 2?

# keep 1s and 2s (should I keep 2s?)
study1 = subset (study1, study1$check_low==1 | study1$check_low==2) 

#attention check #2 (alphabet)
LETTERS_IN_ALPHABET = 26
EXTRA_LETTER = LETTERS_IN_ALPHABET + 1
length (subset (study1$check_alphabet, study1$check_alphabet==LETTERS_IN_ALPHABET))
length (subset (study1$check_alphabet, study1$check_alphabet==EXTRA_LETTER)) 
length (subset (study1$check_alphabet, study1$check_alphabet>EXTRA_LETTER)) 
length (subset (study1$check_alphabet, study1$check_alphabet<LETTERS_IN_ALPHABET)) 

#eliminate all but 26 and 27
#27 was common enough that it seems like a counting mistake, rather than attention failure
study1 = subset (study1, study1$check_alphabet==LETTERS_IN_ALPHABET | study1$check_alphabet==EXTRA_LETTER)

#calculate height, ignoring half-inches for now
FEET_TO_INCHES = 12
study1$height1 = (study1$feet1)*FEET_TO_INCHES+(study1$inches1)#+(study1$halfinches1-1)*0.5   
study1$height2 = (study1$feet2)*FEET_TO_INCHES+(study1$inches2)#+(study1$halfinches2-1)*0.5  
study1$height = pmax (study1$height1, study1$height2, na.rm=TRUE) #IS na.rm a standard command?

#reverse-code such that higher is more harm avoidant
SCALE_MAX = 5
CONVERSION_FACTOR = SCALE_MAX + 1  #Not sure if you want to name it "conversion factor" because this is really vague but I couldn't think of anything else! 
study1$seatbelt = CONVERSION_FACTOR-study1$seatbelt  
study1$jaywalk = CONVERSION_FACTOR-study1$jaywalk


### study 2 cleaning ###

#calculate height
SCALE_TO_FEET = 2
SCALE_TO_INCHES = 1
study2$height = (study2$feet_code+SCALE_TO_FEET)*FEET_TO_INCHES+(study2$inches_code-SCALE_TO_INCHES) 

#recode the only participant to choose "other"
#sex labels 1=M, 2=F, 3=other
PARTICIPANT_NUMBER = 81
study2$sex_code[PARTICIPANT_NUMBER] = NA 
study2$sex_code = as.factor (study2$sex_code)

#reverse code scale items
study2$ha5 = CONVERSION_FACTOR-study2$ha5
study2$ha9 = CONVERSION_FACTOR-study2$ha9
study2$ha11 = CONVERSION_FACTOR-study2$ha11
study2$ha11a = CONVERSION_FACTOR-study2$ha11a

#create variable for completion time
study2$time = as.numeric(difftime(mdy_hm(study2$end), mdy_hm(study2$start))) 

#removes 25 participants who finished in less than 5 minutes 
study2 = subset(study2, study2$time>=MIN_ACCEPTABLE_TIME)  #not sure if this is right?

#honestresponse -- there are no 3s, so don't filter out anyone
#parent labels 1=yes, 2=no
#polical orientation: high is conservative
#computertype 1=desktop, 2=laptop/notebook, 3=phone



### Scales ###

library(psych)

# both studies
TDDS_sexual    = c("t_sex1", "t_sex2", "t_sex3", "t_sex4", "t_sex5", "t_sex6", "t_sex7")
TDDS_pathogen  = c("t_path1", "t_path2", "t_path3", "t_path4", "t_path5", "t_path6", "t_path7")
TDDS_moral     = c("t_mo1", "t_mo2", "t_mo3", "t_mo4", "t_mo5", "t_mo6", "t_mo7")
TDDS_all       = c(TDDS_sexual, TDDS_pathogen, TDDS_moral)

DOSPERT_social       = c("d_s1",  "d_s2",  "d_s3",  "d_s4",  "d_s5",  "d_s6")
DOSPERT_recreational = c("d_r1",  "d_r2",  "d_r3",  "d_r4",  "d_r5",  "d_r6")
DOSPERT_financial    = c("d_f1",  "d_f2",  "d_f3",  "d_f4",  "d_f5",  "d_f6")
DOSPERT_healthsafety = c("d_hs1", "d_hs2", "d_hs3", "d_hs4", "d_hs5", "d_hs6")
#DOSPERT_harmavoid   = c("d_hs3",  "d_hs4",    "d_hs5",  "d_hs6") #harm avoiding DOSERT items 
DOSPERT_ethical      = c("d_e1",  "d_e2",  "d_e3",  "d_e4",  "d_e5",  "d_e6")
DOSPERT_all          = c(DOSPERT_social, DOSPERT_recreational, DOSPERT_financial, DOSPERT_healthsafety, DOSPERT_ethical)

#Study 1
harm_avoidance1 = c("seatbelt","lockdoor","jaywalk")
allscaleitems1  = c(TDDS_all, DOSPERT_all, harm_avoidance1)

#make key and score the scales
key_all1 = make_keys(study1,list(
  TDDS_sexual = TDDS_sexual, 
  TDDS_moral = TDDS_moral, 
  TDDS_pathogen = TDDS_pathogen,
  TDDS_all = TDDS_all,
  d_social      = DOSPERT_social, 
  d_recreational= DOSPERT_recreational,
  d_financial   = DOSPERT_financial,
  d_healthsafety= DOSPERT_healthsafety,
  d_ethical     = DOSPERT_ethical, 
  DOSPERT       = DOSPERT_all,
  #  d_harmavoid   = DOSPERT_harmavoid,
  harm_avoid1   = harm_avoidance1))

scores1 = scoreItems(key.all1[allscaleitems1,], study1[,allscaleitems1], missing=TRUE, impute="none") #Is key.all a standard command?

#save scores as study1 variables
a = as_data_frame(cbind(scores1$scores, ID=study1$ID))
study1 = merge(study1, a, by="ID")
rm(a)

### FINAL study1 data FILE ###
### export it for sharing ###
### dim: 941 x 93
write.csv (study1, "finaldata_study1.csv")


#Study 2
harm_avoidance2  = c("ha1","ha2","ha3","ha4","ha5","ha6","ha7","ha8","ha9","ha10","ha11")
Curtis_relevant = c("c_a_r","c_b_r","c_c_r","c_d_r","c_e_r","c_f_r","c_g_r", "c_gummaggot")
Curtis_relevant_limited = c("c_a_r","c_b_r","c_c_r","c_d_r","c_e_r","c_f_r","c_g_r")
Curtis_irrelevant = c("c_a_i","c_b_i","c_c_i", "c_d_i","c_e_i","c_f_i","c_g_i") #add the other non-paired?
# we don't use irrelevant for many analysis though
allscaleitems2  = c(TDDS_all, DOSPERT_all, Curtis_relevant, Curtis_irrelevant, harm_avoidance2)

#make key and score the scales
key.all2 = make.keys(study2,list( #make.keys?
  TDDS_sexual = TDDS_sexual, 
  TDDS_moral = TDDS_moral, 
  TDDS_pathogen = TDDS_pathogen,
  TDDS_all = TDDS_all,
  Curtis = Curtis_relevant,
  #  Curtis_limited=Curtis_relevant_limited,
  #  c_irrelevant = Curtis_irrelevant,
  d_social      = DOSPERT_social, 
  d_recreational= DOSPERT_recreational,
  d_financial   = DOSPERT_financial,
  d_healthsafety= DOSPERT_healthsafety,
  d_ethical     = DOSPERT_ethical, 
  DOSPERT       = DOSPERT_all,
  #  d_harmavoid   = DOSPERT_harmavoid,
  harm_avoid2   = harm_avoidance2))

scores2 = scoreItems(key.all2[allscaleitems2,], study2[,allscaleitems2], missing=TRUE, impute="none")

#save scores as variables
a = as_data_frame(cbind(scores2$scores, ID=study2$ID))
study2 = merge(study2, a, by="ID")
rm(a)

### FINAL study2 data FILE
### export it for sharing
### dim: 473 x 115
write.csv (study2, "finaldata_study2.csv")


## SCALE VALIDATION ##

#Study 2
#Curtis Disgust item paired comparisons
tests_curtis_items = list()
tests_curtis_items[["pair a"]] = t.test(study2$c_a_r, study2$c_a_i, paired=TRUE, alternative = "greater", na.action = na.omit) #t.test, na.action, na.omit? 
tests.curtis.items[["pair b"]] = t.test(study2$c_b_r, study2$c_b_i, paired=TRUE, alternative = "greater", na.action = na.omit)
tests.curtis.items[["pair c"]] = t.test(study2$c_c_r, study2$c_c_i, paired=TRUE, alternative = "greater", na.action = na.omit)
tests.curtis.items[["pair d"]] = t.test(study2$c_d_r, study2$c_d_i, paired=TRUE, alternative = "greater", na.action = na.omit)
tests.curtis.items[["pair e"]] = t.test(study2$c_e_r, study2$c_e_i, paired=TRUE, alternative = "greater", na.action = na.omit)
tests.curtis.items[["pair f"]] = t.test(study2$c_f_r, study2$c_f_i, paired=TRUE, alternative = "greater", na.action = na.omit)
tests.curtis.items[["pair g"]] = t.test(study2$c_g_r, study2$c_g_i, paired=TRUE, alternative = "greater", na.action = na.omit)

Curtis_paired_comparisons = t(sapply(tests_curtis_items, function(x) {
  c(x$estimate,
    ci_lower = x$conf_int[1],
    ci_upper = x$conf_int[2],
    p.value = x$p.value, #p.value?
    x$parameter)
}))

HIGH_PRECISION = 3
print(Curtis_paired_comparisons, digits=HIGH_PRECISION)
########################
#####   TABLE   ########
########################

# reported in supplement
#Pathogen-relevant rated more disgusting than paired stimuli
write.table(Curtis_paired_comparisons, row_names=TRUE, file="CurtisPairedComparisonTable.csv", sep=",") 

#Harm Avoidance
#driver's license doesn't matter for seatbelt so can ignore ha1a
t.test(study2$ha1 ~ study2$ha1a, na.action=na.omit)


########################
#####   TABLE   ########
########################
#make a table with the alpha values
# Table 2 in ms
alphatable = merge( t(scores1$alpha), t(scores2$alpha), by="row_names"   , all=TRUE  )
colnames(alphatable) = c("(Sub)scale", "Study 1", "Study 2")
print(alphatable, digits=HIGH_PRECISION, row_names = FALSE, right=F)  #how many sig figs
write.table(alphatable, row_names=FALSE, file="AlphaTable.csv", sep=",") 



### Analysis of Hypotheses ###

###########################################################################################
#Females, compared to males, will show more disgust and harm avoidance and less risk taking
###########################################################################################

# DISGUST + HARM AVOIDANCE SEX DIFF
# study 1 
#raw effect and p values based on Welsh's t
measure_disgustharm1 = list ("TDDS_all", "TDDS_sexual", "TDDS_moral", "TDDS_pathogen", "harm_avoid1")
tests_sex_disgustharm1 = lapply (measure_disgustharm1, function (x) 
{t.test (study1[,x] ~ study1$sex_code, alternative="less", na.action = na.omit)}) 

sex_differences_disgustharm1 = as.data.frame( #as.data.frame? 
  t(sapply(tests_sex_disgustharm1, function(x) {
    c( x$estimate[2]-x$estimate[1],
       x$p.value,
       x$parameter)
  })))
colnames(sex_differences_disgustharm1) = c("raw effect", "p.value","df")
row_names(sex_differences_disgustharm1) = measure_disgustharm1
print(sex_differences_disgustharm1, digits=HIGH_PRECISION )

# calculate d
library(effsize)
d_sex_disgustharm1 = lapply (measure_disgustharm1, function (x) 
  HIGH_CONFIDENCE = 0.95
  {cohen.d (study1[,x] ~ study1$sex_code, na.rm=T,conf_level=HIGH_CONFIDENCE)}) #cohen.d? na.rm?

d_sex_differences_disgustharm1 = as.data.frame(
  t(sapply(d_sex_disgustharm1, function(x) {
    c(-x$estimate, #these are reversed to be consistent with meta-analysis effect direction
      -x$conf_int[2] , 
      -x$conf_int[1])
  })))

colnames(d_sex_differences_disgustharm1) = c("d", "-95%CI" , "+95%CI")
row_names(d_sex_differences_disgustharm1) = measure_disgustharm1
print(d_sex_differences_disgustharm1, digits=HIGH_PRECISION)

#summarize study 1 disgust & harm avoid
sex_disgustharm1 = merge(d_sex_differences_disgustharm1,  sex_differences_disgustharm1, by="row_names", all=TRUE  )
print(sex_disgustharm1, digits=HIGH_PRECISION)


# study 2 disgust & harm avoidance raw effect and p values based on Welsh's t
measures_disgustharm2 = list ("TDDS_all", "TDDS_sexual", "TDDS_moral", "TDDS_pathogen", "Curtis", "harm_avoid2")
t_tests_sex_disgustharm2 = lapply (measures_disgustharm2, function (x) 
{t.test (study2[,x] ~ study2$sex_code, alternative="less", na.action = na.omit)}) 

t_sex_differences_disgustharm2 = as.data.frame(
  t(sapply(t_tests_sex_disgustharm2, function(x) {c(
    x$estimate[2]-x$estimate[1],
    x$p.value,
    x$parameter)
  })))
colnames(t_sex_differences_disgustharm2) = c("raw effect" ,"p.value","df")
row_names(t_sex_differences_disgustharm2) = measures_disgustharm2
print(t_sex_differences_disgustharm2, digits=HIGH_PRECISION)

#  calculate d
d_sex_disgustharm2 = lapply (measures_disgustharm2, function (x) 
{cohen.d (study2[,x], study2$sex_code,  na.rm=T,conf_level=HIGH_CONFIDENCE)})

d_sex_differences_disgustharm2 = as.data.frame(
  t(sapply(d_sex_disgustharm2, function(x) {
    c(-x$estimate, #these are reversed to be consistent with meta-analysis effect direction
      -x$conf_int[2] , 
      -x$conf_int[1])
  })))

colnames(d_sex_differences_disgustharm2) = c("d", "-95%CI" , "+95%CI")
row_names(d_sex_differences_disgustharm2) = measures_disgustharm2
print(d_sex_differences_disgustharm2, digits=HIGH_PRECISION)

#summarize study 2 disgust
sex_disgustharm2 = merge(d_sex_differences_disgustharm2, t_sex_differences_disgustharm2,  by="row_names", all=TRUE  )
print(sex_disgustharm2, digits=HIGH_PRECISION)

########################
#| | | TABLE  | | | | |#
########################
DisgustSexDifferences = merge(x=sex_disgustharm1 , y=sex_disgustharm2,
                              by="Row_names",all=T, suffixes=c(".study1", ".study2"))
print(DisgustSexDifferences, digits=HIGH_PRECISION)
#write.table(DisgustSexDifferences, file="DisgustSex.csv", sep=",", row.names=F) 

#simpler table for MS Table 3
SexDiffs_DigustHarm_MS = merge(d_sex_differences_disgustharm1,  d_sex_differences_disgustharm2, by="row_names", all=TRUE, suffixes=c(".study1", ".study2")  )
print(SexDiffs_DigustHarm_MS, digits=HIGH_PRECISION)
write.table(SexDiffs_DigustHarm_MS, file="DisgustHarmSex.csv", sep=",", row.names=F)


# RISK TAKING SEX DIFF

#study 1 risk taking
# raw effect and p
measures_risk1 = list("DOSPERT","d_social","d_recreational","d_financial","d_healthsafety","d_ethical","wager")
tests_sex_risk1 = lapply (measures_risk1, function (x) {t.test (study1[,x] ~ study1$sex_code, alternative="greater", na.action = na.omit)}) 
sex_differences_risk1 = as.data.frame(
  t(sapply(tests.sex.risk1, function(x) {
    c(x$estimate[1] - x$estimate[2], 
      x$p.value,
      x$parameter)
  })))
colnames(sex_differences_risk1) = c("raw_effect", "p.value","df")
row_names(sex_differences_risk1) = measures_risk1
print(as.data.frame(sex_differences_risk1), digits=HIGH_PRECISION)

# d and 95% CIs
d_sex_risk1 = lapply (measures_risk1, function (x) 
{cohen.d (study1[,x] ~ study1$sex_code, na.rm=T,conf_level=HIGH_CONFIDENCE)}) 

d_sex_differences_risk1 = as.data.frame(
  t(sapply(d_sex_risk1, function(x) {
    c(x$estimate, #these are reversed to be consistent with meta-analysis effect direction
      x$conf_int[1] , 
      x$conf_int[2])
  })))

colnames(d_sex_differences_risk1) = c("d", "-95%CI" , "+95%CI")
row_names(d_sex_differences_risk1) = measures_risk1
print(d_sex_differences_risk1, digits=HIGH_PRECISION)

#summarize study 1 risk
sex_risk1 = merge(d_sex_differences_risk1,  sex_differences_risk1, by="row_names", all=TRUE  )
print(sex_risk1, digits=HIGH_PRECISION)


#study 2 risk taking
# raw effect and p
measures_risk2 = list ("DOSPERT","d_social","d_recreational","d_financial","d_healthsafety","d_ethical")
tests_sex_risk2 = lapply (measures_risk2, function (x) {t.test (study2[,x] ~ study2$sex_code, alternative="greater", na.action = na.omit)}) 
sex_differences_risk2 = as.data.frame(
  t(sapply(tests_sex_risk2, function(x) {
    c(x$estimate[1] - x$estimate[2], 
      x$p.value,
      x$parameter)
  })))
colnames(sex_differences_risk2) = c("raw_effect", "p.value", "df")
row_names(sex_differences_risk2) = measures_risk2
print(as.data.frame(sex.differences.risk2), digits=HIGH_PRECISION)

# d and 95% CIs
d_sex_risk2 = lapply (measures_risk2, function (x) 
{cohen.d (study2[,x] ~ study2$sex_code, na.rm=T,conf_level=HIGH_CONFIDENCE)}) 

d_sex_differences_risk2 = as.data.frame(
  t(sapply(d.sex.risk2, function(x) {
    c(x$estimate, #these are reversed to be consistent with meta-analysis effect direction
      x$conf_int[1] , 
      x$conf_int[2])
  })))

colnames(d_sex_differences_risk2) = c("d", "-95%CI" , "+95%CI")
row_names(d_sex_differences_risk2) = measures_risk2
print(d_sex_differences_risk2, digits=HIGH_PRECISION)

#summarize study 2 risk
sex_risk2 = merge(d_sex_differences_risk2,  sex_differences_risk2, by="row_names", all=TRUE  )
print(sex_risk2, digits=HIGH_PRECISION)

########################
#####   TABLE   ########
########################
RiskSexDifferences = merge(sex_risk1 , sex_risk2,
                           by="Row_names",all=T, suffixes=c(".study1", ".study2"))

row.order = c(5,4,3,1,2,6,7)

print(RiskSexDifferences[order(row.order),], digits=HIGH_PRECISION) #row.order?
#write.table(RiskSexDifferences[order(row.order),], file="RiskSex.csv", sep=",", row_names=F) 


# simpler table for MS -- Table 3
RiskSexDiff_MS = merge(d_sex_differences_risk1,  d_sex_differences_risk2, by="row_names", all=TRUE, suffixes=c(".study1", ".study2")  )
print(RiskSexDiff_MS, digits=HIGH_PRECISION)
write.table(RiskSexDiff_MS, file="RiskSex.csv", sep=",", row.names=F) 

# manually combine those last 2 tables for manuscript





# HARM AVOIDANCE ITEM-BY-ITEM SEX DIFF
#report in supplement
#study 1 harm avoidance raw effect & p
tests_sex_ha1 = lapply (harm_avoidance1, function (x) {t.test (study1[,x] ~ study1$sex_code, 
                                                               alternative="less", 
                                                               na.action = na.omit)}) 
sex_differences_ha1 = as.data.frame(
  t(sapply(tests_sex_ha1, function(x) {
    c(x$estimate[2] - x$estimate[1],
      x$p.value,
      x$parameter)
  })))
colnames(sex_differences_ha1) = c("raw effect", "p.value", "df")
row_names(sex_differences_ha1) = harm_avoidance1
print (sex_differences_ha1, digits=HIGH_PRECISION, row_names = TRUE, right=F)

#study 1 d, 95% CI
d_sex_ha1 = lapply (harm_avoidance1, function (x) 
{cohen.d (study1[,x] ~ study1$sex_code, na.rm=T,conf_level=HIGH_CONFIDENCE)}) 

d_sex_differences_ha1 = as.data.frame(
  t(sapply(d_sex_ha1, function(x) {
    c(-x$estimate, #these are reversed to be consistent with meta-analysis effect direction
      -x$conf_int[2] , 
      -x$conf_int[1])
  })))

colnames(d_sex_differences_ha1) = c("d", "-95%CI" , "+95%CI")
row_names(d_sex_differences_ha1) = harm_avoidance1
print(d_sex_differences_ha1, digits=HIGH_PRECISION)

#summarize study 1 harm avoidance
sex_ha1 = merge(d_sex_differences_ha1,  sex_differences_ha1, by="row_names", all=TRUE  )
print(sex_ha1, digits=HIGH_PRECISION)
########################
#####   TABLE   ########
########################
write.table(sex_ha1, file="HarmSex1.csv", sep=",", row_names=F) 

#study 2 harm avoidance
tests_sex_ha2 = lapply (harm_avoidance2, function (x) {t.test (study2[,x] ~ study2$sex_code, alternative="less", na.action = na.omit)}) 
sex_differences_ha2 = as.data.frame(
  t(sapply(tests_sex_ha2, function(x) {
    c(x$estimate[2] - x$estimate[1],
      x$p.value,
      x$parameter)
  })))
colnames(sex_differences_ha2) = c("raw effect", "p.value", "df")
row.names(sex_differences_ha2) = harm_avoidance2
print (sex_differences_ha2, digits=HIGH_PRECISION, row_names = T, right=F)

#study 2 d, 95% CI
d_sex_ha2 =  lapply (harm_avoidance2, function (x) 
{cohen.d (study2[,x] ~ study2$sex_code, na.rm=T,conf_level=HIGH_CONFIDENCE)}) 

d_sex_differences_ha2 = as.data.frame(
  t(sapply(d_sex_ha2, function(x) {
    c(-x$estimate, #these are reversed to be consistent with meta-analysis effect direction
      -x$conf_int[2] , 
      -x$conf_int[1])
  })))

colnames(d_sex_differences_ha2) = c("d", "-95%CI" , "+95%CI")
row_names(d_sex_differences_ha2) = harm_avoidance2
print(d_sex_differences_ha2, digits=HIGH_PRECISION)

#summarize study 2 harm avoidance
sex_ha2 = merge(d_sex_differences_ha2,  sex_differences_ha2, by="row_names" , all=TRUE  )
sex_ha2 = merge(study2_items, sex_ha2, by_x="row_names", by_y="Row_names")
print(sex_ha2, digits=HIGH_PRECISION)
########################
#####   TABLE   ########
########################
write.table(sex_ha2, file="HarmSex2.csv", sep=",", row_names=F) 

# men more flu shots??
library(Rmisc)
summarySE(data = study2, measurevar= "ha7", groupvars = "sex_code",
          na.rm = TRUE, conf_interval = HIGH_CONFIDENCE)
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
RDH = study1

#select relevant (sub)scales for the key and for the item list

#key
temp_key = make_keys (RDH, list(
  #  Curtis = Curtis_relevant, 
  TDDS = TDDS_all
  #  ,t_sex=     TDDS_sexual 
  #  ,t_moral=   TDDS_moral 
  #  ,t_pathogen=TDDS_pathogen
  ,DOSPERT = DOSPERT_all
  #  ,d_social      = DOSPERT_social 
  #  ,d_recreational= DOSPERT_recreational
  #  ,d_financial   = DOSPERT_financial
  #  ,d_healthsafety= DOSPERT_healthsafety
  #  ,d_ethical     = DOSPERT_ethical
  ,harm_avoid1    = harm_avoidance1
))

#item list
temp_items = c(
  TDDS_all
  ,DOSPERT_all
  #  ,Curtis_relevant
  , harm_avoidance1
)

#calculate and display/save scores
temp_scores = scoreItems (temp_key[temp_items,], RDH [,temp_items], missing=T, impute="none")
print(temp_scores$corrected, digits=2) #shows calculated rs

ITERATIONS = 300 #Another un-creative name that you can change if you want
temp_effects = cor.ci(RDH[,temp_items], keys=temp_key[temp_items,], n_iter = ITERATIONS, plot=FALSE) #saves bootstrapped effect & ci   #300 = number of iterations 

Study1_RDH_Overview = temp_effects
#Study1_RDH_Details = temp_effects
#Study2_RDH_Overview = temp_effects
#Study2_RDH_Details = temp_effects
temp_effects


### STUDY 1 DETAILS ###

#define RDH data set
RDH = study1

#select relevant (sub)scales for the key and for the item list

#key
temp_key = make_keys (RDH, list(
  #  Curtis = Curtis_relevant, 
  TDDS = TDDS_all
  ,t_sex = TDDS_sexual 
  ,t_moral = TDDS_moral 
  ,t_pathogen = TDDS_pathogen
  ,DOSPERT = DOSPERT_all
  ,d_social = DOSPERT_social 
  ,d_recreational = DOSPERT_recreational
  ,d_financial    = DOSPERT_financial
  ,d_healthsafety = DOSPERT_healthsafety
  ,d_ethical      = DOSPERT_ethical
  ,harm_avoid1    = harm_avoidance1
))

#item list
temp_items = c(
  TDDS_all
  ,DOSPERT_all
  #  ,Curtis_relevant
  ,harm_avoidance1
)

#calculate and display/save scores
temp_scores = scoreItems (temp_key[temp_items,], RDH [,temp_items], missing=T, impute="none")
print(temp_scores$corrected, digits=2) #shows calculated rs

temp_effects = cor.ci(RDH[,temp_items], keys=temp_key[temp_items,], n_iter = ITERATIONS, plot=FALSE) #saves bootstrapped effect & ci

#Study1_RDH_Overview = temp_effects
Study1_RDH_Details = temp_effects
#Study2_RDH_Overview = temp_effects
#Study2_RDH_Details = temp_effects
temp_effects

## repeat for each sex ##

# Males #

#define RDH data set
RDH = subset(study1, study1$sex_code==1) #what is 1?

#calculate and display/save scores
LOW_PRECISION = 2
temp_scores = scoreItems (temp_key[temp_items,], RDH [,temp_items], missing=T, impute="none")
print(temp_scores$corrected, digits=LOW_PRECISION) #shows calculated rs 

temp_effects = cor.ci(RDH[,temp_items], keys=temp_key[temp_items,], n_iter = ITERATIONS, plot=FALSE) #saves bootstrapped effect & ci

#Study1_RDH_Overview = temp_effects
Study1_RDH_Details_m = temp_effects
#Study2_RDH_Overview = temp_effects
#Study2_RDH_Details  = temp_effects
temp_effects

# Females #

#define RDH data set
RDH = subset(study1, study1$sex_code==2) #women 

#calculate and display/save scores
temp_scores = scoreItems (temp_key[temp_items,], RDH [,temp_items], missing=T, impute="none")
print(temp_scores$corrected, digits=LOW_PRECISION) #shows calculated rs   #digits = sig figs / decimal places

temp_effects = cor.ci(RDH[,temp_items], keys=temp_key[temp_items,], n_iter = ITERATIONS, plot=FALSE) #saves bootstrapped effect & ci

#Study1_RDH_Overview = temp_effects
Study1_RDH_Details_f = temp_effects
#Study2_RDH_Overview = temp_effects
#Study2_RDH_Details  = temp_effects
temp_effects



### STUDY 2 OVERVIEW ###

#define RDH data set
RDH = study2

#select relevant (sub)scales for the key and for the item list

#key
temp_key = make_keys (RDH, list(
  Curtis = Curtis_relevant, 
  TDDS = TDDS_all
  #  ,t_sex=     TDDS_sexual 
  #  ,t_moral=   TDDS_moral 
  #  ,t_pathogen=TDDS_pathogen
  ,DOSPERT = DOSPERT_all
  #  ,d_social      = DOSPERT_social 
  #  ,d_recreational= DOSPERT_recreational
  #  ,d_financial   = DOSPERT_financial
  #  ,d_healthsafety= DOSPERT_healthsafety
  #  ,d_ethical     = DOSPERT_ethical
  ,harm_avoid2    = harm_avoidance2
))

#item list
temp_items = c(
  TDDS_all
  ,DOSPERT_all
  ,Curtis_relevant
  ,harm_avoidance2
)

#calculate and display/save scores
temp_scores = scoreItems (temp_key[temp_items,], RDH [,temp_items], missing=T, impute="none")
print(temp_scores$corrected, digits=LOW_PRECISION) #shows calculated rs

temp_effects = cor.ci(RDH[,temp_items], keys=temp_key[temp_items,], n_iter = ITERATIONS, plot=FALSE) #saves bootstrapped effect & ci

#Study1_RDH_Overview = temp_effects
#Study1_RDH_Details = temp_effects
Study2_RDH_Overview = temp_effects
#Study2_RDH_Details = temp_effects
print(temp_effects)


### STUDY 2 DETAILS ###

#define RDH data set
RDH = study2

#select relevant (sub)scales for the key and for the item list

#key
temp_key = make_keys (RDH, list(
  Curtis = Curtis_relevant, 
  TDDS = TDDS_all
  ,t_sex = TDDS_sexual 
  ,t_moral = TDDS_moral 
  ,t_pathogen = TDDS_pathogen
  ,DOSPERT = DOSPERT_all
  ,d_social      = DOSPERT_social 
  ,d_recreational= DOSPERT_recreational
  ,d_financial   = DOSPERT_financial
  ,d_healthsafety= DOSPERT_healthsafety
  ,d_ethical     = DOSPERT_ethical
  ,harm_avoid2   = harm_avoidance2
))

#item list
temp_items = c(
  TDDS_all
  ,DOSPERT_all
  ,Curtis_relevant
  ,harm_avoidance2
)

#calculate and display/save scores
temp_scores = scoreItems (temp_key[temp_items,], RDH [,temp_items], missing=T, impute="none")
print(temp_scores$corrected, digits=LOW_PRECISION) #shows calculated rs

temp_effects = cor.ci(RDH[,temp_items], keys=temp_key[temp_items,], n_iter = ITERATIONS, plot=FALSE) #saves bootstrapped effect & ci

#Study1_RDH_Overview = temp_effects
#Study1_RDH_Details = temp_effects
#Study2_RDH_Overview = temp_effects
Study2_RDH_Details = temp_effects
print(temp_effects)


## males only ##

#define RDH data set
RDH = subset( study2, study2$sex_code==1)

#calculate and display/save scores
temp_scores = scoreItems (temp_key[temp_items,], RDH [,temp_items], missing=T, impute="none")
print(temp_scores$corrected, digits=LOW_PRECISION) #shows calculated rs

temp_effects = cor.ci(RDH[,temp_items], keys=temp_key[temp_items,], n_iter = ITERATIONS, plot=FALSE) #saves bootstrapped effect & ci

#Study1_RDH_Overview = temp_effects
#Study1_RDH_Details = temp_effects
#Study2_RDH_Overview = temp_effects
Study2_RDH_Details_m = temp_effects
print(temp_effects)

## females only ##

#define RDH data set
RDH = subset( study2, study2$sex_code==2)

#calculate and display/save scores
temp_scores = scoreItems (temp_key[temp_items,], RDH [,temp_items], missing=T, impute="none")
print(temp_scores$corrected, digits=LOW_PRECISION) #shows calculated rs

temp_effects = cor.ci(RDH[,temp_items], keys=temp_key[temp_items,], n_iter = ITERATIONS, plot=FALSE) #saves bootstrapped effect & ci

#Study1_RDH_Overview = temp_effects
#Study1_RDH_Details = temp_effects
#Study2_RDH_Overview = temp_effects
Study2_RDH_Details_f = temp_effects
print(temp_effects)

#####################################################################
# the objects below contain the correlation results reported in the ms
# i have simply plucked them out by hand for Table 4 & supplement
#####################################################################

Study1_RDH_Overview
Study1_RDH_Details
Study1_RDH_Details_f
Study1_RDH_Details_m

Study2_RDH_Overview
Study2_RDH_Details
Study2_RDH_Details_f
Study2_RDH_Details_m

#these are just the CIs for easy finding if anyone wants them
write.table(Study1_RDH_Details$ci, file="Study1allcorrelations.csv", sep=",", row_names=T) 
write.table(Study2_RDH_Details$ci, file="Study2allcorrelations.csv", sep=",", row_names=T) 

write.table(Study1_RDH_Details_f$ci, file="Study1allcorrelationsFemale.csv", sep=",", row_names=T) 
write.table(Study2_RDH_Details_f$ci, file="Study2allcorrelationsFemale.csv", sep=",", row_names=T) 

write.table(Study1_RDH_Details_m$ci, file="Study1allcorrelationsMale.csv", sep=",", row_names=T) 
write.table(Study2_RDH_Details_m$ci, file="Study2allcorrelationsMale.csv", sep=",", row_names=T) 


### Plots of correlations ###
# don't plan to include in paper, but nice to see

library(ggplot2)
library(reshape2)
# ALL DISGUST MEASURES
#study 1 risk-disgust
plot_study1 = melt(study1[,c("DOSPERT","TDDS_all","TDDS_sexual","TDDS_moral","TDDS_pathogen")], id="DOSPERT")
ggplot(plot_study1, aes(x=DOSPERT, y=value, col=variable)) + geom_point(shape=1) + geom_smooth(method=lm)  +ylab("Disgust") #what is 1
#study 2 risk-disgust
plot_study2 = melt(study2[,c("DOSPERT","TDDS_all","TDDS_sexual","TDDS_moral","TDDS_pathogen", "Curtis")], id="DOSPERT")
ggplot(plot_study2, aes(x=DOSPERT, y=value, col=variable)) + geom_point(shape=1) + geom_smooth(method=lm)  +ylab("Disgust")

#study 1 harm-disgust
plot_study1 = melt(study1[,c("harm_avoid1","TDDS_all","TDDS_sexual","TDDS_moral","TDDS_pathogen")], id="harm_avoid1")
ggplot(plot_study1, aes(x=harm_avoid1, y=value, col=variable)) + geom_point(shape=1) + geom_smooth(method=lm)  +ylab("Disgust")
#study 2 harm-disgust
plot_study2 = melt(study2[,c("harm_avoid2","TDDS_all","TDDS_sexual","TDDS_moral","TDDS_pathogen")], id="harm_avoid2")
ggplot(plot_study2, aes(x=harm_avoid2, y=value, col=variable)) + geom_point(shape=1) + geom_smooth(method=lm)  +ylab("Disgust")











#now moving on to...

###############################################
### HARM AVOIDANCE ITEMS & (RISK & DISGUST) ###
###############################################

#general task here is item-by-item correlations of the
# harm items with the various scale means
# since the Harm Avoidance scale has low alpha

#study1

#DOSPERT
harm_DOSPERT_rs =  lapply (harm_avoidance1, function (x) {cor.test (study1[,x], study1$DOSPERT, alternative="less", use="complete.obs")})  #what is cor.test and cor.ci from earlier?
harm_DOSPERT_table = as.data.frame(
  t(sapply(harm_DOSPERT_rs, function(x) {
    c(x$parameter,
      x$estimate,
      x$p.value
    )
  })))
colnames(harm_DOSPERT_table) = c("df", "DOSPERT r", "DOSPERT p")
row_names(harm_DOSPERT_table) = harm_avoidance1
#harm_DOSPERT_table = merge(harm_DOSPERT_table, items, by="row_names")
print(harm_DOSPERT_table, digits=HIGH_PRECISION, row_names = T, right=F)

#add TDDSs#
#TDDS Pathogen
harm_t_pathogen_rs = lapply (harm_avoidance1, function (x) {cor.test (study1[,x], study1$TDDS.pathogen, alternative="greater", use="complete.obs")}) 
harm_t_pathogen_table = as.data.frame(
  t(sapply(harm_t_pathogen_rs, function(x) {
    c(x$estimate,
      x$p.value)
  })))
colnames(harm_t_pathogen_table) = c("T path r", "T path p")
row_names(harm_t_pathogen_table) = harm_avoidance1
print (harm_t_pathogen_table, digits=HIGH_PRECISION, row_names = T, right=F)

Harm_Risk_Disgust_Table = merge(harm_DOSPERT_table, harm_t_pathogen_table, by="row_names")
#harm_t_pathogen_table = merge(harm_t_pathogen_table, items, by="row_names")

#TDDS Sexual
harm_t_sex_rs = lapply (harm_avoidance1, function (x) {cor.test (study1[,x], study1$TDDS.sex, alternative="greater", use="complete.obs")}) 
harm_t_sex_table = as.data.frame(
  t(sapply(harm_t_sex_rs, function(x) {
    c(x$estimate,
      x$p.value)
  })))
colnames(harm_t_sex_table) = c("T sex r", "T sex p")
row_names(harm_t_sex_table) = harm_avoidance1
#harm_t_sex_table = merge(harm_t_sex_table, items, by="row_names")
#harm_t_sex_table = harm_t_sex_table[c(4,2,3)]
print (harm_t_sex_table, digits=HIGH_PRECISION, row_names = T, right=F)

Harm_Risk_Disgust_Table = merge(Harm_Risk_Disgust_Table, harm_t_sex_table, by_x="Row_names", by_y="row_names")

#TDDS Moral
harm_t_moral_rs = lapply (harm_avoidance1, function (x) {cor.test (study1[,x], study1$TDDS_moral, alternative="greater", use="complete_obs")}) 
harm_t_moral_table = as.data.frame(
  t(sapply(harm_t_moral_rs, function(x) {
    c(x$estimate,
      x$p.value)
  })))
colnames(harm_t_moral_table) = c("T moral r", "T moral p")
row_names(harm_t_moral_table) = harm_avoidance1
#harm_t_moral_table = merge(harm_t_moral_table, items, by="row_names")
#harm_t_moral_table = harm_t_moral_table[c(4,2,3)]
print (harm_t_moral_table, digits=HIGH_PRECISION, row_names = T, right=F)

Harm_Risk_Disgust_Table = merge(Harm_Risk_Disgust_Table, harm_t_moral_table, by_x="Row_names", by_y="row_names")

print(Harm_Risk_Disgust_Table, digits=LOW_PRECISION, row_names = F, left=T)

write.csv (print(Harm_Risk_Disgust_Table, digits=LOW_PRECISION, row_names = F, left=T), "HarmRiskDisgust1.csv")


#study2

#DOSPERT
harm_DOSPERT_rs = lapply (harm_avoidance2, function (x) {cor.test (study2[,x], study2$DOSPERT, alternative="less", use="complete_obs")}) 
harm_DOSPERT_table = as.data.frame(
  t(sapply(harm_DOSPERT_rs, function(x) {
    c(x$parameter,
      x$estimate,
      x$p.value
    )
  })))
colnames(harm_DOSPERT_table) = c("df", "DOSPERT r", "DOSPERT p")
row_names(harm_DOSPERT_table) = harm_avoidance2
harm_DOSPERT_table = merge(harm_DOSPERT_table, study2_items, by="row_names")
print(harm_DOSPERT_table, digits=HIGH_PRECISION, row_names = T, right=F)

#add TDDSs#
#TDDS Pathogen
harm_t_pathogen_rs = lapply (harm_avoidance2, function (x) {cor.test (study2[,x], study2$TDDS_pathogen, alternative="greater", use="complete_obs")}) 
harm_t_pathogen_table = as.data.frame(
  t(sapply(harm_t_pathogen_rs, function(x) {
    c(x$estimate,
      x$p.value)
  })))
colnames(harm_t_pathogen_table) = c("T path r", "T path p")
row_names(harm_t_pathogen_table) = harm_avoidance2
print (harm_t_pathogen_table, digits=HIGH_PRECISION, row_names = T, right=F)

Harm_Risk_Disgust_Table = merge(harm_DOSPERT_table, harm_t_pathogen_table, by_x="Row_names", by_y="row_names")
#harm_t_pathogen_table = merge(harm_t_pathogen_table, items, by="row_names")

#TDDS Sexual
harm_t_sex_rs = lapply (harm_avoidance2, function (x) {cor.test (study2[,x], study2$TDDS_sex, alternative="greater", use="complete_obs")}) 
harm_t_sex_table = as.data.frame(
  t(sapply(harm_t_sex_rs, function(x) {
    c(x$estimate,
      x$p.value)
  })))
colnames(harm_t_sex_table) = c("T sex r", "T sex p")
row_names(harm_t_sex_table) = harm_avoidance2
#harm_t_sex_table = merge(harm_t_sex_table, items, by="row_names")
#harm_t_sex_table = harm_t_sex_table[c(4,2,3)]
print (harm_t_sex_table, digits=HIGH_PRECISION, row_names = T, right=F)

Harm_Risk_Disgust_Table = merge(Harm_Risk_Disgust_Table, harm_t_sex_table, by_x="Row_names", by_y="row_names")

#TDDS Moral
harm_t_moral_rs = lapply (harm_avoidance2, function (x) {cor.test (study2[,x], study2$TDDS_moral, alternative="greater", use="complete_obs")}) 
harm_t_moral_table = as.data.frame(
  t(sapply(harm_t_moral_rs, function(x) {
    c(x$estimate,
      x$p.value)
  })))
colnames(harm_t_moral_table) = c("T moral r", "T moral p")
row_names(harm_t_moral_table) = harm_avoidance2
#harm_t_moral_table = merge(harm_t_moral_table, items, by="row_names")
#harm_t_moral_table = harm_t_moral_table[c(4,2,3)]
print (harm_t_moral_table, digits=HIGH_PRECISION, row_names = T, right=F)

Harm_Risk_Disgust_Table = merge(Harm_Risk_Disgust_Table, harm_t_moral_table, by_x="Row_names", by_y="row_names")

print(Harm_Risk_Disgust_Table, digits=LOW_PRECISION, row_names = F, left=T)

#add Curtis

#TDDS Moral
harm_curtis_rs = lapply (harm_avoidance2, function (x) {cor.test (study2[,x], study2$Curtis, alternative="greater", use="complete_obs")}) 
harm_curtis_table = as.data.frame(
  t(sapply(harm_curtis_rs, function(x) {
    c(x$estimate,
      x$p.value)
  })))
colnames(harm_curtis_table) = c("Curtis r", "Curtis p")
row_names(harm_curtis_table) = harm_avoidance2
print (harm_curtis_table, digits=HIGH_PRECISION, row_names = T, right=F)

Harm_Risk_Disgust_Table = merge(Harm_Risk_Disgust_Table, harm_curtis_table, by_x="Row_names", by_y="row_names")

print(Harm_Risk_Disgust_Table, digits=LOW_PRECISION, row_names = F, left=T)

write.csv (print(Harm_Risk_Disgust_Table, digits=LOW_PRECISION, row_names = F, left=T), "HarmRiskDisgust2.csv")









# DISGUST SEX DIFFS TO INCLUDE IN META-ANALYSIS
# are these redundant with the above? (I did them at different times)
# study 1 
#raw effect and p values based on Welsh's t
measures_disgust1 = list ("TDDS_all", "TDDS_sexual", "TDDS_moral", "TDDS_pathogen")
tests_sex_disgustharm1 = lapply (measures_disgust1, function (x) 
{t.test (study1[,x] ~ study1$sex_code, alternative="less", na.action = na.omit)}) 

sex_differences_disgust1 = as.data.frame(
  t(sapply(tests_sex_disgustharm1, function(x) {
    c( x$estimate[2]-x$estimate[1],
       x$p.value,
       x$parameter)
  })))
colnames(sex_differences_disgust1) = c("raw effect", "p.value","df")
row_names(sex_differences_disgust1) = measures_disgust1
print(sex_differences_disgust1, digits=HIGH_PRECISION)

# calculate d
library(effsize)
d_sex_disgustharm1 = lapply (measures_disgust1, function (x) 
{cohen.d (study1[,x] ~ study1$sex_code, na.rm=T,conf_level=HIGH_CONFIDENCE)}) 

d_sex_differences_disgust1 = as.data.frame(
  t(sapply(d_sex_disgustharm1, function(x) {
    c(-x$estimate, #these are reversed to be consistent with meta-analysis effect direction
      -x$conf_int[2] , 
      -x$conf_int[1])
  })))

colnames(d_sex_differences_disgust1) = c("d", "-95%CI" , "+95%CI")
row_names(d_sex_differences_disgust1) = measures_disgust1
print(d_sex_differences_disgust1, digits=HIGH_PRECISION)

#summarize study 1 disgust
sex_disgustharm1 = merge(d_sex_differences_disgust1,  sex_differences_disgust1, by="row_names", all=TRUE  )
print(sex_disgustharm1, digits=HIGH_PRECISION)


# study 2 disgust raw effect and p values based on Welsh's t
measures_disgust2 = list ("TDDS_all", "TDDS_sexual", "TDDS_moral", "TDDS_pathogen", "Curtis")
t_tests_sex_disgust2 =lapply (measures_disgust2, function (x) 
{t.test (study2[,x] ~ study2$sex_code, alternative="less", na.action = na.omit)}) 

t_sex_differences_disgust2 = as.data.frame(
  t(sapply(t_tests_sex_disgust2, function(x) {c(
    x$estimate[2]-x$estimate[1],
    x$p.value,
    x$parameter)
  })))
colnames(t_sex_differences_disgust2) = c("raw effect" ,"p.value","df")
row_names(t_sex_differences_disgust2) = measures_disgust2
print(t_sex_differences_disgust2, digits=HIGH_PRECISION)

#  calculate d
d_sex_disgust2 = lapply (measures_disgust2, function (x) 
{cohen.d (study2[,x], study2$sex_code,  na.rm=T,conf_level=HIGH_CONFIDENCE)})

d_sex_differences_disgust2 = as.data.frame(
  t(sapply(d_sex_disgust2, function(x) {
    c(-x$estimate, #these are reversed to be consistent with meta-analysis effect direction
      -x$conf_int[2] , 
      -x$conf_int[1])
  })))

colnames(d_sex_differences_disgust2) = c("d", "-95%CI" , "+95%CI")
row_names(d_sex_differences_disgust2) = measures_disgust2
print(d_sex_differences_disgust2, digits=HIGH_PRECISION)

#summarize study 2 disgust
sex_disgust2 = merge(d_sex_differences_disgust2, t_sex_differences_disgust2,  by="row_names", all=TRUE  )
print(sex_disgust2, digits=HIGH_PRECISION)

########################
#| | | TABLE  | | | | |#
########################
DisgustSexDifferences = merge(x=sex_disgustharm1 , y=sex_disgust2,
                              by="Row_names",all=T, suffixes=c(".study1", ".study2"))
print(DisgustSexDifferences, digits=HIGH_PRECISION)
write.table(DisgustSexDifferences, file="DisgustSex95.csv", sep=",", row_names=F) 


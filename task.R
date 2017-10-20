#I am changing this file a little bit
# rachel commented here.

### Loading libraries
install.packages("pastecs")
install.packages("GGally")
library(pastecs)
library(Hmisc)
library(GGally)
library(MASS)
library(plyr)


rm(list = ls())
### Loading data
mhsa <- read.csv("~/Desktop/CEPR/Phone Interview/MHSA_case_data.csv") 


### Renaming variables 
colnames(mhsa)[1:21] <- c("gradyear","school","id","male","frl","lep","sped","math8","read8",
                          "satv","satm","satw","grad","gradin4","enrl","enrl4","enrl2",
                          "days","days4","days2","counselor")

### Changing data type
str(mhsa) 
mhsa$gradyear <- as.factor(mhsa$gradyear)
mhsa$id <- as.factor(mhsa$id)
mhsa$male <- as.factor(mhsa$male)
mhsa$frl <- as.factor(mhsa$frl)
mhsa$lep <- as.factor(mhsa$lep)
mhsa$sped <- as.factor(mhsa$sped)
mhsa$grad <- as.factor(mhsa$grad)
mhsa$gradin4 <- as.factor(mhsa$gradin4)
#mhsa$enrl <- as.factor(mhsa$enrl)
#mhsa$enrl4 <- as.factor(mhsa$enrl4)
#mhsa$enrl2 <- as.factor(mhsa$enrl2)
mhsa$counselor <- as.factor(mhsa$counselor)
str(mhsa) 

####################################################Exploratory data analysis

###### Univariate
mhsa_num <- mhsa[,c("math8","read8","satv","satm","satw","days","days4","days2")] # 8 numerical
mhsa_cat <- mhsa[,c("gradyear","school","male","frl","lep","sped","gradin4","enrl",
                    "enrl4","enrl2","counselor")] # 12 categorical

#-----Summaries of each variable
sapply(mhsa_num, summary) # NA: math8 & read8(2500), satv & satm(3350), satw(8600), days(6500), days4(7500), days2(8900)
sapply(mhsa_cat, summary) # NA: 'counselor'(283)
summary(factor(mhsa$enrl4))

#----- Plot of each variable
sapply(mhsa_num, hist, breaks=30) # normal: math8, satv, satm, satw | left-skewed: read8, days, days4 | right skewed : days2 
sapply(mhsa_cat, plot) # High school: size differ | gender: balanced | frl: somewhat unbalanced | lep,sped: very unbalanced | gradin4: almost all | enrl: unbalanced (only 60%) | enrl4: more 0, quite balanced | enrl2: unbalanced, more 0 |


###### Bivariate
#--- Numerical
ggpairs(mhsa_num[,1:5]) # high correlation among the grades

#--- dropping grad
drops <- c("grad")
mhsa <- mhsa[ , !(names(mhsa) %in% drops)]

#################################################### Simple regressions

#--- Enrollment in any college
#------------ All scores
fit1 <- glm(enrl ~ gradyear + school + male + frl + lep + sped 
            + math8 + read8 + satv + satm + satw + gradin4 + counselor, 
            family=binomial("logit"), data=mhsa)
summary(fit1) # significant: Male, FRL, math8, satm, counselor42,71,103, 112
              # 9191 rows deleted

#------------ SAT scores
fit2 <- glm(enrl ~ gradyear + school + male + frl + lep + sped 
            + satv + satm + satw + gradin4 + counselor, 
            family=binomial("logit"), data=mhsa)
summary(fit2) # significant: male, satm, counselor 71,97,112
              # 8699 rows deleted

#------------ 8th grade scores
fit3 <- glm(enrl ~ gradyear + school + male + frl + lep + sped 
            + math8 + read8 + gradin4 + counselor, 
            family=binomial("logit"), data=mhsa)
summary(fit3) # significant: HS: King, Meadowbrook, South Falls, male, frl, sped, math8, read8, gradin4, counselor 44,51111, 112, 115
              # 2724 rows deleted
fit3_step <- stepAIC(fit3, direction="both", na.action=na.exclude)


#####################################################  Imputation of missing data
#---------------------------------- math8 & satm

# Imputing using aregImpute in Hsmic package, based on the theoretically important variables
impute_math8_satm <- aregImpute(~ school + male + frl + lep + sped 
                         + math8 + satm + gradin4, data = mhsa, n.impute = 5)
# turning into a data frame
impute_math8_df <- data.frame(impute_math8_satm$imputed$math8)
impute_satm_df <- data.frame(impute_math8_satm$imputed$satm)

# taking means of 5 imputed values for each student
math8_all <- data.frame(rowMeans(impute_math8_df, dims = 1))
satm_all <- data.frame(rowMeans(impute_satm_df, dims = 1))

# replacing the names
names(math8_all) <- c("math8")
names(satm_all) <- c("satm")

# replace the imputed values with NA in the data
mhsa$math8[which(is.na(mhsa$math8))] <- math8_all[which(!is.na(math8_all$math8)),]
mhsa$satm[which(is.na(mhsa$satm))] <- satm_all[which(!is.na(satm_all$satm)),]
describe(mhsa) 


#---------------------------------- read8 & satv
# Imputing using aregImpute in Hsmic package, based on the theoretically important variables
impute_read8_satv_satw <- aregImpute(~ school + male + frl + lep + sped 
                           + read8 + satv + satw + gradin4, data = mhsa, n.impute = 5)
# turning into a data frame
impute_read8_df <- data.frame(impute_read8_satv_satw$imputed$read8)
impute_satv_df <- data.frame(impute_read8_satv_satw$imputed$satv)
impute_satw_df <- data.frame(impute_read8_satv_satw$imputed$satw)

# taking means of 5 imputed values for each student
read8_all <- data.frame(rowMeans(impute_read8_df, dims = 1))
satv_all <- data.frame(rowMeans(impute_satv_df, dims = 1))
satw_all <- data.frame(rowMeans(impute_satw_df, dims = 1))

# replacing the names
names(read8_all) <- c("read8")
names(satv_all) <- c("satv")
names(satw_all) <- c("satw")

# replace the imputed values with NA in the data
mhsa$read8[which(is.na(mhsa$read8))] <- read8_all[which(!is.na(read8_all$read8)),]
mhsa$satv[which(is.na(mhsa$satv))] <- satv_all[which(!is.na(satv_all$satv)),]
mhsa$satw[which(is.na(mhsa$satw))] <- satw_all[which(!is.na(satw_all$satw)),]
describe(mhsa) 


#---------------------------------- counselor
# Imputing using aregImpute in Hsmic package, based on the theoretically important variables
impute_counselor <- aregImpute(~ school + counselor, data = mhsa, n.impute = 5)

# turning into a data frame
impute_counselor_df <- data.frame(impute_counselor$imputed$counselor)
impute_counselor_df



#################################################### Simple regressions with imputed values
impute_counselor <- aregImpute(~ school + counselor, data = mhsa, n.impute = 5)


#--- Enrollment in any college
#------------ All scores
fit1.b <- glm(enrl ~ gradyear + school + male + frl + lep + sped 
            + math8 + read8 + satv + satm + satw + gradin4 + counselor, 
            family=binomial("logit"), data=mhsa)
summary(fit1.b) # significant: King, male, frl, read8, satm, gradin4, counsellors 112, 115
                # 283 rows deleted

#------------ SAT scores
fit2.b <- glm(enrl ~ gradyear + school + male + frl + lep + sped 
            + satv + satm + satw + gradin4 + counselor, 
            family=binomial("logit"), data=mhsa)
summary(fit2.b) # significant: King HS, male, frl, satm, grad4, counselor 112, 115


#------------ 8th grade scores
fit3.b <- glm(enrl ~ gradyear + school + male + frl + lep + sped 
            + math8 + read8 + gradin4 + counselor, 
            family=binomial("logit"), data=mhsa)
summary(fit3.b) # significant: HS: King HS, male, frl, math8, read8, gradin4, counselor 97, 112, 115















#--------------------------------------- enrl and days

new <- subset(mhsa, is.na(days4) | is.na(enrl4))

new <-mhsa[mhsa$enrl4 == '1' & is.na(days4),]
View(new)


######################### others
impute_read8 <- aregImpute(~ school + male + frl + lep + sped 
                           + read8 + gradin4, data = mhsa, n.impute = 5)
impute_read8_df <- data.frame(impute_read8$imputed$read8)
read8_all <- data.frame(rowMeans(impute_read8_df, dims = 1))
names(read8_all) <- c("read8")
mhsa$read8[which(is.na(mhsa$read8))] 

mhsa$read8[which(is.na(mhsa$read8))] <- read8_all[which(!is.na(read8_all$read8)),]
describe(mhsa$read8)



# example
impute_math8 <- aregImpute(~ school + male + frl + lep + sped 
                   + math8 + gradin4, data = mhsa, n.impute = 5)
# get a list of those variables with imputed values
imp.vars <- names(impute_math8$imputed)

# compute mean imputed value for each variable
# and extract the original indices of the missing data, by variable
imp.vars.mean <- lapply(x.ar$imputed, function(i) apply(i, 1, mean))
imp.vars.idx <- lapply(imp.vars.mean, function(i) as.integer(names(i)))

mhsa$math8[which(is.na(mhsa$math8))] <- math8_all[which(!is.na(math8_all$math8)),]
describe(mhsa$math8)
# copy origial data
x.no.na <- x

# loop over imputed variables
for(i in imp.vars)
{
  print(i)
  # get the mean imputations for this variable
  imp.i <- imp.vars.mean[[i]]
  # get the original indices for NA
  idx.i <- imp.vars.idx[[i]]
  # replace original NA with imputed values
  x.no.na[idx.i, i] <- imp.i
}






mhsa_new <- merge(mhsa, math_all, by=)

dim(math8_all)
x<- math8_all
View(math8_all)
str(x)
mhsa_new <- mhsa
nrow(mhsa_new$math8[is.na(mhsa_new$math8) & !is.na(mhsa_new$satv)])
mhsa_new$math8[is.na(mhsa_new$math8) & !is.na(mhsa_new$satv)] <- math8_all
$nocue_dv[is.na(exp$donate)]
new2 <- mhsa_new[is.na(mhsa_new$math8) & !is.na(mhsa_new$satv),] 

View(new2)
impute_arg_read8 <- aregImpute(~ school + male + frl + lep + sped 
                         + math8 + gradin4, data = mhsa, n.impute = 5)



missing_scores_m = mhsa[ which(mhsa$math8 == 'NA' & mhsa$satm == 'NA'), ]



dim(missing_scores_m)
View(missing_scores_m)
summary(missing_scores_m$school)

library("Hmisc")

str(mhsa)


impute_arg <- aregImpute(~ school + male + frl + lep + sped 
                         + math8 + gradin4, data = mhsa, n.impute = 5)
match='closest')
impute_arg
impute_arg$imputed$math8[1:5,]
math <- data.frame(impute_arg$imputed$math8[1:5,])
math2 <- rowMeans(math, dims = 1)
math2
ddply(math, .(group), cMeacns)

# Impute
lm(math8 ~ year + male + school + , data=mhsa)


exp$donate[is.na(exp$donate)] <- exp$nocue_dv[is.na(exp$donate)]


#--- Stepwise Regression
fit1_step <- stepAIC(fit1, direction="both", na.rm=FALSE)
step$anova # display results

3300/11400

################################################  Collapsing data by school
mhsa_sch <- ddply(mhsa2, "school", summarize, male=mean(male), frl=mean(frl), lep=mean(lep),
                  sped=mean(sped), math8=mean(math8,, na.rm=TRUE), read8=mean(read8,, na.rm=TRUE),
                  satv=mean(satv, na.rm=TRUE), satm=mean(satm, na.rm=TRUE), satw=mean(satw, na.rm=TRUE),
                  gradin4=mean(gradin4), enrl=mean(enrl),enrl4=mean(enrl4),enrl2=mean(enrl2),
                  days=mean(days, na.rm=TRUE),days4=mean(days4, na.rm=TRUE),days2=mean(days2, na.rm=TRUE))
View(mhsa_sch) 

# collapsing by counselors
mhsa_cslr <- ddply(mhsa2, "counselor", summarize, male=mean(male), frl=mean(frl), lep=mean(lep),
                  sped=mean(sped), math8=mean(math8,, na.rm=TRUE), read8=mean(read8,, na.rm=TRUE),
                  satv=mean(satv, na.rm=TRUE), satm=mean(satm, na.rm=TRUE), satw=mean(satw, na.rm=TRUE),
                  gradin4=mean(gradin4), enrl=mean(enrl),enrl4=mean(enrl4),enrl2=mean(enrl2),
                  days=mean(days, na.rm=TRUE),days4=mean(days4, na.rm=TRUE),days2=mean(days2, na.rm=TRUE))
View(mhsa_sch)
View(mhsa_cslr)
mhsa_cslr$counselor <- as.factor(mhsa_cslr$counselor)

plot(enrl ~ school, data=mhsa_sch)
plot(enrl2 ~ school, data=mhsa_sch)
plot(enrl4 ~ school, data=mhsa_sch)

plot(math8 ~ school, data=mhsa_sch)
plot(satw ~ school, data=mhsa_sch)
plot(satv ~ school, data=mhsa_sch)

plot(enrl~ counselor, data=mhsa_cslr)

#na.omit(mhsa[c(math8, read8, counselor)])
#complete_dat = data[is.na(complete.cases(data)),]
#sapply(mhsa, describe)
#describe(mhsa$school)

#complete_dat = data[!is.na(complete.cases(data)),]
#View(newdata)



### script for analysis of clutch size, hatching failure + fledging success senesence vs early life telomere length ### 

### best models 
# clutch size = cmp.egg.model2
# hatching success = age.model1
# fledging success = fledge.model.tmb

#### LOAD + SORT DATA #### 
library(ggplot2)
library(plyr)
library(dplyr)
library(readr)


### updated database with most recent census data on the 23/03/2022

fulldata <- read_csv("Master Database 4 (1).csv", 
                     col_types = cols(delta.ct.linregpcr = col_number(), 
                                      female.RTL.linregpcr = col_number(), 
                                      female.reprod.age = col_number(),
                                      female.lifespan = col_number(),
                                      year = col_character()))
View(fulldata)
View(fulldata)
View(fulldata)

# remove empty columns # 

fulldata<-Filter(function(x)!all(is.na(x)), fulldata)

# sort out the classes of the variables # 

fulldata$female.cohort<-as.factor(fulldata$female.cohort)
fulldata$clutch<-as.factor(fulldata$clutch)
fulldata$st.clutch<-as.factor(fulldata$st.clutch)
fulldata$female.code<-as.factor(fulldata$female.code)
fulldata$year<-as.factor(fulldata$year)
fulldata$female.lifespan<-as.numeric(fulldata$female.lifespan)
fulldata$fledged<-as.numeric(fulldata$fledged)
fulldata$hatched<-as.numeric(fulldata$hatched)
fulldata$unhatched<-as.numeric(fulldata$unhatched)
fulldata$female.age<-as.numeric(fulldata$female.age)
fulldata$laying.date<-as.Date.character(fulldata$laying.date, format = "%d/%m/%Y")
fulldata$hatch.date<-as.Date.character(fulldata$hatch.date, format = "%d/%m/%Y")
fulldata$female.dob<-as.Date.character(fulldata$female.dob, format = "%d/%m/%Y")
fulldata$rtl.delta.ct<-as.numeric(fulldata$rtl.delta.ct)
fulldata$females.mother<-as.factor(fulldata$females.mother)
fulldata$female.code<-as.factor(fulldata$female.code)
fulldata$reprod.lifespan<-as.numeric(fulldata$reprod.lifespan)

fulldata$female.age<-fulldata$female.age/365
fulldata$female.lifespan<-fulldata$female.lifespan/365
fulldata$days.before.death<-fulldata$days.before.death/365
fulldata$male.age<-fulldata$male.age/365

# create new clutch variable relevant to reproductive effort

fulldata$clutch.effort[fulldata$st.clutch == "1"] <- "1"
fulldata$clutch.effort[fulldata$st.clutch == "R1"] <- "2"
fulldata$clutch.effort[fulldata$st.clutch == "2"] <- "2"
fulldata$clutch.effort[fulldata$st.clutch == "R2"] <- "3"
fulldata$clutch.effort[fulldata$st.clutch == "3"] <- "3"

# classify female age into years

fulldata$female.age.year[fulldata$female.age<365]<-1
fulldata$female.age.year[fulldata$female.age>=365 & fulldata$female.age<730]<-2
fulldata$female.age.year[fulldata$female.age>=730 & fulldata$female.age<(365*3)]<-3
fulldata$female.age.year[fulldata$female.age>=(365*3) & fulldata$female.age<(365*4)]<-4
fulldata$female.age.year[fulldata$female.age>=(365*4) & fulldata$female.age<(365*5)]<-5
fulldata$female.age.year[fulldata$female.age>=(365*5) & fulldata$female.age<(365*6)]<-6
fulldata$female.age.year[fulldata$female.age>=(365*6) & fulldata$female.age<(365*7)]<-7
fulldata$female.age.year[fulldata$female.age>=(365*7) & fulldata$female.age<(365*8)]<-8
fulldata$female.age.year[fulldata$female.age>=(365*8) & fulldata$female.age<(365*9)]<-9
fulldata$female.age.year[fulldata$female.age>=(365*9) & fulldata$female.age<=(3661)]<-10


# put in inbreeding coefficients
fulldata<-left_join(fulldata, inbreeding_values_2)

# put in lifetime reproductive success variables
fulldata<-left_join(fulldata,lifetime.reprod, by="female.code")

# create complete df of female age data #

female.age.data<-data.frame(fulldata$female.code, fulldata$female.cohort, fulldata$female.age,
                            fulldata$female.lifespan, fulldata$eggs,fulldata$hatching.success, 
                            fulldata$hatched, fulldata$unhatched, 
                            fulldata$year, 
                            fulldata$rtl.delta.ct,
                            fulldata$fledged, fulldata$rel.laying.date)

female.age.data<-na.omit(female.age.data)

names(female.age.data)<-c("female.code", "female.cohort",
                          "female.age", "female.lifespan", "eggs", 
                          "hatching.success", "hatched", 
                          "unhatched", "year", 
                          "linreg.delta.ct",
                          "fledged", "rel.laying.date")

female.age.data<-left_join(female.age.data, lifetime.reprod, by="female.code")
#female.age.data<-left_join(female.age.data, inbreeding_values_2, by="female.code")

female.age.data$female.age<-as.numeric(female.age.data$female.age)
female.age.data$female.reprod.age<-as.numeric(female.age.data$female.reprod.age)
female.age.data$hatched<-as.numeric(female.age.data$hatched)
female.age.data$unhatched<-as.numeric(female.age.data$unhatched)

# scaling down variable to year instead of days
female.age.data$female.age<-female.age.data$female.age/365
female.age.data$female.lifespan<-female.age.data$female.lifespan/365
female.age.data$female.reprod.age<-female.age.data$female.reprod.age/365

# classify as year
female.age.data$female.age.year[female.age.data$female.age<1]<-1
female.age.data$female.age.year[female.age.data$female.age>=1 & female.age.data$female.age<2]<-2
female.age.data$female.age.year[female.age.data$female.age>=2 & female.age.data$female.age<(1*3)]<-3
female.age.data$female.age.year[female.age.data$female.age>=(1*3) & female.age.data$female.age<(1*4)]<-4
female.age.data$female.age.year[female.age.data$female.age>=(1*4) & female.age.data$female.age<(1*5)]<-5
female.age.data$female.age.year[female.age.data$female.age>=(1*5) & female.age.data$female.age<(1*6)]<-6
female.age.data$female.age.year[female.age.data$female.age>=(1*6) & female.age.data$female.age<(1*7)]<-7
female.age.data$female.age.year[female.age.data$female.age>=(1*7) & female.age.data$female.age<(1*8)]<-8
female.age.data$female.age.year[female.age.data$female.age>=(1*8) & female.age.data$female.age<(1*9)]<-9
female.age.data$female.age.year[female.age.data$female.age>=(1*9) & female.age.data$female.age<=(10.5)]<-10


#### HATCHING SUCCESS ####
# including year as a random factor gives a better AIC value and makes sense as hatching success varies across years # 
# including ID as a random factor controls for selective disappearance when testing age effects # 

#full  model

library(glmmTMB)
age.model<-glmmTMB(cbind(hatched,unhatched)~ linreg.delta.ct*(I(female.age^2))+linreg.delta.ct*female.age +
                     female.lifespan + (1|year)+ (1|female.code) , data=female.age.data,family="betabinomial")

## with scaled age + telomere length


female.age.data$scaled.age<-scale(female.age.data$female.age, scale=FALSE)

female.age.data$linreg.delta.ct<-scale(female.age.data$linreg.delta.ct, scale=FALSE)

age.model1<-glmmTMB(cbind(hatched,unhatched)~ scaled.age+linreg.delta.ct+(I(scaled.age^2))+
                      female.lifespan + (1|year)+ (1|female.code) + (1|rel.laying.date) , data=female.age.data,family="betabinomial")

testDispersion(age.model1)

# including interaction between linear age term and rtl messes up residuals plot 

DHARMa::plotQQunif(age.model1)
DHARMa::plotResiduals(age.model1)
DHARMa::testOverdispersion(age.model1)

# much better diagnostics in glmmTMB in betabinomial


summary(age.model)

summary(age.model1)


### breakpoint age model ###
# package strucchange to estimate breakpoints 

library(strucchange)

bp.hatching<-breakpoints(cbind(hatched,unhatched)~female.age, h=0.15, breaks=5,
                         data=female.age.data)

summary(bp.hatching)
plot(bp.hatching)
# plots bayesian information critera (similar to AIC - lower is preferred) 
# and residual sum of squares (measure of discrepancy between data and model - smaller = tighter fit) 
# agasinst number of breakpoints
# based on plot - 1 breakpoint has lowest BIC but 2 breakpoints is only marginally higher BIC and much lower RSS)
# the package recommends that the two breakpoints be at 0.34 and 0.71

#create breakpoints at points recommended by strucchange
bp = 0.42

#create a function which can divide female age variable by breakpoints
b1 <- function(x, bp) ifelse(x < bp, x, 0)
b2 <- function(x, bp) ifelse(x < bp, 0, x)


library(nlme)

breakpoint1 <- glmer(cbind(hatched, unhatched) ~ b1(female.age, bp) + b2(female.age, bp)+
                       female.age*linreg.delta.ct + female.lifespan + (1|year) +(1|female.code), 
                     nAGQ=0, data = female.age.data, na.action=na.exclude, family="binomial")

testDispersion(breakpoint1)

#it's a bit overdispersed so i'm going to adjust results for a "quasi-binomial family"

quasi_table <- function(model,ctab=coef(summary(model)),
                        phi=overdisp_fun(model)["ratio"]) {
  qctab <- within(as.data.frame(ctab),
                  {   `Std. Error` <- `Std. Error`*sqrt(phi)
                  `z value` <- Estimate/`Std. Error`
                  `Pr(>|z|)` <- 2*pnorm(abs(`z value`), lower.tail=FALSE)
                  })
  return(qctab)
}
printCoefmat(quasi_table(breakpoint1),digits=3)

summary(breakpoint1)


#try with breakpoints that make sense by eyeballing data
bp = 2
bp2 = 5
#create a function which can divide female age variable by breakpoints
b1 <- function(x, bp) ifelse(x < bp, x, 0)
b2 <- function(x, bp) ifelse(x < bp, 0, x)
b3 <- function(x, bp2) ifelse(x < bp2, x, 0)
b4 <- function(x, bp2) ifelse(x < bp2, 0, x)

library(nlme)

breakpoint2 <- glmer(cbind(hatched, unhatched) ~ b1(female.age, bp) + b2(female.age, bp)+
                       b3(female.age, bp2) + b4(female.age, bp2)+
                       female.age*linreg.delta.ct + female.lifespan  + (1|year) +(1|female.code) + (1|rel.laying.date), nAGQ=0, data = female.age.data, na.action=na.exclude, family="binomial")

summary(breakpoint2)
anova(breakpoint2, test="Chisq")
drop1(breakpoint2)

car::Anova(breakpoint2)

plot(breakpoint2)

# drop breakpoints
breakpoint2.a <- glmer(cbind(hatched, unhatched) ~ 
                         female.age*linreg.delta.ct + female.lifespan + (1|year) +(1|female.code)+ (1|rel.laying.date), nAGQ=0, data = female.age.data, na.action=na.exclude, family="binomial")
anova(breakpoint2,breakpoint2.a)

# drop age
breakpoint2.b <- glmer(cbind(hatched, unhatched) ~ linreg.delta.ct + female.lifespan + (1|year) +(1|female.code)+ (1|rel.laying.date), nAGQ=0, data = female.age.data, na.action=na.exclude, family="binomial")
anova(breakpoint2,breakpoint2.b)

# drop delta RTL
breakpoint2.ca <- glmer(cbind(hatched, unhatched) ~ b1(female.age, bp) + b2(female.age, bp)+
                          b3(female.age, bp2) + b4(female.age, bp2)+
                          female.age + female.lifespan + (1|year) +(1|female.code)+ (1|rel.laying.date), nAGQ=0, data = female.age.data, na.action=na.exclude, family="binomial")
anova(breakpoint2, breakpoint2.ca)

# drop lifespan
breakpoint2.d <- glmer(cbind(hatched, unhatched) ~ b1(female.age, bp) + b2(female.age, bp)+
                         b3(female.age, bp2) + b4(female.age, bp2)+
                         female.age*linreg.delta.ct  + (1|year) +(1|female.code)+ (1|rel.laying.date), nAGQ=0, data = female.age.data, na.action=na.exclude, family="binomial")
anova(breakpoint2,breakpoint2.d)

#drop interaction
breakpoint2.e <- glmer(cbind(hatched, unhatched) ~ b1(female.age, bp) + b2(female.age, bp)+
                         b3(female.age, bp2) + b4(female.age, bp2)+
                         female.age+linreg.delta.ct + female.lifespan + (1|year) +(1|female.code)+ (1|rel.laying.date), nAGQ=0, data = female.age.data, na.action=na.exclude, family="binomial")
anova(breakpoint2,breakpoint2.e)

# drop clutch effort (not significant, remove from model)
breakpoint2.f <- glmer(cbind(hatched, unhatched) ~ b1(female.age, bp) + b2(female.age, bp)+
                         b3(female.age, bp2) + b4(female.age, bp2)+
                         female.age*linreg.delta.ct + female.lifespan  + (1|year) +(1|female.code), nAGQ=0, data = female.age.data, na.action=na.exclude, family="binomial")
anova(breakpoint2,breakpoint2.f)



#### RATE OF SENESCENCE IN TRAITS ####

aging_data<-fulldata %>% filter(female.age >5)

hatching.slopes.model <- glm(cbind(hatched, unhatched) ~ female.age + female.code, data = aging_data, na.action=na.exclude, family="binomial")
summary(hatching.slopes.model)
## get regression coefficients (slopes)
hatching.slopes<-coef(hatching.slopes.model)

fledging.slopes.model<-glm(cbind(fledged, dead.chicks) ~ female.age + female.code, data = aging_data, na.action=na.exclude, family="binomial")
summary(fledging.slopes.model)
fledging.slopes<-as.data.frame(coef(fledging.slopes.model), col.names(c("female.code", "fledging.rate.senescence")))

eggs.slopes.model<-glm(eggs ~ female.age + female.code, data = aging_data, na.action=na.exclude, family="poisson")
summary(eggs.slopes.model)
eggs.slopes<-coef(eggs.slopes.model)

data

#### CLUTCH SIZE ####
#test female age effects on reproductive characteristics # 
# first : clutch size # 
ggplot(data=female.age.data, aes(x=female.age, y=eggs))+
  geom_count(aes(x=female.age, y=eggs),
             data = NULL,
             stat = "sum",
             position = "identity",
             na.rm = FALSE,
             show.legend = TRUE,
             inherit.aes = FALSE,
             colour="#56B4E9",
             alpha=0.5)+
  geom_smooth()+
  xlab("Female Age (days)")+
  ylab("Clutch Size")+
  theme_minimal()+
  scale_size(range=c(1,10))

# try a COM-Poisson model (good for underdispersed data such as clutch size) # 
library(glmmTMB)

female.age.data$female.code<-as.character(female.age.data$female.code)


# quadratic age
# scaled age
## BEST MODEL FOR CLUTCH SIZE ##
cmp.egg.model2<-glmmTMB(formula=eggs~scaled.age+linreg.delta.ct*I(scaled.age^2)+female.lifespan+
                          (1|year)+(1|female.code)+(1|rel.laying.date), 
                        data=female.age.data, family="compois")

summary(cmp.egg.model2)

DHARMa::plotQQunif(cmp.egg.model2)
DHARMa::plotResiduals(cmp.egg.model2)

cmp.egg.model2a<-glmmTMB(formula=eggs~female.age+linreg.delta.ct+I(female.age^2)+female.lifespan+
                                                                     (1|year)+(1|female.code)+(1|rel.laying.date), 
                                                                   data=female.age.data, family="compois"))

summary(cmp.egg.model2a)

cmp.egg.model2b<-glmmTMB(formula=eggs~female.age*linreg.delta.ct*I(female.age^2)+female.lifespan+
                           (1|year)+(1|female.code)+(1|rel.laying.date), 
                         data=female.age.data, family="compois"))

summary(cmp.egg.model2b)

cmp.egg.model2c<-glmmTMB(formula=eggs~female.age*linreg.delta.ct+I(female.age^2)+female.lifespan+
                           (1|year)+(1|female.code)+(1|rel.laying.date), 
                         data=female.age.data, family="compois"))

summary(cmp.egg.model2c)

library(DHARMa)
testOverdispersion(cmp.egg.model2)
## no underdispersion when including age as quadratic

DHARMa::plotQQunif(cmp.egg.model2)
DHARMa::plotResiduals(cmp.egg.model2)

## diagnostic plots look good

summary(cmp.egg.model2)

# anova not possible on cmp model, use summary statistics
# age, telomere and interaction has significant affect on eggs laid # 


# plot mean clutch size by female age (catergorical)

summarydata.clutch<-ddply(fulldata, c("female.age.year"), summarise,
                          N=length(female.code),
                          mean=mean(eggs),
                          sd = sd(eggs),
                          se = sd/sqrt(N)
)

summarydata.clutch$female.age.year<-as.numeric(summarydata$female.age.year)

clutch.plot<-ggplot(data=na.omit(summarydata.clutch), aes(x=female.age.year, y=mean))+
  geom_point(size=2.5, colour="hotpink3")+
  geom_line(colour="hotpink3", size=0.75)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, size=0.75, colour="hotpink3", alpha=0.5)+
  theme_minimal()+
  xlab("Female Age (years)")+
  ylab("Clutch Size")+
  scale_x_discrete(name="Female Age (Years)", limits=c("1","2","3","4","5","6","7","8","9","10"))

grid.arrange(hatching.plot, clutch.plot)


#### FLEDGING SUCCESS ####
## test female age affects on fledging success

female.age.data$fledging.success<-female.age.data$fledged/female.age.data$hatched
female.age.data$dead.chicks<-female.age.data$hatched-female.age.data$fledged

# plot mean fledgling success size by female age (catergorical)

ggplot(data=female.age.data, aes(x=female.age, y=fledged/hatched))+
  geom_count(aes(x=female.age, y=fledged/hatched))+
  geom_smooth()

summarydata.fledge<-ddply(na.omit(female.age.data), c("female.age.year"), summarise,
                          N=length(female.code),
                          mean=mean(fledged),
                          sd = sd(fledged),
                          se = sd/sqrt(N)
)

summarydata.fledge$female.age.year<-as.numeric(summarydata$female.age.year)

fledge.plot<-ggplot(data=na.omit(summarydata.fledge), aes(x=female.age.year, y=mean))+
  geom_point(size=2.5, colour="orange1")+
  geom_line(colour="orange1", size=0.75)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, size=0.75, colour="orange1",fill="orange1", alpha=0.5)+
  theme_minimal()+
  xlab("Female Age (years)")+
  ylab("Fledging Success")+
  scale_x_discrete(name="Female Age (Years)", limits=c("1","2","3","4","5","6","7","8","9","10"))

fledge.data<-data.frame(fulldata$female.code, fulldata$female.age, fulldata$year, 
                        fulldata$female.lifespan, fulldata$dead.chicks, fulldata$fledged, fulldata$rel.laying.date, fulldata$rtl.delta.ct, fulldata$male.age)

names(fledge.data)<-c("female.code", "female.age", "year", "female.lifespan", "dead.chicks", "fledged", "rel.laying.date", "rtl.delta.ct", "male.age")

fledge.data<-na.omit(fledge.data)

# model age against fledging success, use zero inflated model because counts of fledglings are zero inflated 

#include male age 
fledge.model.2<-glmmTMB(cbind(dead.chicks,fledged)~ I(female.age^2)*rtl.delta.ct + female.age +
                          female.lifespan + (1|female.code) + (1|year) + (1|male.age) +
                          (1|rel.laying.date),
                        data=fledge.data,
                        ziformula = ~1, 
                        family=binomial)

# won't converge - correlation between female age and male age?
# remove male age
fledge.model.2<-glmmTMB(cbind(dead.chicks,fledged)~ I(female.age^2)*rtl.delta.ct + female.age +
                          female.lifespan + (1|female.code) + (1|year) +
                          (1|rel.laying.date),
                        data=fledge.data,
                        ziformula = ~1, 
                        family=binomial)

testDispersion(fledge.model.2)

DHARMa::plotResiduals(fledge.model.1)
DHARMa::plotQQunif(fledge.model.1)

# diagnostics good 

summary(fledge.model.2)
#AIC = 589.4

## with males

fledge.model.tmb.male<-glmmTMB(cbind(fledged,dead.chicks)~ I(scaled.age^2)*rtl.delta.ct + scaled.age*rtl.delta.ct+
                                 female.lifespan + (1|year)+ (1|female.code) + (1|male.code) + 
                                 (1|rel.laying.date), data=fledge.data, ziformula = ~1, family=binomial)



# with age scaled

fledge.data$scaled.age<-scale(fledge.data$female.age, scale=FALSE)

### BEST MODEL
fledge.model.tmb<-glmmTMB(cbind(fledged,dead.chicks)~ I(scaled.age^2)*rtl.delta.ct + scaled.age*rtl.delta.ct+
                            female.lifespan + (1|year)+ (1|female.code) +
                            (1|rel.laying.date), data=fledge.data, ziformula = ~1, family=binomial)

Anova.glmmTMB(fledge.model.tmb)

summary(fledge.model.tmb)
summary(fledge.model.tmb.male)
# no difference, exclude male ID



#### FINAL MODELS ####

#CLUTCH SIZE 
cmp.egg.model2<-glmmTMB(formula=eggs~scaled.age*linreg.delta.ct+linreg.delta.ct*I(scaled.age^2)+female.lifespan+
                          (1|year)+(1|female.code)+(1|rel.laying.date), 
                        data=female.age.data, family="compois")

summary(cmp.egg.model2)

DHARMa::plotQQunif(cmp.egg.model2)
DHARMa::plotResiduals(cmp.egg.model2)
DHARMa::testOverdispersion(cmp.egg.model2)

#HATCHING SUCCESS


short.tel.hatch.model1<-glmmTMB(cbind(hatched,unhatched)~ scaled.age*linreg.delta.ct+(I(scaled.age^2))*linreg.delta.ct +
                                  female.lifespan + (1|year)+ (1|female.code) + (1|rel.laying.date), 
                                data=(filter(female.age.data, telomere.class == "Short Early-Life Telomeres")),family="betabinomial")

summary(short.tel.hatch.model1)

long.tel.hatch.model1<-glmmTMB(cbind(hatched,unhatched)~ scaled.age*linreg.delta.ct+(I(scaled.age^2))*linreg.delta.ct +
                                 female.lifespan + (1|year)+ (1|female.code) + (1|rel.laying.date) , data=(filter(female.age.data, telomere.class == "Long Early-Life Telomeres")),family="betabinomial")

summary(long.tel.hatch.model1)

age.model1a<-glmmTMB(cbind(hatched,unhatched)~ scaled.age*linreg.delta.ct+(I(scaled.age^2)) +
                       female.lifespan + (1|year)+ (1|female.code) + (1|rel.laying.date) , data=female.age.data,family="betabinomial")

DHARMa::plotQQunif(age.model1)
DHARMa::plotResiduals(age.model1)
DHARMa::testOverdispersion(age.model1)

summary(age.model1a)
summary(age.model1)

library(ggeffects)


plot(ggpredict(age.model1, terms="scaled.age [all]", colorAsFactor=TRUE))
short.tel.model.predictions<-ggpredict(short.tel.hatch.model1, terms="scaled.age")
short.plot<-plot(ggpredict(short.tel.hatch.model1, terms="scaled.age [all]"))
long.tel.model.predictions<-ggpredict(long.tel.hatch.model1, terms="scaled.age [all]")
plot(ggpredict(long.tel.hatch.model1, terms="scaled.age [all]"))


#FLEDGING SUCCESS
fledge.model.tmb<-glmmTMB(cbind(fledged,dead.chicks)~ I(scaled.age^2)*rtl.delta.ct + scaled.age*rtl.delta.ct+
                            female.lifespan + (1|year)+ (1|female.code) +
                            (1|rel.laying.date), data=fledge.data, ziformula = ~1, family=binomial)


DHARMa::plotQQunif(fledge.model.tmb)
DHARMa::plotResiduals(fledge.model.tmb)
DHARMa::testOverdispersion(fledge.model.tmb)

summary(fledge.model.tmb)


#### PLOTS ####
# plot telomere versus hatching success by age

summary(female.age.data$linreg.delta.ct)

female.age.data$telomere.class<-NA

summary(dead.females$rtl.delta.ct)

# split based on median 

female.age.data$telomere.class[female.age.data$linreg.delta.ct<(0.12711)]<-"Short Early-Life Telomeres"
female.age.data$telomere.class[female.age.data$linreg.delta.ct>=(0.12711) & female.age.data$linreg.delta.ct<4]<-"Long Early-Life Telomeres"

fulldata$telomere.class[fulldata$rtl.delta.ct<(0.12711)]<-"Short Early-Life Telomeres"
fulldata$telomere.class[fulldata$rtl.delta.ct>=(0.12711) & fulldata$rtl.delta.ct<4]<-"Long Early-Life Telomeres"

dead.females$telomere.class[dead.females$rtl.delta.ct<(0.127)]<-"Short Early-Life Telomeres"
dead.females$telomere.class[dead.females$rtl.delta.ct>=(0.127) & dead.females$rtl.delta.ct<4]<-"Long Early-Life Telomeres"

female.age.data$telomere.class<-as.factor(female.age.data$telomere.class)

female.age.data$telomere.class<-factor(female.age.data$telomere.class, levels=c("Short Early-Life Telomeres","Long Early-Life Telomeres"))

### PLOTS ###

plot.female.age.data<-na.omit(female.age.data)


# hatching success plot


p2<-ggplot(data = na.omit(female.age.data), aes(x=female.age, y=hatching.success, colour=telomere.class))+
  geom_count(aes(x=female.age, y=hatching.success),
             data = NULL,
             stat = "sum",
             position = "identity",
             na.rm = FALSE,
             show.legend = FALSE,
             inherit.aes = FALSE,
             colour="lightseagreen",
             alpha=0.5)+
  geom_smooth(method="auto",level=0.95, show.legend = FALSE, alpha=0.2, colour="lightseagreen")+
  facet_wrap(~telomere.class,ncol=2)+
  xlab("Female Age (Years)")+
  ylab("Hatching Success")+
  theme_minimal()+
  theme(strip.text = element_blank(), axis.ticks.x=element_blank(), 
        axis.text.x=element_blank(), axis.title.x=element_blank()) 
p1

# add in predicted curve

names(short.tel.model.predictions)[1]<-scaled.age

plot1<-ggplot(data = na.omit(filter(female.age.data, telomere.class == "Short Early-Life Telomeres")), aes(x=scaled.age, y=hatching.success))+
  geom_count(aes(x=scaled.age, y=hatching.success),
             data = NULL,
             stat = "sum",
             position = "identity",
             na.rm = FALSE,
             show.legend = FALSE,
             inherit.aes = FALSE,
             colour="lightseagreen",
             alpha=0.5)+
  xlab("Female Age (Years)")+
  ylab("Hatching Success")+theme_minimal() 
xlim(0,10)  

ggplot(data=short.tel.model.predictions)+
  geom_line(aes(x=x,y=predicted), colour="lightseagreen", size=1)+
  geom_ribbon(aes(ymax=predicted+1,ymin=predicted-1))


plot1+geom_smooth(data=short.tel.model.predictions, aes(x=x,y=predicted), colour="lightseagreen", size=1)

geom_line(data=short.tel.model.predictions, aes(x=scaled.age,y=predicted), colour="lightseagreen", size=1)+
  geom_ribbon(data=short.tel.model.predictions, aes(ymax=(predicted+std.error),ymin=(predicted-std.error)))

long.ones<-filter(female.age.data, telomere.class == "Long Early-Life Telomeres")  

long.ones$x<-long.tel.model.predictions$x
long.ones$predicted<-long.tel.model.predictions$predicted
long.ones$upper <- long.tel.model.predictions$conf.high
long.ones$lower <- long.tel.model.predictions$conf.low

plot2<-ggplot(data =long.ones, aes(x=scaled.age, y=hatching.success))+
  geom_count(aes(x=scaled.age, y=hatching.success),
             data = NULL,
             stat = "sum",
             position = "identity",
             na.rm = FALSE,
             show.legend = FALSE,
             inherit.aes = FALSE,
             colour="lightseagreen",
             alpha=0.5)+
  xlab("Female Age (Years)")+
  ylab("Hatching Success")+  theme_minimal()+  xlim(0,10) +
  geom_line(aes(x=x,y=predicted), colour="lightseagreen", size=1)+
  geom_CI
geom_ribbon(aes(ymax=upper, ymin=lower,alpha=0.8))

ror
long.tel.model.predictions$predicted-long.tel.model.predictions$std.error


plot(short.tel.model.predictions)


# clutch size plot

p1<-ggplot(data = na.omit(female.age.data), aes(x=female.age, y=eggs, colour=telomere.class))+
  geom_count(aes(x=female.age, y=eggs),
             data = NULL,
             stat = "sum",
             position = "identity",
             na.rm = FALSE,
             show.legend = FALSE,
             inherit.aes = FALSE,
             colour="hotpink2",
             alpha=0.5)+
  geom_smooth(method="auto",level=0.95, show.legend = FALSE, alpha=0.2, colour="hotpink2")+
  facet_wrap(~telomere.class)+
  xlab("Female Age (Years)")+
  ylab("Clutch Size")+
  theme_minimal() +
  theme(strip.text.x = element_text(size = 11), axis.ticks.x=element_blank(), 
        axis.text.x=element_blank(), axis.title.x=element_blank())

# fledging success plot


p3<-ggplot(data = na.omit(female.age.data), aes(x=female.age, y=(fledged/hatched), colour=telomere.class))+
  geom_count(aes(x=female.age, y=fledged),
             data = NULL,
             stat = "sum",
             position = "identity",
             na.rm = FALSE,
             show.legend = FALSE,
             inherit.aes = FALSE,
             colour="orange1",
             alpha=0.5)+
  geom_smooth(method="auto",level=0.95, show.legend = FALSE, alpha=0.2, colour="orange1")+
  facet_wrap(~telomere.class,ncol=2)+
  xlab("Female Age (Years)")+
  ylab("Fledging Success")+
  theme_minimal() +
  theme(strip.text = element_blank()) 

### recruits plot


p4<-ggplot(data = na.omit(female.age.data), aes(x=female.age, y=annual.recruits, colour=telomere.class))+
  geom_count(aes(x=female.age, y=annual.recruits),
             data = NULL,
             stat = "sum",
             position = "identity",
             na.rm = FALSE,
             show.legend = FALSE,
             inherit.aes = FALSE,
             colour="hotpink2",
             alpha=0.5)+
  geom_smooth(method="auto",level=0.95, show.legend = FALSE, alpha=0.2, colour="hotpink2")+
  facet_wrap(~telomere.class)+
  xlab("Female Age (Years)")+
  ylab("Recruits")+
  theme_minimal() +
  theme(strip.text.x = element_text(size = 11), axis.ticks.x=element_blank(), 
        axis.text.x=element_blank(), axis.title.x=element_blank())

library(gridExtra)

grid.arrange(p1, p2, p3, nrow = 3)

library(cowplot)
plot_grid(p1,p2,p3,nrow=3,align="v", labels="AUTO")


#### LIFETIME REPRODUCTIVE SUCCESS ####

# get count of all eggs laid in lifetime by females 

lifetime.reprod<-fulldata %>%
  filter(!is.na(female.code)) %>%
  filter(dead.or.alive == "dead") %>%
  group_by(female.code) %>%
  summarise(lifetime.eggs=sum(eggs, na.rm=TRUE), lifetime.hatched=sum(hatched,na.rm=TRUE), lifetime.fledged=sum(fledged,na.rm=TRUE))

# get mean clutch size, hatching success and fledging success of a female

mean.reprod<-fulldata %>%
  filter(!is.na(female.code)) %>%
  group_by(female.code) %>%
  summarise(mean.clutch.size=mean(eggs, na.rm=TRUE), 
            mean.hatching.success=mean(hatching.success, na.rm=TRUE), 
            mean.fledge.success=mean(fledged/hatched, na.rm=TRUE))

colnames(lifetime.reprod)<-c("female.code", "lifetime.eggs", "lifetime.hatched", "lifetime.fledged")

fulldata<-left_join(fulldata,lifetime.reprod, by="female.code")

female.age.data<-left_join(female.age.data, lifetime.reprod, by="female.code")

female.age.data<-left_join(female.age.data, mean.reprod)

#only measure dead birds, remove duplicates of individual female

dead.or.alive<-data.frame(fulldata$female.code, fulldata$dead.or.alive)
colnames(dead.or.alive)<-c("female.code","dead.or.alive")

female.age.data<-left_join(female.age.data, dead.or.alive, by="female.code")
female.age.data<-left_join(female.age.data, fulldata[,c(9,35)])

dead.females<-female.age.data %>%
  dplyr::select(female.code, female.cohort, female.lifespan, linreg.delta.ct, lifetime.eggs, 
                lifetime.hatched, lifetime.fledged, lifetime.recruits, mean.clutch.size, mean.hatching.success, mean.fledge.success) %>%
  filter(!duplicated(female.code))

dead.females.1<-fulldata %>%
  filter(dead.or.alive == "dead") %>%
  dplyr::select(female.code, reprod.lifespan, age.first.reprod, rtl.delta.ct, lifetime.eggs, 
                lifetime.hatched, lifetime.fledged, lifetime.recruits) %>%
  filter(!duplicated(female.code))

dead.females<-na.omit(dead.females)

dead.females.1<-na.omit(dead.females.1)


#### LRS plots ####
# plot telomere length vs eggs laid in lifetime 

lifetime.eggs.plot<-ggplot(data=dead.females, aes(x=rtl.delta.ct, y=lifetime.eggs))+
  geom_point(colour="hotpink2")+
  geom_smooth(method="lm",level=0.95, alpha=0.2, colour="hotpink2")+
  xlab("Lifespan (years)")+
  ylab("Lifetime Eggs Laid")+
  theme_minimal()+
  theme(strip.text.x = element_text(size = 11), axis.ticks.x=element_blank(), 
        axis.text.x=element_blank(), axis.title.x=element_blank())

cor.test(dead.females$rtl.delta.ct, dead.females$lifetime.fledged)

ggplot(data=dead.females, aes(x=telomere.class, y=lifetime.eggs.hatched))+
  geom_boxplot()

# no relationship

# plot telomere length vs eggs hatched in lifetime 

lifetime.hatched.plot<-ggplot(data=dead.females, aes(x=rtl.delta.ct, y=lifetime.hatched))+
  #facet_wrap(~telomere.class)+
  geom_point(colour="lightseagreen")+
  geom_smooth(method="lm",level=0.95, alpha=0.2, colour="lightseagreen")+
  xlab("Lifespan (years)")+
  ylab("Lifetime Eggs Hatched")+
  theme_minimal()+
  theme(strip.text.x = element_text(size = 11), axis.ticks.x=element_blank(), 
        axis.text.x=element_blank(), axis.title.x=element_blank())

ggplot(data=dead.females, aes(x=telomere.class, y=lifetime.hatched))+
  geom_boxplot()
# no relationship


# plot telomere length vs chicks fledged in lifetime

lifetime.fledged.plot<-ggplot(data=dead.females, aes(x=rtl.delta.ct, y=lifetime.fledged))+
  geom_point(colour="orange1")+
  geom_smooth(method="lm",level=0.95, alpha=0.2, colour="orange1")+
  xlab("Lifespan (years)")+
  ylab("Lifetime Chicks Fledged")+
  theme_minimal()


# plot telomere length vs recruits in a lifetime
lifetime.recruits.plot<-ggplot(data=dead.females, aes(x=rtl.delta.ct, y=lifetime.recruits))+
  geom_point(colour="orange1")+
  geom_smooth(method="lm",level=0.95, alpha=0.2, colour="orange1")+
  xlab("Early-life Relative Telomere Length")+
  ylab("Lifetime Chicks Fledged")+
  theme_minimal()

cor.test(dead.females$rtl.delta.ct, dead.females$lifetime.recruits)

### plot all 

library(gridExtra)

grid.arrange(lifetime.eggs.plot,lifetime.hatched.plot,lifetime.fledged.plot, nrow=1)


### telomere length vs lifespan

full.dead.females<-fulldata %>%
  filter(dead.or.alive == "dead") %>%
  filter(!duplicated(female.code))

full.dead.females<-left_join(full.dead.females,lifetime.reprod, by="female.code")

lifespan.plot<-ggplot(data=dead.females, aes(y=female.lifespan, x=rtl.delta.ct))+
  geom_point(colour="lightseagreen", alpha=0.7)+
  geom_smooth(method="lm",level=0.95, alpha=0.15, colour="hotpink3")+
  xlab("Relative Telomere Length")+
  ylab("Female Lifespan (years)")+
  theme_minimal()+
  theme(strip.text.x = element_text(size = 11), axis.ticks.x=element_blank(), 
        axis.text.x=element_blank(), axis.title.x=element_blank())

# correlation between telomere length and female lifespan
cor.test(dead.females$linreg.delta.ct, dead.females$female.lifespan)

# construct linear model that controls for cohort of female
lifespan.model<-glmmTMB(female.lifespan ~ linreg.delta.ct + (1|female.cohort), data=dead.females)

DHARMa::plotResiduals(lifespan.model)
DHARMa::plotQQunif(lifespan.model)
DHARMa::testDispersion(lifespan.model)
# all diagnostics good

summary(lifespan.model)
#no sig effect of telomere length, not a lot of variance (0.22) explained by cohort

# no relationship

# correlation between telomere length and female reproductive lifespan
cor.test(dead.females.1$rtl.delta.ct, dead.females.1$reprod.lifespan)

#### LRS models ####

library(glmmTMB)
library(DH)
### best model
lifetime.fledge.model.com<-glmmTMB(formula=lifetime.fledged~ linreg.delta.ct + female.lifespan + (1|female.cohort),
                                   family="poisson",ziformula = ~1,
                                   data=na.omit(dead.females))


DHARMa::plotResiduals(lifetime.fledge.model.com)
DHARMa::plotQQunif(lifetime.fledge.model.com)
DHARMa::testDispersion(lifetime.fledge.model.com)

summary(lifetime.fledge.model.com)

anova(lifetime.fledge.model.com)
anova(lifetime.fledge.model.com,lifetime.fledge.model.com.b)

# plot residuals vs fitted
res <- resid(lifetime.fledge.model.com)
plot(log(predict(lifetime.fledge.model.com)), res)
abline(h=0, lty=2)

# all ok

qqnorm(resid(lifetime.fledge.model.com))
qqline(resid(lifetime.fledge.model.com))

# looks fine
dead.females$lifetime.eggs<-as.integer(dead.females$lifetime.eggs)
dead.females$lifetime.hatched<-as.integer(dead.females$lifetime.hatched)
dead.females$lifetime.fledged<-as.integer(dead.females$lifetime.fledged)
dead.females$lifetime.recruits<-as.integer(dead.females$lifetime.recruits)



### repeat for eggs
### best model - data was overdispersed 
lifetime.egg.model<-glmmTMB(lifetime.eggs ~ linreg.delta.ct + female.lifespan + (1|female.cohort),
                            family="compois", data=dead.females)

lifetime.egg.model.b<-glmer(lifetime.eggs ~  female.lifespan + (1|f.value.),
                            family=poisson(link = "log"), data=na.omit(dead.females))

anova(lifetime.egg.model,lifetime.egg.model.c)

lifetime.egg.model.c<-glmer(lifetime.eggs ~ rtl.delta.ct + (1|f.value.),
                            family=poisson(link = "log"), data=na.omit(dead.females))



DHARMa::plotResiduals(lifetime.egg.model)
DHARMa::plotQQunif(lifetime.egg.model)
DHARMa::testDispersion(lifetime.egg.model)

summary(lifetime.egg.model)


## test signifcance rtl
cmplrtest(lifetime.egg.model,lifetime.egg.model.c)
## significant, Chisq = 3.86, p=0.049*

## test significance of f value
anova(lifetime.egg.model, lifetime.egg.model.b)
## not, Chisq = 1.4, 0.22

overdisp_fun(lifetime.hatch.model)

# plot residuals vs fitted
res <- residuals(lifetime.egg.model, type="deviance")
plot(log(predict(lifetime.egg.model)), res)
abline(h=0, lty=2)
# weird pattern here

qqnorm(resid(lifetime.egg.model))
qqline(resid(lifetime.egg.model))
## qqplot is fine 

### repeat for hatchlings
lifetime.hatch.model<-glmmTMB(lifetime.hatched ~ linreg.delta.ct + female.lifespan + (1|female.cohort),
                              family="compois", data=na.omit(dead.females))

lifetime.hatch.model.b<-glmmTMB(formula=lifetime.eggs ~ (1|female.lifespan),
                                family="compois",
                                data=na.omit(dead.females))

lifetime.hatch.model.c<-glmmTMB(formula=lifetime.hatched~ rtl.delta.ct,
                                family="compois",
                                data=na.omit(dead.females))


DHARMa::plotResiduals(lifetime.hatch.model)
DHARMa::plotQQunif(lifetime.hatch.model)
DHARMa::testDispersion(lifetime.hatch.model)

summary(lifetime.hatch.model)


library(ggfortify)
# plot residuals vs fitted
res <- residuals(lifetime.hatch.model, type="deviance")
plot(log(predict(lifetime.hatch.model)), res)
abline(h=0, lty=2)
# weird pattern here

qqnorm(resid(lifetime.hatch.model))
qqline(resid(lifetime.hatch.model))

summary(lifetime.hatch.model)
## test signifcance rtl 
anova(lifetime.hatch.model,lifetime.hatch.model.a)
## significant, Chisq = 8.14, p=0.004*

## test significance of f value
anova(lifetime.hatch.model, lifetime.hatch.model.b)
## not significant chisq = 0.33, p=0.56

## repeat for fledglings

hist(dead.females$lifetime.fledged)


### repeat for lifetime recruits, but it's zero inflated

library(pscl)

lifetime.recruit.model<-glmmTMB(formula=lifetime.recruits~ linreg.delta.ct + female.lifespan + (1|female.cohort),
                                family="poisson",ziformula = ~1,
                                data=na.omit(dead.females))

DHARMa::plotResiduals(lifetime.recruit.model)
DHARMa::plotQQunif(lifetime.recruit.model)
DHARMa::testDispersion(lifetime.recruit.model)

summary(lifetime.recruit.model)

## plot residuals, similar to qq plot - looking for normal distribution
hist(lifetime.recruit.model$residuals)

qqnorm(resid(lifetime.recruit.model))
qqline(resid(lifetime.recruit.model))

lrtest(lifetime.recruit.model, lifetime.recruit.model1)

## looking for no pattern
plot(lifetime.recruit.model$residuals, lifetime.recruit.model$fitted.values)


recruit.model<-hurdle(annual.recruits ~ linreg.delta.ct*female.age + female.lifespan | 
                        linreg.delta.ct*female.age + female.lifespan ,
                      dist="poisson", link="logit", data=na.omit(female.age.data))

## eggs

ggplot(data=fulldata, aes(y=female.lifespan/365, x=rtl.delta.ct))+
  geom_point()+
  geom_smooth()

ggplot(data=fulldata, aes(x=female.lifespan/365, y=eggs.y))+
  geom_point(colour="hotpink2")+
  geom_smooth(method="auto",level=0.95, alpha=0.2, colour="hotpink2")+
  xlab("Female Lifespan (years)")+
  ylab("Lifetime Number of Eggs Laid")+
  theme_minimal()
#theme(strip.text.x = element_text(size = 11), axis.ticks.x=element_blank(), 
#     axis.text.x=element_blank(), axis.title.x=element_blank())

##hatched

ggplot(data=dead.females, aes(x=linreg.delta.ct, y=lifetime.hatched))+
  geom_point()+
  geom_smooth(method="lm")

ggplot(data=fulldata, aes(x=female.lifespan/365, y=hatched.y))+
  geom_point(colour="lightseagreen")+
  geom_smooth(method="auto",level=0.95, alpha=0.2, colour="lightseagreen")+
  xlab("Female Lifespan (years)")+
  ylab("Lifetime Number of Eggs Hatched")+
  theme_minimal()

## fledged

ggplot(data=dead.females, aes(x=linreg.delta.ct, y=lifetime.fledged))+
  geom_point()+
  geom_smooth(method="lm")

ggplot(data=fulldata, aes(x=female.lifespan/365, y=fledged))+
  geom_point(colour="orange1")+
  geom_smooth(method="auto",level=0.95, alpha=0.2, colour="orange1")+
  xlab("Female Lifespan (years)")+
  ylab("Lifetime Number of Chicks Fledged")+
  theme_minimal()

### no effect of lifespan (of dead females) on lifetime reproductive output 

### does telomere length affect lifespan?

ggplot(data=dead.females, aes(x=female.lifespan, y=f.value.))+
  geom_point()+
  geom_smooth(method="lm")

# does telomere length affect average reproductive success

ggplot(data=female.age.data, aes(x=telomere.class, y=mean.fledge.success))+
  geom_boxplot()


#### LRS VS LIFESPAN #### 

ggplot(data=effortdata, aes(x=female.lifespan, y=lifetime.eggs.laid))+
  geom_point()+
  geom_smooth()

cor.test(effortdata$female.lifespan,effortdata$lifetime.eggs.laid)

ggplot(data=effortdata, aes(x=female.lifespan, y=lifetime.eggs.hatched))+
  geom_point()+
  geom_smooth()

### POST PEAK ####
## subset data to be after peak hatching successs

View(telomere.summary.b) 
# peak in hatching success for short telomeres is at year 5, and year 4 for long telomeres

View(telomere.summary.eggs)
# peak in clutch size for long telomeres = year 3, for short telomeres, year 6

regression.short.hatching<-female.age.data %>% 
  filter(female.age.year > 5) %>%
  filter(telomere.class == "Short Early-Life Telomeres")

View(regression.short.hatching)

coef(lm(female.age ~ hatching.success, regression.short.hatching))

# slope = -2.0664

regression.long.hatching<-female.age.data %>% 
  filter(female.age.year > 4) %>%
  filter(telomere.class == "Long Early-Life Telomeres")

View(regression.long.hatching)

coef(lm(female.age ~ hatching.success, regression.long.hatching))

# slope = -0.155289 - much shallower 

### for clutch size

regression.short.eggs<-female.age.data %>% 
  filter(female.age.year > 6) %>%
  filter(telomere.class == "Short Early-Life Telomeres")


coef(lm(female.age ~ eggs, regression.short.eggs))

# slope = -0.507

regression.long.eggs<-female.age.data %>% 
  filter(female.age.year > 3) %>%
  filter(telomere.class == "Long Early-Life Telomeres")

coef(lm(female.age ~ eggs, regression.long.eggs))

# slope = -0.118 # again much shallower 

### look at days before death as opposed to age

# hatching success plot



#### REPROD TRAITS VS LIFETIME FITNESS (RECRUITS) ####

cor.test(dead.females$mean.hatching.success, dead.females$lifetime.recruits)
# cor = 0.039
# p = 0.1
cor.test(dead.females$lifetime.fledged, dead.females$lifetime.recruits)

ggplot(data=fulldata, aes(x=hatching.success, y=lifetime.recruits))+
  geom_count(aes(x=hatching.success, y=lifetime.recruits),
             data = NULL,
             stat = "sum",
             position = "identity",
             na.rm = FALSE,
             show.legend = TRUE,
             inherit.aes = FALSE,
             colour="#56B4E9",
             alpha=0.8)

## clutch size vs lifetime recruits
cor.test(dead.females$mean.clutch.size, dead.females$lifetime.recruits)
# cor = 0.03
# p = 0.17
ggplot(data=fulldata, aes(x=eggs, y=lifetime.recruits))+
  geom_count(aes(x=eggs, y=lifetime.recruits),
             data = NULL,
             stat = "sum",
             position = "identity",
             na.rm = FALSE,
             show.legend = TRUE,
             inherit.aes = FALSE,
             colour="#56B4E9",
             alpha=0.8)+
  geom_smooth(method="lm")

## fledging success
cor.test(dead.females$mean.fledge.success, dead.females$lifetime.recruits)
# cor = 0.15
# p = 0.2
cor.test(fulldata$fledged, fulldata$lifetime.recruits)
# cor = 0.13
# p = 6.943e-08
lifetimefitness.plot1<-ggplot(data=fulldata, aes(x=fledged, y=lifetime.recruits))+
  geom_count(aes(x=fledged, y=lifetime.recruits),
             data = NULL,
             stat = "sum",
             position = "identity",
             na.rm = FALSE,
             show.legend = FALSE,
             inherit.aes = FALSE,
             colour="lightseagreen",
             alpha=0.8)+
  geom_smooth(method="lm", colour="hotpink2", alpha=0.3)+
  theme_minimal()+
  xlab("Number of Chicks Fledged (per nest)")+
  ylab("Lifetime Fitness (recruits)")+
  scale_y_continuous(limits=c(0,10), breaks=c(0,2,4,6,8,10))

## recruits per year
cor.test(dead.females$mean.recruits, fulldata$lifetime.recruits)
# cor = 0.13
# p = 6.943e-08
ggplot(data=fulldata, aes(x=annual.recruits, y=lifetime.recruits))+
  geom_count(aes(x=annual.recruits, y=lifetime.recruits),
             data = NULL,
             stat = "sum",
             position = "identity",
             na.rm = FALSE,
             show.legend = TRUE,
             inherit.aes = FALSE,
             colour="#56B4E9",
             alpha=0.8)+
  geom_smooth(method="lm")

## reprod lifespan
cor.test(fulldata$reprod.lifespan, fulldata$lifetime.recruits)
# cor = 0.38
# p = <2.2e-16
ggplot(data=fulldata, aes(x=reprod.lifespan/365, y=lifetime.recruits))+
  geom_count(aes(x=reprod.lifespan/365, y=lifetime.recruits),
             data = NULL,
             stat = "sum",
             position = "identity",
             na.rm = FALSE,
             show.legend = TRUE,
             inherit.aes = FALSE,
             colour="#56B4E9",
             alpha=0.8)+
  geom_smooth(method="lm")

## lifespan
cor.test(dead.females$female.lifespan, dead.females$lifetime.recruits)
# cor = 0.45
# p = <2.2e-16

lifetimefitness.plot.2<-ggplot(data=fulldata, aes(x=female.lifespan, y=lifetime.recruits))+
  geom_count(aes(x=female.lifespan, y=lifetime.recruits),
             data = NULL,
             stat = "sum",
             position = "identity",
             na.rm = FALSE,
             show.legend = FALSE,
             inherit.aes = FALSE,
             colour="hotpink2",
             alpha=0.8)+
  geom_smooth(method="lm", colour="lightseagreen", alpha=0.2)+
  theme_minimal()+
  xlab("Lifespan (years)")+
  ylab("Lifetime Fitness (recruits)")+
  scale_y_continuous(limits=c(0,10), breaks=c(0,2,4,6,8,10))+
  theme(axis.ticks.y=element_blank(), 
        axis.text.y=element_blank(), axis.title.y=element_blank())

library(gridExtra)  

grid.arrange(lifetimefitness.plot1,lifetimefitness.plot.2, nrow=1, ncol=2)

cor.test(dead.females$female.lifespan, dead.females$rtl.delta.ct)
# cor = -0.12
# p = 0.27
ggplot(data=dead.females, aes(x=rtl.delta.ct, y=female.lifespan))+
  geom_count(aes(x=rtl.delta.ct, y=female.lifespan),
             data = NULL,
             stat = "sum",
             position = "identity",
             na.rm = FALSE,
             show.legend = TRUE,
             inherit.aes = FALSE,
             colour="#56B4E9",
             alpha=0.8)+
  geom_smooth(method="lm")


#### COMPARE LINREGPCR AND QUANTSTUDIO CTS ####

library(readr)
ct_compare <- read_csv("~/Documents/telomere paper/ct comparison quantstudio and linregpcr.csv")

ct_compare<-ct_compare %>%
  filter(female.code %in% female.age.data$female.code)

colnames(ct_compare)[1]<-"female.code"

ct_compare$female.code<-as.character(ct_compare$female.code)


cor.test(ct_compare$linreg_tel_Cq,ct_compare$quantstudio_tel_cq,method="pearson")

# telomere CTs not significantly different between linregpcr and quantstudio pcr

cor.test(ct_compare$linreg_rag1_Cq,ct_compare$quantstudio_rag1_cq,method="pearson")


# rag1 CTs not significantly different between linregpcr and quantstudio pcr


cor.test(ct_compare$linreg_deltadelta_ct,ct_compare$quantstudio_deltadelta_ct,method="pearson")


female.age.data<-left_join(female.age.data,ct_compare[,c(1,9)])

## run hatching model with quantstudio deltadelta ct values


female.age.data$quantstudio_deltadelta_ct<-scale(female.age.data$quantstudio_deltadelta_ct, scale=FALSE)

age.model1_quant<-glmmTMB(cbind(hatched,unhatched)~ scaled.age+quantstudio_deltadelta_ct*(I(scaled.age^2))+
                            female.lifespan + (1|year)+ (1|female.code) + (1|rel.laying.date) , data=female.age.data,family="betabinomial")


# including interaction between linear age term and rtl messes up residuals plot and makes everything not significant 

DHARMa::plotQQunif(age.model1_quant)
DHARMa::plotResiduals(age.model1_quant)
DHARMa::testOverdispersion(age.model1_quant)

# much better diagnostics in glmmTMB in betabinomial

summary(age.model1_quant)

# same results as when using linregpcr

## clutch size 

cmp.egg.model2_quant<-glmmTMB(formula=eggs~scaled.age+quantstudio_deltadelta_ct*I(scaled.age^2)+female.lifespan+
                                (1|year)+(1|female.code)+(1|rel.laying.date), 
                              data=female.age.data, family="compois")
summary(cmp.egg.model2_quant)


## fledging success 

fledge.data<-left_join(fledge.data,ct_compare[,c(1,9)])

female.age.data$dead.chicks<-female.age.data$hatched-female.age.data$fledged

fledge.model.tmb_quant<-glmmTMB(cbind(fledged,dead.chicks)~ I(scaled.age^2)*quantstudio_deltadelta_ct + scaled.age +
                                  female.lifespan + (1|year)+ (1|female.code) +
                                  (1|rel.laying.date), data=fledge.data, ziformula = ~1, family=binomial)
summary(fledge.model.tmb_quant)

#### DNA clean up ####

clean_up <- read_csv("~/Documents/telomere paper/clean up.csv", 
                     col_types = cols(female.code = col_character()))
library(dplyr)
ct_compare<-left_join(ct_compare,clean_up)
library(ggplot2)
ggplot(ct_compare, aes(x=cleaned, y=linreg_deltadelta_ct))+
  geom_boxplot()+
  xlab("DNA Cleanup Performed")+
  ylab("Relative Telomere Length")+
  theme_minimal()

cleaned<-filter(ct_compare, cleaned=="Yes")
uncleaned<-filter(ct_compare, cleaned=="No")

t.test(cleaned$linreg_deltadelta_ct,uncleaned$linreg_deltadelta_ct,alterive="two.sided", paired=FALSE)

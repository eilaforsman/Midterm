rm(list = ls())
list.files ()

#Stapeldiagram medelantal per plats, medel på enskilda snitten först 
#Antal HW vs kontroll
#Antal skillnad i lokaler beror på region?
#Two-way ANOVA HW vs kontroll och antal per lokal
#Medelvärde antal per lokal (Pålsjö 1,2,3,4,5)
#5vs6ggr
#Skillnad i död/levande eller antal mellan strukturer

#Basic####
getwd()

setwd("~/Documents/Documents/R/Statcourse/Midterm")

dataFDA <- read.csv("dataFDA.csv", sep=";")
head(dataFDA)
str(dataFDA)
dataFDA$Struktur <- as.factor(dataFDA$Struktur)
dataFDA$Lokal <- as.factor(dataFDA$Lokal)
table(dataFDA$Lokal)
summary(dataFDA)


library(plyr)
dataFDA_sub <- subset(dataFDA, Lokal != "H_Pålsjö_efter_HW")
mean_LokalFDA<-ddply(dataFDA_sub,"Lokal",summarise,
                  mean_antal=mean(Antal))


head(dataFDA_sub)

#install.packages("stringr")
library(stringr)

#Sort_data####

ID_split <- as.data.frame(str_split_fixed(dataFDA_sub$ID, "_", 2))
head(ID_split)




dataFDA_sub$ID_spec <- ID_split$V1
paste(dataFDA_sub$Lokal, dataFDA_sub$ID_spec, sep ="_")
dataFDA_sub$Prov<-paste(dataFDA_sub$Lokal, dataFDA_sub$ID_spec, sep ="_")
head(dataFDA_sub)


Prov<-ddply(dataFDA_sub, "Prov", summarise, mean_antal=mean(Antal))
head(Prov)

nchar(dataFDA_sub$Prov)
dataFDA_sub$newprov <- substr(dataFDA_sub$Prov, 1, nchar(dataFDA_sub$Prov) - 2)
Meantot<-ddply(dataFDA_sub, "newprov", summarise, mean_tot=mean(Antal))
Meantot$order <- c(4,5,1,9,10,11,12,13,14,2,8,6,7,3)

#Basic Plotting####

library(plyr)
barplot(mean_antal~Lokal,las=1, data=mean_LokalFDA,srt=35)

axis(side=1,labels = FALSE)

barplot(mean_antal~Lokal,data=mean_LokalFDA, xaxt="n")
text(x = 1:length(levels(mean_LokalFDA$Lokal)),
     y = par("usr")[3] - 0.45,
     labels = levels(mean_LokalFDA$Lokal),
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 50,
     cex = 1.0)

#Data exploring####

hist(dataFDA_sub$Antal, xlab="Number of cells", las=1)

dataFDA_sub$Behandling = factor(dataFDA_sub$Behandling, levels=c("Kontroll", "Heatweed"))
m = lm(Antal~Behandling, data=dataFDA_sub)
anova(m)
summary(m)

#Not allowed to use ANOVA to draw conclusions because data is not normally distributed

#Site test#####


kruskal.test(dataFDA_sub$Antal ~ dataFDA_sub$Prov) #Big difference between root samples

kruskal.test(dataFDA_sub$Antal ~ dataFDA_sub$newprov) #Big difference between sites if control included

HWsites <- subset(dataFDA_sub, dataFDA_sub$newprov != "Helsingborg_kontroll" & dataFDA_sub$newprov != "Motala_kontroll" & dataFDA_sub$newprov != "Vellinge_kontroll")
# Removes control samples to check if test sites has an effect

kruskal.test(HWsites$Antal ~ HWsites$newprov) #Big difference between HWsites, (pseudo rep)!

#Mixed Model#####

library(glmmTMB)
library(MASS)
library(car)
library(lme4)
library(lmerTest)

?glmmTMB
?lme4
?lmerTest

#LMM förutsätter normalfördelning
#m3 = lmer(response ~ fixed factor + ev fixed 2+ fixed factor 1* fixed 2 + (1|random), data=data_sub)


#GLMM
#m3 = glmer(response ~ fixed factor + ev fixed 2+ fixed factor 1* fixed 2 + (1|random), family= "poisson", data=data_sub)

#Assumes equal variances between groups, (levene.test()), sig is problem 
#Om anova inte funkar -> testa Anova


m2 = glm.nb(Antal ~ Behandling, data=dataFDA_sub)
summary(m2) #Fitted a generalized linear model with negative binomial distribution
#to account for overdispersion. Big difference but doesn't take site into account

1-(m2$deviance/m2$null.deviance)
#[1] 0.04534703

m3 = glmer(dataFDA_sub$Antal ~ dataFDA_sub$Behandling + (1|dataFDA_sub$Lokal), family="poisson", data=dataFDA_sub)

Anova(m3) #assumes equal variance between groups

leveneTest(dataFDA_sub$Antal ~ dataFDA_sub$Behandling) #was significant -> not equal variances

library(nlme) #for weights


m4 = lme(Antal ~ Behandling, random = ~ 1|Lokal, weights = varIdent(form= ~1|Behandling), data=dataFDA_sub)

anova(m4) #Assumes normal distribution

m5 = glmmTMB(Antal ~ Behandling + (1|Lokal), dispformula = ~ Behandling, family="poisson", data=dataFDA_sub)
summary(m5)

Anova(m5) #Correct data distribution and with non equal variance, site as a random factor

boxplot (sqrt(dataFDA_sub$Antal) ~ dataFDA_sub$Behandling)

resid(m5)
boxplot(resid(m5) ~ dataFDA_sub$Behandling)

m6 = glmer.nb(Antal ~ Behandling + (1|Lokal), family="poisson", data=dataFDA_sub)
m7 = glmmTMB(Antal ~ Behandling + (1|Lokal/Prov), family="nbinom1", data=dataFDA_sub)
summary(m6) 
summary(m7)#Includes site and sample as random factor and correct overdispersion, 
             #correct data distribution

boxplot(resid(m6) ~ dataFDA_sub$Behandling)
boxplot(resid(m7) ~ dataFDA_sub$Behandling)

plot(Antal ~ Behandling, data=dataFDA_sub)

#Results####

coef = summary(m7)
coef = coef[["coefficients"]][["cond"]]
exp(coef[1,1])
exp(coef[1,1] + coef[2,1])

stats = ddply(dataFDA_sub, "Behandling", summarize, 
               median_treat = median(Antal),
               mean_treat = mean(Antal),
              sd_treat = sd(Antal),
              se_treat = sd_treat/sqrt(length(Antal)))


#Fancy plotting######

#install.packages("ggplot2")
library(ggplot2)

head(Meantot)
par(mfrow=c(1,1))
?ggplot()



FDAMeans<-ddply(dataFDA_sub,"Lokal", summarize, N=length(Antal),
                         mean.antal=mean(na.omit(Antal)),
                        sd.FDA=sd(na.omit(Antal)),
                         se.FDA=sd.FDA/sqrt(N))

ggplot(Meantot, aes(x=reorder(Meantot$newprov, Meantot$order), y=Meantot$mean_tot)) +
        geom_bar(width = 0.75, stat = "identity", position ="dodge", alpha = 0.8) +
        geom_errorbar(data=FDAMeans, aes(ymin= mean.antal - se.FDA, ymax=mean.antal + se.FDA),
                   width = 0.13, alpha = 1, position=position_dodge(0.75)) +
        theme_classic() + 
        scale_y_continuous(limits = c(0,150), expand = c(0,0)) +
        labs(y="Mean number per cross section", x="", 
             title = "Number of live cells per cross section from each site") +
        theme(legend.position = c(0.9,0.9), 
              legend.title = element_blank(),
              plot.title = element_text (hjust = 0.5),
              text = element_text(size=28, family= "Times"),
              axis.text.x = element_text(size = 20, angle = 60,
                                         hjust = 1, color = "grey1")) +
        theme(axis.ticks.length=unit(.25, "cm"))

ggsave("Celler_FDA_plot.png", plot = last_plot(), device = "png",
       scale = 1, width = 12, height = 8,
       dpi = 600)

#Mean number of cells per sample####


SampleMeans <- ddply(dataFDA_sub,"Prov", summarize, N=length(Antal),
                     mean.antal=mean(na.omit(Antal)),
                     sd.FDA=sd(na.omit(Antal)),
                     se.FDA=sd.FDA/sqrt(N))
str(SampleMeans)
SampleMeans$Prov = as.character(SampleMeans$Prov)


SampleMeans$newprov <- substr(SampleMeans$Prov, 1, nchar(SampleMeans$Prov) - 2)

Sample = aggregate(cbind(SampleMeans$mean.antal, SampleMeans$sd.FDA, 
                         SampleMeans$se.FDA), 
                   by=list(newprov=SampleMeans$newprov), FUN =sum)


Meantot$mean_sample = Sample$V1
Meantot$sd = Sample$V2
Meantot$se = Sample$V3

ggplot(Meantot, aes(x=reorder(newprov, order), y=mean_sample)) +
  geom_bar(width = 0.75, stat = "identity", position ="dodge", alpha = 0.8) +
  geom_errorbar(data=Meantot, aes(ymin= mean_sample - se, 
                                  ymax=mean_sample + se),
                width = 0.13, alpha = 1, position=position_dodge(0.75)) +
  theme_classic() + 
  scale_y_continuous(limits = c(0,500), expand = c(0,0)) +
  labs(y="Mean number of cells", x="", 
       title = "Number of live cells per sample from each site") +
  theme(legend.position = c(0.9,0.9), 
        legend.title = element_blank(),
        plot.title = element_text (hjust = 0.5),
        text = element_text(size=28, family= "Times"),
        axis.text.x = element_text(size = 20, angle = 60,
                                   hjust = 1, color = "grey1")) +
  theme(axis.ticks.length=unit(.25, "cm"))

ggsave("Celler_prov_plot.png", plot = last_plot(), device = "png",
       scale = 1, width = 10, height = 8,
       dpi = 600)

#Variance####

vardat = split(dataFDA_sub, f=dataFDA_sub$Behandling)
datC = vardat$Kontroll
datHW = vardat$Heatweed

hist(datC$Antal)

m = glm.nb(Antal ~ 1, data=datC)
summary(m)

hist(datHW$Antal)


m2 = glm.nb(Antal ~ 1, data=datHW)
summary(m2)

coefs = summary(m)$coef
coefs2 = summary(m2)$coef

var(datC$Antal)
var(datHW$Antal)
var(datC$Antal)/(var(datC$Antal)+var(datHW$Antal))
var(datHW$Antal)/(var(datC$Antal)+var(datHW$Antal))




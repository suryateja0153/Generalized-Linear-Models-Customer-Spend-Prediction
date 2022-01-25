#Author: Suryateja Chalapati

#Import Libraries
library(readxl)
library(car)
library(lmtest)
library(ggpubr)
library(stargazer)
library(data.table)
library(MASS)
library(AER)
library(pscl)

setwd("C:/Users/surya/Downloads")

orc <- read_excel("OnlineRetailCampaign.xlsx", sheet = 'Data')
names(orc) <- tolower(colnames(orc))

#NA values column wise
sapply(orc, function(x) sum(is.na(x)))
str(orc)

#Feature Engineering/Pre-processing
orc$historysegment <- NULL
orc$phone <- ifelse(orc$channel == 'Phone' | orc$channel == 'Multichannel', 1, 0)
orc$web <- ifelse(orc$channel == 'Web' | orc$channel == 'Multichannel', 1, 0)
orc$channel <- NULL
orc$campaign <- as.factor(orc$campaign)
orc$zipcode <- as.factor(orc$zipcode)
orc$campaign <- relevel(orc$campaign, ref='No E-Mail')
orc$spend <- round(orc$spend)
orc_rd <- orc
orc <- orc[orc$spend > 0,]
#orc_woz <- orc

#Checking if DV is suitable for OLS
mean(orc_rd$spend)
sd(orc_rd$spend)
hist(orc_rd$spend, col = 'lightcoral', main = "Histogram of Spend", xlab = 'Spend', ylab = 'Frequency')
hist(log(orc_rd$spend), col = 'lightgreen', main = "Histogram of log(Spend)", xlab = 'log(Spend)', ylab = 'Frequency')

#Checking if filtering and rounding off changes anything
mean(orc$spend)
sd(orc$spend)
hist(orc$spend, col = 'lightcoral', main = "Histogram of Spend", xlab = 'Spend', ylab = 'Frequency')
hist(log(orc$spend), col = 'lightgreen', main = "Histogram of log(Spend)", xlab = 'log(Spend)', ylab = 'Frequency')

#Regression models
m1 <- lm(log(spend) ~ recency + history + zipcode + newcustomer 
          + phone + web + campaign*mens + campaign*womens, data = orc)

m2 <- glm(spend ~ recency + history + zipcode + newcustomer 
          + phone + web + campaign*mens + campaign*womens, data = orc, family=poisson (link=log))

m3 <- glm(spend ~ recency + history + zipcode + newcustomer 
          + phone + web + campaign*mens + campaign*womens, data = orc, family=quasipoisson (link=log))

m4 <- glm.nb(spend ~ recency + history + zipcode + newcustomer 
             + phone + web + campaign + mens + womens, data = orc)

m4.A <- glm.nb(spend ~ recency + campaign*history + campaign*zipcode + campaign*newcustomer 
               + campaign*phone + campaign*web + campaign*mens + campaign*womens, data = orc)

m4.B <- glm.nb(spend ~ recency + history + zipcode + newcustomer 
               + phone + web + campaign + mens + womens, data = orc)

m5 <- hurdle(spend ~ recency + campaign*history + campaign*zipcode + campaign*newcustomer 
             + campaign*phone + campaign*web + campaign*mens + campaign*womens | visit + conversion, data=orc_rd, link="logit", dist="negbin")

m6 <- hurdle(spend ~ recency + history + zipcode + newcustomer 
             + phone + web + campaign + mens + womens | visit + conversion, data=orc_rd, link="logit", dist="negbin")

#vif test fail
m7 <- zeroinfl(spend ~ recency + campaign*history + campaign*zipcode + campaign*newcustomer 
               + campaign*phone + campaign*web + campaign*mens + campaign*womens | visit + conversion, data=orc_rd, link="logit", dist="negbin")

m8 <- zeroinfl(spend ~ recency + history + zipcode + newcustomer 
               + phone + web + campaign + mens + womens | visit + conversion, data=orc_rd, link="logit", dist="negbin")

#Dispersion test
#dispersiontest(m4)

#Stargazer
stargazer(m4, m6, m8, type='text', single.row = TRUE)
stargazer(m4, m4.A, m4.B, m5, type='text', single.row = TRUE)
stargazer(m4.A, m4.B, m5, m6, type='text', single.row = TRUE)
stargazer(m5, m7, m8, type='text', single.row = TRUE)

#vif(m4)
vif(m4.A)
vif(m4.B)
vif(m5)
#vif(m6)

dwtest(m5)
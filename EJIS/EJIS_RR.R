FULL_DATA <- read.csv("C:/Users/kmpoo/Dropbox/HEC/Project 2 -   License/EJIS/Data/FullData_20190517.csv", header = TRUE)
dim(FULL_DATA)
class(FULL_DATA)
head(FULL_DATA)

levels(f_topic)

### Start of EJIS Stata code port
FULL_DATA$n_main_language <- NULL
FULL_DATA$n_main_language <- factor(FULL_DATA$MAIN_LANGUAGE)
FULL_DATA$X_d <- NULL
FULL_DATA$X_t <- NULL
FULL_DATA$X_t0 <- NULL
FULL_DATA$logforks <- log(FULL_DATA$FORKS)

#********************************************************************************
#******************************* MAIN REGRESSION ******************************** 
#********************************************************************************

library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)

km_fit <- survfit(Surv(SURVIVAL_PERIOD_1017_DAY, DEATH_EVENT_2Y) ~ 1, data=FULL_DATA)
autoplot(km_fit)
# Kaplan meir curves with treatment
km_trt_fit <- survfit(Surv(SURVIVAL_PERIOD_1017_DAY, DEATH_EVENT_2Y) ~ GNU_FLAG, data=FULL_DATA)
autoplot(km_trt_fit)

# Cox model

streg  c.SUBSCRIPTIONS c.log_com_preq c.NO_LANGUAGES c.logsize c.OWNER_PUBLICREPO c.AVG_COMMITS_COMMITTER c.STARS_1017 i.OWNER_FLAG i.GNU_FLAG c.DEG_SUPER i.FINAL_CONTRIBUTORS i.n_main_language i.MONTH_CREATED  if LICENCE_NAME != "Not Found" & LICENCE_NAME != "Other", distribution(lognormal) vce(robust)

                      

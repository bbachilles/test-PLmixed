library(haven)
Hoffman <- read_dta("D:/OneDrive/Methodological References/Multi-level Model References/Cross-classified Models/Hoffman_STATA_Chapter11b/STATA_Chapter11b/STATA_Chapter11b_analysis.dta")
library(dplyr)
glimpse(Hoffman)


##Using PLmixed 
library(PLmixed)
unique(Hoffman$year) # 3 years, each given 1 row (9999 is a wildcard value for cluster)
#3 classes - one obs/year so a 3X3 matrix
acute.lam <- rbind(c(1, 0, 0), 
                   c(0, 1, 0), 
                   c(0, 0, 1))

Hoffman2 <- as.data.frame(Hoffman)
Hoffman2 <- Hoffman2[order(Hoffman2$year), ]
unique(Hoffman2$year)

###Acute Model
##Estimated with PLmixed, but also can be done with lme4
acute.plmixed <- PLmixed(effort ~ as.factor(year) +  (0 + yz | classid_year0)   
                         + (0 + yo | classid_year1) + (0 + yt | classid_year2) 
                         + (1 | studentid), data = Hoffman2,  
                         factor = list(c("yz", "yo", "yt")), load.var = "year",
                         lambda = list(acute.lam))
summary(acute.plmixed)

##Using lme4
Hoffman2$t0 <- (Hoffman2$year == 0)*1
Hoffman2$t1 <- (Hoffman2$year == 1)*1
Hoffman2$t2 <- (Hoffman2$year == 2)*1

acute.lme4 <- lme4::lmer(effort ~ as.factor(year) +  (0 + t0 | classid_year0)   
                         + (0 + t1 | classid_year1) + (0 + t2 | classid_year2) 
                         + (1 | studentid), data = Hoffman2, REML = T)
                         #, control = lmerControl(optimizer = "bobyqa"))

summary(acute.lme4)

###Transfer Model
##Estimated only with PLmixed
transfer.lam <- rbind(c(1, 0, 0), 
                      c(NA, 1, 0), 
                      c(NA, NA, 1))
transfer.plmixed <- PLmixed(effort ~ as.factor(year) +  (0 + yz | classid_year0)   
                         + (0 + yo | classid_year1) + (0 + yt | classid_year2) 
                         + (1 | studentid), data = Hoffman2,  
                         factor = list(c("yz", "yo", "yt")), load.var = "year",
                         lambda = list(transfer.lam))
summary(transfer.plmixed)


anova(acute.plmixed$lme4, transfer.plmixed$lme4)

texreg::screenreg(acute.lme4)

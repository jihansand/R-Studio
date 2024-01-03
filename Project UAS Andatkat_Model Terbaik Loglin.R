library(questionr)
library(tidyverse)
library(knitr)

Age <-factor(rep(c("<30","30+"),each=8))
Age
Smoking <-factor(rep(c("<5","5+"),each=4,times=2))
Smoking
Gestation <-factor(rep(c("<=260",">260"),each=2,times=4)) 
Gestation
Inf.Surv <-factor(rep(c("No","Yes"), times = 8))
Inf.Surv
counts<-c(50,315,24,4012,9,40,6,459,41,147,14,1594,4,11,1,124)
Infant <- data.frame(Age, Smoking, Gestation, Inf.Surv, counts) 

Infant <- Infant %>% 
  rename(A = "Age") %>% 
  rename(S = "Smoking") %>% 
  rename(G = "Gestation") %>%
  rename(I = "Inf.Surv")

# no interaksi
A_S_G_I <- glm(counts ~ A + S + G + I, family = poisson, data = Infant)
`(A_S_G_I)` <- round(exp(predict(A_S_G_I, data.frame(Infant))), 2)

# interaksi 2 arah
# interaksi on I
AI_SI_GI <- glm(counts ~ A + S + G + I + A:I + S:I + G:I, family = poisson, data = Infant)
`(AI_SI_GI)` <- round(exp(predict(AI_SI_GI, data.frame(Infant))), 2) 

# interaksi on A
II <- glm(counts ~ A + S + G + I + S:A + G:A + I:A, family = poisson, data = Infant)
`(AS_AG_AI)` <- round(exp(predict(AS_AG_AI, data.frame(Infant))), 2) 

# interaksi on S
AS_SG_SI <- glm(counts ~ A + S + G + I + A:S + G:S + I:S, family = poisson, data = Infant)
`(AS_SG_SI)` <- round(exp(predict(AS_SG_SI, data.frame(Infant))), 2) 

# interaksi on G
AG_SG_GI <- glm(counts ~ A + S + G + I + A:G + S:G + I:G, family = poisson, data = Infant)
`(AG_SG_GI)` <- round(exp(predict(AG_SG_GI, data.frame(Infant))), 2)

AS_AG_AI_SG_SI_GI <- glm(counts ~ A + S + G + I + A:S + A:G + A:I + S:G + S:I + I:G, family = poisson, data = Infant)
`(AS_AG_AI_SG_SI_GI)` <- round(exp(predict(AS_AG_AI_SG_SI_GI, data.frame(Infant))), 2) 

# interaksi 3 arah
# on A
ASG_AGI_ASI <- glm(counts ~ A + S + G + I + A:S + A:G + A:I + S:G + S:I + I:G + A:S:G + A:G:I + A:S:I, family = poisson, data = Infant)
`(ASG_AGI_ASI)` <- round(exp(predict(ASG_AGI_ASI, data.frame(Infant))), 2) 

# on S
ASG_SGI_ASI <- glm(counts ~ A + S + G + I + A:S + A:G + A:I + S:G + S:I + I:G + A:S:G + S:G:I + A:S:I, family = poisson, data = Infant)
`(ASG_AGI_ASI)` <- round(exp(predict(ASG_AGI_ASI, data.frame(Infant))), 2)

# on G
ASG_AGI_SGI <- glm(counts ~ A + S + G + I + A:S + A:G + A:I + S:G + S:I + I:G + A:S:G + A:G:I + S:G:I, family = poisson, data = Infant)
`(ASG_AGI_SGI)` <- round(exp(predict(ASG_AGI_SGI, data.frame(Infant))), 2)

# on I
ASI_AGI_SGI <- glm(counts ~ A + S + G + I + A:S + A:G + A:I + S:G + S:I + I:G + A:S:I + A:G:I + S:G:I, family = poisson, data = Infant)
`(ASI_AGI_SGI)` <- round(exp(predict(ASI_AGI_SGI, data.frame(Infant))), 2)

ASG_AGI_ASI_SGI <- glm(counts ~ A + S + G + I + A:S + A:G + A:I + S:G + S:I + I:G + A:S:G + A:G:I + A:S:I + S:G:I, family = poisson, data = Infant)
`(ASG_AGI_ASI_SGI)` <- round(exp(predict(ASG_AGI_ASI_SGI, data.frame(Infant))), 2)

# interaksi 4 arah
ASGI <- glm(counts ~ A + S + G + I + A:S + A:G + A:I + S:G + S:I + I:G + A:S:G + A:G:I + A:S:I + S:G:I + A:S:G:I, family = poisson, data = Infant)
`(ASGI)` <- round(exp(predict(ASGI, data.frame(Infant))), 2) 


residualP <- function(x, digits = 2){
  pValue <- 1 - pchisq(deviance(x), df.residual(x))
  value <- format(round(pValue, digits), scientific = FALSE)
  if(value == 0) {
    paste0("< 0.", strrep("0", digits-1), "1")
  } else {
    value
  }
}

xData <- xtabs(counts ~ A + S + G + I, data = Infant)
ftable(xData)

# Backward Selection
library(MASS)
saturated <- loglm(counts ~ A*S*G*I, data = Infant, family = "Poisson")
saturated

backloglm <- step(saturated, direction = "backward")
best <- glm(counts ~ A + S + G + I + A:S + A:G + A:I + G:I, family = poisson, data = Infant)
`(AS, AG, AI, GI)` <- round(exp(predict(best, data.frame(Infant))), 2)
summary(best)

backloglm$lrt
backloglm$df
print(backloglm)
coef(backloglm)

ftable(fitted(backloglm))
ftable(resid(backloglm))

# Menentukan Model Terbaik
ASG_AI_SI_GI <- glm(counts ~ A + S + G + I + A:I + S:I + G:I + A:S:G, family = poisson, data = Infant)
AGI_AS_SG_SI <- glm(counts ~ A + S + G + I + A:S + S:G + S:I + A:G:I, family = poisson, data = Infant)
ASI_AG_SG_GI <- glm(counts ~ A + S + G + I + A:G + S:G + I:G + A:S:I, family = poisson, data = Infant)
SGI_AS_AG_AI <- glm(counts ~ A + S + G + I + A:S + A:G + A:I + S:G:I, family = poisson, data = Infant)
AS_AG_AI_SG_GI <- glm(counts ~ A + S + G + I + A:S + A:G + A:I + S:G + I:G, family = poisson, data = Infant)

`Menentukan Model Terbaik` <-  
  bind_cols(Model = c("(A, S, G, I)", "(AS, AG, AI)", "(AG, SG, GI)", "(AS, AG, AI, GI)", 
                      "(AS, AG, AI, SG, GI)", "(ASG, AI, SI, GI)",
                      "(ASI, AG, SG, GI)", "(SGI, AS, AG, AI)", "(ASG, SGI, ASI)", "(ASGI)"),
            Deviance = c(deviance(A_S_G_I), deviance(AS_AG_AI), deviance(AG_SG_GI), deviance(best), 
                         deviance(AS_AG_AI_SG_GI), deviance(ASG_AI_SI_GI), 
                         deviance(ASI_AG_SG_GI),deviance(SGI_AS_AG_AI), deviance(ASG_SGI_ASI), deviance(ASGI)),
            df = c(df.residual(A_S_G_I), df.residual(AS_AG_AI), df.residual(AG_SG_GI), df.residual(best), 
                   df.residual(AS_AG_AI_SG_GI), df.residual(ASG_AI_SI_GI),
                   df.residual(ASI_AG_SG_GI), df.residual(SGI_AS_AG_AI), df.residual(ASG_SGI_ASI), df.residual(ASGI)),
            `P-value` = c(residualP(A_S_G_I, 4), residualP(AS_AG_AI, 4), residualP(AG_SG_GI, 4), residualP(best, 4),
                          residualP(AS_AG_AI_SG_GI, 4), residualP(ASG_AI_SI_GI, 4),
                          residualP(ASI_AG_SG_GI, 4), residualP(SGI_AS_AG_AI), residualP(ASG_SGI_ASI, 4), residualP(ASGI, 4)),
            `AIC` = c(AIC(A_S_G_I), AIC(AS_AG_AI), AIC(AG_SG_GI), AIC(best), AIC(AS_AG_AI_SG_GI), 
                      AIC(ASG_AI_SI_GI), AIC(ASI_AG_SG_GI), AIC(SGI_AS_AG_AI), AIC(ASG_SGI_ASI), AIC(ASGI))) %>% 
  mutate(Deviance = round(Deviance, 1))

kable(`Menentukan Model Terbaik`)

# Uji Interaksi
SG <- glm(counts ~ S + G + S:G, family = poisson, data = Infant)
SG_hom <- glm(counts ~ S + G, family = poisson, data = Infant)
AS <- glm(counts ~ A + S + A:S, family = poisson, data = Infant)
AS_hom <- glm(counts ~ A + S, family = poisson, data = Infant)
AG <- glm(counts ~ A + G + A:G, family = poisson, data = Infant)
AG_hom <- glm(counts ~ A + G, family = poisson, data = Infant)
anova(AS_hom, AS, test = "Chisq")
anova(AG_hom, AG, test = "Chisq")
anova(SG_hom, SG, test = "Chisq")

AGI <- glm(counts ~ A + G + I + A:G + A:I + G:I + A:G:I, family = poisson, data = Infant)
AGI_hom <- glm(counts ~ A + G + I + A:G + A:I + G:I, family = poisson, data = Infant)
anova(AGI_hom, AGI, test = "Chisq")

# Expected Table COntingency
`Table Expectedd` <-  bind_cols(`Age` = c("<30", rep("", 7), "30+", rep("",7)),
                                `Smoking` = rep(c("<5",rep(" ",3),"5+",rep(" ",3)),2),
                                `Gestation` = rep(c("<=260", " ", ">260", ""),4),
                                `Infant Survival` = rep(c("No", "Yes"), 8),
                                `(ASGI)` = `(ASGI)`,
                                `(AS, AG, AI, GI)` = `(AS, AG, AI, GI)`) 

knitr::kable(`Table Expectedd`)
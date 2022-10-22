#library(haven)
library(tidyverse)
#library(magrittr)
library(tableone)
library(WeightIt)
library(cobalt)
#library(mice)
library(missRanger)
library(survival)
library(survminer)
library(survey)
library(quantreg)
#library(casebase)
library(splines)
#library(glmnet)



#####################################tidy done------
dt <- read.csv('dt.csv')
names(dt) <- c('index', 'ID', 'sex', 'age', 'marriage', 
               'occupation', 'weight', 'heigh', 'alcohol','smoke_index',
               'smoke', 'infection', 'GI', 'rash', 'fever',
               'discontinuation', 'ab_liver', 'bone_suppression', 'hospitalization', 'cell_type',
               'chem_regimen', 'metastasis', 'stage', 'radiotherapy', 'invasive',
               "RBC.fi",   "HGB.FI",  "HCT.FI",   "MCV.FI",   "MCH.FI",   "MCHC.FI",  "CV.FI",   
               "SD.FI",    "PLT.FI",   "WBC.FI",   "NEUT.FI",  "LYMPH.FI", "MONO.FI",  "EO.fi",   
               "BASO.FI",  "TBIL.fi",  "DBIL.FI",  "IBIL.FI",  "ALT.fi",   "AST.fi",   "TP.FI",  
               "ALB.FI",   "GLU.FI",   "UREA.FI",  "CREA.fi", "URIC.fi",  "TG.fi",    "CHOL1.fi",
               "HDL.fi",   "LDL.fi",   "ALP.FI",   "GGT.FI",   "CK.FI",    "LDH.FI",   "NA.FI",   
               "K.FI",     "CL.FI",    "CA.FI",    "MG.Fi",    "P.Fi",     "PT.FI",    "INR.FI",  
               "APTT.FI",  "FIB.FI", 'FI_records', 'FI_records_2c', 
               'FI_lab', 'FI_lab_3c', 
               'death_date', 'death_indicator','diagnosis_date','surv_time')
dt <- dt %>% 
  mutate(alcohol=
           case_when(alcohol=='经常'~'always',
                                alcohol=='每天'~'always',
                                alcohol=='偶尔'~'sometimes',
                                alcohol=='无'~'never')) %>% 
  mutate(invasive=
           case_when(invasive==0 ~ 0,
                     invasive!=0 ~ 1)) %>% 
  mutate(BMI=weight/((heigh/100)^2))
dt <- dt %>% mutate(other_AE=
                      case_when(fever==1 ~ 1,
                                rash==1 ~ 1,
                                TRUE ~ 0)) %>% 
  mutate(sex=case_when(sex=='女' ~ 'female',
                              sex=='男' ~ 'male')) %>% 
  mutate(marriage=
        case_when(marriage=='已婚' ~ 'married',
         marriage=='丧偶' ~ 'widowed',
         marriage=='离异' ~ 'divorced'))

###dt1
dt1 <- dt %>% select("sex", "age", "marriage", "occupation",'alcohol',
         "smoke_index", "smoke", "infection", "GI","discontinuation", 
         "ab_liver", "bone_suppression","hospitalization", "cell_type",
         "chem_regimen", "metastasis", "stage", "radiotherapy", "invasive",
         "death_indicator","surv_time","BMI", "other_AE",
         'FI_records', 'FI_records_2c')
write.csv(dt1, 'dt1.csv')

###dt2
dt2 <- dt %>% select("sex", "age", "marriage", "occupation",'alcohol',
                     "smoke_index", "smoke", "infection", "GI","discontinuation", 
                     "ab_liver", "bone_suppression","hospitalization", "cell_type",
                     "chem_regimen", "metastasis", "stage", "radiotherapy", "invasive",
                     "death_indicator","surv_time","BMI", "other_AE",
                     "RBC.fi",   "HGB.FI",  "HCT.FI",   "MCV.FI",   "MCH.FI",   "MCHC.FI",  "CV.FI",   
                     "SD.FI",    "PLT.FI",   "WBC.FI",   "NEUT.FI",  "LYMPH.FI", "MONO.FI",  "EO.fi",   
                     "BASO.FI",  "TBIL.fi",  "DBIL.FI",  "IBIL.FI",  "ALT.fi",   "AST.fi",   "TP.FI",  
                     "ALB.FI",   "GLU.FI",   "UREA.FI",  "CREA.fi", "URIC.fi",  "TG.fi",    "CHOL1.fi",
                     "HDL.fi",   "LDL.fi",   "ALP.FI",   "GGT.FI",   "CK.FI",    "LDH.FI",   "NA.FI",   
                     "K.FI",     "CL.FI",    "CA.FI",    "MG.Fi",    "P.Fi",     "PT.FI",    "INR.FI",  
                     "APTT.FI",  "FIB.FI")
write.csv(dt2, 'dt2.csv')


# Part one ----------------------------------------------------------------
#####################Part one: record-based FI
###data desciption
dt1 <- read_csv('dt1.csv')
###Table one
tableone <- CreateTableOne(data = dt1, addOverall = T,
                           strata = 'FI_records_2c',
                           vars = c("sex", "age", "marriage", "occupation",'alcohol',
                                    "smoke_index", "smoke", "infection", "GI","discontinuation", 
                                    "ab_liver", "bone_suppression","hospitalization", "cell_type",
                                    "chem_regimen", "metastasis", "stage", "radiotherapy", "invasive",
                                    "death_indicator","surv_time","BMI", "other_AE"),
                           factorVars = c("sex","marriage", "occupation",'alcohol',"smoke",
                                     "infection", "GI","discontinuation", "ab_liver", "bone_suppression",
                                     "cell_type","chem_regimen", "metastasis", "stage", "radiotherapy", 
                                     "invasive",  "death_indicator", "other_AE"))
tableone %>% 
  print(nonnormal=c('smoke_index', 'hospitalization',"surv_time"), smd=T) %>% 
  write.csv('tableone.csv')

###factor
dt1 %<>% mutate_at(c('occupation', 'smoke', 'infection', 'GI', 'discontinuation',
                     'ab_liver', 'bone_suppression', 'alcohol',
                     'cell_type', 'chem_regimen', 'metastasis', 'stage',
                     'radiotherapy', 'invasive', 'other_AE',
                     'FI_records_2c'), funs(factor(.)))

###imputaion
md.pattern(dt1)
imp <- mice(dt1, m=1, method = 'pmm') %>% complete()
dt1$metastasis <- imp$metastasis
dt1$alcohol <- imp$alcohol

###cut exposure
#dt1$FI_records_3c <- cut(dt1$FI_records, breaks = c(-Inf, 0.2, 0.3, Inf), 
#                         labels = c('0', '1', '2'))
#dt1$FI_records_3c %>% table()


###data analysis
###model 1 ipw
ipw1 <- weightit(FI_records_2c~sex+age, data = dt1, stabilize = T)
ipw1$weights %>% summary()

###outcome1: all cause
#k-m
fit1_km <- survfit(Surv(surv_time, death_indicator) ~ FI_records_2c, 
                   data = dt1, weights = ipw1$weights)
ggsurvplot(fit1_km)
#cox
fit1_cox <- coxph(Surv(surv_time, death_indicator) ~ FI_records_2c, 
                  data = dt1, weights = ipw1$weights)
summary(fit1_cox)
###outcome2: infection
design1 <- svydesign(~1, weights = ipw1$weights, data = dt1)
fit1_infection <- svyglm(infection ~ FI_records_2c, design=design1,
                         family=quasibinomial())
fit1_infection$coefficients %>% exp()
fit1_infection %>% confint() %>% exp()
###outcome3: bone_suppression
fit1_bone_suppression <- svyglm(bone_suppression ~ FI_records_2c, design=design1,
                  family=quasibinomial())
fit1_bone_suppression$coefficients %>% exp()
fit1_bone_suppression %>% confint() %>% exp()
###outcome4: discontinuation
fit1_discontinuation <- svyglm(discontinuation ~ FI_records_2c, design=design1,
                                family=quasibinomial())
fit1_discontinuation$coefficients %>% exp()
fit1_discontinuation %>% confint() %>% exp()
###outcome5: ab_liver
fit1_ab_liver <- svyglm(ab_liver ~ FI_records_2c, design=design1,
                               family=quasibinomial())
fit1_ab_liver$coefficients %>% exp()
fit1_ab_liver %>% confint() %>% exp()
###outcome6: GI
fit1_GI <- svyglm(GI ~ FI_records_2c, design=design1,
                        family=quasibinomial())
fit1_GI$coefficients %>% exp()
fit1_GI %>% confint() %>% exp()
###outcome7: hospitalization
fit1_hospitalization <- rq(hospitalization ~ FI_records_2c,
                           weights = ipw1$weights, data = dt1)
fit1_hospitalization %>% summary()


###model 2 ipw
ipw2 <- weightit(FI_records_2c~sex+age+alcohol+smoke+BMI,
                 data = dt1, stabilize = T)
ipw2$weights %>% summary()

###outcome1: all cause
#k-m
fit2_km <- survfit(Surv(surv_time, death_indicator) ~ FI_records_2c, 
                   data = dt1, weights = ipw2$weights)
ggsurvplot(fit2_km)
#cox
fit2_cox <- coxph(Surv(surv_time, death_indicator) ~ FI_records_2c, 
                  data = dt1, weights = ipw2$weights)
summary(fit2_cox)
###outcome2: infection
design2 <- svydesign(~1, weights = ipw2$weights, data = dt1)
fit2_infection <- svyglm(infection ~ FI_records_2c, design=design2,
                         family=binomial())
fit2_infection$coefficients %>% exp()
fit2_infection %>% confint() %>% exp()
###outcome3: bone_suppression
fit2_bone_suppression <- svyglm(bone_suppression ~ FI_records_2c, design=design2,
                                family=quasibinomial())
fit2_bone_suppression$coefficients %>% exp()
fit2_bone_suppression %>% confint() %>% exp()
###outcome4: discontinuation
fit2_discontinuation <- svyglm(discontinuation ~ FI_records_2c, design=design2,
                               family=quasibinomial())
fit2_discontinuation$coefficients %>% exp()
fit2_discontinuation %>% confint() %>% exp()
###outcome5: ab_liver
fit2_ab_liver <- svyglm(ab_liver ~ FI_records_2c, design=design2,
                        family=quasibinomial())
fit2_ab_liver$coefficients %>% exp()
fit2_ab_liver %>% confint() %>% exp()
###outcome6: GI
fit2_GI <- svyglm(GI ~ FI_records_2c, design=design2,
                  family=quasibinomial())
fit2_GI$coefficients %>% exp()
fit2_GI %>% confint() %>% exp()
###outcome7: hospitalization
fit2_hospitalization <- rq(hospitalization ~ FI_records_2c,
                           weights = ipw2$weights, data = dt1)
fit2_hospitalization %>% summary()


###model 3 ipw
ipw3 <- weightit(FI_records_2c~sex+age+alcohol+smoke+BMI
                 +cell_type+chem_regimen+metastasis+stage+radiotherapy+invasive,
                 data = dt1, stabilize = T)
ipw3$weights %>% summary()
##plot
tiff('ipwplot.tiff', res = 300, width = 300*8, height = 300*6)
love.plot(ipw3, thresholds = 0.1)
dev.off()
tiff('psplot.tiff', res = 300, width = 300*8, height = 300*6)
bal.plot(ipw3)
dev.off()

###outcome1: all cause
#k-m
fit3_km <- survfit(Surv(surv_time, death_indicator) ~ FI_records_2c, 
                   data = dt1, weights = ipw3$weights)
fit3_km %>% summary()
#plot
tiff('KMplot1.tiff', res = 300, width = 300*8, height = 300*6)
ggsurvplot(fit3_km, risk.table = T, 
           surv.median.line = 'hv', pval = T,
           palette = 'lancet',
           legend.labs=c('Non-frailty', 'Frailty'),
           legend.title='',
           xlab='Months')
dev.off()
#cox
fit3_cox <- coxph(Surv(surv_time, death_indicator) ~ FI_records_2c, 
                  data = dt1, weights = ipw3$weights)
summary(fit3_cox)

#cox excluding months
dt1$weights <- ipw3$weights
dt1_e3m <- dt1 %>% filter(surv_time>12)
coxph(Surv(surv_time, death_indicator) ~ FI_records_2c, 
                  data = dt1_e3m, weights = weights) %>% summary()

# #nomogram by cox model
# library(rms)
# dt2 <- dt1 %>% 
#   rename(eFI_scores=FI_records)
# dt_nomo <- datadist(dt2)
# options(datadist='dt_nomo')
# fit_nomo_psm <- psm(Surv(surv_time, death_indicator) ~ eFI_scores+sex+age+alcohol+smoke+BMI
#                     +cell_type+chem_regimen+metastasis+stage+radiotherapy+invasive, 
#                     data = dt2, dist = 'lognormal')
# med <- Quantile(fit_nomo_psm)
# surv <- Survival(fit_nomo_psm)
# ###median survival time
# tiff('nomo.tiff', res = 300, width = 300*12, height = 300*9)
# nomogram(fit_nomo_psm, fun=function(x) med(lp=x), 
#          funlabel="Median Survival Months",
#          lp=F) %>% 
#   plot()
# dev.off()

###outcome2: infection
design3 <- svydesign(~1, weights = ipw3$weights, data = dt1)
fit3_infection <- svyglm(infection ~ FI_records_2c, design=design3,
                         family=binomial())
fit3_infection$coefficients %>% exp()
fit3_infection %>% confint() %>% exp()
###outcome3: bone_suppression
fit3_bone_suppression <- svyglm(bone_suppression ~ FI_records_2c, design=design3,
                                family=quasibinomial())
fit3_bone_suppression$coefficients %>% exp()
fit3_bone_suppression %>% confint() %>% exp()
###outcome4: discontinuation
fit3_discontinuation <- svyglm(discontinuation ~ FI_records_2c, design=design3,
                               family=quasibinomial())
fit3_discontinuation$coefficients %>% exp()
fit3_discontinuation %>% confint() %>% exp()
###outcome5: ab_liver
fit3_ab_liver <- svyglm(ab_liver ~ FI_records_2c, design=design3,
                        family=quasibinomial())
fit3_ab_liver$coefficients %>% exp()
fit3_ab_liver %>% confint() %>% exp()
###outcome6: GI
fit3_GI <- svyglm(GI ~ FI_records_2c, design=design3,
                  family=quasibinomial())
fit3_GI$coefficients %>% exp()
fit3_GI %>% confint() %>% exp()
###outcome7: hospitalization
fit3_hospitalization <- rq(hospitalization ~ FI_records_2c,
                           weights = ipw3$weights, data = dt1)
fit3_hospitalization %>% summary()

###overall survival
#overall survial
sink('RMST.txt')
print('********************************************************************')
print('model 1')
akm_rmst(time = dt1$surv_time, status = dt1$death_indicator,
         group = dt1$FI_records_2c, weight = ipw1$weights)
print('********************************************************************')
print('model 2')
akm_rmst(time = dt1$surv_time, status = dt1$death_indicator,
         group = dt1$FI_records_2c, weight = ipw2$weights)
print('********************************************************************')
print('model 3')
akm_rmst(time = dt1$surv_time, status = dt1$death_indicator,
         group = dt1$FI_records_2c, weight = ipw3$weights)
sink()


#####Per unit 
dt1$FI_records_per0.1 <- dt1$FI_records*10
ipw_perunit <- weightit(FI_records_per0.1~sex+age+alcohol+smoke+BMI
                 +cell_type+chem_regimen+metastasis+stage+radiotherapy+invasive,
                 data = dt1, stabilize = T, 
                 method = 'ebal')
ipw_perunit$weights %>% summary()
##plot
tiff('ipwplot_perunit.tiff', res = 300, width = 300*8, height = 300*6)
love.plot(ipw_perunit, thresholds = 0.1)
dev.off()

###all cause death
#cox
fit_perunit_cox <- coxph(Surv(surv_time, death_indicator) ~ FI_records_per0.1,
                         weights = ipw_perunit$weights, data = dt1)
fit_perunit_cox %>% summary()
###outcome2: infection
design_perunit <- svydesign(~1, weights = ipw_perunit$weights, data = dt1)
fit_perunit_infection <- svyglm(infection ~ FI_records_per0.1, design=design_perunit,
                         family=binomial())
fit_perunit_infection$coefficients %>% exp()
fit_perunit_infection %>% confint() %>% exp()
###outcome3: bone_suppression
fit_perunit_bone_suppression <- svyglm(bone_suppression ~ FI_records_per0.1, design=design_perunit,
                                family=binomial())
fit_perunit_bone_suppression$coefficients %>% exp()
fit_perunit_bone_suppression %>% confint() %>% exp()
###outcome4: discontinuation
fit_perunit_discontinuation <- svyglm(discontinuation ~ FI_records_per0.1, design=design_perunit,
                                       family=binomial())
fit_perunit_discontinuation$coefficients %>% exp()
fit_perunit_discontinuation %>% confint() %>% exp()
###outcome5: ab_liver
fit_perunit_ab_liver <- svyglm(ab_liver ~ FI_records_per0.1, design=design_perunit,
                                      family=binomial())
fit_perunit_ab_liver$coefficients %>% exp()
fit_perunit_ab_liver %>% confint() %>% exp()
###outcome6: GI
fit_perunit_GI <- svyglm(GI ~ FI_records_per0.1, design=design_perunit,
                               family=binomial())
fit_perunit_GI$coefficients %>% exp()
fit_perunit_GI %>% confint() %>% exp()
###outcome7: hospitalization
fit_perunit_hospitalization <- rq(hospitalization ~ FI_records_per0.1,
                           weights = ipw_perunit$weights, data = dt1)
fit_perunit_hospitalization %>% summary()


#####Per 1SD
dt1 %>% summarise(sd=sd(FI_records))  ###0.0719
dt1 <- dt1 %>% mutate(FI_records_persd=FI_records/0.0719)
dt1 %>% summarise(per=summary(FI_records), persd=summary(FI_records_persd))

###model
ipw_persd <- weightit(FI_records_persd~sex+age+alcohol+smoke+BMI
                        +cell_type+chem_regimen+metastasis+stage+radiotherapy+invasive,
                        data = dt1, stabilize = T, 
                        method = 'ebal')
ipw_persd$weights %>% summary()
##plot
tiff('ipwplot_persd.tiff', res = 300, width = 300*8, height = 300*6)
love.plot(ipw_persd, thresholds = 0.1)
dev.off()

###all cause death
#cox
fit_persd_cox <- coxph(Surv(surv_time, death_indicator) ~ FI_records_persd,
                         weights = ipw_persd$weights, data = dt1)
fit_persd_cox %>% summary()
###outcome2: infection
design_persd <- svydesign(~1, weights = ipw_persd$weights, data = dt1)
fit_persd_infection <- svyglm(infection ~ FI_records_persd, design=design_persd,
                                family=binomial())
fit_persd_infection$coefficients %>% exp()
fit_persd_infection %>% confint() %>% exp()
###outcome3: bone_suppression
fit_persd_bone_suppression <- svyglm(bone_suppression ~ FI_records_persd, design=design_persd,
                                       family=binomial())
fit_persd_bone_suppression$coefficients %>% exp()
fit_persd_bone_suppression %>% confint() %>% exp()
###outcome4: discontinuation
fit_persd_discontinuation <- svyglm(discontinuation ~ FI_records_persd, design=design_persd,
                                      family=binomial())
fit_persd_discontinuation$coefficients %>% exp()
fit_persd_discontinuation %>% confint() %>% exp()
###outcome5: ab_liver
fit_persd_ab_liver <- svyglm(ab_liver ~ FI_records_persd, design=design_persd,
                               family=binomial())
fit_persd_ab_liver$coefficients %>% exp()
fit_persd_ab_liver %>% confint() %>% exp()
###outcome6: GI
fit_persd_GI <- svyglm(GI ~ FI_records_persd, design=design_persd,
                         family=binomial())
fit_persd_GI$coefficients %>% exp()
fit_persd_GI %>% confint() %>% exp()
###outcome7: hospitalization
fit_persd_hospitalization <- rq(hospitalization ~ FI_records_persd,
                                  weights = ipw_persd$weights, data = dt1)
fit_persd_hospitalization %>% summary()



# Part two ----------------------------------------------------------------
# ###lab FI and mortality risk 
# dt <- read.csv('dt2.csv')
# 
# dt[,25:68] <- dt[,25:68] %>% missRanger(pmm.k = 1)
# 
# # form <- paste0('death_indicator', '~', paste('log(surv_time)', paste(names(dt[25:68]), collapse = '+'), sep = '+')) %>% 
# #   as.formula()
# # 
# # fit <- fitSmoothHazard(formula = form, time = 'surv_time', nfolds=10,
# #                        data = dt, family = 'glmnet',
# #                        type.measure='auc'
# #                        )
# # fit
# # ###print
# # sink('glmnet11.txt')
# # fit %>% print()
# # coef.glmnet(fit, s=fit$lambda.min)
# # predict(fit, type='nonzero', s = fit$lambda.min)
# # sink()
# # ###plot
# # tiff('cvglmnet11.tiff', res = 300, width = 300*10, height = 300*6)
# # plot(fit)
# # dev.off()
# 
# 
# ###19 based lab FI
# ###"RBC.fi", "CV.FI", "PLT.FI", "WBC.FI", "MONO.FI", 
# ###"BASO.FI",  "IBIL.FI", "ALB.FI",  "URIC.fi", "TG.fi",   
# ###"HDL.fi",  "GGT.FI", "LDH.FI", "NA.FI", "K.FI",   
# ###"CL.FI", "CA.FI",  "PT.FI", "FIB.FI"
# dt_19 <- dt %>% select("sex", "age", "marriage", "occupation",'alcohol',
#                             "smoke_index", "smoke", "infection", "GI","discontinuation", 
#                             "ab_liver", "bone_suppression","hospitalization", "cell_type",
#                             "chem_regimen", "metastasis", "stage", "radiotherapy", "invasive",
#                             "death_indicator","surv_time","BMI", "other_AE",
#                     "RBC.fi", "CV.FI", "PLT.FI", "WBC.FI", "MONO.FI",
#                     "BASO.FI",  "IBIL.FI", "ALB.FI",  "URIC.fi", "TG.fi", 
#                     "HDL.fi",  "GGT.FI", "LDH.FI", "NA.FI", "K.FI",   
#                     "CL.FI", "CA.FI",  "PT.FI", "FIB.FI")
# dt_19 <- dt_19 %>% rowwise() %>% 
#   mutate(labFI=sum(c_across(24:42))) %>% ungroup()
# dt_19 <- dt_19 %>% mutate(std_labFI=labFI/19) %>% 
#   mutate(labFI_3c=cut(std_labFI, breaks = c(-Inf, 0.2, 0.35, Inf), 
#                       labels = c('0', '1', '2')))
# 
# ###10 based lab FI
# ###"PLT.FI", "WBC.FI"
# ###"ALB.FI",  "URIC.fi", 
# ###"HDL.fi", "LDH.FI", "NA.FI",   
# ###"CA.FI",  "PT.FI", "FIB.FI"
# dt_10 <- dt %>% select("sex", "age", "marriage", "occupation",'alcohol',
#                     "smoke_index", "smoke", "infection", "GI","discontinuation", 
#                     "ab_liver", "bone_suppression","hospitalization", "cell_type",
#                     "chem_regimen", "metastasis", "stage", "radiotherapy", "invasive",
#                     "death_indicator","surv_time","BMI", "other_AE",
#                     "PLT.FI","WBC.FI", 
#                     "ALB.FI", 
#                     "URIC.fi", 
#                     "HDL.fi", 
#                     "LDH.FI", "NA.FI",   
#                     "CA.FI", "PT.FI", "FIB.FI")
# dt_10 <- dt_10 %>% rowwise() %>% 
#   mutate(labFI=sum(c_across(24:33))) %>% ungroup()
# dt_10 <- dt_10 %>% mutate(std_labFI=labFI/10) %>% 
#   mutate(labFI_3c=cut(std_labFI, breaks = c(-Inf, 0.2, 0.35, Inf), 
#                       labels = c('0', '1', '2')))
# 
# 
# ###imputation
# dt_19 <- dt_19 %>% mutate(alcohol=as.factor(alcohol),
#                     smoke=as.factor(smoke),
#                     cell_type=as.factor(cell_type),
#                     metastasis=as.factor(metastasis),
#                     stage=as.factor(stage),
#                     radiotherapy=as.factor(radiotherapy),
#                     invasive=as.factor(invasive)
#                     ) %>% 
#   missRanger(pmm.k = 1)
# 
# ###bind outcomes
# dt_19 <- dt_19 %>% mutate(anyAE=case_when(
#   infection==0 & GI==0 & other_AE==0 ~ 0,
#   TRUE ~ 1))
# 
# ###imputation
# dt_10 <- dt_10 %>% mutate(alcohol=as.factor(alcohol),
#                           smoke=as.factor(smoke),
#                           cell_type=as.factor(cell_type),
#                           metastasis=as.factor(metastasis),
#                           stage=as.factor(stage),
#                           radiotherapy=as.factor(radiotherapy),
#                           invasive=as.factor(invasive)
# ) %>% 
#   missRanger(pmm.k = 1)
# 
# ###bind outcomes
# dt_10 <- dt_10 %>% mutate(anyAE=case_when(
#   infection==0 & GI==0 & other_AE==0 ~ 0,
#                              TRUE ~ 1))


# ###Table one for 19
# tableone <- CreateTableOne(data = dt_19, addOverall = T,
#                            strata = 'labFI_3c',
#                            vars = c("sex", "age", "marriage", "occupation",'alcohol',
#                                     "smoke_index", "smoke", "infection", "GI","discontinuation", 
#                                     "ab_liver", "bone_suppression","hospitalization", "cell_type",
#                                     "chem_regimen", "metastasis", "stage", "radiotherapy", "invasive",
#                                     "death_indicator","surv_time","BMI", "other_AE"),
#                            factorVars = c("sex","marriage", "occupation",'alcohol',"smoke",
#                                           "infection", "GI","discontinuation", "ab_liver", "bone_suppression",
#                                           "cell_type","chem_regimen", "metastasis", "stage", "radiotherapy", 
#                                           "invasive",  "death_indicator", "other_AE"))
# tableone %>% 
#   print(nonnormal=c('smoke_index', 'hospitalization',"surv_time"), smd=T) %>% 
#   write.csv('tableone_19FI.csv')
# 
# ###Table one for 10
# tableone <- CreateTableOne(data = dt_10, addOverall = T,
#                            strata = 'labFI_3c',
#                            vars = c("sex", "age", "marriage", "occupation",'alcohol',
#                                     "smoke_index", "smoke", "infection", "GI","discontinuation", 
#                                     "ab_liver", "bone_suppression","hospitalization", "cell_type",
#                                     "chem_regimen", "metastasis", "stage", "radiotherapy", "invasive",
#                                     "death_indicator","surv_time","BMI", "other_AE"),
#                            factorVars = c("sex","marriage", "occupation",'alcohol',"smoke",
#                                           "infection", "GI","discontinuation", "ab_liver", "bone_suppression",
#                                           "cell_type","chem_regimen", "metastasis", "stage", "radiotherapy", 
#                                           "invasive",  "death_indicator", "other_AE"))
# tableone %>% 
#   print(nonnormal=c('smoke_index', 'hospitalization',"surv_time"), smd=T) %>% 
#   write.csv('tableone_10FI.csv')
# 
# ###write csv
# write_csv(dt_10, 'dt_10FI.csv')
# write_csv(dt_19, 'dt_19FI.csv')


##################data analysis-----------------
#dt <- read_csv('dt_10FI.csv')
dt <- read_csv('dt_19FI.csv')

dt <- dt %>% mutate(
  labFI_3c=as_factor(labFI_3c)
)


####################model 2 ipw
ipw2 <- weightit(labFI_3c~sex+age+alcohol+smoke+BMI,
                 data = dt, stabilize = T)
ipw2$weights %>% summary()

###outcome1: all cause
#k-m
fit2_km <- survfit(Surv(surv_time, death_indicator) ~ labFI_3c, 
                   data = dt, weights = ipw2$weights)
ggsurvplot(fit2_km)
#cox
fit2_cox <- coxph(Surv(surv_time, death_indicator) ~ labFI_3c, 
                  data = dt, weights = ipw2$weights)
summary(fit2_cox)

###outcome2: infection
design2 <- svydesign(~1, weights = ipw2$weights, data = dt)
fit2_infection <- svyglm(infection ~ labFI_3c, design=design2,
                         family=binomial())

###outcome3: any AE
fit2_anyAE <- svyglm(anyAE ~ labFI_3c, design=design2,
                                family=quasibinomial())

###outcome4: other AE
fit2_other_AE <- svyglm(other_AE ~ labFI_3c, design=design2,
                               family=quasibinomial())

###outcome5: GI
fit2_GI <- svyglm(GI ~ labFI_3c, design=design2,
                        family=quasibinomial())

###print
sink('10FI -Model 2.txt')
print('-----------IPW-----------')
summary(ipw2)
print('-----------End------------')
print('-----------death----------')
summary(fit2_cox)
print('-----------End------------')
print('-----------any AE----------')
fit2_anyAE %>% broom::tidy(conf.int=T, exp=T)
print('-----------End------------')
print('-----------infection----------')
fit2_infection %>% broom::tidy(conf.int=T, exp=T)
print('-----------End------------')
print('-----------other AE----------')
fit2_other_AE %>% broom::tidy(conf.int=T, exp=T)
print('-----------End------------')
print('-----------GI----------')
fit2_GI %>% broom::tidy(conf.int=T, exp=T)
print('-----------End------------')
sink()


#####################model 3 ipw
ipw3 <- weightit(labFI_3c~sex+age+alcohol+smoke+BMI+
                 cell_type+chem_regimen+metastasis+stage+radiotherapy+invasive,
                 data = dt, stabilize = T)
ipw3$weights %>% summary()
##plot
tiff('ipwplot -Model 3.tiff', res = 300, width = 300*8, height = 300*6)
love.plot(ipw3, thresholds = 0.1)
dev.off()


###outcome1: all cause
#k-m
fit3_km <- survfit(Surv(surv_time, death_indicator) ~ labFI_3c, 
                   data = dt, weights = ipw3$weights)
fit3_km %>% summary()
#plot
tiff('KMplot -Model 3.tiff', res = 300, width = 300*8, height = 300*6)
ggsurvplot(fit3_km, risk.table = T, 
           surv.median.line = 'hv', pval = T,
           palette = 'lancet',
           legend.labs=c('Non-frailty', 'Pre-frailty', 'Frailty'),
           legend.title='')
dev.off()
#cox
fit3_cox <- coxph(Surv(surv_time, death_indicator) ~ labFI_3c, 
                  data = dt, weights = ipw3$weights)
summary(fit3_cox)

###outcome2: infection
design3 <- svydesign(~1, weights = ipw3$weights, data = dt)
fit3_infection <- svyglm(infection ~ labFI_3c, design=design3,
                         family=binomial())

###outcome3: any AE
fit3_anyAE <- svyglm(anyAE ~ labFI_3c, design=design3,
                                family=quasibinomial())

###outcome4: other AE
fit3_other_AE <- svyglm(other_AE ~ labFI_3c, design=design3,
                               family=quasibinomial())

###outcome5: GI
fit3_GI <- svyglm(GI ~ labFI_3c, design=design3,
                        family=quasibinomial())

###print
sink('10FI -Model 3.txt')
print('-----------IPW-----------')
summary(ipw3)
print('-----------End------------')
print('-----------death----------')
summary(fit3_cox)
print('-----------End------------')
print('-----------any AE----------')
fit3_anyAE %>% broom::tidy(conf.int=T, exp=T)
print('-----------End------------')
print('-----------infection----------')
fit3_infection %>% broom::tidy(conf.int=T, exp=T)
print('-----------End------------')
print('-----------other AE----------')
fit3_other_AE %>% broom::tidy(conf.int=T, exp=T)
print('-----------End------------')
print('-----------GI----------')
fit3_GI %>% broom::tidy(conf.int=T, exp=T)
print('-----------End------------')
sink()


###overall survival
#overall survial
sink('RMST -Model 2 and Model 3.txt')
print('********************************************************************')
print('model 2')
akm_rmst(time = dt$surv_time, status = dt$death_indicator,
         group = dt$labFI_3c, weight = ipw2$weights)
print('********************************************************************')
print('model 3')
akm_rmst(time = dt$surv_time, status = dt$death_indicator,
         group = dt$labFI_3c, weight = ipw3$weights)
sink()


#################Per unit 
dt$std_labFI_per0.1 <- dt$std_labFI*10
ipw_perunit <- weightit(std_labFI_per0.1~sex+age+alcohol+smoke+BMI
                        +cell_type+chem_regimen+metastasis+stage+radiotherapy+invasive,
                        data = dt, stabilize = T, 
                        method = 'ebal')
ipw_perunit$weights %>% summary()

###all cause death
#cox
fit_perunit_cox <- coxph(Surv(surv_time, death_indicator) ~ std_labFI_per0.1,
                         weights = ipw_perunit$weights, data = dt)
fit_perunit_cox %>% summary()

###outcome2: infection
design_perunit <- svydesign(~1, weights = ipw_perunit$weights, data = dt)
fit_perunit_infection <- svyglm(infection ~ std_labFI_per0.1, design=design_perunit,
                                family=binomial())

###outcome3: any AE
fit_perunit_anyAE <- svyglm(anyAE ~ std_labFI_per0.1, design=design_perunit,
                                       family=binomial())

###outcome4: other AE
fit_perunit_other_AE <- svyglm(other_AE ~ std_labFI_per0.1, design=design_perunit,
                                      family=binomial())

###outcome5: GI
fit_perunit_GI <- svyglm(GI ~ std_labFI_per0.1, design=design_perunit,
                               family=binomial())

###print
sink('10FI -Per0.1 -Model 3.txt')
print('-----------IPW-----------')
summary(ipw_perunit)
print('-----------End------------')
print('-----------death----------')
summary(fit_perunit_cox)
print('-----------End------------')
print('-----------any AE----------')
fit_perunit_anyAE %>% broom::tidy(conf.int=T, exp=T)
print('-----------End------------')
print('-----------infection----------')
fit_perunit_infection %>% broom::tidy(conf.int=T, exp=T)
print('-----------End------------')
print('-----------other AE----------')
fit_perunit_other_AE %>% broom::tidy(conf.int=T, exp=T)
print('-----------End------------')
print('-----------GI----------')
fit_perunit_GI %>% broom::tidy(conf.int=T, exp=T)
print('-----------End------------')
sink()



######################Per 1SD
dt %>% summarise(sd=sd(std_labFI))  ###0.1287719  ###0.17962
dt <- dt %>% mutate(std_labFI_persd=std_labFI/0.17962)
dt %>% summarise(per=summary(std_labFI), persd=summary(std_labFI_persd))

###model
ipw_persd <- weightit(std_labFI_persd~sex+age+alcohol+smoke+BMI
                      +cell_type+chem_regimen+metastasis+stage+radiotherapy+invasive,
                      data = dt, stabilize = T, 
                      method = 'ebal')
ipw_persd$weights %>% summary()


###all cause death
#cox
fit_persd_cox <- coxph(Surv(surv_time, death_indicator) ~ std_labFI_persd,
                       weights = ipw_persd$weights, data = dt)
fit_persd_cox %>% summary()

###outcome2: infection
design_persd <- svydesign(~1, weights = ipw_persd$weights, data = dt)
fit_persd_infection <- svyglm(infection ~ std_labFI_persd, design=design_persd,
                              family=binomial())
fit_persd_infection %>% broom::tidy(conf.int=T)

###outcome3: any AE
fit_persd_anyAE <- svyglm(anyAE ~ std_labFI_persd, design=design_persd,
                                     family=binomial())
fit_persd_anyAE %>% broom::tidy(conf.int=T)

###outcome4: other AE
fit_persd_other_AE <- svyglm(other_AE ~ std_labFI_persd, design=design_persd,
                                    family=binomial())
fit_persd_other_AE %>% broom::tidy(conf.int=T)

###outcome5: GI
fit_persd_GI <- svyglm(GI ~ std_labFI_persd, design=design_persd,
                       family=binomial())
fit_persd_GI %>% broom::tidy(conf.int=T)


###print
sink('10FI -Per1SD -Model 3.txt')
print('-----------IPW-----------')
summary(ipw_persd)
print('-----------End------------')
print('-----------death----------')
summary(fit_persd_cox)
print('-----------End------------')
print('-----------any AE----------')
fit_persd_anyAE %>% broom::tidy(conf.int=T, exp=T)
print('-----------End------------')
print('-----------infection----------')
fit_persd_infection %>% broom::tidy(conf.int=T, exp=T)
print('-----------End------------')
print('-----------other AE----------')
fit_persd_other_AE %>% broom::tidy(conf.int=T, exp=T)
print('-----------End------------')
print('-----------GI----------')
fit_persd_GI %>% broom::tidy(conf.int=T, exp=T)
print('-----------End------------')
sink()



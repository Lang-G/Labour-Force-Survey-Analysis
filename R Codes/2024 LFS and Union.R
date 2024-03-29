install.packages('glmnet')

install.packages("fastDummies")  
install.packages('sandwitch')
install.packages('lmtest')
plm
install.packages('plm')
install.packages('car')
install.packages('stargazer')
install.packages('gtsummary')
install.packages('mgcv')
# Install haven package
library("haven") 

## Loading packages###
library("foreign")
library("ggthemes")
library(dplyr)
library('tidyverse')
library(tidymodels)
library(rpart.plot)
library(glmnet)
library(cvTools)
library(fastDummies)
library(lmtest)
library('BBmisc')
library('plm')
library(car)
library(stargazer)
library(gtsummary)
library(sandwich)
library(mgcv)
## Clean Data#####

Sector<-c('Public sector employees', 'Private sector employees')

Union_non_Union_Only=c('Union member','Non-unionized')

Employed<- c('Employed, at work', 'Employed, absent from work')

Loaded_Data<-list.files(path='./') %>% lapply(read.spss) %>% bind_rows%>%
filter(NAICS_21 %in% c('Health care and social assistance'))%>% 
filter(LFSSTAT %in% Employed )

df<-Loaded_Data%>%select(-c('AGE_6','NOC_10','ESTSIZE','SURVYEAR','REC_NUM','AHRSMAIN','ATOTHRS','YAWAY','HRSAWAY','XTRAHRS','DURUNEMP','FLOWUNEM','UNEMFTPT','WHYLEFTO','WHYLEFTN','DURJLESS','AVAILABL','LKPUBAG','LKEMPLOY','LKRELS','LKATADS','LKOTHERN','PRIORACT','YNOLOOK','TLOLOOK','PREVTEN','LKANSADS','EVERWORK','FTPTLAST','LFSSTAT','UTOTHRS','NAICS_21'))%>% filter(!is.na(HRLYEARN))  %>%mutate_if(is.factor, recode_factor, missing ="Missing")
# LFSSTAT REMOVED DUE TO MLTICOLINARITY WITH THE COLUMN YBSENT and similar reason for 'UTOTHRS'.

df<-df[-C(8,39,19), ]%>%tibble()
str(df)
df<-df%>%tibble()
df2<-df%>%filter(PROV=='British Columbia')%>% select(-c('PROV')) 
# Prepare df3 for Lasso regression##
df3<-df%>%filter(PROV=='British Columbia')%>% select(-c('PROV','WKSAWAY','PAYAWAY','MJH','WHYPT','SCHOOLN','EFAMTYPE','AGYOWNK','FINALWT','YABSENT','FTPTMAIN')) %>% dummy_cols()%>% select(-c('SURVMNTH',	'CMA'	,'AGE_12',	'SEX',	'MARSTAT',	'EDUC'	,	'COWMAIN',	'IMMIG'	,'NOC_43'	,'UNION',	'PERMTEMP',	'FIRMSIZE'))
write.csv(df3,"/Users/wutau/Desktop/R learning/BC G R27/Documentation/FILES.csv")
df2[is.character(df2) & df2=="<NA>"] <- "Missing"
rlang::last_trace()

df[is.numeric(df) & is.na(df)] <- 0
fd0<-df %>% mutate_if(is.factor, recode_factor, .missing ="Missing")
df_cor <- df%>% 
 mutate(across(where(is.factor), as.character))

df_cor[is.character(df_cor) & is.na(df_cor)] <- 'Missing'
df_cor$YABSENT[is.character(df_cor$YABSENT) & is.na(df_cor$YABSENT)] <- 'Missing' #working in this case 

df_core <- df_cor%>% 
 mutate(across(where(is.character), as.factor))
print(unique(df_core$YABSENT ))
which(is.na(df_cor$YABSENT))
print(unique(df_cor$YABSENT ))
summary(df_cor$YABSENT )

print(unique(df_cor$YABSENT ))
print(unique(df$YABSENT ))
is.factor(fd0$YABSENT)
fd0$YABSENT %>% mutate_if(is.factor, recode_factor, .missing ="Missing")

print(unique(df$UNION))

print(unique(df$YABSENT ))
str(df)

df<-df[,-8]
df<-df[,-17]
df<-df[,-39]

ggplot(data = df%>% 
        filter(!is.na(UNION)), mapping = aes(x = HRLYEARN))+
 geom_histogram()+scale_y_continuous(trans = 'log10')+labs(title="Trend of Hourly Salary Histogram",x ="Hourly Salary", y = "Count")+theme_economist()+theme(text = element_text(size = 10),legend.title = element_text(size = 11),axis.title.x = element_text(size = 11),axis.title.y = element_text(size = 11))

ggplot(data = df%>% 
        filter(!is.na(UNION)), mapping = aes(x = HRLYEARN))+
 geom_histogram()+labs(title="Trend of Hourly Salary Histogram",x ="Hourly Salary", y = "Count")+theme_economist()+theme(text = element_text(size = 10),legend.title = element_text(size = 11),axis.title.x = element_text(size = 11),axis.title.y = element_text(size = 11))

ggplot(data = df%>% 
        filter(!is.na(UNION)), mapping = aes(x = HRLYEARN, y = after_stat(density), fill="Survey Month"))+
 geom_freqpoly(mapping = aes(colour = SURVMNTH), binwidth = 1)+ xlim(15, 55)+labs(title="Trend of Hourly Salary by Survey and Union",x ="Hourly Salary", y = "Density", color="Survey Month")+theme_economist()+theme(text = element_text(size = 10),legend.title = element_text(size = 11),axis.title.x = element_text(size = 11),axis.title.y = element_text(size = 11))+
 facet_wrap( . ~ UNION,ncol=1)

ggplot(data = df%>% 
        filter(!is.na(UNION)), mapping = aes(x = HRLYEARN))+
 geom_boxplot(mapping = aes(colour = SURVMNTH),)+ xlim(15, 55)+labs(title="Trend of Hourly Salary by Survey and Union",x ="Hourly Salary", y = "Density", color="Survey Month")+theme_economist()+theme(text = element_text(size = 10),legend.title = element_text(size = 11),axis.title.x = element_text(size = 11),axis.title.y = element_text(size = 11))+
 facet_wrap( . ~ UNION,ncol=1)

ggplot(data = df%>% 
        filter(!is.na(UNION)), mapping = aes(x = HRLYEARN, fill="Survey Month"))+
 geom_freqpoly(mapping = aes(colour = SURVMNTH), binwidth = 1)+ xlim(15, 55)+labs(title="Trend of Hourly Salary by Survey and Union",x ="Hourly Salary", y = "Counts", color="Survey Month")+theme_economist()+theme(text = element_text(size = 10),legend.title = element_text(size = 11),axis.title.x = element_text(size = 11),axis.title.y = element_text(size = 11))+
 facet_wrap( . ~ UNION,ncol=1)

ggplot(data = df%>% filter(COWMAIN %in% Sector)%>% filter(UNION %in% Union_non_Union_Only), mapping = aes(x = HRLYEARN, fill="Survey Month"))+
 geom_freqpoly(mapping = aes(colour = SURVMNTH), binwidth = 1)+ xlim(15, 55)+labs(title="Trend of Hourly Salary by Survey and Union",x ="Hourly Salary", y = "Counts", color="Survey Month")+theme_economist()+theme(text = element_text(size = 10),legend.title = element_text(size = 11),axis.title.x = element_text(size = 11),axis.title.y = element_text(size = 11))+
 facet_wrap( . ~ UNION,ncol=1)

# Normal Wage##
ggplot(data = df%>% filter(COWMAIN %in% Sector)%>% filter(UNION %in% Union_non_Union_Only), mapping = aes(x = HRLYEARN, fill="Survey Month"))+
 geom_freqpoly(mapping = aes(colour = SURVMNTH), binwidth = 1)+ xlim(15, 55)+labs(title="Trend of Hourly Salary by Survey and Union",x ="Hourly Salary", y = "Counts", color="Survey Month")+theme_economist()+theme(text = element_text(size = 10),legend.title = element_text(size = 11),axis.title.x = element_text(size = 11),axis.title.y = element_text(size = 11))+
 facet_wrap( . ~ UNION+COWMAIN,ncol=1)

ggplot(data = df%>% filter(COWMAIN %in% Sector)%>% filter(UNION %in% Union_non_Union_Only), mapping = aes(x = HRLYEARN,y = after_stat(density), fill="Survey Month"))+
 geom_freqpoly(mapping = aes(colour = SURVMNTH), binwidth = 1)+ xlim(15, 55)+labs(title="Trend of Hourly Salary by Survey and Union",x ="Hourly Salary", y = "Density", color="Survey Month")+theme_economist()+theme(text = element_text(size = 10),legend.title = element_text(size = 11),axis.title.x = element_text(size = 11),axis.title.y = element_text(size = 11))+
 facet_wrap( . ~ UNION+COWMAIN,ncol=1)

### log wage##

ggplot(data = df%>% filter(COWMAIN %in% Sector)%>% filter(UNION %in% Union_non_Union_Only), mapping = aes(x = HRLYEARN,y = after_stat(density), fill="Survey Month"))+
 geom_freqpoly(mapping = aes(colour = SURVMNTH), binwidth = 1)+scale_y_continuous(trans = 'log10')+ xlim(15, 55)+labs(title="Trend of Hourly Salary by Survey and Union",x ="Hourly Salary", y = "Density", color="Survey Month")+theme_economist()+theme(text = element_text(size = 10),legend.title = element_text(size = 11),axis.title.x = element_text(size = 11),axis.title.y = element_text(size = 11))+
 facet_wrap( . ~ UNION+COWMAIN,ncol=1)

ggplot(data = df%>% filter(COWMAIN %in% Sector)%>% filter(UNION %in% Union_non_Union_Only), mapping = aes(x = HRLYEARN), fill="Survey Month")+
 geom_boxplot(mapping = aes(colour = SURVMNTH))+ xlim(15, 55)+labs(title="Distribution of Hourly Salary by Survey and Union",x ="Hourly Salary", y = NULL, color="Survey Month")+theme_economist()+theme(text = element_text(size = 10),legend.title = element_text(size = 11),axis.title.x = element_text(size = 11),axis.title.y=element_blank(),
                                                                                                                                                                                                         axis.text.y=element_blank(),
                                                                                                                                                                                                         axis.ticks.y=element_blank())+
 facet_wrap( . ~ UNION+COWMAIN,ncol=1)

## Creating grafts for BC###

ggplot(data = df2%>% filter(COWMAIN %in% Sector)%>% filter(UNION %in% Union_non_Union_Only), mapping = aes(x = HRLYEARN,y = after_stat(density), fill="Survey Month"))+
 geom_freqpoly(mapping = aes(colour = SURVMNTH), binwidth = 1)+ xlim(15, 55)+labs(title="Trend of Hourly Salary by Survey and Union",x ="Hourly Salary", y = "Density", color="Survey Month")+theme_economist()+theme(text = element_text(size = 10),legend.title = element_text(size = 11),axis.title.x = element_text(size = 11),axis.title.y = element_text(size = 11))+
 facet_wrap( . ~ UNION+COWMAIN,ncol=1)

ggplot(data = df2%>% filter(COWMAIN %in% Sector)%>% filter(UNION %in% Union_non_Union_Only), mapping = aes(x = HRLYEARN), fill="Survey Month")+
 geom_boxplot(mapping = aes(colour = SURVMNTH))+ xlim(15, 55)+labs(title="Distribution of Hourly Salary by Survey and Union",x ="Hourly Salary", y = NULL, color="Survey Month")+theme_economist()+theme(text = element_text(size = 10),legend.title = element_text(size = 11),axis.title.x = element_text(size = 11),axis.title.y=element_blank(),
                                                                                                                                                                                                  axis.text.y=element_blank(),
                                                                                                                                                                                                  axis.ticks.y=element_blank())+
 facet_wrap( . ~ UNION+COWMAIN,ncol=1)

set.seed(123)
data_split2 <- initial_split(df2, prop = 0.75)
train_data2 <- training(data_split2)
test_data2 <- testing(data_split2)

# Create a decision tree model specification
tree_spec <- decision_tree() %>%
 set_engine("rpart") %>%
 set_mode("regression")

# Fit the model to the training data
tree_fit <- tree_spec %>%
 fit(HRLYEARN ~ ., data = train_data)

# Make predictions on the testing data
predictions <- tree_fit %>%
 predict(test_data) %>%
 pull(.pred)

# Calculate RMSE and R-squared
metrics <- metric_set(rmse, rsq)
model_performance <- test_data %>%
 mutate(predictions = predictions) %>%
 metrics(truth = HRLYEARN, estimate = predictions)

print(model_performance)

rpart.plot(tree_fit$fit, type = 4, extra = 101, under = TRUE, cex = 0.8, box.palette = "auto")

rules <- rpart.rules(tree_fit$fit)
print(rules)

# Load the necessary library
library(vip)

# Create a variable importance plot
var_importance <- vip::vip(tree_fit, num_features = 10)
print(var_importance)

fitO<- HRLYEARN ~ AGE_12+SEX+EDUC+MARSTAT+COWMAIN+IMMIG+PERMTEMP+UNION+FIRMSIZE+TENURE+UHRSMAIN
fit1<- log(HRLYEARN) ~ AGE_12+SEX+EDUC+MARSTAT+COWMAIN+IMMIG+PERMTEMP+UNION+FIRMSIZE+TENURE+UHRSMAIN

glm1<-glm(fitO,data = train_data2)

glm1og<-glm(fitO,data = train_data2,family = quasipoisson)

glm1og1<-glm(fit1,data = train_data2)
plmmode<-plm(fit1,data = train_data2, index = c('SURVMNTH','NOC_43'), model = "within", 
             effect = "twoways")

summary.glm(glm1)
summary.glm(glm1og)
summary.glm(glm1og1)
summary(plmmode)
bptest(glm1og1)## residue has skedasticity 
bptest(plmmode) ## residue has skedastucity
shapiro.test(resid(plmmode)) ## residue not normal
dwtest(fit1,data =train_data2 )
durbinWatsonTest(resid(glm1og1)) ## no autocorrelation exists
pdwtest(plmmode) ## no autocorrelation exists
durbinWatsonTest(plmmode) 
vcovHC(plmmode,type = "HC3")

AIC(glm1,glm1og1,best_model)
AIC_adj <- function(mod){
 # Number of observations
 n.N   <- nrow(mod$model)
 # Residuals vector
 u.hat <- residuals(mod)
 # Variance estimation
 s.sq  <- log( (sum(u.hat^2)/(n.N)))
 # Number of parameters (incl. constant) + one additional for variance estimation
 p     <-  length(coef(mod)) + 1
 
 # Note: minus sign cancels in log likelihood
 aic <- 2*p  +  n.N * (  log(2*pi) + s.sq  + 1 ) 
 
 
 return(aic)
}

AIC_adj(plmmode)
AIC_adj(best_model)
data_split <- initial_split(df3, prop = 0.75)
train_data <- training(data_split)
test_data <- testing(data_split)

glmnet()
xx=addNA( cbind(train_data$AGE_12,train_data$SEX,train_data$EDUC,train_data$MARSTAT,train_data$COWMAIN,train_data$IMMIG,train_data$PERMTEMP,train_data$UNION,train_data$FIRMSIZE,train_data$TENURE,train_data$UHRSMAIN))
xx= cbind(train_data$AGE_12,train_data$SEX,train_data$EDUC,train_data$MARSTAT,train_data$COWMAIN,train_data$IMMIG,train_data$PERMTEMP,train_data$UNION,train_data$FIRMSIZE,train_data$TENURE,train_data$UHRSMAIN)
xx= as.data.frame(cbind(train_data$AGE_12,train_data$SEX,train_data$EDUC,train_data$MARSTAT,train_data$COWMAIN,train_data$IMMIG,train_data$PERMTEMP,train_data$UNION,train_data$FIRMSIZE,train_data$TENURE,train_data$UHRSMAIN))
xxx<-makeX(train_data, na.impute = TRUE)
cv.glmnet(xxx,train_data$HRLYEARN,alpha=1)
cv.glmnet(xxx,train_data$HRLYEARN,alpha=0)

Lasso<-cv.glmnet(xxx,log(train_data$HRLYEARN),alpha=1 ,nfolds = 5)
Ridge<-cv.glmnet(xxx,log(train_data$HRLYEARN),alpha=0,nfolds = 5)
cv.glmnet(xxx,log(train_data$HRLYEARN),alpha=0.8,nfolds = 5)
cv.glmnet(xxx,log(train_data$HRLYEARN),alpha=0.5,nfolds = 5)
coef.glmnet(Lasso)
coef(Lasso,Lasso$lambda.1se)
plot.cv(Lasso,Lasso$lambda.1se)
plot.cv
plot(Lasso)
summary(Lasso)
predict(Lasso$finalModel, type = 'coefficients')

best_lambda <- Lasso$lambda.min
best_lambda

best_model <- glmnet(xxx,log(train_data$HRLYEARN), alpha = 1, lambda = best_lambda)
coef(best_model)
AIC_BIC(best_model)
## calculating AIC and BIC for Elasticity net estimation
AIC_BIC<-function(fit) {

tLL <- fit$nulldev - deviance(fit)
k <- fit$df
n <- fit$nobs
AICc <- -tLL+2*k+2*k*(k+1)/(n-k-1)
BIC<-log(n)*k - tLL
return(c(AICc,BIC))
}

library(stargazer)

stargazer(glm1,glm1og1,plmmode,type = 'html', out = 'results.html')
stargazer(plmmode,type = 'html', out = 'results.html')

tbl_regression(plmmode, exponentiate = FALSE)

# creating formula for Gam
formula_gam<- log(HRLYEARN)~ s(UHRSMAIN,bs='bs') + s(PAIDOT) + s(UNPAIDOT )+ s(TENURE) + SURVMNTH_January + SURVMNTH_February + SURVMNTH_March + SURVMNTH_April + SURVMNTH_May + SURVMNTH_June + SURVMNTH_July + SURVMNTH_August + SURVMNTH_September + SURVMNTH_October + SURVMNTH_November + CMA_Vancouver + `AGE_12_20 to 24 years` + `AGE_12_25 to 29 years` + `AGE_12_30 to 34 years` + `AGE_12_35 to 39 years` + `AGE_12_40 to 44 years` + `AGE_12_45 to 49 years` + `AGE_12_50 to 54 years` + `AGE_12_55 to 59 years` + `AGE_12_60 to 64 years` + `AGE_12_65 to 69 years` + `AGE_12_70 and over` + SEX_Female + MARSTAT_Married + `MARSTAT_Living in common-law` + MARSTAT_Widowed + MARSTAT_Separated + MARSTAT_Divorced 
+ `EDUC_Some high school` + `EDUC_High school graduate` + `EDUC_Some postsecondary` + `EDUC_Postsecondary certificate or diploma` + `EDUC_Bachelor's degree` + `EDUC_Above bachelor's degree` + `COWMAIN_Public sector employees` + `COWMAIN_Private sector employees` + `COWMAIN_Self-employed incorporated, with paid help` + `COWMAIN_Self-employed incorporated, no paid help` + `COWMAIN_Self-employed unincorporated, with paid help` + `IMMIG_Immigrant, landed 10 or less years earlier` + `IMMIG_Immigrant, landed more than 10 years earlier` + `NOC_43_Legislative and senior management occupations` + `NOC_43_Specialized middle management occupations` + `NOC_43_Middle management occupations in retail and wholesale trade and customer services` + `NOC_43_Middle management occupations in trades, transportation, production and utilities` + `NOC_43_Professional occupations in finance` + `NOC_43_Professional occupations in business` + `NOC_43_Administrative and financial supervisors and administrative occupations` + `NOC_43_Administrative occupations and transportation logistics occupations` + `NOC_43_Administrative and financial support and supply chain logistics occupations` + `NOC_43_Professional occupations in natural sciences` + `NOC_43_Professional occupations in applied sciences (except engineering)` + `NOC_43_Professional occupations in engineering` + `NOC_43_Technical occupations related to natural and applied sciences` + `NOC_43_Health treating and consultation services professionals` + `NOC_43_Therapy and assessment professionals` + `NOC_43_Nursing and allied health professionals` + `NOC_43_Technical occupations in health` + `NOC_43_Assisting occupations in support of health services` + `NOC_43_Professional occupations in law` + `NOC_43_Professional occupations in education services` + `NOC_43_Professional occupations in social and community services` + `NOC_43_Professional occupations in government services` + `NOC_43_Occupations in front-line public protection services` + `NOC_43_Paraprofessional occupations in legal, social, community and education services` + `NOC_43_Assisting occupations in education and in legal and public protection` + `NOC_43_Care providers and public protection support occupations and student monitors, crossing guards and related occupations` + `NOC_43_Professional occupations in art and culture` + `NOC_43_Technical occupations in art, culture and sport` + `NOC_43_Occupations in art, culture and sport` + `NOC_43_Support occupations in art, culture and sport` + `NOC_43_Retail sales and service supervisors and specialized occupations in sales and services` + `NOC_43_Occupations in sales and services` + `NOC_43_Sales and service representatives and other customer and personal services occupations` + `NOC_43_Sales and service support occupations` + `NOC_43_Technical trades and transportation officers and controllers` + `NOC_43_General trades` + `NOC_43_Mail and message distribution, other transport equipment operators and related maintenance workers` + `NOC_43_Helpers and labourers and other transport drivers, operators and labourers` + 
 `NOC_43_Supervisors and occupations in natural resources, agriculture and related production` + `NOC_43_Workers and labourers in natural resources, agriculture and related production` + `NOC_43_Supervisors, central control and process operators in processing, manufacturing and utilities and aircraft assemblers an` + `NOC_43_Machine operators, assemblers and inspectors in processing, manufacturing and printing` + `NOC_43_Labourers in processing, manufacturing and utilities` + `UNION_Union member` + `UNION_Non-unionized` + PERMTEMP_Permanent + `PERMTEMP_Temporary, seasonal job` + `PERMTEMP_Temporary, term or contract job` + 
 `FIRMSIZE_20 to 99 employees` + `FIRMSIZE_100 to 500 employees` + `FIRMSIZE_More than 500 employees`

formula_gam<- log(HRLYEARN)~ s(UHRSMAIN,bs='bs') + s(PAIDOT) + s(UNPAIDOT )+ s(TENURE) + SURVMNTH_January + SURVMNTH_February + SURVMNTH_March + SURVMNTH_April + SURVMNTH_May + SURVMNTH_June + SURVMNTH_July + SURVMNTH_August + SURVMNTH_September + SURVMNTH_October + SURVMNTH_November + CMA_Vancouver + AGE_12_20 to 24 years + `AGE_12_25 to 29 years` + `AGE_12_30 to 34 years` + `AGE_12_35 to 39 years` + `AGE_12_40 to 44 years` + `AGE_12_45 to 49 years` + `AGE_12_50 to 54 years` + `AGE_12_55 to 59 years` + `AGE_12_60 to 64 years` + `AGE_12_65 to 69 years` + `AGE_12_70 and over` + SEX_Female + MARSTAT_Married + `MARSTAT_Living in common-law` + MARSTAT_Widowed + MARSTAT_Separated + MARSTAT_Divorced 
+ `EDUC_Some high school` + `EDUC_High school graduate` + `EDUC_Some postsecondary` + `EDUC_Postsecondary certificate or diploma` + `EDUC_Bachelor's degree` + `EDUC_Above bachelor's degree` + `COWMAIN_Public sector employees` + `COWMAIN_Private sector employees` + `COWMAIN_Self-employed incorporated, with paid help` + `COWMAIN_Self-employed incorporated, no paid help` + `COWMAIN_Self-employed unincorporated, with paid help` + `IMMIG_Immigrant, landed 10 or less years earlier` + `IMMIG_Immigrant, landed more than 10 years earlier` + `NOC_43_Legislative and senior management occupations` + `NOC_43_Specialized middle management occupations` + `NOC_43_Middle management occupations in retail and wholesale trade and customer services` + `NOC_43_Middle management occupations in trades, transportation, production and utilities` + `NOC_43_Professional occupations in finance` + `NOC_43_Professional occupations in business` + `NOC_43_Administrative and financial supervisors and administrative occupations` + `NOC_43_Administrative occupations and transportation logistics occupations` + `NOC_43_Administrative and financial support and supply chain logistics occupations` + `NOC_43_Professional occupations in natural sciences` + `NOC_43_Professional occupations in applied sciences (except engineering)` + `NOC_43_Professional occupations in engineering` + `NOC_43_Technical occupations related to natural and applied sciences` + `NOC_43_Health treating and consultation services professionals` + `NOC_43_Therapy and assessment professionals` + `NOC_43_Nursing and allied health professionals` + `NOC_43_Technical occupations in health` + `NOC_43_Assisting occupations in support of health services` + `NOC_43_Professional occupations in law` + `NOC_43_Professional occupations in education services` + `NOC_43_Professional occupations in social and community services` + `NOC_43_Professional occupations in government services` + `NOC_43_Occupations in front-line public protection services` + `NOC_43_Paraprofessional occupations in legal, social, community and education services` + `NOC_43_Assisting occupations in education and in legal and public protection` + `NOC_43_Care providers and public protection support occupations and student monitors, crossing guards and related occupations` + `NOC_43_Professional occupations in art and culture` + `NOC_43_Technical occupations in art, culture and sport` + `NOC_43_Occupations in art, culture and sport` + `NOC_43_Support occupations in art, culture and sport` + `NOC_43_Retail sales and service supervisors and specialized occupations in sales and services` + `NOC_43_Occupations in sales and services` + `NOC_43_Sales and service representatives and other customer and personal services occupations` + `NOC_43_Sales and service support occupations` + `NOC_43_Technical trades and transportation officers and controllers` + `NOC_43_General trades` + `NOC_43_Mail and message distribution, other transport equipment operators and related maintenance workers` + `NOC_43_Helpers and labourers and other transport drivers, operators and labourers` + 
 `NOC_43_Supervisors and occupations in natural resources, agriculture and related production` + `NOC_43_Workers and labourers in natural resources, agriculture and related production` + `NOC_43_Supervisors, central control and process operators in processing, manufacturing and utilities and aircraft assemblers an` + `NOC_43_Machine operators, assemblers and inspectors in processing, manufacturing and printing` + `NOC_43_Labourers in processing, manufacturing and utilities` + `UNION_Union member` + `UNION_Non-unionized` + PERMTEMP_Permanent + `PERMTEMP_Temporary, seasonal job` + `PERMTEMP_Temporary, term or contract job` + 
 `FIRMSIZE_20 to 99 employees` + `FIRMSIZE_100 to 500 employees` + `FIRMSIZE_More than 500 employees`

print(formula_gam)
train_data$`AGE_12_15 to 19 years``


gam(formula_gam, data = train_data )
gam(log(HRLYEARN)~s(UHRSMAIN,bs='bs') + s(PAIDOT) + s(UNPAIDOT )+ s(TENURE)+. , data = train_data )
gam(log(HRLYEARN)~.,data=train_data2)
dwtest(fit1,data =train_data2 )
write.csv(train_data2,"/Users/wutau/Desktop/R learning/BC G R27/Documentation/trains2.csv")

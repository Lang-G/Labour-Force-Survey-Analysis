install.packages("tidyverse")

install.packages("vip")               # Install ggthemes package

library("foreign")
library("ggthemes")
library(dplyr)
library('tidyverse')
library(tidymodels)
library(rpart.plot)
install.packages("randomForest") 

install.packages("GGally")             # Install GGally package
library("GGally")
library("randomForest")

getwd()
setwd("C:/Users/wutau/Desktop/R learning/BC G R27/Data")
read.spss('./Data/LFS_April_2023.sav')
df<-list.files(path='./') %>% 
 lapply(read.spss) %>% 
 bind_rows
summary(df)
sample_n(df, 5)
colSums(is.na(df))
ggpairs(dfNav4)

dfNav4<-df %>% filter_at(vars(NOC_43),any_vars(. %in% c('Professional occupations in social and community services')))

dfNAIC21<-df %>% filter_at(vars(NAICS_21),any_vars(. %in% c('Health care and social assistance')))
summary(df[UNION,])
union
hrlyearn

ggplot(data=dfNAIC21) + geom_bar(mapping=aes(x=UNION))
ggplot(data=dfNAIC21) +  geom_histogram(mapping=aes(x=HRLYEARN), binwidth = 0.5)

dfS <- dfNAIC21 %>% 
 filter(HRLYEARN <= 100)

dfSS <- dfNAIC21 %>% 
 filter(HRLYEARN <= 75)

dfSSS <- dfNAIC21 %>% 
 filter(HRLYEARN <= 55)%>% filter(HRLYEARN >= 15)

ggplot(data = dfNAIC21, mapping = aes(x = HRLYEARN, colour = UNION)) +
 geom_freqpoly(binwidth = 0.1)
ggplot(data = dfSSS, mapping = aes(y = HRLYEARN, colour = UNION)) +
 geom_freqpoly(binwidth = 0.2)

ggplot(data = dfSSS, mapping = aes(y = HRLYEARN, x = UNION)) +geom_point()

ggplot(data = dfNAIC21, mapping = aes(x = HRLYEARN, y = ..density..)) + 
 geom_freqpoly(mapping = aes(colour = UNION), binwidth = 3)+ xlim(15, 55)

ggplot(data = dfNAIC21, mapping = aes(x = HRLYEARN, y = after_stat(density), fill="Survey Month"))  + 
geom_freqpoly(mapping = aes(colour = SURVMNTH), binwidth = 3)+ xlim(18, 40)+labs(title="Trend of Hourly Salary by Survey",x ="Hourly Salary", y = "Density", color="Survey Month")+theme_economist()+geom_point(data=dfNAIC21 %>% filter(HRLYEARN > 20),pch=21,size=4,colour="purple")
                                                                                                                                                                       

ggplot(data = dfSSS, mapping = aes(x = HRLYEARN, y = ..density..)) + 
 geom_freqpoly(mapping = aes(colour = SURVMNTH), binwidth = 6) 


ggplot(data = dfNAIC21, mapping = aes(x = HRLYEARN, y = ..density.., fill="Survey Month"))+
geom_freqpoly(mapping = aes(colour = SURVMNTH), binwidth = 3)+ xlim(15, 55)+labs(title="Trend of Hourly Salary by Survey",x ="Hourly Salary", y = "Density", color="Survey Month")+theme_economist_white(base_size = 10)+scale_colour_economist()+theme(text = element_text(size = 10),legend.title = element_text(size = 11),axis.title.x = element_text(size = 11),axis.title.y = element_text(size = 11))


ggplot(data = dfNAIC21%>% 
        filter(!is.na(UNION)), mapping = aes(x = HRLYEARN, y = ..density.., fill="Survey Month"))+
 geom_freqpoly(mapping = aes(colour = SURVMNTH), binwidth = 1)+ xlim(15, 55)+labs(title="Trend of Hourly Salary by Survey",x ="Hourly Salary", y = "Density", color="Survey Month")+theme_economist()+theme(text = element_text(size = 10),legend.title = element_text(size = 11),axis.title.x = element_text(size = 11),axis.title.y = element_text(size = 11))+
 facet_wrap( . ~ UNION,ncol=1)

Sector<-c('Public sector employees', 'Private sector employees')

ggplot(data = dfNAIC21%>% filter(!is.na(HRLYEARN))%>% filter(PROV=='British Columbia')%>%
        filter(!is.na(UNION)) %>% filter(COWMAIN %in% Sector) , mapping = aes(x = HRLYEARN, y = ..density.., fill="Survey Month"))+
 geom_freqpoly(mapping = aes(colour = SURVMNTH), binwidth = 1)+ xlim(15, 55)+labs(title="Trend of Hourly Salary by Survey",x ="Hourly Salary", y = "Density", color="Survey Month")+theme_economist()+theme(text = element_text(size = 10),legend.title = element_text(size = 11),axis.title.x = element_text(size = 11),axis.title.y = element_text(size = 11))+
 facet_wrap( . ~ UNION+COWMAIN,ncol=1)


cowmain
COWMAIN

ggplot(data = dfNAIC21%>% 
        filter(UNION %in% c('Union member','Non-unionized')) %>% filter(COWMAIN %in% Sector) , mapping = aes(x = HRLYEARN, y = ..density.., fill="Survey Month"))+
 geom_freqpoly(mapping = aes(colour = SURVMNTH), binwidth = 1)+ xlim(15, 55)+labs(title="Trend of Hourly Salary by Survey",x ="Hourly Salary", y = "Density", color="Survey Month")+theme_economist()+theme(text = element_text(size = 10),legend.title = element_text(size = 11),axis.title.x = element_text(size = 11),axis.title.y = element_text(size = 11))+
 facet_wrap( . ~ UNION+COWMAIN,ncol=1)
LIST.S<-dfNAIC21[['NOC_43']]
LIST.S<- as.data.frame(LIST.S)
LIST.S%>% distinct(LIST.S,.keep_all=TRUE)

ggplot(data = dfNAIC21%>% filter(!is.na(HRLYEARN))%>% filter(PROV=='British Columbia')%>%
        filter(UNION %in% c('Union member','Non-unionized')) %>% filter(COWMAIN %in% Sector) , mapping = aes(x = HRLYEARN,  fill="Survey Month"))+
 geom_freqpoly(mapping = aes(colour = SURVMNTH), binwidth = 1)+ xlim(15, 55)+labs(title="Trend of Hourly Salary by Survey",x ="Hourly Salary", y = "Count", color="Survey Month")+theme_economist()+theme(text = element_text(size = 10),legend.title = element_text(size = 11),axis.title.x = element_text(size = 11),axis.title.y = element_text(size = 11))+
 facet_wrap( . ~ UNION+COWMAIN,ncol=1) ### Absolute number of absence##
library('stats')
test1<-glm(HRLYEARN~., data =dfNAIC21 )
df2<-dfNAIC21%>% filter(!is.na(HRLYEARN))%>% filter(PROV=='British Columbia')%>%
 filter(UNION %in% c('Union member','Non-unionized')) %>% filter(COWMAIN %in% Sector)%>% as_tibble()
df2<-df2[,-8]
df2<-df2[,-38]
df2<-df2[,-17]
df2[is.na(df2)]='0'
df2<-addNA(df2,ifany = FALSE)

df2$AGE_6<-addNA(df2$AGE_6,ifany = FALSE)

df2$AGE_12<-addNA(df2$AGE_12,ifany = FALSE)
classifier_RF = randomForest(x = df2$AGE_12,
                             y = df2$HRLYEARN,
                             ntree = 100,na.action=na.roughfix)

candidatesnodata.index <- c()
for (j in (1 : ncol(dfNAIC21)))   {
 
 if (    is.numeric(dfNAIC21[ ,j])  &  length(unique(as.numeric(dfNAIC21[ ,j]))) == 1      )
 {candidatesnodata.index <- append(candidatesnodata.index,j)}
}

?rfImpute

summary(df2$AGE_6)

df2<-df2 %>% mutate(AGE_13=as.factor(AGE_13),AGE_13 = ifelse(AGE_12 %in% c('15 to 19 years','20 to 24 years','25 to 29 years') ,AGE_6, AGE_12))
Summary(df2$AGE_12)

max(df2$AGE_13)

set.seed(123)
data_split <- initial_split(df2, prop = 0.75)
train_data <- training(data_split)
test_data <- testing(data_split)

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

Summary(df2$LFSSTAT)

print(unique(df2$LFSSTAT))

Sys.getlocale() 
# Data
inputData<-read.csv("G:\\My Drive\\2. Course Teaching\\SEM I20212022\\BSD3443 Statistical Modelling\\Slide\\Sharing\\WCGS_data.txt")


summary(inputData) #Descriptive Statistics
#recode the non-numeric data
status<-recode(inputData[,11], yes ="1", no="0")
behave<-recode(inputData[,8], B1 ="0",B2 ="0", B3 ="0", B4 ="0", A1="1",A2="1",A3="1",A4="1")
#combine recode data and rearrange column based on numeric and non-numeric
ncigs<-as.numeric(inputData[,9])
time<-as.numeric(inputData[,13])
status<-as.numeric(status)
behave<-as.numeric(behave)
type<-inputData[,12]
inputData<-cbind(inputData[,2:7],ncigs,status,behave,time, type)
summary(inputData) #Descriptive Statistics

#to check data
apply(inputData[,1:10], 2, FUN=summary)

pairs(inputData[,1:10])  # traditional pairs plot (only for column 2 to 7)-can rearrange the column of the data
round(cor(inputData[,1:8]),4)
boxplot(inputData[,1:10])  # always a good idea to check for gross outliers
boxplot(inputData[,1:9])  # always a good idea to check for gross outliers



#to cound 0 and 1 chd
table(inputData$status)
round(mean(inputData$status)*100, 1)

#Primary exposure of interest is 'behave'
table(inputData$behave)
round(mean(inputData$behave)*100, 1)

#Cross-tabulation and exposure-specific incidence
table(inputData$behave, inputData$status)
#to get mean for each status 
round(tapply(inputData$status, list(inputData$behave), FUN=mean) * 100, 1)


#Once we check the data, we could clean/decide with too strange values, missing data and etc. Foexample;
inputData$status[inputData$status > 500] 
inputData<- na.omit(inputData) 
#to check data again
apply(inputData[,1:10], 2, FUN=summary)

#Generalised Linear Model (GLM) for behave only
Model0<- glm(status ~ 1, data = inputData, family = "binomial")  # Null model
Model1<-glm(status ~ behave, data = inputData, family = "binomial")  # behave variable
summary(Model0)
summary(Model1)

#GLM for more variable consider
Model2<-glm(status ~ behave + age + weight + sbp + chol + ncigs, data = inputData, family = "binomial")  # behave variable
summary(Model2)

#GLM for more variable consider (full Model)
Model3<-glm(status ~ behave + age + weight + sbp + chol + ncigs + height + dbp + time, data = inputData, family = "binomial")  # behave variable
summary(Model3)

library(RODBC) #connect to sql server
library(DBI)
library(caret)
# Modeling packages
library(caret)    # for classification and regression training
library(kernlab)  # for fitting SVMs
library(lubridate )#to calculate the age
library(mice) #for imputation
library(randomForest)
library(plyr)
library(epiR)
library(JOUSBoost)


library(dplyr)

library(adabag)



channel <- odbcDriverConnect("DRIVER=SQL Server; SERVER=ZEUS")

df<-sqlQuery(channel,"
select * from alm.vinna.WorkingTable", stringsAsFactors=FALSE ) # see make_table.sql 

df<-sqlQuery(channel,"
select * from alm.vinna.WorkingTable3", stringsAsFactors=FALSE ) # see make_table.sql 



Pop_err<-function(mat){return(
  abs(mat[1,2]-mat[2,1])/sum(mat)
)
}

accuracy<-function(mat){return(
  abs(mat[1,1]+mat[2,2])/sum(mat)
)
}



sensitivity<-function(mat){return(
mat[1,1]/(mat[1,1]+mat[2,1])
)
}

specificity<-function(mat){return(
mat[2,2]/(mat[1,2]+mat[2,2])
)
}

dftot<-df[,-which(names(df) %in% c('carOwner','ID'))]


df1<-df[which(df$citizenship==1),]
df0<-df[which(df$citizenship==0),]
df1<-df1[,-which(names(df) %in% c('carOwner','ID','income_increase_max','citizenship','children'))]
df0<-df0[,-which(names(df) %in% c('carOwner','ID','income_increase_2yr','citizenship','student_abroad','children'))]




# pick the dataframe #df1 for icelanders, df0 for foreigners, dftot for everybody
ddf<-df1

#ddf$age<-ifelse(ddf$age<=19 | ddf$age>=35,0,1)

ddf$time_in_iceland <- ifelse(ddf$citizenship==1,1,ddf$time_in_iceland/df$age)

#consider removing children, too much correlation with married, skoli and homeowner
names(ddf)
cols<-c('Presence','citizenship','ever_abroad','married','children','homeOwner','months_worked_increase','sex','region') #factors
ddf[cols] <- lapply(ddf[cols], factor)  ## as.factor() could also be used


#plot the correlations between variables
DataExplorer::plot_correlation(
  data=ddf, 
  type = "all",
  maxcat = 50L, cor_args = list(),
  title = NULL,
  theme_config = list(legend.position ="bottom"))



#examples of plots to see the data dependence
#histogram with presence 
ggplot(ddf, aes(x=Presence)) + stat_count(width = 0.5,color="black", fill="orange")+
  scale_x_discrete(name ="Presence", 
                   limits=c(0,1))+ scale_y_continuous(name='Number',breaks=c(586,17124))

#histogram with homeOwner counts
ggplot(ddf, aes(x=homeOwner, fill=as.factor(Presence))) + stat_count(binwidth=0.2)+
  scale_fill_discrete(name="Presence", breaks=c('0','1'), labels=c("Out", "In"))+
  scale_x_discrete(name ="Home owner", 
                   limits=c(0,1))


ggplot(ddf, aes(x=income_increase_2yr/max(income_increase_2yr), colour=as.factor(Presence)) ) + 
  geom_density(alpha=0.1)  +
  labs(x= "income increase") +
  theme(legend.position = "bottom") +
  scale_colour_discrete(name="Presence", breaks=c('0','1'), labels=c("Out", "In"))+
  xlim(-.05, .15)


ddf$income
ggplot(ddf, aes(x=income/max(income), colour=as.factor(Presence)) ) + 
  geom_density(alpha=0.1)  +
  labs(x= "income") +
  theme(legend.position = "bottom") +
  scale_colour_discrete(name="Presence", breaks=c('0','1'), labels=c("Out", "In"))+
  xlim(0, 0.15)

ggplot(ddf, aes(x=age, colour=as.factor(Presence)) ) + 
  geom_density(alpha=0.1)  +
  labs(x= "age") +
  theme(legend.position = "bottom") +
  scale_colour_discrete(name="Presence", breaks=c('0','1'), labels=c("Out", "In"))



ggplot2::ggplot(ddf, aes(x=ever_abroad, fill=as.factor(Presence)) ) + 
  geom_bar(alpha=1)  +
  labs(x= "ever lived abroad") +
  theme(legend.position = "bottom") +
  scale_fill_discrete(name="Presence", breaks=c(0,1), labels=c("Out", "In"))+scale_x_discrete(name ="Ever lived abroad", 
                                                                                              limits=c(0,1))



ggplot2::ggplot(ddf[ddf$citizenship==0,], aes(x=time_in_iceland, colour=as.factor(Presence)) ) + 
  geom_density(alpha=0.1)  +
  labs(x= "time in iceland (foreign citizens)") +
  theme(legend.position = "bottom") +
  scale_colour_discrete(name="Presence", breaks=c(0,1), labels=c("Out", "In"))


ggplot2::ggplot(ddf, aes(x=age, colour=as.factor(Presence)) ) + 
  geom_density(alpha=0.1)  +
  labs(x= "age") +
  theme(legend.position = "bottom") +
  scale_colour_discrete(name="Presence", breaks=c(0,1), labels=c("Out", "In")
  )

colnames(ddf)
#########################################Random forest######################################### 

set.seed(565)
index0 <- sample(1:nrow(ddf),round(0.7*nrow(ddf)))
training<- ddf[index0,]
training$Presence<-as.factor(training$Presence)
testing <- ddf[-index0,]
testing$Presence<-as.factor(testing$Presence)
length(training[,1])



#----------------------------------find the optimal cutoff

#https://tidypredict.tidymodels.org/ convert to sql models


sensitivity_vector<-c()
specificity_vector<-c()
accuracy_vector<-c()
pop_error<-c()

minimizing<-c()

error_specificity<-c()


ivec<-c(seq(0.01,0.99,0.01))

i=0.01

nout<-plyr::count(training$Presence)$freq[1]
nfor<-plyr::count(training$citizenship)$freq[1]


for (i in ivec){
  
  rfcutoff = randomForest(Presence~.,training,ntree=50,cutoff=c(i,1-i)) #
  predictCutoff<-predict(rfcutoff, newdata = testing)
 
  conf_matrix<-table(predictCutoff,as.factor(testing$Presence))
  
  conf_matrix_prop<-conf_matrix/sum(conf_matrix)
  
  minimize<-Ff(conf_matrix_prop[2,1],conf_matrix_prop[1,2],conf_matrix_prop[1,1],conf_matrix_prop[2,2])
  print(minimize)
  
  minimizing<-c(minimizing,minimize)
  
  anal<-epi.tests(conf_matrix,conf.level = 0.95)

  sensitivity_vector<-c(sensitivity_vector, sensitivity(conf_matrix))
  specificity_vector<-c(specificity_vector, sepecificity(conf_matrix))
  accuracy_vector<-c(accuracy_vector, accuracy(conf_matrix))
  pop_error<-c(pop_error,Pop_err(conf_matrix))
  
  print(paste(as.character(i),'/',as.character(ivec[length(ivec)])) )
  
}

alph=c(2,10,2)
alph=alph/sum(alph)

minimizing<-alph[1]*pop_error+alph[2]*(1-specificity_vector)+alph[3]*(1-sensitivity_vector)


tolerance=0.98 #set the minimum allowed specificity value

plotAccuracy_forest<-data.frame(cutoff=ivec,sensitivity=sensitivity_vector,specificity=specificity_vector,accuracy=accuracy_vector,overall_accuracy=0.1*(sensitivity_vector)+0.9*(specificity_vector),Function=minimizing)

best_cutoff<-
  plotAccuracy_forest[plotAccuracy_forest$sensitivity==max(plotAccuracy_forest[plotAccuracy_forest$specificity>=tolerance,]$sensitivity),]$cutoff



ggplot(data=plotAccuracy_forest)+
  geom_line(mapping=aes(y=specificity,x= cutoff,color="specificity"),size=1 ) +
  geom_line(mapping=aes(y=accuracy,x= cutoff,color="accuracy"),size=1) +
  geom_line(mapping=aes(x=cutoff,y=sensitivity, color="sensitivity"),size=1)+ 
  geom_line(aes(x=cutoff,y=Function, color="Ff"),size=1)+ 
  geom_line(aes(x=cutoff,y=pop_error, color="pop. error"),size=1)+ 
  geom_vline(xintercept = igood, linetype="dotted", color = "orange", size=1)+ 
 # geom_vline(xintercept = best_cutoff, linetype="dotted", color = "orange", size=1)+
  theme(legend.title= element_blank(),legend.position = "bottom")+ ylab('')


rfcutoff = randomForest(Presence~.,training,ntree=200,cutoff=c(igood,1-igood)) #
predictCutoff<-predict(rfcutoff, newdata = testing)


confusion_matrix<-confusionMatrix(predictCutoff,as.factor(testing$Presence))

confusion_matrix$byClass
confusion_matrix$overall
igood
CM=confusion_matrix$table
Ff(CM)
abs(CM[1,2]-CM[2,1])/sum(CM)*100



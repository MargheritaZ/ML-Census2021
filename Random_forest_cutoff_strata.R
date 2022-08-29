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
library(adabag)


library(tidyr)
library(tibble)
library(dplyr)


## get data
# query the sql-database and extract data, from R ----------------
# get real data of old Census, call it df
# for example, if using a linux computer:
#library(RODBC) #connect to sql server

channel <- odbcDriverConnect("DRIVER=Mydriver; SERVER=Myserver")
#working dataframe in the server
df<-sqlQuery(channel,"select * from mydatabase.Mytable", stringsAsFactors=FALSE ) # see make_table.sql 



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

ddf<-df
#ddf$time_in_iceland <- ifelse(ddf$citizenship==1,1,ddf$time_in_iceland/df$age)

#plot the internal correlations
DataExplorer::plot_correlation(
  data=ddf, 
  type = "all",
  maxcat = 50L, cor_args = list(),
  title = NULL,
  theme_config = list(legend.position ="bottom",axis.text.x = element_blank()))


ggplot(ddf, aes(x=homeOwner, fill=as.factor(Presence))) + geom_histogram(binwidth=0.2)+
  scale_fill_discrete(name="Presence", breaks=c('0','1'), labels=c("Out", "In"))

#ddf$studying_abroad<-as.factor(ddf$studying_abroad)

dev.off()

ggplot(ddf, aes(x=income_increase_2yr/max(income_increase_2yr), colour=as.factor(Presence)) ) + 
  geom_density(alpha=0.1)  +
  labs(x= "income increase") +
  theme(legend.position = "bottom") +
  scale_colour_discrete(name="Presence", breaks=c('0','1'), labels=c("Out", "In"))+
  xlim(-.05, .15)


ggplot(ddf, aes(x=Presence)) + stat_count(width = 0.5,color="black", fill="orange")+
  scale_x_discrete(name ="Presence", 
                   limits=c(0,1))+ scale_y_continuous(name='Number',breaks=c(586,17124))

ggplot(ddf, aes(x=homeOwner, fill=as.factor(Presence))) + stat_count(binwidth=0.2)+
  scale_fill_discrete(name="Presence", breaks=c('0','1'), labels=c("Out", "In"))+
  scale_x_discrete(name ="Home owner", 
                  breaks=c('0','1'), labels=c("0", "1"))

#Turning categorical columns into factors
cols<-c('Presence','citizenship','ever_abroad','married','children','homeOwner','months_worked_increase','sex','region') #factors
ddf[cols] <- lapply(ddf[cols], factor)  ## as.factor() could also be used

#########################################Random forest######################################### 

set.seed(565)
index0 <- sample(1:nrow(ddf),round(0.7*nrow(ddf)))
training<- ddf[index0,]
training$Presence<-as.factor(training$Presence)
testing <- ddf[-index0,]
testing$Presence<-as.factor(testing$Presence)



nout<-plyr::count(training$Presence)$freq[1]
nfor<-plyr::count(training$citizenship)$freq[1]
nin<-plyr::count(training$Presence)$freq[2]

#linear combination of population error, specificity and sensitivity, with convenient factors
alph=c(2,10,2)
alph=alph/sum(alph)

Ff<-function(mat){return(
  alph[1]*abs(mat[1,2]-mat[2,1])/sum(mat)
  +alph[2]*(mat[2,1])/(mat[2,2]+mat[2,1])
  +alph[3]*(mat[1,2])/(mat[1,2]+mat[1,1])
)
}



#choose the grill of stratification  (fracvec) and cutoff
fracvec<-1.3^ seq(-5,12) # exponential factor for stratification proportion
ivec<-c(seq(0.3,0.9,0.05))

#ivec=seq(0.27,0.4,0.01)
#fracvec=seq(14,19,0.5)

#make an empty matrix to later fill with the measures of choice
indexes_df <- as.data.frame(matrix(nrow=length(ivec),ncol=length(fracvec)))
colnames(indexes_df)<-as.character(fracvec)
row.names(indexes_df)<-as.character(ivec)

sensitivity_df<-indexes_df
specificity_df<-indexes_df
accuracy_df<-indexes_df


func<-indexes_df

for(j in seq(1:length(fracvec))){

    for (i in seq(1:length(ivec))){
  
          rfcutoff = randomForest(Presence~.,training,ntree=50,cutoff=c(ivec[i],1-ivec[i]),strata=training$Presence,sampsize=c(floor(nout),floor(fracvec[j]*nout))) #
        
          predictCutoff<-predict(rfcutoff, newdata = testing)
          
          conf_matrix<-table(predictCutoff,as.factor(testing$Presence))
  
          sensitivity_df[i,j]<-sensitivity(conf_matrix)#$.estimate
          specificity_df[i,j]<-specificity(conf_matrix)#$.estimate
          accuracy_df[i,j]<-accuracy(conf_matrix)
  
          func[i,j]<-Ff(conf_matrix)
            #print progress
          print(paste('i=',as.character(i),'/',as.character(length(ivec))))
 
        }
    print(paste('-------------------------j=',as.character(j),'/',as.character(length(fracvec))))
  
    }

colnames(sensitivity_df)<-as.character(round(as.numeric(colnames(sensitivity_df)),2))
colnames(specificity_df)<-as.character(round(as.numeric(colnames(specificity_df)),2))
colnames(func)<-as.character(round(as.numeric(colnames(func)),1))

# minimum acceptable tolerance for specificity
tolerance_specificity<-0.98

sensitivity_dfif<-sensitivity_df

specificity_tol<-ifelse(specificity_df>=tolerance_specificity,1,0)


#dataframe with only acceptable valuse of sensitivity, given specificity
for(i in seq(1:nrow(sensitivity_df))){
  for(j in seq(1:ncol(sensitivity_df))){
    sensitivity_dfif[i,j]<-sensitivity_df[i,j]*specificity_tol[i,j]
  }
}

names(sensitivity_dfif)<-names(sensitivity_df)

#plotting the grills

specificity_df %>% 
  as.data.frame() %>%
  rownames_to_column("cutoff") %>%
  pivot_longer(-c(cutoff), names_to = "strata", values_to = "specificity") %>%
  # mutate(strata= fct_relevel(strata,colnames(func))) %>%
  ggplot(aes(x=as.factor(as.numeric(strata)), y=cutoff, fill=specificity)) + 
  geom_raster() + xlab('Stratification')+
  scale_fill_viridis_c()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


sensitivity_df %>% 
  as.data.frame() %>%
  rownames_to_column("cutoff") %>%
  pivot_longer(-c(cutoff), names_to = "strata", values_to = "sensitivity") %>%
  # mutate(strata= fct_relevel(strata,colnames(func))) %>%
  ggplot(aes(x=as.factor(as.numeric(strata)), y=cutoff, fill=sensitivity)) + 
  geom_raster() + xlab('Stratification')+
  scale_fill_viridis_c()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

sensitivity_dfif %>% 
  as.data.frame() %>%
  rownames_to_column("cutoff") %>%
  pivot_longer(-c(cutoff), names_to = "strata", values_to = "sensitivity") %>%
  # mutate(strata= fct_relevel(strata,colnames(func))) %>%
  ggplot(aes(x=as.factor(as.numeric(strata)), y=cutoff, fill=sensitivity)) + 
  geom_raster() + xlab('Stratification')+
  scale_fill_viridis_c()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))




 setwd("C:\\Users\\nn\\Downloads")

#Importing the file 
CC<-read.csv("CC General.csv")
str(CC)
names(CC)
#Treating missing values
CC$MINIMUM_PAYMENTS[is.na(CC$MINIMUM_PAYMENTS)==TRUE]<-mean(CC$MINIMUM_PAYMENTS,na.rm = TRUE)
CC$CREDIT_LIMIT[is.na(CC$CREDIT_LIMIT)==TRUE]<-mean(CC$CREDIT_LIMIT,na.rm = TRUE)

#user defined function
mystats <- function(x) {
  nmiss<-sum(is.na(x))
  a <- x[!is.na(x)]
  m <- mean(a)
  n <- length(a)
  s <- sd(a)
  min <- min(a)
  pctls<-quantile(a,probs=c(0.01, 0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.975,0.99))
  max <- max(a)
  return(c(n=n, nmiss=nmiss,  mean=m, stdev=s,min = min, pctls=pctls,max=max))
}

names(CC)

vars<-c("BALANCE","PURCHASES","ONEOFF_PURCHASES","INSTALLMENTS_PURCHASES","BALANCE_FREQUENCY",
        "CASH_ADVANCE","PURCHASES_FREQUENCY","ONEOFF_PURCHASES_FREQUENCY","PURCHASES_INSTALLMENTS_FREQUENCY",
        "CASH_ADVANCE_FREQUENCY","CASH_ADVANCE_TRX","PURCHASES_TRX", "CREDIT_LIMIT","PAYMENTS","MINIMUM_PAYMENTS",
        "PRC_FULL_PAYMENT","TENURE")
diag_stats<-t(data.frame(apply(CC[vars], 2, mystats)))

write.csv(diag_stats, "diag_stats.csv")

#OUTLIER CAPPING

CC$BALANCE[CC$BALANCE>7499.401119]<-7499.401119
CC$PURCHASES[CC$PURCHASES>5673.482500]<-5673.482500
CC$ONEOFF_PURCHASES[CC$ONEOFF_PURCHASES>3963.904750]<-3963.904750
CC$INSTALLMENTS_PURCHASES[CC$INSTALLMENTS_PURCHASES>2528.212250]<-2528.212250
CC$CASH_ADVANCE[CC$CASH_ADVANCE>6545.233540]<-6545.233540
CC$CASH_ADVANCE_FREQUENCY[CC$CASH_ADVANCE_FREQUENCY>0.833333]<-0.833333
CC$CASH_ADVANCE_TRX[CC$CASH_ADVANCE_TRX>21.000000]<-21.000000
CC$PURCHASES_TRX[CC$PURCHASES_TRX>82]<-82
CC$CREDIT_LIMIT[CC$CREDIT_LIMIT>14000.000000]<-14000.000000
CC$PAYMENTS[CC$PAYMENTS>8740.880117]<-8740.880117
CC$MINIMUM_PAYMENTS[CC$MINIMUM_PAYMENTS>4256.026103]<-4256.026103









#Data set for deriving kpis
CC1<-CC




library(dplyr)
#Key Performance indicators(Customer profile)
CC1<-mutate(CC1,Monthpurchase=CC1$PURCHASES/12,Monthcashadvance=CC1$CASH_ADVANCE/12
           ,LimitUsage=CC1$BALANCE/CC1$CREDIT_LIMIT,paytomin=CC1$PAYMENTS/CC1$MINIMUM_PAYMENTS)
CC1$type[CC1$ONEOFF_PURCHASES==0 & CC1$INSTALLMENTS_PURCHASES==0]<-"None"
CC1$type[CC1$ONEOFF_PURCHASES>0 & CC1$INSTALLMENTS_PURCHASES==0]<-"One off"
CC1$type[CC1$ONEOFF_PURCHASES==0 & CC1$INSTALLMENTS_PURCHASES>0]<-"Installment"
CC1$type[CC1$ONEOFF_PURCHASES>0 & CC1$INSTALLMENTS_PURCHASES>0]<-"Both"
#Factor Analysis
CC1$type<-NULL
CC1$CUST_ID<-NULL

library(psych )
install.packages("GPArotation")
library(GPArotation)
CC$CUST_ID<-NULL
corrm<-cor(CC)

#Deciding no of factors using scree plot and Kaiser test
scree(corrm,factors = T,pc=T,main="Scree plot",hline = NULL,add=F)
 
eigen_values<-mutate(data.frame(eigen(corrm)$values)
                     ,cum_sum_eigen=cumsum(eigen.corrm..values)
                     , pct_var=eigen.corrm..values/sum(eigen.corrm..values)
                     , cum_pct_var=cum_sum_eigen/sum(eigen.corrm..values))

write.csv(eigen_values,"C:\\Users\\nn\\Downloads\\EigenCasestudy.csv")

#factor loadings

FA<-fa(r=corrm,6, rotate="varimax", fm="ml")               ### CONDUCTING FACTOR ANALYSIS
print(FA)                                                    ### PRINT THE RESULTS
FA_SORT<-fa.sort(FA)                                         ### SORTING THE LOADINGS
ls(FA_SORT)                                                  ### LISTING OUT THE OBJECTS
FA_SORT$loadings
                                            
Loadings<-data.frame(FA_SORT$loadings[1:ncol(CC),])
write.csv(Loadings,"C:\\Users\\nn\\Downloads\\LoadingsCasestudy.csv")


 


############     CLUSTERING    ############



#Performing cluster anlaysis on variables chosen after factor analysis

inputdata_final<-CC1[c("BALANCE","PURCHASES","ONEOFF_PURCHASES","INSTALLMENTS_PURCHASES",
                         "CASH_ADVANCE","PURCHASES_FREQUENCY","ONEOFF_PURCHASES_FREQUENCY",
                         "CASH_ADVANCE_FREQUENCY","CREDIT_LIMIT","PAYMENTS","MINIMUM_PAYMENTS",
                         "PRC_FULL_PAYMENT")]

#standardizing the data
inputdata_final=scale(inputdata_final)


#building clusters using k-means

cluster_three <- kmeans(inputdata_final,3)
cluster_four <- kmeans(inputdata_final,4)
cluster_five <- kmeans(inputdata_final,5)
cluster_six <- kmeans(inputdata_final,6)

names(cluster_three)

#We add the clusters to CC1 which has the KPIs
CC1_new<-cbind(CC1,clust_3=cluster_three$cluster,clust_4=cluster_four$cluster,clust_5=cluster_five$cluster,
              clust_6=cluster_six$cluster)


#Graph
library(cluster)

clusplot(inputdata_final,
         cluster_five$cluster,
         color = TRUE,
         lines = 6,
         labels = 2)



#converting into factors
CC1_new$clust_3=factor(CC1_new$clust_3)
CC1_new$clust_4=factor(CC1_new$clust_4)
CC1_new$clust_5=factor(CC1_new$clust_5)
CC1_new$clust_6=factor(CC1_new$clust_6)

str(CC1_new)
names(CC1)
#profiling
install.packages("tables")
require(tables)
profilekpi<-tabular(1+BALANCE+PURCHASES+ONEOFF_PURCHASES+INSTALLMENTS_PURCHASES+
                 CASH_ADVANCE+PURCHASES_FREQUENCY+ ONEOFF_PURCHASES_FREQUENCY+
                 CASH_ADVANCE_FREQUENCY+CREDIT_LIMIT+PAYMENTS+MINIMUM_PAYMENTS+
                 PRC_FULL_PAYMENT
                 ~ mean+(mean*clust_3)+(mean*clust_4)+(mean*clust_5)+(mean*clust_6), 
                 data=CC1_new)

profilekpi1<-as.matrix(profilekpi)
profilekpi1<-data.frame(profilekpi1)
View(profilekpi1)


profilekpi<-tabular(1~length+(length*clust_3)+(length*clust_4)+(length*clust_5)+(length*clust_6),
                 data=CC1_new)
profilekpi2<-as.matrix(profilekpi)
profilekpi2<-data.frame(profilekpi2)
View(profilekpi2)


write.csv(profilekpi1,"profilekpi1.csv",row.names = F)
write.csv(profilekpi2,"profilekpi2.csv",row.names = F)

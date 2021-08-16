setwd('/Volumes/Fan/20210707HPDesktop/HR_Data/GitHub')

library(data.table)
library(igraph)
library(plyr)
library(tidyr)

cluster_company<- fread('Company_Community_2015_GC_Louvain.csv')

company_industry1<- fread('Company_Sector_Community1.csv')
company_industry2<- fread('Company_Sector_Community2.csv')
Industry<- unique(company_industry1$Industry)
Industry1<- c('Health Care',	'Consumer Discretionary',	'Information Technology',
              'Financials',	'Materials',	'Energy',	'Utilities',	'Communication Services',
              'Real Estate',	'Consumer Staples',	'Industrials')
id_indu<- c(9,7,6,2,1,5,4,10,8,3,11)
industry_m<- data.frame(X=Industry[id_indu],Y=Industry1)
id<- match(company_industry2$Industry,industry_m$Y)
company_industry2$Industry<- industry_m$X[id]
company_industry<- unique(rbind(company_industry1,company_industry2))

cluster_length<- cluster_company[,.(.N),Cluster]
setorder(cluster_length,-N)
la<- 20
cluster_num<- cluster_length$Cluster[1:la]

id<- which(company_industry$Cluster%in%cluster_num)
company_industry<- company_industry[id,]

iter<- 1000
####

entropy<- vector()
for(j in 1:iter){
  print(j)
  company_industry_new<- company_industry
  company_industry_new$Cluster<- sample(cluster_num,dim(company_industry_new)[1],replace = T)
  
  combine_num<- ddply(company_industry_new,.(Cluster,Industry),nrow)
  
  industry_num<- company_industry_new[,.(.N),by=Industry]
  industry_num<- industry_num[match(Industry,industry_num$Industry),]
  
  industry_combine<- matrix(0,nrow = la,ncol = length(Industry))
  row.names(industry_combine)<- 1:la
  colnames(industry_combine)<- Industry
  for(i in 1:la){
    id<- which(combine_num$Cluster==cluster_num[i])
    id1<- match(combine_num$Industry[id],Industry)
    
    industry_combine[i,id1]<- combine_num$V1[id]/industry_num$N[id1]
  }
  
  ## homopjily
  dt<- industry_combine
  Entropies <- vector()
  for (i in 1: dim(dt)[2]){
    Entropies[i]<- 1-sum(dt[,i]*log2(dt[,i]),na.rm = T)/log2(1/dim(dt)[1])
  }
  entropy<- cbind(entropy,Entropies)
}

entropy_all<- data.table()
for(i in seq_along(Industry)){
  entropy_all<- cbind(entropy_all,data.frame(P=entropy[i,]))
}
entropy_all<- data.frame(entropy_all)[,id_indu]

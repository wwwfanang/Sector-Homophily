
library(data.table)
library(pheatmap)
library(RColorBrewer)
library(plyr)
library(grid)

cluster_company<- fread('Company_Community_2015_GC_Louvain.csv')
cluster_length<- cluster_company[,.(.N),Cluster]
setorder(cluster_length,-N)
la<- 20
cluster_num<- cluster_length$Cluster[1:la]

# Entropy -----------------------------------------------------------------
company_industry1<- fread('Company_Sector_Community1.csv')
company_industry2<- fread('Company_Sector_Community2.csv')
company_industry2<- unique(company_industry2)

Industry<- unique(company_industry1$Industry)
Industry1<- c('Health Care',	'Consumer Discretionary',	'Information Technology',	
              'Financials',	'Materials',	'Energy',	'Utilities',	'Communication Services',	
              'Real Estate',	'Consumer Staples',	'Industrials')
id_indu<- c(9,7,6,2,1,5,4,10,8,3,11)
industry_m<- data.frame(X=Industry[id_indu],Y=Industry1)
id<- match(company_industry2$Industry,industry_m$Y)  
company_industry2$Industry<- industry_m$X[id]

company_industry_new<- unique(rbind(company_industry1,company_industry2))
id<- which(company_industry_new$Cluster%in%cluster_num)
company_industry_new<- company_industry_new[id,]

industry_num<- company_industry_new[,.(.N),by=Industry]
id<- match(Industry,industry_num$Industry)
industry_num<- industry_num[id,]

combine_num<- ddply(company_industry_new,.(Cluster,Industry),nrow)

industry_combine2<- matrix(0,nrow = la,ncol = length(Industry))
row.names(industry_combine2)<- 1:la
colnames(industry_combine2)<- Industry
for(i in 1:la){
  id<- which(combine_num$Cluster==cluster_num[i])
  id1<- match(combine_num$Industry[id],Industry)
  
  industry_combine2[i,id1]<- combine_num$V1[id]/industry_num$N[id1]
}

draw_colnames_45 <- function (coln, ...) {
  m = length(coln)
  x = (1:m)/m - 1/2/m
  grid.text(coln, x = x, y = unit(0.96, "npc"), vjust = .5,
            hjust = 1, rot = 90, gp = gpar(...)) 
}
assignInNamespace(x="draw_colnames", value="draw_colnames_45",
                  ns=asNamespace("pheatmap"))

industry_combine2<- industry_combine2[,id_indu]
industry_name<- c('Media','Finance','Electronics','Defense','Education',
                  'Clothing','Software','Pharma & Healthcare','Education West','Geography - US-MA',
                  'Energy','Non-US(Global)','Global Tech','Political','Hospitality','Consulting','Geography - US-IL',
                  'Communications','Banks','Consumable')
colnames(industry_combine2)<- Industry1
rownames(industry_combine2)<- industry_name
pheatmap(industry_combine2,cluster_col=F,cluster_rows=F,display_numbers = T,
         number_format = "%.2f",fontsize = 12,angle_col = '270',
         color = colorRampPalette(rev(brewer.pal(n = 3, name ="RdYlBu")))(100),
         main='(a)')

## sector homophily
dt<- industry_combine2
Entropies <- vector()
for (i in 1: dim(dt)[2]){
  Entropies[i]<- 1-sum(dt[,i]*log2(dt[,i]),na.rm = T)/log2(1/dim(dt)[1])
}
Entropies<- data.frame(Industry[id_indu],Entropies)

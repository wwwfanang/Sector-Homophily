
library(data.table)
library(igraph)
library(pheatmap)
library(RColorBrewer)

ends_all<- fread('Ends-All_1990-2015_Hash.csv')
cluster_community<- fread('Company_Community_2015_GC_Louvain.csv')

## GC
ends_unique<- ends_all[,.(.N),by=.(Company.F,Company.T)]
colnames(ends_unique)[3]<- 'Moves'
setorder(ends_unique,-Moves)

g_all<- graph_from_data_frame(ends_unique,directed = F)

com<- components(g_all)
id<- which(com$membership!=which.max(com$csize))
g_all1<- g_all-vertices(V(g_all)$name[id])
ends_all<- ends_all[which(Company.F%in%V(g_all1)$name)]
ends_all<- ends_all[which(Company.T%in%V(g_all1)$name)]

## community
community_length<- cluster_community[,.(.N),Cluster]
colnames(community_length)[2]<- 'Size'
setorder(community_length,-Size)

## choose the top 20 largest communities
la<- 20
cluster_la<- community_length$Cluster[1:la]

## move matrix

data_all<- data.table(From=ends_all$Company.F,To=ends_all$Company.T)
id1<- match(data_all$From,cluster_community$Company)
id2<- match(data_all$To,cluster_community$Company)
data_all<- data.table(data_all,From.C=cluster_community$Cluster[id1],To.C=cluster_community$Cluster[id2])

data2<- data_all[which(From.C%in%cluster_la)]
data2<- data2[which(To.C%in%cluster_la)]

data1_num<- data2[,.(.N),by=.(From.C,To.C)]

cluster_c<- matrix(0,ncol = la,nrow = la)## log 10 number of moves
for(j in 1:dim(data1_num)[1]){
  # print(j)
  id<- c(data1_num$From.C[j],data1_num$To.C[j])
  id_c<- match(id,cluster_la)
  cluster_c[id_c[1],id_c[2]]<- log10(data1_num$N[j])
}
industry_name<- c('Media','Finance','Electronics','Defense','Education',
                  'Clothing','Software','Pharma & Healthcare','Education West','Geography - US-MA',
                  'Energy','Non-US(Global)','Global Tech','Political','Hospitality','Consulting','Geography - US-IL',
                  'Communications','Banks','Consumable')
colnames(cluster_c)<- industry_name
rownames(cluster_c)<- industry_name
pheatmap(cluster_c,cluster_col=F,cluster_rows=F,display_numbers = T,
         number_format = "%.1f",breaks=seq(3,6,0.1),fontsize_col = 15,
         color = colorRampPalette(rev(brewer.pal(n = 3, name ="RdYlBu")))(31))
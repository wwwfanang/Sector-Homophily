setwd('/Volumes/Fan/20210707HPDesktop/HR_Data/GitHub')

library(data.table)
library(igraph)
library(networkD3)
library(dplyr)

ends_all<- fread('Ends-All_1990-2015_Hash.csv')
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
cluster_community<- fread('Company_Community_2015_GC_Louvain.csv')
community_length<- cluster_community[,.(.N),Cluster]
colnames(community_length)[2]<- 'Size'
setorder(community_length,-Size)
##
la<- 20
cluster_la<- community_length$Cluster[1:la]
id_choose<- c(1:4,6:8,11,13,18:20)
cluster_la<- cluster_la[id_choose]
cluster_lan<- c('Media','Finance','Electronics','Defence','Clothing',
                'Software','Pharma & Healthcare','Energy','Global Tech',
                'Communications','Banks Holding Insurance','Consumable')

# Yearly ------------------------------------------------------------------

data_all<- data.table(From=ends_all$Company.F,To=ends_all$Company.T,Time=ends_all$Time)
id1<- match(data_all$From,cluster_community$Company)
id2<- match(data_all$To,cluster_community$Company)
data_all<- data.table(data_all,From.C=cluster_community$Cluster[id1],To.C=cluster_community$Cluster[id2])

data2<- data_all[which(From.C%in%cluster_la)]
data2<- data2[which(To.C%in%cluster_la)]

year<- c(as.Date('2000-01-01'),as.Date('2016-01-01'))
yeargap<- '3 year'
year_seq<- seq(from=year[1],to=year[2],by=yeargap)

links<- data.frame()
for(i in 1:(length(year_seq)-1)){
  print(i)
  data1<- data2[Time>=year_seq[i]&Time<year_seq[i+1]]
  data1_num<- data1[,.(.N),by=.(From.C,To.C)]
  colnames(data1_num)<- c('source','target','value')
  id1<- match(data1_num$source,cluster_la)
  id2<- match(data1_num$target,cluster_la)
  mm<- cluster_lan[id1]
  num<- 1
  repeat{
    if(num==i)
      break
    mm<- paste(mm,' ',sep='')
    num<- num+1
  }
  data1_num$source<- mm
  
  mm<- cluster_lan[id2]
  num<- 1
  repeat{
    mm<- paste(mm,' ',sep='')
    num<- num+1
    if(num==(i+1))
      break
  }
  data1_num$target<- mm
  
  links<- rbind(links,data1_num)
}

nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique()
)

nodes<- cluster_lan
for(i in 2:length(year_seq)){
  
  mm<- cluster_lan
  num<- 1
  repeat{
    if(num==i)
      break
    mm<- paste(mm,' ',sep='')
    num<- num+1
  }
  
  nodes<- c(nodes,mm)
}
nodes<-data.frame(name=nodes)

links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

## link choose
id<- which(links$value>1000)
links1<- links[id,]
p <- sankeyNetwork(Links = links1, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name", fontSize = 14,
                   sinksRight=FALSE)
p

saveNetwork(p, 'test.html')

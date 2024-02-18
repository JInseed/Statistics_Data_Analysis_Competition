library(caret)
library(tidyverse)
library(cluster)
library(factoextra)
library(kableExtra)

#tour 실험   27/2, 1/27
arch=read.csv('C:/Users/82102/Desktop/문화 관광 project/군집화/관광지역피봇.csv',header=TRUE)

rownames(arch)=arch[[1]]

arch=arch %>% 
  filter(지역명!='경기 여주군') %>% 
  select(-지역명,  -산업관광지, -건축.조형물)

arch.m=arch 

#업종 편차 제거
df_sd=sd(as.matrix(arch.m))
for (i in 1:dim(arch.m)[2]){
  arch.m[,i]=(arch.m[,i]/(sd(as.matrix(arch.m[,i]))*(2/3)+df_sd*(1/3)))^(27/2)
}

#지역 편차 제거
df_mean=mean(as.matrix(arch.m))
for (i in 1:dim(arch.m)[1]){
  arch.m[i,]=(arch.m[i,]/(mean(as.matrix(arch.m[i,]))*(1/2)+df_mean*(1/2)))^(1/27)
}

set.seed(1004)
km = kmeans(arch.m, centers = 4)

fviz_cluster(km, data = arch.m, geom = "text")+theme(legend.position = "none")

sil = silhouette(km$cluster, dist(arch.m))
fviz_silhouette(sil)

#실루엣
fviz_nbclust(arch.m, kmeans, method = "silhouette",k.max = 20) + 
  theme_minimal() + 
  ggtitle("Silhouette Method")


#Elbow Method
fviz_nbclust(arch.m, kmeans, method = "wss", k.max = 10) + 
  theme_minimal() + 
  ggtitle("Elbow Method")


sm <-km$centers %>%
  t()  %>%  
  kable("pandoc", digits = 3)

summary(arch.m)

sm

km$cluster[km$cluster==1]
km$centers


arch$cluster=km$cluster
df2=as.data.frame(arch$지역명,km$cluster)
view(df)
df=arch[,c('지역명','cluster')]

arch=arch %>% 
  filter(지역명!='경기 여주군') %>% 
  select(지역명)

arch$cluster2=km$cluster

arch=arch %>% 
  mutate(거주지=지역명) %>% 
  select(-지역명)

view(arch)




























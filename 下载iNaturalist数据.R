install.packages("rinat")

library(rinat)
library(readxl)
library(tidyr)
library(dplyr)
library(data.table)
#设定搜索范围
bounds<-c(26,73.19,39.47,104.47)

#下载数据

#建立路径
setwd("D:\\工作汇报_段浚东\\西藏水鸟数据下载")

#读取名录文件。
Species <- read_excel("西藏水鸟名录整理_段 22-07-04.xlsx")

#提取“种拉丁名”这一列以获取物种名。
Name <- data.table(Species[,"种拉丁名"])

#给Name这个数据表的列标题重新命名为species。
colnames(Name) <- "Species"

#subset函数进行筛选非空值的name。
Name <- subset(Name,!is.na(Species))
i=1


#按照species建立以物种名命名的文件夹。
for (i in c(1:nrow(Name))){
  
  #若循环中出现错误或警告则跳过继续运行循环
    tryCatch({ 
      mainDir1 <- sprintf("D:\\工作汇报_段浚东\\西藏水鸟数据下载/inaturlist/rinat/%s.csv",Name[i,1])
      if (file.exists(mainDir)){
      } else {
        print(Name[i,1])
        
        deer<-get_inat_obs(taxon_name=Name[i,1],quality="research",maxresults=10000,bounds=bounds)
        DATA <- deer[,c("observed_on","latitude","longitude","scientific_name")]
        #将observed_on列拆分成year和month两列
        a<-separate(DATA,observed_on,c("year","month"))
        #截取2011年到2021年间的数据
        b<-subset(a,a$year>=2011&year<2022)
        #保存数据
        write.csv(b, mainDir1,row.names = F)
        
      }},
      warning=function(w){},
      error=function(e){},
      finally={
        mainDir2 <- sprintf("D:\\工作汇报_段浚东\\西藏水鸟数据下载/inaturlist/rinat/%s.csv",Name[i+1,1])
       
          print(Name[i+1,1])
          deer<-get_inat_obs(taxon_name=Name[i+1,1],quality="research",maxresults=10000,bounds=bounds)
          DATA <- deer[,c("observed_on","latitude","longitude","scientific_name")]
          #将observed_on列拆分成year和month两列
          a<-separate(DATA,observed_on,c("year","month"))
          #截取2011年到2021年间的数据
          b<-subset(a,a$year>=2011&year<2022)
          #保存数据
          write.csv(b,mainDir2,row.names = F)
        
      })
  
  mainDir3 <- sprintf("D:\\工作汇报_段浚东\\西藏水鸟数据下载/inaturlist/rinat/%s.csv",Name[i+2,1])
  
    print(Name[i+2,1])
    
    deer<-get_inat_obs(taxon_name=Name[i+2,1],quality="research",maxresults=10000,bounds=bounds)
    DATA <- deer[,c("observed_on","latitude","longitude","scientific_name")]
    #将observed_on列拆分成year和month两列
    a<-separate(DATA,observed_on,c("year","month"))
    #截取2011年到2021年间的数据
    b<-subset(a,a$year>=2011&year<2022)
    #保存数据
    write.csv(b,mainDir3,row.names = F)    
          }
           
    


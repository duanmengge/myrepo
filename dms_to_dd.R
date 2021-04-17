library(readxl)
library(dplyr)
library(data.table)

data<-read_excel("Documents/example.xls")
data<-do.call("rbind", replicate(200, data, simplify = FALSE))

f<-function(data){
  Data<-data.frame(lon=(""),
                   lat=(""))
  
  for(j in 1:2){
    for(i in 1:nrow(data)){
      A<-as.character(data[i,j])
      B<- unlist(strsplit(A,split="\\D"))
      B<-as.numeric(B)
      C<- B[1]+B[2]/60+B[3]/3600
      Data[i,j]<-C
    }}
  return(Data)
}
system.time(f(data))

format_row<-function(item){
  B<- unlist(strsplit(item, split="\\D"))
  B<-as.numeric(B)
  C<- B[1]+B[2]/60+B[3]/3600
  
}
system.time(
  data_dplyr<-data %>% rowwise() %>%
    mutate(lon_digi = format_row(lon),
           lat_digi = format_row(lat))
)#%>%将左边的值赋给右边的值。rowwise函数，一行一行的读取数据框；mutate函数，将添加新的变量计算结果并保存以前的变量；transmute函数，添加新的变量但删除之前的变量

data_table<-data.table(data)
system.time(
  result<-data_table[, 
                     .(lon_digi=format_row(lon),
                       lat_digi=format_row(lat)), 
                     by = seq_len(nrow(data_table))]
)

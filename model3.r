library(dismo)
library(ggplot2)
library(ggpubr)
library(data.table)
library(raster)
library(rgdal)
library("rJava")
setwd("/Users/menggeduan/Documents/practise/dispersal_based_ENM/")
province<-readOGR(dsn="/Users/menggeduan/Documents/Data/provinces", layer="bou2_4l")
province_df <- fortify(province)
csv_file<-"/Users/menggeduan/Documents/SOLEL/occ.csv"
occs<-read.table(csv_file, head=T, sep=",")
occs<-occs[,-1] 
env_base<-"/Users/menggeduan/Documents/Data/ENV/Annually"
ESM<-"EC-Earth3-Veg"
SSP<-"SSP585"
Start_year<-2015
vars<-c("pr_%d_sum", "tasmin_%d_min", "tasmax_%d_max")
mask<-raster("/Users/menggeduan/Documents/Data/mask.tif")
plot(mask)
stacked_rasters<-stack(sprintf("%s/%s_%s_%s.tif", 
                               env_base, ESM, SSP,
                               sprintf(vars, Start_year)))
extent(stacked_rasters[[1]])
china_ext<-c(70, 140, 0, 55)
china_raster<-crop(stacked_rasters, china_ext)
plot(stacked_rasters)
#使用maxent[dismo]模型建模
model<-maxent(stacked_rasters, occs)
#空间模型预测，预测一个光栅对象,predict(dismo\raster)
pred_test <-predict(model,stacked_rasters)
pred <- predict(model, china_raster) 
plot(pred)
#使用randomPoints【dismo】函数随机提取背景值
bg <- randomPoints(stacked_rasters, 10000)
#evaluate函数【dismo】评价模型 p：存在点 a：缺失点 x：预测变量（光栅对象）
e1 <- evaluate(model, p=occs, a=bg, x=china_raster)
#threshold【dismo】函数找到阈值，将评价结果转换为二进制分数
th<-threshold(e1)
#extract【raster】函数使用样本经纬度从栅格对象中提取值
all_training_v<-extract(pred_test, occs)
#计算栅格层的单元格值的0.05分位数和0.1分位数。
training_present<-quantile(all_training_v, c(0.05, 0.1))
#par(mfrow=c(2,2))
#plot(all_training_v, main="all_training_v")
#plot(pred, main="pred")
#hist【raster】函数创建一个栅格层值的直方图
hist(all_training_v)
hist(pred)
#赋值
kappa<-th$kappa
#将0.1分位数的值赋值给thr
thr<-training_present[2]
#将空间模型预测的栅格对象转换为点并数据points数据框（x，y，layer）
points<-data.table(rasterToPoints(pred_test))
#如果数据框points的layer值大于等于0.1分位数的值，返回1，否则返回0
points$bin<-ifelse(points$layer>=thr, 1, 0)
#画图【ggplot2】 ggplot函数初始化一个框架ggplot对象 +为plot添加组件;geom_tile函数将空间模型预测作图；geom_path()将各省的地理位置按照它们在数据中出现的顺序连接起来，边界颜色为灰色，粗细为0.2;geom_point函数将样本点标记在图中
p1<-ggplot()+geom_tile(data=points, aes(x=x, y=y, fill=layer))+
  geom_path(data = province_df, 
            aes(x = long, y = lat, group = group),
            color = 'gray', size = .2)+
  geom_point(data=occs, aes(x=x, y=y), shape=4)+
  #scale_fill_gradient2函数在空间预测模型中创建一个发散的颜色梯度
  scale_fill_gradient2(
    low = "lightblue",
    mid = "white",
    high = "red",
    midpoint =thr,
    space = "Lab",#must
    na.value = "grey50",#缺少值的颜色
    guide = "colourbar",#图例为渐变色
    aesthetics = "fill"#美学
  )+
  theme_bw()#显示完整主题

p1
#将空间模型预测bin=1的点取出并与样本点，中国地图联合作图
p2<-ggplot()+geom_tile(data=points[bin==1], aes(x=x, y=y), fill="pink")+
  geom_point(data=occs, aes(x=x, y=y), shape=4)+
  geom_path(data = province_df, 
            aes(x = long, y = lat, group = group),
            color = 'gray', size = .2)+
  theme_bw()

p2

#创建函数min_dist
min_dist<-function(x, y, ypoints){
  min(sqrt((x-ypoints$x)^2+(y-ypoints$y)^2), na.rm = T)
}

#previous_points<-points[bin==1]
#extract[raster]函数根据points的x，y点从mask图中提取值？
points$is_continent<-extract(mask, points[, c("x", "y")])
#将points中在大陆中的点取出放入points（去除海洋上的点）
points<-points[!is.na(points$is_continent)]
#occs数据导入previous_points数据框
previous_points<-occs
#将previous_points数据框的列名改为x，y
colnames(previous_points)<-c("x", "y")
# 计算previous_points的行数并赋值给n_pixel
n_pixel<-nrow(previous_points)

#while(T){
for (i in c(1:7)){
  points$dist<-points[, min_dist(x, y, previous_points), 
                      by = 1:nrow(points)]$V1#通过points的型x，y列和prvious_points数据框利用mid_dist函数计算并赋入points的dist列中
  points$dispersalable<-
    ifelse((points$bin==1)&(points$dist<0.8), 1, 0)#如果points中bin列等于1且dist列小于0.8，返回1，否则返回0并赋值给dispersalable列
  previous_points<-points[(bin==1)&(dispersalable==1)]#将points中bin等于1且dispersalable等于1的行放入previous_points数据框中
  if (n_pixel==nrow(previous_points)){
    break()
  }#如果previous_points的行等于n_pixel，则跳出循环
  
  n_pixel<-nrow(previous_points)#将previous_points的行赋值给n_pixel
}#问题：for循环的作用是否多余
#与原来的p2图相比，去掉了海域的潜在分布预测
p2<-ggplot()+geom_tile(data=previous_points, aes(x=x, y=y), fill="pink")+
  geom_point(data=occs, aes(x=x, y=y), shape=4)+
  geom_path(data = province_df, 
            aes(x = long, y = lat, group = group),
            color = 'gray', size = .2)+
  theme_bw()

p2
#将2015年建立的数据模型映射到2020-2100的/空间气候中进行预测
for (year in c(2020:2100)){
  #输出年份
  print(year)
  #将每一年的环境数据使用stack‘函数转换为RasterStack对象
  stacked_rasters_future<-stack(sprintf("%s/%s_%s_%s.tif", 
                                        env_base, ESM, SSP,
                                        sprintf(vars, year)))
  #使用crop函数创建stack_rasters三个图层的栅格/空间对象的各个地理子集，70-140的经度，0-55的纬度
  china_raster_future<-crop(stacked_rasters_future, china_ext)
  #plot(china_raster_future)
  #  将当年的气候图层名换成2015年的气候图层名
  names(china_raster_future)<-names(china_raster)
  #当年的空间模型预测
  pred_future <- predict(model, china_raster_future)
  #将当年空间模型预测的栅格对象转换为点并放入points_future数据框（x，y，layer）
  points_future<-data.table(rasterToPoints(pred_future))
  #如果数据框points_future的layer值大于等于0.1分位数的值，返回1，否则返回0
  points_future$bin<-ifelse(points_future$layer>=thr, 1, 0)
  #通过points_future的x，y列和prvious_points数据框利用mid_dist函数计算并赋入points_futures的dist列中
  points_future$dist<-points_future[, min_dist(x, y, previous_points), 
                                    by = 1:nrow(points_future)]$V1
  #如果points_future中bin列等于1且dist列小于0.8，返回1，否则返回0并赋值给dispersalable列
  points_future$dispersalable<-
    ifelse((points_future$bin==1)&(points_future$dist<0.8), 1, 0)
  #extract[raster]函数根据points_future的x，y点从mask图中提取值
  points_future$is_continent<-extract(mask, points_future[, c("x", "y")])
  #将points_future中在大陆中的点取出放入points_future（去除海洋上的点）
  points_future<-points_future[!is.na(points_future$is_continent)]
  #画图 ggplot函数初始化一个框架ggplot对象 +为plot添加组件;geom_tile函数将空间模型预测作图；geom_path()将各省的地理位置按照它们在数据中出现的顺序连接起来，边界颜色为灰色，粗细为0.2;geom_point函数将样本点标记在图中；scale_fill_gradient2函数在空间预测模型中创建一个发散的颜色梯度
  p1<-ggplot()+geom_tile(data=points_future, aes(x=x, y=y, fill=layer))+
    geom_path(data = province_df, 
              aes(x = long, y = lat, group = group),
              color = 'gray', size = .2)+
    geom_point(data=occs, aes(x=x, y=y), shape=4)+
    scale_fill_gradient2(
      low = "lightblue",
      mid = "white",
      high = "red",
      midpoint =thr,
      space = "Lab",
      na.value = "grey50",
      guide = "colourbar",
      aesthetics = "fill"
    )+
    labs(fill="Suitability")+             #图名为“suitability”
    ggtitle("Potential distribution")+    #图例为“Potential distribution”
    theme_bw()+
    theme(axis.title = element_blank(),   #坐标轴什么也不画，也不分配空间
          legend.position="bottom",       #图例的位置为底部
          legend.text = element_blank())+ #图例项目标签为空，也不分配空间
    xlim(china_ext[1], china_ext[2])+     #x轴的起始点结束点
    ylim(china_ext[3], china_ext[4])      #y轴的起始点结束点
  
  #p1
  #将空间模型预测bin=1的点取出并与样本点，中国地图作图，潜在分布图
  p2<-ggplot()+geom_tile(data=points_future[bin==1], aes(x=x, y=y, fill=factor("Suitable")))+
    geom_point(data=occs, aes(x=x, y=y), shape=4)+
    geom_path(data = province_df, 
              aes(x = long, y = lat, group = group),
              color = 'gray', size = .2)+
    theme_bw()+
    scale_fill_manual(values=c("pink"))+
    ggtitle("Binary potential distribution")+
    labs(fill="")+
    theme(axis.title = element_blank(),
          legend.position="bottom")+
    xlim(china_ext[1], china_ext[2])+
    ylim(china_ext[3], china_ext[4])
  
  #p2
  #潜在分布+扩散图
  p3<-ggplot()+
    geom_tile(data=points_future[bin==1], aes(x=x, y=y, fill=factor(dispersalable)))+
    #geom_text(data=points_future[bin==1], aes(x=x, y=y, label=sprintf("%.1f",dist)))+
    geom_point(data=occs, aes(x=x, y=y), shape=4)+
    geom_path(data = province_df, 
              aes(x = long, y = lat, group = group),
              color = 'gray', size = .2)+
    scale_fill_manual(breaks = c(0,1), values=c("lightblue", "pink"), 
                      labels=c("Inaccessible", "Accessible"))+
    ggtitle("Accessible area")+
    labs(fill="Accessibility")+
    theme_bw()+
    theme(axis.title = element_blank(),
          legend.position="bottom")+
    xlim(china_ext[1], china_ext[2])+
    ylim(china_ext[3], china_ext[4])
  #ggsave(p3, filename=sprintf("/Users/menggeduan/Documents/Data/ENV_results/%s/%d_text.png", "CLCuD", year), width=30, height=12)
  #p3
  #将p1、p2、p3一同输出赋给pp，格式为1行3列
  pp<-ggarrange(p1, p2, p3, nrow=1, ncol=3)
  # annotate_fugure函数注释排列图pp为Cotton leaf curl disease (2020)
  pp<-annotate_figure(pp,
                      top = text_grob(sprintf("Cotton leaf curl disease (%d)", year),
                                      color = "black", face = "bold", size = 14))
  # 保存pp用某一指定格式
  ggsave(pp, filename=sprintf("/Users/menggeduan/Documents/SOLEL/SOLEL_results/%d.png",year), width=10, height=4)
  previous_points<-points_future[(bin==1)&(dispersalable==1)]
}
#将图片转换为gif
library(gifski)
png_path <- file.path("/Users/menggeduan/Documents/Data/ENV_results/CLCuD/", "2%03d.png")
png_files <- sprintf(png_path, 20:100)
gif_file <- file.path("/Users/menggeduan/Documents/","CLCuD.gif")#创建临时文件
gifski(png_files, gif_file)#拼接

#
png_path <- file.path("/Users/menggeduan/Documents/Data/ENV_results/CLCuD/", "2%03d.png")
png_files <- sprintf(png_path, 20:100)
gif_file <- file.path("/Users/menggeduan/Documents/","CLCuD.gif")#创建临时文件
gifski(png_files, gif_file)#拼接
unlink(png_files)#删除文件和目录
utils::browseURL(gif_file)

#
library(gapminder)
library(ggplot2)
makeplot <- function(){
  datalist <- split(gapminder, gapminder$year)
  lapply(datalist, function(data){
    p <- ggplot(data, aes(gdpPercap, lifeExp, size = pop, color = continent)) +
      scale_size("population", limits = range(gapminder$pop)) + geom_point() + ylim(20, 90) +
      scale_x_log10(limits = range(gapminder$gdpPercap)) + ggtitle(data$year) + theme_classic()
    print(p)
  })
}
gif_file <- file.path("/Users/menggeduan/Documents/", 'mtcars.gif')
save_gif(makeplot(), gif_file, 1280, 720, res = 144)
utils::browseURL(gif_file)

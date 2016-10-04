library(ggplot2)
#geom_abline(geom_hline, geom_vline)
view(mtcars)
p<-ggplot(mtcars,aes(wt,mpg))+
  geom_point()
p+geom_vline(xintercept = 5)
p+geom_vline(xintercept = 1:5)
p+geom_hline(yintercept = 20)
p + geom_abline(intercept = 20)

a<-coef(lm(mpg ~ wt, data = mtcars))
p + geom_abline(intercept = a[1],slope = a[2],col="red")

p + geom_smooth(method = "lm", se = FALSE)
p + geom_smooth(method = "lm")

p <- ggplot(mtcars, aes(mpg, wt)) +
  geom_point() +
  facet_wrap(~ cyl)

#Não entendi
mean_wt <- data.frame(cyl = c(4, 6, 8), wt = c(2.28, 3.11, 4.00))
p + geom_hline(aes(yintercept = wt), mean_wt)

#geom_bar(stat_count)
View(mpg)
unique(mpg$class)

g<- ggplot(mpg,aes(class))
g+geom_bar()

g+geom_bar(aes(weight=displ)) #simplesmente fez a soma do valor de displ
library(dplyr)
mpg %>%
  group_by(class)%>%
  summarise(sum(displ))

df <- data.frame(trt = c("a", "b", "c","a"), 
                 outcome = c(2, 2, 3, 3)
                 )
ggplot(df,aes(trt,outcome))+
  geom_bar(stat = "identity") #de fato esta realizando a soma de todos os valores por trt

ggplot(df,aes(trt))+
  geom_bar()

ggplot(df, aes(trt, outcome)) +
  geom_point()

df <- data.frame(x = rep(c(2.9, 3.1, 4.5), c(5, 10, 4)))
ggplot(df, aes(x)) + geom_bar() #vira um histograma

ggplot(df, aes(x)) + geom_histogram(binwidth = 0.10)

g + geom_bar(aes(fill = drv,weight=displ))

g + geom_bar(aes(fill = drv), position = "dodge")
g + geom_bar(aes(fill = drv), position = "fill")

#geom_bin2d
View(diamonds)
d <- ggplot(diamonds,aes(x,y)) + xlim(4,10) + ylim(4,10)
d + geom_bin2d()
d + geom_bin2d(bins=10)
d + geom_bin2d(bins=30)
d + geom_bin2d(binwidth=c(0.1,0.1))

#geom_boxplot
p <- ggplot(mpg, aes(class, hwy))
p + geom_boxplot()
p + geom_point()
p + geom_jitter()
p + geom_boxplot() + geom_jitter()
p + geom_boxplot() + geom_jitter(width = 0.2)
p + geom_boxplot() + geom_jitter(aes(color=class),width = 0.2)
p + geom_boxplot() + coord_flip()
p + geom_boxplot(notch = TRUE)
p + geom_boxplot(varwidth = TRUE) #A variancia fica representada na horizontal
p + geom_boxplot(fill = "green", colour = "#3366FF")
p + geom_boxplot(outlier.colour = "red", outlier.shape = 1)
p + geom_boxplot(aes(colour = drv))
View(diamonds)
ggplot(diamonds, aes(carat, price)) +
  geom_boxplot()
ggplot(diamonds, aes(carat, price)) +
  geom_boxplot(aes(group = cut_width(carat, 0.25)))

#geom_contour
View(faithfuld)
v <- ggplot(faithfuld, aes(waiting, eruptions, z = density))
v + geom_contour()

x<-c(1,1,2,2,1)
y<-c(1,2,2,1,1)
z<-data.frame(x,y)
ggplot(z,aes(x,y))+
  geom_line(size=3,color="red")

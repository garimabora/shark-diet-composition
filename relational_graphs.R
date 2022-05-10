morpho = read.csv("C:/Users/garim/Desktop/dissertation/primary data/morphometric_data.csv")

#depth of capture vs body weight
x <- morpho$Depth.caught.m.
y <- morpho$Body.Weight
shark<-morpho$Shark.Species

library(ggplot2)
my_df <- data.frame(x = x, y = y, group = shark)
ggplot(my_df,aes(x=x,y=y))+
  geom_point(aes(colour=shark))+
  scale_color_discrete("shark")+
  xlab("depth")+
  ylab("body weight")+
  theme(axis.line = element_line(colour = "black", # Changes the default theme
                                 size = 0.24))

#depth of capture vs lat
y1 <- morpho$lat
shark<-morpho$Shark.Species

library(ggplot2)
my_df <- data.frame(x = x, y = y1, group = shark)
ggplot(my_df,aes(x=x,y=y))+
  geom_point(aes(colour=shark))+
  scale_color_discrete("shark")+
  xlab("depth")+
  ylab("lat")+
  theme(axis.line = element_line(colour = "black", # Changes the default theme
                                 size = 0.24))

#length and body mass
tl<-morpho$Total.Length
bw<-morpho$Body.Weight
my_df <- data.frame(x = tl, y = bw, group = shark)
ggplot(my_df,aes(x=tl,y=bw))+
  geom_point(aes(colour=shark))+
  scale_color_discrete("shark")+
  xlab("Total length")+
  ylab("Body weight")+
  theme(axis.line = element_line(colour = "black", # Changes the default theme
                                 size = 0.24))

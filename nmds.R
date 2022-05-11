library(vegan)
library(dplyr)


pc = read.csv("C:/Users/garim/Desktop/dissertation/primary data/prey_allfamily.csv")
com = pc[,4:ncol(pc)]
com1 <- com %>% 
  filter_all(any_vars(. != 0))
m_com = as.matrix(com1)
set.seed(123)
nmds = metaMDS(m_com, distance = "bray",autotransform = FALSE,k=2)
nmds
plot(nmds)

#ggplot


ordiplot(nmds,type="n")
orditorp(nmds,display="species",col="red",air=0.01)
orditorp(nmds,display="sites",cex=1.25,air=0.01)

write.csv(com1,"C:/Users/garim/Desktop/dissertation/primary data/com1.csv", row.names = FALSE)
data.scores = as.data.frame(scores(nmds))
pc1 = read.csv("C:/Users/garim/Desktop/dissertation/primary data/com1 - Copy.csv")
data.scores$Shark = pc1$Shark
data.scores$Depth = pc1$Depth
data.scores$Location = pc1$fishingrounds
head(data.scores)

library(ggplot2)

#hull
grp.a <- data.scores[data.scores$Location == "North Fishing Grounds", ][chull(data.scores[data.scores$Location == "North Fishing Grounds", c("NMDS1", "NMDS2")]), ]  # hull values for grp north fishing grounds
grp.b <- data.scores[data.scores$Location == "Malvan fishing grounds", ][chull(data.scores[data.scores$Location == "Malvan fishing grounds" | data.scores$Location == "Sindhudurg fort", c("NMDS1", "NMDS2")]), ]  # hull values for malvan fishing grounds
grp.c <- data.scores[data.scores$Location == "South fishing grounds", ][chull(data.scores[data.scores$Location == "South fishing grounds", c("NMDS1", "NMDS2")]), ]  # hull values for grp Scoliodon laticaudus

hull.data <- rbind(grp.a, grp.b,grp.c)  #combine grp.a and grp.b
hull.data


xx = ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) +
  geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,fill=Location,group=Location),alpha=0.30) +
  geom_point(size = 2, aes(colour=Shark))+ 
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
        legend.text = element_text(size = 12, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", colour = "Shark", y = "NMDS2", shape = "Location")

xx
library(vegan)

sl=read.csv("C:/Users/garim/Desktop/dissertation/primary data/sl_acc.csv")
cl=read.csv("C:/Users/garim/Desktop/dissertation/primary data/cl_acc.csv")
spl=read.csv("C:/Users/garim/Desktop/dissertation/primary data/spl_acc.csv")
csp=read.csv("C:/Users/garim/Desktop/dissertation/primary data/csp_acc.csv")

sp1=specaccum(sl)
sp2=specaccum(cl)
sp3=specaccum(spl)
sp4=specaccum(csp)

plot(sp4,xlim=c(0,60),ylim=c(0,15),ylab="Cumulative prey frequency",xlab="Number of stomachs",col="green")
+lines(sp2,ylab="Cumulative prey frequency",col="red")
+lines(sp3,ylab="Cumulative prey frequency",col="yellow")
+lines(sp1,ylab="Cumulative prey frequency",col="blue")

legend("topleft",c("Carpetshark","Spadenose","Blacktip","ScallopedHammerhead"),fill=c("green","blue","red","yellow"),cex=0.7)

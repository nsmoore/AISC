install.packages("OptSurvDesign", repos=NULL, type ="source")
library(OptSurvDesign)
get_S(Sa = 12, ra = 10, lambda1 = log(2)/10, lambda0 = log(2)/20, d=1)
get_N(Sa = 12, ra = 10, lambda1 = log(2)/10, lambda0 = log(2)/20, d=1)
seq.sa <-seq(0, 50, 0.1)
tab.ra10<-c()
tab.ra20<-c()
tab.ra30<-c()

#original
for(k in 1:length(seq.sa)){
  tab.ra10 <- c(tab.ra10, get_S_noround(Sa = seq.sa[k], ra = 10, lambda1 = log(2)/20, lambda0 = log(2)/10, d=1))
  tab.ra20 <- c(tab.ra20, get_S_noround(Sa = seq.sa[k], ra = 20, lambda1 = log(2)/20, lambda0 = log(2)/10, d=1))
  tab.ra30 <- c(tab.ra30, get_S_noround(Sa = seq.sa[k], ra = 30, lambda1 = log(2)/20, lambda0 = log(2)/10, d=1))
}

seq.sf10 <-  tab.ra10 - seq.sa
seq.sf20 <-  tab.ra20 - seq.sa
seq.sf30 <-  tab.ra30 - seq.sa

par(mfrow=c(1,2))
plot(seq.sa, tab.ra10, type="l", col="gray", xlab="Sa", ylab="S", ylim=c(-20, 100), xlim=c(0, 50),xaxs="i",yaxs="i", main="S vs Sa", lwd=3)
lines(seq.sa, tab.ra20,col="gray", lwd=3)
lines(seq.sa, tab.ra30,col="gray", lwd=3)
lines(seq.sa[tab.ra10>=seq.sa], tab.ra10[tab.ra10>=seq.sa], col="blue", lwd=3)
lines(seq.sa[tab.ra20>=seq.sa], tab.ra20[tab.ra20>=seq.sa], col="red", lwd=3)
lines(seq.sa[tab.ra30>=seq.sa], tab.ra30[tab.ra30>=seq.sa],col="green", lwd=3)

plot(seq.sa, seq.sf10, type="l", col="gray", xlab="Sa", ylab="Sf", ylim=c(-20, 100), xlim=c(0, 50), xaxs="i", yaxs="i", main="Sf vs Sa", lwd=3)
lines(seq.sa, seq.sf20, col="gray", lwd=3)
lines(seq.sa, seq.sf30, col="gray", lwd=3)
lines(seq.sa[tab.ra10>=seq.sa], seq.sf10[tab.ra10>=seq.sa], col="blue", lwd=3)
lines(seq.sa[tab.ra20>=seq.sa], seq.sf20[tab.ra20>=seq.sa], col="red", lwd=3)
lines(seq.sa[tab.ra30>=seq.sa], seq.sf30[tab.ra30>=seq.sa],col="green", lwd=3)


#first derivative
d.tab.ra10<-c()
d.tab.ra20<-c()
d.tab.ra30<-c()
for(k in 1:length(seq.sa)){
  d.tab.ra10 <- c(d.tab.ra10, (tab.ra10[k+1] - tab.ra10[k])/0.1)
  d.tab.ra20 <- c(d.tab.ra20, (tab.ra20[k+1] - tab.ra20[k])/0.1)
  d.tab.ra30 <- c(d.tab.ra30, (tab.ra30[k+1] - tab.ra30[k])/0.1)
}

d.seq.sf10 <-  d.tab.ra10 - seq.sa
d.seq.sf20 <-  d.tab.ra20 - seq.sa
d.seq.sf30 <-  d.tab.ra30 - seq.sa

par(mfrow=c(1,2))
plot(seq.sa, d.tab.ra10, type="p", col="blue", xlab="Sa", ylab="dS", ylim=c(-20, 100), xlim=c(0, 50),xaxs="i",yaxs="i", main="dS vs Sa", lwd=3)
lines(seq.sa, d.tab.ra20,col="red", lwd=3, type="p")
lines(seq.sa, d.tab.ra30,col="green", lwd=3, type="p")
lines(seq.sa[tab.ra10>=seq.sa], tab.ra10[tab.ra10>=seq.sa], col="blue", lwd=3)
lines(seq.sa[tab.ra20>=seq.sa], tab.ra20[tab.ra20>=seq.sa], col="red", lwd=3)
lines(seq.sa[tab.ra30>=seq.sa], tab.ra30[tab.ra30>=seq.sa],col="green", lwd=3)

plot(seq.sa, d.seq.sf10, type="p", col="blue", xlab="Sa", ylab="dSf", ylim=c(-100, 100), xlim=c(0, 50), xaxs="i", yaxs="i", main="dSf vs Sa", lwd=3)
lines(seq.sa, d.seq.sf20, col="red", lwd=3, type="p")
lines(seq.sa, d.seq.sf30, col="green", lwd=3, type="p")
lines(seq.sa[tab.ra10>=seq.sa], seq.sf10[tab.ra10>=seq.sa], col="blue", lwd=3)
lines(seq.sa[tab.ra20>=seq.sa], seq.sf20[tab.ra20>=seq.sa], col="red", lwd=3)
lines(seq.sa[tab.ra30>=seq.sa], seq.sf30[tab.ra30>=seq.sa],col="green", lwd=3)

#second derivative
dd.tab.ra10<-c()
dd.tab.ra20<-c()
dd.tab.ra30<-c()
for(k in 1:length(seq.sa)){
  dd.tab.ra10 <- c(dd.tab.ra10, (d.tab.ra10[k+1] - d.tab.ra10[k])/0.1)
  dd.tab.ra20 <- c(dd.tab.ra20, (d.tab.ra20[k+1] - d.tab.ra20[k])/0.1)
  dd.tab.ra30 <- c(dd.tab.ra30, (d.tab.ra30[k+1] - d.tab.ra30[k])/0.1)
}

dd.seq.sf10 <-  dd.tab.ra10 - seq.sa
dd.seq.sf20 <-  dd.tab.ra20 - seq.sa
dd.seq.sf30 <-  dd.tab.ra30 - seq.sa

par(mfrow=c(1,2))
plot(seq.sa, dd.tab.ra10, type="p", col="blue", xlab="Sa", ylab="ddS", ylim=c(-20, 100), xlim=c(0, 50),xaxs="i",yaxs="i", main="ddS vs Sa", lwd=3)
lines(seq.sa, dd.tab.ra20,col="red", lwd=3, type="p")
lines(seq.sa, dd.tab.ra30,col="green", lwd=3, type="p")
lines(seq.sa[tab.ra10>=seq.sa], tab.ra10[tab.ra10>=seq.sa], col="blue", lwd=3)
lines(seq.sa[tab.ra20>=seq.sa], tab.ra20[tab.ra20>=seq.sa], col="red", lwd=3)
lines(seq.sa[tab.ra30>=seq.sa], tab.ra30[tab.ra30>=seq.sa],col="green", lwd=3)

plot(seq.sa, dd.seq.sf10, type="p", col="blue", xlab="Sa", ylab="ddSf", ylim=c(-20, 100), xlim=c(0, 50), xaxs="i", yaxs="i", main="ddSf vs Sa", lwd=3)
lines(seq.sa, dd.seq.sf20, col="red", lwd=3, type="p")
lines(seq.sa, dd.seq.sf30, col="green", lwd=3, type="p")
lines(seq.sa[tab.ra10>=seq.sa], seq.sf10[tab.ra10>=seq.sa], col="blue", lwd=3)
lines(seq.sa[tab.ra20>=seq.sa], seq.sf20[tab.ra20>=seq.sa], col="red", lwd=3)
lines(seq.sa[tab.ra30>=seq.sa], seq.sf30[tab.ra30>=seq.sa],col="green", lwd=3)

# cost
# enroll_ppt
# 
# total_cost <- 

#test edit by jon

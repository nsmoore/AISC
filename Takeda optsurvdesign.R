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
  tab.ra10 <- c(tab.ra10, get_S(Sa = seq.sa[k], ra = 10, lambda1 = log(2)/20, lambda0 = log(2)/10, d=1))
  tab.ra20 <- c(tab.ra20, get_S(Sa = seq.sa[k], ra = 20, lambda1 = log(2)/20, lambda0 = log(2)/10, d=1))
  tab.ra30 <- c(tab.ra30, get_S(Sa = seq.sa[k], ra = 30, lambda1 = log(2)/20, lambda0 = log(2)/10, d=1))
}

seq.sf10 <-  tab.ra10 - seq.sa
seq.sf20 <-  tab.ra20 - seq.sa
seq.sf30 <-  tab.ra30 - seq.sa


#DEFINE NEW VARIABLES:
#costpt = x.jk.pt = per-patient costs ($59.5k/patient) = pt recruitment + pt retention + RN or clinical RA + MD + clinical procedure + labs
#costsite = x.jk.site = per-site costs (est. $50k/site for recruitment?)  
  # = site recruitment + site retention*S + administrative staff*S + site monitoring*S*30 + x.jk.pt * number of planned pts per site
  # = site recruitment + S(site retention + administrative staff + site monitoring*30) + x.jk.pt * number of planned pts per site
  # = site recruitment + S(costm) + costpt * number of planned pts per site
#x.jk = per-study costs = costsite*Nsites = (costrecruitment + S(costm) + costpt * r)*N/r
#costm = Cost of each month in the study ($37k/day)
#N = number of patients = ra*Sa = ra*seq.Sa
#Nsites = number of sites = N/r
#r = average number of pts that can be accrued per site
#S = duration of trial in months
costpt <- 59.5
costm <- 37*30
costrecruitment <- 50
r <- 10
seq.N.ra10 <- seq.sa*10
seq.N.ra20 <- seq.sa*20
seq.N.ra30 <- seq.sa*30

#Total cost of study - Nick
# x.jk <- (costrecruitment + S(costm) + costpt * r)*N/r

x.jk.ra10<-c()
x.jk.ra20<-c()
x.jk.ra30<-c()
for(k in 1:length(seq.sa)){
  x.jk.ra10 <- c(x.jk.ra10,(costrecruitment + tab.ra10[k]*(costm) + costpt * r)*seq.N.ra10[k]/r)
  x.jk.ra20 <- c(x.jk.ra20,(costrecruitment + tab.ra20[k]*(costm) + costpt * r)*seq.N.ra20[k]/r)
  x.jk.ra30 <- c(x.jk.ra30,(costrecruitment + tab.ra30[k]*(costm) + costpt * r)*seq.N.ra30[k]/r)
}


# costs in thousands
# number of monitoring days = number of total days /30 (once a month)


set.seed(2015-04-13)


par(mfrow=c(1,2))
plot(seq.sa, tab.ra10, type="l", col="gray", xlab="Sa", ylab="S", ylim=c(-20, 100), xlim=c(0, 50),xaxs="i",yaxs="i", main="S vs Sa", lwd=3)
lines(seq.sa, tab.ra20,col="gray", lwd=3)
lines(seq.sa, tab.ra30,col="gray", lwd=3)
lines(seq.sa[tab.ra10>=seq.sa], tab.ra10[tab.ra10>=seq.sa], col="blue", lwd=3)
lines(seq.sa[tab.ra20>=seq.sa], tab.ra20[tab.ra20>=seq.sa], col="red", lwd=3)
lines(seq.sa[tab.ra30>=seq.sa], tab.ra30[tab.ra30>=seq.sa],col="green", lwd=3)
par(new=T)
plot(seq.sa, x.jk.ra10, type="l", col="gray", axes=F, xlab=NA, ylab=NA, cex=1.2)
lines(seq.sa, x.jk.ra20,col="gray", cex=1.2)
lines(seq.sa, x.jk.ra30,col="gray", cex=1.2)
lines(seq.sa[tab.ra10>=seq.sa], x.jk.ra10[tab.ra10>=seq.sa], col="blue", lwd=3)
lines(seq.sa[tab.ra20>=seq.sa], x.jk.ra20[tab.ra20>=seq.sa], col="red", lwd=3)
lines(seq.sa[tab.ra30>=seq.sa], x.jk.ra30[tab.ra30>=seq.sa],col="green", lwd=3)
axis(side = 4)
mtext(side = 4, line = 3, 'Cost ($k)')



plot(seq.sa, seq.sf10, type="l", col="gray", xlab="Sa", ylab="Sf", ylim=c(-20, 100), xlim=c(0, 50), xaxs="i", yaxs="i", main="Sf vs Sa", lwd=3)
lines(seq.sa, seq.sf20, col="gray", lwd=3)
lines(seq.sa, seq.sf30, col="gray", lwd=3)
lines(seq.sa[tab.ra10>=seq.sa], seq.sf10[tab.ra10>=seq.sa], col="blue", lwd=3)
lines(seq.sa[tab.ra20>=seq.sa], seq.sf20[tab.ra20>=seq.sa], col="red", lwd=3)
lines(seq.sa[tab.ra30>=seq.sa], seq.sf30[tab.ra30>=seq.sa],col="green", lwd=3)
par(new=T)
plot(seq.sa, x.jk.ra10, type="l", col="gray", axes=F, xlab=NA, ylab=NA, cex=1.2)
lines(seq.sa, x.jk.ra20,col="gray", cex=1.2)
lines(seq.sa, x.jk.ra30,col="gray", cex=1.2)
lines(seq.sa[tab.ra10>=seq.sa], x.jk.ra10[tab.ra10>=seq.sa], col="blue", lwd=3)
lines(seq.sa[tab.ra20>=seq.sa], x.jk.ra20[tab.ra20>=seq.sa], col="red", lwd=3)
lines(seq.sa[tab.ra30>=seq.sa], x.jk.ra30[tab.ra30>=seq.sa],col="green", lwd=3)
axis(side = 4)
mtext(side = 4, line = 3, 'Cost ($k)')

#Making data frame
inf_power <- data.frame(Sa=seq.sa,S.ra10=tab.ra10,Cost.ra10=x.jk.ra10,S.ra10=tab.ra20,Cost.ra20=x.jk.ra20,S.ra30=tab.ra30,Cost.ra30=x.jk.ra30)

#ggplot time
install.packages("ggplot2")
install.packages("tidyverse")
library(ggplot2)
library(grid)


#Godwin code
designs <- tibble(
  
  Sa = rep(seq.sa, 3),
  
  S = c(tab.ra10, tab.ra20, tab.ra30),
  
  Sf = S-Sa,
  
  ra = rep(c(10,20,30),each=length(seq.sa)),
  
  N = c(seq.sa*10, seq.sa*20, seq.sa*30),
  
  cost=(costrecruitment + S*(costd) + costpt * r)* N / r
  
  revenue = (number of months on patent from start of trial - S) * (projected revenue per month)

  
  # benefit
  
)

mincost <- group_by(designs,ra) %>%
  
  slice(which.min(cost))

# patient benefit https://www-ncbi-nlm-nih-gov.ezp-prod1.hul.harvard.edu/pmc/articles/PMC4854260/
# revenue: 800k-8M lost per day of delay https://www.prnewswire.com/news-releases/clinical-trial-delays-cost-pharmaceutical-companies-55044607.html

plot1 <- designs %>%
  
  na.omit() %>%
  
  filter(S<100 | cost<1e5) %>%
  
  ggplot(aes(x=Sa,y=S)) +
  
  geom_line(aes(color=factor(ra),lty=factor(Sf<0))) +
  
  labs(color="accrual rate") +
  
  guides(lty=F)



plot2 <- designs %>%
  
  na.omit() %>%
  
  filter(S<100 | cost<1e5) %>%
  
  ggplot(aes(x=Sa,y=cost)) +
  
  geom_line(aes(color=factor(ra),lty=factor(Sf<0))) +
  
  labs(color="accrual rate") +
  
  guides(lty=F)



grid.newpage()

grid.draw(rbind(ggplotGrob(plot1),
                
                ggplotGrob(plot2),
                
                size = "last"))




library(ggpubr)

ggarrange(plot1,
          
          plot2,
          
          ncol=1,
          
          nrow=2,
          
          common.legend = TRUE,
          
          legend="right")

######

#example plotting with 2 y axes
par(mar = c(5,5,2,5))
with(d, plot(x, logp, type="l", col="red3", 
             ylab=expression(-log[10](italic(p))),
             ylim=c(0,3)))

par(new = T)
with(d, plot(x, n, pch=16, axes=F, xlab=NA, ylab=NA, cex=1.2))
axis(side = 4)
mtext(side = 4, line = 3, 'Number genes selected')
legend("topleft",
       legend=c(expression(-log[10](italic(p))), "N genes"),
       lty=c(1,0), pch=c(NA, 16), col=c("red3", "black"))

###

#original
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


#original - no round
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

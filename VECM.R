### Co-integration test
library(urca)
library(forecast)
install.packages("readxl")
require(readxl)
install.packages("MTS")
require(MTS)
install.packages("vars")
library(vars)
install.packages("fUnitRoots")
library(fUnitRoots)

da=read_excel("FANG.xlsx")
amzn_ = da[,2]
fb_ = da[,3]
googl_ = da[,4]
nflx_ = da[,5]
amzn = ts(log(na.omit(as.numeric(unlist(amzn_[[1]])))))
fb = ts(log(na.omit(as.numeric(unlist(fb_[[1]])))))
googl = ts(log(na.omit(as.numeric(unlist(googl_[[1]])))))
nflx = ts(log(na.omit(as.numeric(unlist(nflx_[[1]])))))

z = cbind(amzn,fb,googl,nflx)
a = cbind(fb,googl)
zt = diffM(z,1)

acf(z[,1])
acf(z[,2])
acf(z[,3])
acf(z[,4])
acf(zt[,1])
acf(zt[,2])
acf(zt[,3])
acf(zt[,4])

adfTest(z[,1],lags=1,type="c")
adfTest(z[,2],lags=1,type="c")
adfTest(z[,3],lags=1,type="c")
adfTest(z[,4],lags=1,type="c")
adfTest(zt[,1],lags=1,type="c")
adfTest(zt[,2],lags=1,type="c")
adfTest(zt[,3],lags=1,type="c")
adfTest(zt[,4],lags=1,type="c")

# plot FANG
plot(merge(as.zoo(fb), as.zoo(amzn), as.zoo(googl), as.zoo(nflx)), 
     plot.type = "single", 
     lty = c(2, 1),
     lwd = 2,
     xlab = "Date",
     ylab = "Price",
     ylim = c(0, 15),
     main = "FANG")

lines(as.zoo(fb), 
      col = "black",
      lwd = 1,)
lines(as.zoo(amzn), 
      col = "grey",
      lwd = 2,)
lines(as.zoo(nflx), 
      col = "steelblue",
      lwd = 2,)
lines(as.zoo(googl), 
      col = "red",
      lwd = 1,)

# add horizontal line add 0
abline(0, 0)

# add a legend
legend("topright", 
       legend = c("FB", "AMZN", "NFLX", "GOOGL"),
       col = c("black", "grey", "steelblue", "red"),
       lwd = c(2, 2, 2),
       lty = c(1, 1, 1))

VARselect(a, lag.max = 10, type = "const")
VARselect(a, lag.max = 10, type = "const")$selection
cointest = ca.jo(a, K = 2, type= "eigen", ecdet = c("const"), spec = c("transitory"))
cointest@teststat[2]
cointest@teststat[1]
cointest@cval
summary(cointest)
ac = ca.jo(z, K = 5, ecdet = c("none"), spec = c("transitory"))
summary(ac)

ad = ca.jo(z, K = 5, ecdet = c("none"), type = c("trace"), spec = c("transitory"))
summary(ad)

m2=ca.jo(bnd,K=2,ecdet=c("none"))
summary(m2)

m3=ca.jo(bnd,K=2,ecdet=c("none"),spec=c("transitory"))
summary(m3)

m4=ca.jo(bnd,K=2,ecdet=c("none"),type=c("trace"),spec=c("transitory"))
summary(m4)

wt=z[,1]-0.002*z[,2]
adfTest(wt,lags=3,type="c")

############### Estimation of ECM model
m1=ECMvar1(z,3,wt) ## Given the co-integrated vector

m2=refECMvar1(m1)  ####### Refine the model fit

beta=c(1,-0.002) ### Initial value of co-integration
m3=ECMvar(z,3,beta,include.const=F) #### Joint estimation



#### 4-Macro Example
da=read.table("q-4macro.txt",header=T)

zt=cbind(log(da$rgnp),da$tb3m,log(da$m1sk),da$gs10)
colnames(zt) <- c("rgnp","tb3m","lnm1","gs10")
m1=VARorderI(zt)

summary(m2)


require(fUnitRoots)
require(urca)

m2=ca.jo(zt,K=5,ecdet=c("const"),spec=c("transitory"))
summary(m2)

m3=ca.jo(zt,K=5,ecdet=c("const"),spec=c("transitory"),type=c("trace"))
summary(m3)



w1t=zt[,1]-0.282*zt[,2]-0.792*zt[,3]+0.313*zt[,4]
w2t=zt[,1]-0.78*zt[,2]-0.673*zt[,3]+0.773*zt[,4]
adfTest(w1t,lags=6,type="c")


adfTest(w2t,lags=6,type="c")

MTSdiag(m3)  ### Model checking









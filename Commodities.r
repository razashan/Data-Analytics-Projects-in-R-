
##################### Commodity Project - Oil - Brendan Bouchaud, Thomas Grimault, Elias Essaoudini, Mouhaned Yousef #####################


source("C:/Users/Brend/OneDrive/Neoma Business School/Msc Finance & Big Data/Applied data science 2/Commodities - Pierre Six/GWPackage/Functions/gwco.R")
source("C:/Users/Brend/OneDrive/Neoma Business School/Msc Finance & Big Data/Applied data science 2/Commodities - Pierre Six/GWPackage/Functions/gwcoP.R")
source("C:/Users/Brend/OneDrive/Neoma Business School/Msc Finance & Big Data/Applied data science 2/Commodities - Pierre Six/GWPackage/Functions/plotWCO.R")
source("C:/Users/Brend/OneDrive/Neoma Business School/Msc Finance & Big Data/Applied data science 2/Commodities - Pierre Six/GWPackage/Functions/gwcoOutput.R")
source("C:/Users/Brend/OneDrive/Neoma Business School/Msc Finance & Big Data/Applied data science 2/Commodities - Pierre Six/GWPackage/Functions/gwcoP.R")
source("C:/Users/Brend/OneDrive/Neoma Business School/Msc Finance & Big Data/Applied data science 2/Commodities - Pierre Six/GWPackage/Functions/mpgwcoP.R")
source("C:/Users/Brend/OneDrive/Neoma Business School/Msc Finance & Big Data/Applied data science 2/Commodities - Pierre Six/GWPackage/Functions/mpgwcoOutput.R")
source("C:/Users/Brend/OneDrive/Neoma Business School/Msc Finance & Big Data/Applied data science 2/Commodities - Pierre Six/GWPackage/Functions/plotMPWCO.R")
source("C:/Users/Brend/OneDrive/Neoma Business School/Msc Finance & Big Data/Applied data science 2/Commodities - Pierre Six/GWPackage/Functions/allFunctions.R")
source("C:/Users/Brend/OneDrive/Neoma Business School/Msc Finance & Big Data/Applied data science 2/Commodities - Pierre Six/GWPackage/Functions/detC.R")

install.packages("doParallel", repos="http://R-Forge.R-project.org")
install.packages("R.matlab")



library(biwavelet)
library(doParallel)
library(matlab)
library("readxl")

# Import  data

data<-read_excel("C:/Users/Brend/OneDrive/Neoma Business School/Msc Finance & Big Data/Applied data science 2/Commodities - Pierre Six/Oil.xlsx")

# names

attach(data)

#########    Wavelet Parameters to compute the WTs    ##########

dt<-1/12
dj<-1/30
n.sur<-10
p=1
q=1

low.period<- 2*dt
up.period<-15

#########################################################################################################################
################################ For basis & Inventory   ###############################################################
#######################################################################################################################

# ----------- Wavelet Transforms of basis and Inventory -------------

WTY<-gwt(data$Basis,dt=dt,dj=dj,low.period=low.period,up.period=up.period)
WTX<- gwt(data$Ninv,dt=dt,dj=dj,low.period=low.period,up.period=up.period)

##### Smoothing parameters for coherency computation #####

wt.type ='ham'
wt.size =3
ws.type ='ham'
ws.size =3


# --------  Computation of Wavelet Coherency of basis & inventory --------  #

WCO<-gwco(data$Basis,data$Ninv,dt=dt,dj=dj,low.period=low.period,up.period=up.period,wt.type=wt.type,wt.size=wt.size,
          ws.type=ws.type,ws.size=ws.size)


#---------------------------        Computation of Phases -----------------------------##################

#  ------- 1/6 ~ 0.75 years freq. band -------
lowFP1<-1/6
upFP1<- 0.75

WCOAD1<-gwcoOutput(WCO,low.fp=lowFP1,up.fp=upFP1)
phase1<-WCOAD1$phase

# -------- 0.75~ 1.5 years freq. band -------

lowFP2<-0.75
upFP2<-1.5


WCOAD2<-gwcoOutput(WCO,low.fp=lowFP2,up.fp=upFP2)
phase2<-WCOAD2$phase

# -------- 1.5 ~ 6 years freq. band -------

lowFP3<- 1.5
upFP3<-6


WCOAD3<-gwcoOutput(WCO,low.fp=lowFP3,up.fp=upFP3)
phase3<-WCOAD3$phase

# ----------  Plot of Coherency of basis & inventory and Phase -------------------------------------  #
layout(matrix(c(1,2,3,4),2,2,byrow=TRUE))

plotWCO(WCO,pE=20,sig.levels=TRUE, lev=c(0.05,0.1), 
        main="Wavelet  Coherency: Basis & Inventory")
axis(side=2,at=log2(c(2,5,12,20)),lab=c(2,5,12,20),las=1)
axis(side=1, at=20*seq(0,5,1),lab=20*seq(0,5,1))


plot(phase1,type="l",col="blue",lwd=1.5,ylim=c(-pi,pi),xlab="time",ylab="phase",las=1,axes=FALSE,
     main = "Basis & Inventory Phase for 1/6 ~ 0.75 years freq. band" )
axis(side=1, at=120*seq(0,5,1),lab=10*seq(0,5,1))
axis(side=2, at=c(-pi,-(pi/2),0,pi/2,pi),lab=c(expression(-pi),expression(-pi/2),0,expression(pi/2),expression(pi)),las=1)
grid()
box()


plot(phase2,type="l",col="darkgreen",lwd=1.5,ylim=c(-pi,pi),xlab="time",ylab="phase",las=1,axes=FALSE,main="0.75~ 1.5 years freq. band")
axis(side=1, at=120*seq(0,5,1),lab=10*seq(0,5,1))
axis(side=2, at=c(-pi,-(pi/2),0,pi/2,pi),lab=c(expression(-pi),expression(-pi/2),0,expression(pi/2),expression(pi)),las=1)
grid()
box()


plot(phase3,type="l",col="red",lwd=2,ylim=c(-pi,pi),xlab="time",ylab="phase",las=1,axes=FALSE, main = "1.5 ~ 6 years freq. band")
axis(side=1, at=120*seq(0,5,1),lab=10*seq(0,5,1))
axis(side=2, at=c(-pi,-(pi/2),0,pi/2,pi),lab=c(expression(-pi),expression(-pi/2),0,expression(pi/2),expression(pi)),las=1)
grid()
box()

############################## COMENT ##########################################################
# Plot 1 : Red Noise in High periods

# Plot 2 : We can conclude a high volatility during a short time period, with a down trend

# Plot 3 : For a 0.75 to 1.5 year frequency band we can visualize a significant drop in the 2 last quarters 

# Plot 4 : For 1.5 to 6 years freq band we can conclude a more steady frequency band with declines in the last 2 quarters 


########### Basis & Inventory + Main Control (Hedging Pressure) ##############################

# ----- Create matrix with columns Basis,Inventory,Hedging Pressur  ----- #

X = cbind(data$Basis, data$Ninv, data$HP)

# ----- Computation of Partial Coherency of Basis ,Inventory  (controlling for Hedging Pressure) ---- #

index.p=2

PWCO<-mpgwcoP(X,dt=dt,dj=dj,coher.type='part',index.p=index.p,
              n.sur=n.sur,p=p,q=q,up.period=up.period)

#-----------------------        Computation of Phases  -----------------------------------#

#  ------- 1/6 ~ 0.75 years freq. band -------

adOut1<-mpgwcoOutput(PWCO,low.fp=lowFP1,up.fp=upFP1)
phase4<-adOut1$phase

# -------- 0.75~ 1.5 years freq. band -------

adOut2<-mpgwcoOutput(PWCO,low.fp=lowFP2,up.fp=upFP2)
phase5<-adOut2$phase

# -------- 1.5 ~ 6 years freq. band -------

adOut3<-mpgwcoOutput(PWCO,low.fp=lowFP3,up.fp=upFP3)
phase6<-adOut3$phase


# ----------  Plot of Partial Coherency of basis & inventory Controling for Hedging Pressure and Phase -------------------------------------  #

yMark = c(0.25,0.5,0.75,1,1.5,3,6,10,14)
plotMPWCO(PWCO,pE=3,sig.levels=TRUE, lev=c(0.05,0.1))
axis(side=2,at=log2(yMark),lab=yMark,las=1)
axis(side=1, at=seq(1,409,12),lab=seq(1986,2020,1))
box()

plot(phase4,type="l",col="blue",lwd=1.5,ylim=c(-pi,pi),xlab="time",ylab="phase",las=1,axes=FALSE,
     main = " 1/6 ~ 0.75 years freq. band" )
axis(side=1, at=120*seq(0,5,1),lab=10*seq(0,5,1))
axis(side=2, at=c(-pi,-(pi/2),0,pi/2,pi),lab=c(expression(-pi),expression(-pi/2),0,expression(pi/2),expression(pi)),las=1)
grid()
box()


plot(phase5,type="l",col="darkgreen",lwd=1.5,ylim=c(-pi,pi),xlab="time",ylab="phase",las=1,axes=FALSE,main="0.75~ 1.5 years freq. band")
axis(side=1, at=120*seq(0,5,1),lab=10*seq(0,5,1))
axis(side=2, at=c(-pi,-(pi/2),0,pi/2,pi),lab=c(expression(-pi),expression(-pi/2),0,expression(pi/2),expression(pi)),las=1)
grid()
box()


plot(phase6,type="l",col="red",lwd=2,ylim=c(-pi,pi),xlab="time",ylab="phase",las=1,axes=FALSE, main = "1.5 ~ 6 years freq. band")
axis(side=1, at=120*seq(0,5,1),lab=10*seq(0,5,1))
axis(side=2, at=c(-pi,-(pi/2),0,pi/2,pi),lab=c(expression(-pi),expression(-pi/2),0,expression(pi/2),expression(pi)),las=1)
grid()
box()

############################## COMENT ##########################################################

# Plot 1 : Red noise in the first 10 and last 7 years 

# Plot 2 : High volatility 

# Plot 3 : huge decline in the last 2 quarters

# Plot 4 : Decline in the 75% percentile 




########### Basis & Inventory + All the Controls ##############################

# ----- Create matrix with columns Basis,Inventory,Hedging Pressur + other controls  ----- #

K = cbind(data$Basis, data$Ninv, data$HP, data$vol, data$logPrice)

# ----- Computation of Partial Coherency of Basis ,Inventory  (controlling for Hedging Pressure) + Other Controls ---- #

index.p=2

PWCOK<-mpgwcoP(K,dt=dt,dj=dj,coher.type='part',index.p=index.p,
              n.sur=n.sur,p=p,q=q,up.period=up.period)

#-----------------------        Computation of Phases  -----------------------------------#

#  ------- 1/6 ~ 0.75 years freq. band -------

adOut4<-mpgwcoOutput(PWCOK,low.fp=lowFP1,up.fp=upFP1)
phase7<-adOut4$phase

# -------- 0.75~ 1.5 years freq. band -------

adOut5<-mpgwcoOutput(PWCOK,low.fp=lowFP2,up.fp=upFP2)
phase8<-adOut5$phase

# -------- 1.5 ~ 6 years freq. band -------

adOut6<-mpgwcoOutput(PWCOK,low.fp=lowFP3,up.fp=upFP3)
phase9<-adOut6$phase


# ----------  Plot of Partial Coherency of basis & inventory Controling for Hedging Pressure and Other controls and Phase -------------------------------------  #

yMark = c(0.25,0.5,0.75,1,1.5,3,6,10,14)
plotMPWCO(PWCOK,pE=3,sig.levels=TRUE, lev=c(0.05,0.1))
axis(side=2,at=log2(yMark),lab=yMark,las=1)
axis(side=1, at=seq(1,409,12),lab=seq(1986,2020,1))
box()

plotWCO(PWCOK,pE=5,sig.levels=TRUE, lev=c(0.05,0.1), 
        main="Wavelet Coherency and Phase: Basis and Invent")
axis(side=2,at=log2(c(2,5,12,20)),lab=c(2,5,12,20),las=1)
axis(side=1, at=seq(1,409,12),lab=seq(1986,2020,1))

plot(phase7,type="l",col="blue",lwd=1.5,ylim=c(-pi,pi),xlab="time",ylab="phase",las=1,axes=FALSE,
     main = " 1/6 ~ 0.75 years freq. band" )
axis(side=1, at=120*seq(0,5,1),lab=10*seq(0,5,1))
axis(side=2, at=c(-pi,-(pi/2),0,pi/2,pi),lab=c(expression(-pi),expression(-pi/2),0,expression(pi/2),expression(pi)),las=1)
grid()
box()


plot(phase8,type="l",col="darkgreen",lwd=1.5,ylim=c(-pi,pi),xlab="time",ylab="phase",las=1,axes=FALSE,main="0.75~ 1.5 years freq. band")
axis(side=1, at=120*seq(0,5,1),lab=10*seq(0,5,1))
axis(side=2, at=c(-pi,-(pi/2),0,pi/2,pi),lab=c(expression(-pi),expression(-pi/2),0,expression(pi/2),expression(pi)),las=1)
grid()
box()


plot(phase9,type="l",col="red",lwd=2,ylim=c(-pi,pi),xlab="time",ylab="phase",las=1,axes=FALSE, main = "1.5 ~ 6 years freq. band")
axis(side=1, at=120*seq(0,5,1),lab=10*seq(0,5,1))
axis(side=2, at=c(-pi,-(pi/2),0,pi/2,pi),lab=c(expression(-pi),expression(-pi/2),0,expression(pi/2),expression(pi)),las=1)
grid()
box()

############################## COMENT ##########################################################

# Plot 1 : throughout the years we can see the shift if red noise from mid periods to high periods

# Plot 2 : high implied volatility 

# Plot 3 : steadiness in the first 2 quarter, significant drop in phase in the last 2 quarters

# Plot 4 : drop in the 4th quarter , with very low volatility in the first 3 



#########################################################################################################################
################################ For Futures risk premium  & Hedging Pressure  ###############################################################
#######################################################################################################################

# ----------- Wavelet Futures risk premium  & Hedging Pressure -------------

WTy<-gwt(data$FutP,dt=dt,dj=dj,low.period=low.period,up.period=up.period)
WTx<-gwt(data$HP,dt=dt,dj=dj,low.period=low.period,up.period=up.period)

# --------  Computation of Wavelet Coherency ofFutures risk premium  & Hedging Pressure --------  #

WCo<-gwco(data$FutP,data$HP,dt=dt,dj=dj,low.period=low.period,up.period=up.period,wt.type=wt.type,wt.size=wt.size,
          ws.type=ws.type,ws.size=ws.size)


#---------------------------        Computation of Phases -----------------------------##################

#  ------- 1/6 ~ 0.75 years freq. band -------

WCOFH1<-gwcoOutput(WCo,low.fp=lowFP1,up.fp=upFP1)
phaseA<-WCOFH1$phase

# -------- 0.75~ 1.5 years freq. band -------

WCOFH2<-gwcoOutput(WCo,low.fp=lowFP2,up.fp=upFP2)
phaseB<-WCOFH2$phase

# -------- 1.5 ~ 6 years freq. band -------

WCOFH3<-gwcoOutput(WCo,low.fp=lowFP3,up.fp=upFP3)
phaseC<-WCOFH3$phase

# ----------  Plot of Coherency of Futures risk premium  & Hedging Pressure and Phase -------------------------------------  #
layout(matrix(c(1,2,3,4),2,2,byrow=TRUE))

plotWCO(WCo,pE=5,sig.levels=TRUE, lev=c(0.05,0.1), 
        main="Wavelet  Coherency: Future risk premium & Hedging Pressure and Phase")
axis(side=2,at=log2(c(2,5,12,20)),lab=c(2,5,12,20),las=1)
axis(side=1, at=seq(1,409,12),lab=seq(1986,2020,1))


plot(phaseA,type="l",col="blue",lwd=1.5,ylim=c(-pi,pi),xlab="time",ylab="phase",las=1,axes=FALSE,
     main = " 1/6 ~ 0.75 years freq. band" )
axis(side=1, at=120*seq(0,5,1),lab=10*seq(0,5,1))
axis(side=2, at=c(-pi,-(pi/2),0,pi/2,pi),lab=c(expression(-pi),expression(-pi/2),0,expression(pi/2),expression(pi)),las=1)
grid()
box()


plot(phaseB,type="l",col="blue",lwd=1.5,ylim=c(-pi,pi),xlab="time",ylab="phase",las=1,axes=FALSE,
     main = " 1/6 ~ 0.75 years freq. band" )
axis(side=1, at=120*seq(0,5,1),lab=10*seq(0,5,1))
axis(side=2, at=c(-pi,-(pi/2),0,pi/2,pi),lab=c(expression(-pi),expression(-pi/2),0,expression(pi/2),expression(pi)),las=1)
grid()
box()


plot(phaseC,type="l",col="blue",lwd=1.5,ylim=c(-pi,pi),xlab="time",ylab="phase",las=1,axes=FALSE,
     main = " 1/6 ~ 0.75 years freq. band" )
axis(side=1, at=120*seq(0,5,1),lab=10*seq(0,5,1))
axis(side=2, at=c(-pi,-(pi/2),0,pi/2,pi),lab=c(expression(-pi),expression(-pi/2),0,expression(pi/2),expression(pi)),las=1)
grid()
box()


############################## COMENT ##########################################################

# Plot 1 : Risk premium and Hedging pressure eliminates the high red noise throughout the time

# Plot 2 : in small periods we can visualize lower volatility

# Plot 3 : abnormal spikes in phases appears 

# Plot 4 : decline in the 50th percentile and a incline in the 80th percentile 



########### Futures risk premium  & Hedging Pressure + Main Control (Inventory)######################

# ----- Create matrix with columns Fture Risk Premium & Hedging Pressure + iNVENTORY ------- #

x = cbind(data$FutP, data$HP, data$Ninv)

# ----- Computation of Partial Coherency of  Future Risk Premium & Hedging Pressure + iNVENTORY) ---- #

index.p=2

PWCOF<-mpgwcoP(x,dt=dt,dj=dj,coher.type='part',index.p=index.p,
                n.sur=n.sur,p=p,q=q,up.period=up.period)

#-----------------------        Computation of Phases  -----------------------------------#

#  ------- 1/6 ~ 0.75 years freq. band -------

adOuF1<-mpgwcoOutput(PWCOF,low.fp=lowFP1,up.fp=upFP1)
phaseD<-adOuF1$phase

# -------- 0.75~ 1.5 years freq. band -------

adOuF2<-mpgwcoOutput(PWCOF,low.fp=lowFP2,up.fp=upFP2)
phaseE<-adOuF2$phase

# -------- 1.5 ~ 6 years freq. band -------

adOuF3<-mpgwcoOutput(PWCOF,low.fp=lowFP3,up.fp=upFP3)
phaseF<-adOuF3$phase


# ----------  Plot of Partial Coherency of Future Risk Premium & Hedging Pressure + iNVENTORY)----------------# 

yMark = c(0.25,0.5,0.75,1,1.5,3,6,10,14)
plotMPWCO(PWCOF,pE=3,sig.levels=TRUE, lev=c(0.05,0.1))
axis(side=2,at=log2(yMark),lab=yMark,las=1)
axis(side=1, at=seq(1,409,12),lab=seq(1986,2020,1))
box()

plot(phaseD,type="l",col="blue",lwd=1.5,ylim=c(-pi,pi),xlab="time",ylab="phase",las=1,axes=FALSE,
     main = " 1/6 ~ 0.75 years freq. band" )
axis(side=1, at=120*seq(0,5,1),lab=10*seq(0,5,1))
axis(side=2, at=c(-pi,-(pi/2),0,pi/2,pi),lab=c(expression(-pi),expression(-pi/2),0,expression(pi/2),expression(pi)),las=1)
grid()
box()


plot(phaseE,type="l",col="darkgreen",lwd=1.5,ylim=c(-pi,pi),xlab="time",ylab="phase",las=1,axes=FALSE,main="0.75~ 1.5 years freq. band")
axis(side=1, at=120*seq(0,5,1),lab=10*seq(0,5,1))
axis(side=2, at=c(-pi,-(pi/2),0,pi/2,pi),lab=c(expression(-pi),expression(-pi/2),0,expression(pi/2),expression(pi)),las=1)
grid()
box()


plot(phaseF,type="l",col="red",lwd=2,ylim=c(-pi,pi),xlab="time",ylab="phase",las=1,axes=FALSE, main = "1.5 ~ 6 years freq. band")
axis(side=1, at=120*seq(0,5,1),lab=10*seq(0,5,1))
axis(side=2, at=c(-pi,-(pi/2),0,pi/2,pi),lab=c(expression(-pi),expression(-pi/2),0,expression(pi/2),expression(pi)),las=1)
grid()
box()

############################## COMENT ##########################################################

# Plot 1 : few red noises, concentration in the middle between periods 3 - 6

# Plot 2 : abnormal spike at the beginning then low volatility

# Plot 3 : abnormal spikes in phases appear

# Plot 4 : decline in the 25th percentile and then increase  gradually 

########### Futures risk premium  & Hedging Pressure + All the Controls ##############################

# ----- Create matrix with columns Futures risk premium  & Hedging Pressure + inventory as control + all other controls ------- #

k = cbind(data$FutP, data$HP, data$Ninv, data$vol, data$logPrice, data$SP500FutP, data$Q)

# ----- Computation of Partial Coherency of Futures risk premium  & Hedging Pressure + inventory as control + all other controls ---- #

index.p=2

PWCof<-mpgwcoP(k,dt=dt,dj=dj,coher.type='part',index.p=index.p,
               n.sur=n.sur,p=p,q=q,up.period=up.period)

#-----------------------        Computation of Phases  -----------------------------------#

#  ------- 1/6 ~ 0.75 years freq. band -------

adOuF4<-mpgwcoOutput(PWCof,low.fp=lowFP1,up.fp=upFP1)
phaseG<-adOuF4$phase

# -------- 0.75~ 1.5 years freq. band -------

adOuF5<-mpgwcoOutput(PWCof,low.fp=lowFP2,up.fp=upFP2)
phaseH<-adOuF5$phase

# -------- 1.5 ~ 6 years freq. band -------

adOuF6<-mpgwcoOutput(PWCof,low.fp=lowFP3,up.fp=upFP3)
phaseI<-adOuF6$phase


# ----------  Plot of Partial Coherency of Futures risk premium  & Hedging Pressure + inventory as control + all other controls and Phase -------------------------------------  #

yMark = c(0.25,0.5,0.75,1,1.5,3,6,10,14)
plotMPWCO(PWCof,pE=3,sig.levels=TRUE, lev=c(0.05,0.1))
axis(side=2,at=log2(yMark),lab=yMark,las=1)
axis(side=1, at=seq(1,409,12),lab=seq(1986,2020,1))
box()

plot(phaseG,type="l",col="blue",lwd=1.5,ylim=c(-pi,pi),xlab="time",ylab="phase",las=1,axes=FALSE,
     main = " 1/6 ~ 0.75 years freq. band" )
axis(side=1, at=120*seq(0,5,1),lab=10*seq(0,5,1))
axis(side=2, at=c(-pi,-(pi/2),0,pi/2,pi),lab=c(expression(-pi),expression(-pi/2),0,expression(pi/2),expression(pi)),las=1)
grid()
box()


plot(phaseH,type="l",col="darkgreen",lwd=1.5,ylim=c(-pi,pi),xlab="time",ylab="phase",las=1,axes=FALSE,main="0.75~ 1.5 years freq. band")
axis(side=1, at=120*seq(0,5,1),lab=10*seq(0,5,1))
axis(side=2, at=c(-pi,-(pi/2),0,pi/2,pi),lab=c(expression(-pi),expression(-pi/2),0,expression(pi/2),expression(pi)),las=1)
grid()
box()


plot(phaseI,type="l",col="red",lwd=2,ylim=c(-pi,pi),xlab="time",ylab="phase",las=1,axes=FALSE, main = "1.5 ~ 6 years freq. band")
axis(side=1, at=120*seq(0,5,1),lab=10*seq(0,5,1))
axis(side=2, at=c(-pi,-(pi/2),0,pi/2,pi),lab=c(expression(-pi),expression(-pi/2),0,expression(pi/2),expression(pi)),las=1)
grid()
box()


############################## COMENT ##########################################################
# Plot 1 : few red noises, concentration in the middle 

# Plot 2 : lower volatility 

# Plot 3 : Low phases with some spikes 

# Plot 4 : decline in the first 25th percentile then and incline in the 75th percentile

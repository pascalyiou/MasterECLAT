## TP sur le calcul de parametres d'extremes de donnees
## de temperature et precipitation
## autour de Paris, dans une simulation du modele IPSL pour CMIP6
## On regarde les differences entre 1850-2000 et 2010-2100 en RCP8.5
## Pascal Yiou (LSCE), Janvier 2021

## Variables d'environnement (pour macOSX ou linux) a modifier selon les
## besoins
SI=Sys.info()
if(SI[[1]] == "Darwin"){
  Rsource="/Users/yiou/programmes/RStat/MasterECLAT/"
  DATdir="/Users/yiou/data/ECLAT/"
  OUTdir=DATdir
}
if(SI[[1]] == "Linux"){
  Rsource="/home/users/yiou/RStat/MasterECLAT/"
  DATdir="/home/estimr2/yiou/CMIP6/"
  OUTdir="/home/estimr1/yiou/estimr1/XSCOPE/"
}

## Demande d'avoir installe les packages "ncdf4" et "extRemes"
## install.packages(c("ncdf4","extRemes"))
update.packages(c("ncdf4","extRemes"))
library(ncdf4)
library(extRemes)
source(paste(Rsource,"caldat.R",sep="")) ## Contient la fonction "caldat"
setwd(DATdir)


## pr=precipitation
## tas=temperature de surface
list.var=c("pr","tas")
varname="pr"

## Lecture des donnees des fichiers ncdf
fname = paste(varname,"_day_IPSL-CM6A-LR_paris.nc",sep="")

nc=nc_open(fname)
varnc=nc$var[[varname]]
ttdum=nc$dim$time$vals
XX=ncvar_get(nc,varnc)
if(varname=="pr"){
    XX=XX*86400 ## Conversion des kg m-2 s-1 en mm jour-1
}else{
    XX=XX-273 ## Conversion des K en °C
}
## Mettre l'axe des temps (sous forme de jours depuis le 1 janvier 1850
## sous forme (annee, mois, jour)
conv.time=caldat(ttdum+julday(1,1,1850))
nc_close(nc)

## Question
## A quoi ressemblent XX, conv.time?
length(XX)
quantile(XX)
names(conv.time)

## On va definir deux periodes:
## P1: 1850 - 2000
## P2: 2010 - 2100

## Calcul des cumuls annuels: fonction tapply avec sum
XX1.sum = tapply(XX[conv.time$year <= 2000], conv.time$year[conv.time$year <= 2000],sum)
XX2.sum = tapply(XX[conv.time$year >= 2010], conv.time$year[conv.time$year >= 2010],sum)

## Calcul des cumuls d'ete (JJA): fonction tapply avec sum sur les mois 6 a 8
XX1.JJA.sum = tapply(XX[conv.time$year <= 2000 & conv.time$month %in% c(6:8)],
                     conv.time$year[conv.time$year <= 2000 & conv.time$month %in% c(6:8)],sum)
XX2.JJA.sum = tapply(XX[conv.time$year >= 2010 & conv.time$month %in% c(6:8)],
                     conv.time$year[conv.time$year >= 2010 & conv.time$month %in% c(6:8)],sum)

## Calcul des max annuels: fonction tapply avec max
XX1.max = tapply(XX[conv.time$year <= 2000],
                 conv.time$year[conv.time$year <= 2000],max)
XX2.max = tapply(XX[conv.time$year >= 2010], conv.time$year[conv.time$year >= 2010],max)

## Calcul des max d'ete (JJA): fonction tapply avec max sur les mois 6 a 8
XX1.JJA.max = tapply(XX[conv.time$year <= 2000 & conv.time$month %in% c(6:8)],
                     conv.time$year[conv.time$year <= 2000 & conv.time$month %in% c(6:8)],max)
XX2.JJA.max = tapply(XX[conv.time$year >= 2010 & conv.time$month %in% c(6:8)],
                     conv.time$year[conv.time$year >= 2010 & conv.time$month %in% c(6:8)],max)

## Exercice 1:
## Cumuls et max du printemps: MAM

## Exercice 2 (optionel):
## Cumuls et max d'hiver: DJF
## Attention, piege!


## Essai de calcul de GEV/GPD sur une variable synthetique:
## z: simulation d'une variable aleatoire de GEV
## (donc les max d'une autre variable)
z <- revd(100, loc=30, scale=7, shape=0.1)
## z.gev: parametres de GEV de z
z.gev <- fevd(z)
## A quoi ca ressemble?
z.gev
names(z.gev)
names(z.gev$results)

z.summ=summary(z.gev)
z.summ$par
z.summ$se.theta

## Trace des diagnostics
plot(z.gev)

## Calcul de niveaux de retour
return.level(z.gev, return.period=c(2,20,100), do.ci=TRUE)

## Probabilites estimees et periodes de retour
pextRemes(z.gev, q=quantile(z, probs=c(0.85, 0.95, 0.99)), lower.tail=FALSE)
## Ici, q est un ensemble de niveaux de retour
z.gev.rt = 1/(1-pextRemes(z.gev, q=c(50,60,70)))

## Exemple de boostrap sur les valeurs de z
nboot=100
nz=length(z)
z.gev.boot=c()
for(i in 1:nboot){
## Tirage d'un sous-echantillon de taille aleatoire de z    
    z.dum=sample(z,size=nz,replace=TRUE)
    z.dum.gev = fevd(z.dum)
    z.gev.boot = rbind(z.gev.boot,z.dum.gev$results$par)
}
apply(z.gev.boot,2,sd)
## Comparer avec z.summ$se.theta

## En pratique, c'est un peu plus complique a faire pour GPD:
## il peut y avoir plusieurs depassements de seuil par an
## La procedure de sous echantillonnage doit se faire sur les depassements
## de seuil (par sur les jours).

## Calculs de GEV sur les max annuels et JJA
XX1.gev=fevd(XX1.max)
XX2.gev=fevd(XX2.max)
XX1.JJA.gev=fevd(XX1.JJA.max)
XX2.JJA.gev=fevd(XX2.JJA.max)

## Calculs de GPD sur les valeurs annuelles et JJA
##Determination d'un seuil
## Regarder l'influence du quantile (ici quant=0.95)
u0=quantile(XX,probs=0.95)
## u0 = quantile(XX[XX>0],probs=0.95)
## Seuils sur chaque periode
u1 = quantile(XX[conv.time$year <= 2000],probs=0.95)
u2 = quantile(XX[conv.time$year >= 2010],probs=0.95)
## Est-ce que ca change si on ne considere que les jours ou il pleut?
##u1 = quantile(XX[conv.time$year <= 2000 & XX>0],probs=0.9)
##u2 = quantile(XX[conv.time$year >= 2010 & XX>0],probs=0.9)

## Seuils sur chaque periode en JJA
u1.JJA = quantile(XX[conv.time$year <= 2000 & conv.time$month %in% c(6:8)],probs=0.95)
u2.JJA = quantile(XX[conv.time$year >= 2010 & conv.time$month %in% c(6:8)],probs=0.95)
## Est-ce que ca change si on ne considere que les jours ou il pleut?
##u1.JJA = quantile(XX[conv.time$year <= 2000 & conv.time$month %in% c(6:8) & XX>0],probs=0.9)
##u2.JJA = quantile(XX[conv.time$year >= 2010 & conv.time$month %in% c(6:8) & XX>0],probs=0.9)
## type="GP" veut dire "Generalized Pareto" distribution

XX1.gpd = fevd(XX[conv.time$year <= 2000],
               type="GP",threshold=u1,time.units = "365.25/year")
XX1.JJA.gpd = fevd(XX[conv.time$year <= 2000 & conv.time$month %in% c(6:8)],
                   threshold=u1,type="GP",time.units = "92/year")
XX2.gpd = fevd(XX[conv.time$year >= 2010],
               type="GP",threshold=u1,time.units = "365.25/year")
XX2.JJA.gpd = fevd(XX[conv.time$year >= 2010 & conv.time$month %in% c(6:8)],
                   threshold=u1,type="GP",time.units = "92/year")

## Calculs de niveaux de retour pour des periodes de retour de
## 50, 100 et 200 ans
XX1.gev.rl = return.level(XX1.gev, return.period=c(50,100,200), do.ci=TRUE)
XX1.gpd.rl = return.level(XX1.gpd, return.period=c(50,100,200), do.ci=TRUE)
XX2.gev.rl = return.level(XX2.gev, return.period=c(50,100,200), do.ci=TRUE)
XX2.gpd.rl = return.level(XX2.gpd, return.period=c(50,100,200), do.ci=TRUE)

## Calculs de periodes de retour pour des niveaux "hauts"
pextRemes(fit, q=c(17, 20, 25, 30), lower.tail=FALSE)
XX1.gev.rt = 1/(1-pextRemes(XX1.gev, q=c(80,100,120)))

## Figures sur GEV/GPD
plot(XX1.gev)

## Niveaux de retour: "rl"
plot(XX1.gev,type="rl")

## ---------------------------------------------------------------------
## Figures en codes barres
## Petites manipulations pour faciliter les boxplots
XX.max = list(XX1.max,XX2.max)
XX.max.range=range(unlist(XX.max))

XX.sum = list(XX1.sum,XX2.sum)
XX.sum.range = range(unlist(XX.sum))

XX.JJA.sum = list(XX1.JJA.sum,XX2.JJA.sum)
XX.JJA.sum.range = range(unlist(XX.JJA.sum))
## Maxima annuels
pdf(paste(varname,"-max.pdf",sep=""))
layout(matrix(1:2,2,1))
par(mar=c(2,3,1,1))

plot(XX.max.range,c(0,1),type="n",axes=FALSE, xlab="",ylab="")
lines(XX1.max,rep(1,length=length(XX1.max)),col="blue",type="h")
lines(XX2.max,rep(1,length=length(XX2.max)),col="red",type="h")
axis(side=1)
box()

par(mar=c(4,3,1,1))
plot(XX.max.range,c(0.5,2.5),type="n",axes=FALSE,
     xlab=paste("max",varname), ylab="")
boxplot(XX.max,add=TRUE,axes=FALSE,horizontal=TRUE,col=c("blue","red"))
axis(side=1)
axis(side=2,labels=c("T < 2001","T > 2009"), at = c(1,2))
box()
dev.off()

## Maxima d'ete
pdf(paste(varname,"-JJA-max.pdf",sep=""))
layout(matrix(1:2,2,1))
par(mar=c(2,3,1,1))

plot(XX.max.range,c(0,1),type="n",axes=FALSE, xlab="",ylab="")
lines(XX1.JJA.max,rep(1,length=length(XX1.max)),col="blue",type="h")
lines(XX2.JJA.max,rep(1,length=length(XX2.max)),col="red",type="h")
axis(side=1)
box()

par(mar=c(4,3,1,1))
plot(XX.max.range,c(0.5,2.5),type="n",axes=FALSE,
     xlab=paste("max",varname,"(JJA)"),ylab="")
boxplot(XX.max,add=TRUE,axes=FALSE,horizontal=TRUE,col=c("blue","red"))
axis(side=1)
axis(side=2,labels=c("T < 2001","T > 2009"), at = c(1,2))
box()
dev.off()

## Cumuls annuels
pdf(paste(varname,"-sum.pdf",sep=""))
layout(matrix(1:2,2,1))
par(mar=c(2,3,1,1))

plot(XX.sum.range,c(0,1),type="n",axes=FALSE, xlab="",ylab="")
lines(XX1.sum,rep(1,length=length(XX1.max)),col="blue",type="h")
lines(XX2.sum,rep(1,length=length(XX2.max)),col="red",type="h")
axis(side=1)
box()

par(mar=c(4,3,1,1))
plot(XX.sum.range,c(0.5,2.5),type="n",axes=FALSE,
     xlab=paste("sum",varname),ylab="")
boxplot(XX.sum,add=TRUE,axes=FALSE,horizontal=TRUE,col=c("blue","red"))
axis(side=1)
axis(side=2,labels=c("T < 2001","T > 2009"), at = c(1,2))
box()
dev.off()

## Cumuls JJA
pdf(paste(varname,"JJA-sum.pdf",sep=""))
layout(matrix(1:2,2,1))
par(mar=c(2,3,1,1))

plot(XX.JJA.sum.range,c(0,1),type="n",axes=FALSE, xlab="",ylab="")
lines(XX1.JJA.sum,rep(1,length=length(XX1.JJA.sum)),col="blue",type="h")
lines(XX2.JJA.sum,rep(1,length=length(XX2.JJA.sum)),col="red",type="h")
axis(side=1)
box()

par(mar=c(4,3,1,1))
plot(XX.JJA.sum.range,c(0.5,2.5),type="n",axes=FALSE,
     xlab=paste("sum",varname,"(JJA)",sep=""),ylab="")
boxplot(XX.JJA.sum,add=TRUE,axes=FALSE,horizontal=TRUE,col=c("blue","red"))
axis(side=1)
axis(side=2,labels=c("T < 2001","T > 2009"), at = c(1,2))
box()
dev.off()

q("no")

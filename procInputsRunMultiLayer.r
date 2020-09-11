setwd("F:/UH-work/PREBAS simulation/oldForest/anu")
library(data.table)
library(Rprebasso)
library(ggplot2)


coord <- read.csv("inputs/coords.csv",header = T)
load("inputs/weather.rdata")
initVarX <- data.table(read.csv("inputs_multilayer/initVar.csv",header = T))
nLayers <- ncol(initVarX) - 2
sites <- as.character(unique(initVarX$siteID)) ## unique site number
siteX <- which(coord[,1] %in% initVarX$siteID) ## %in% means if the element on the left is exist in the right, if it does, then true, otherwise faulse
nSites <- length(sites)
initVar <- array(NA,dim=c(nSites,7,nLayers))
for(i in 1:nSites){
  initVar[i,,] <- as.matrix(initVarX[siteID==sites[i]][,2:(nLayers+1)])
}
initVar[apply(initVar, 1, function(x)!any(is.na(x))), , , drop=F]
initVar

initVar[is.na(initVar)] <- 0. ### here is the problem where make the figures showing 0, but if it use NA, the prebas doesn't work
initVar[,2,] <- 1
siteInfo <- read.csv("inputs_multilayer/siteInfo.csv",header = T)
siteInfo <- siteInfo[siteX,]
siteInfo$climID <- newClimIDs[siteX]

obsData <- data.table(read.csv("inputs_multilayer/obsData.csv",header = T))
obsData$SiteID <- match(obsData$SiteID,siteInfo$siteID)  
siteInfo$siteID <- 1:nSites
nYears <- obsData[,max(SimYear),by=SiteID]$V1
initPrebas <- InitMultiSite(nYearsMS = nYears,
                            siteInfo = siteInfo,
                            multiInitVar = initVar,
                            defaultThin = 0.,
                            ClCut = 0.,
                            PAR=PAR,
                            TAir = TAir,
                            CO2 = CO2,
                            VPD = VPD,
                            Precip = Precip
                            )

#initPrebas$pCROBAS[17,] <- initPrebas$pCROBAS[17,]
modOut <- multiPrebas(initPrebas)
obsX <- cbind(obsData[,c(1,3,4,2)],1)
# obsData$SimYear[which(obsX<1,arr.ind=T)[,1]] <- 1   ####!!!!!to check
# obsX[which(obsX<1,arr.ind=T)[,1]][,2] <- 1 ####!!!!!to check

simData <- modOut$multiOut[as.matrix(obsX)]
obsData[,mods:=simData]
save(obsData,modOut,file="output/modOut.rdata")
varXs <- unique(obsData$nvar)
for( i in varXs){
  print(ggplot(obsData[nvar==i],aes(x=mods,y=Value),cex = c(3,3,3)) + xlim(0.0001,NA)+geom_point()+ggtitle(varNames[i]))
}  ##add xlim(0.001,NA) to remove the x=o points

initVar[,6,]
initPrebas$multiInitVar[,6,]

## multi initVar has some negtive values for Hc, but even if I change them with positive values, prebas still have errors
## and the error was about Lc rather than Hc. ##Error in if (any(LcCheck < 0)) return("check, some Lc is negative") : 
## missing value where TRUE/FALSE needed


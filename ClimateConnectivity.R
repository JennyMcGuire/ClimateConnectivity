library (plyr)

setwd("/Users/jennymcguire/Dropbox/Research/Corridors/Round_5/GitHub/")

#this is a list of the mean MAT of each core
MAT <- read.csv("Data/MAT.csv")

#this is a list of corridors, the cores that they connect, their lengths & costs
#"Euc_Dist"   "CW_Dist"    "LCP_Length" "Origin"     "Dest"
#skip to line 15 if not using corridor connections
AllUSCorridors<-read.csv("Data/AllUSCorridors.csv")

Links<-as.data.frame(AllUSCorridors)

#this is a table describing the connectivity of adjacent cores: 
"Core1" "Core2"
#note that these connections are not ordered by warmer to colder cores; that is done here
Neighbors<-read.csv("Data/Neighbors.csv")

#First we will determine the Origin (warmer) & Dest (cooler) Cores for all adjacent core pairs

Neighbors<-cbind(Neighbors,Origin=0, Dest=0)

#joining the temperature of each set of cores (Core1 & Core2)
colnames(Neighbors)[1]<-"Cores"
Neighbors<-join(Neighbors, MAT, type= "inner", by= "Cores")
colnames(Neighbors)[colnames(Neighbors)=="Mean"]<-"Mean1"
colnames(Neighbors)[colnames(Neighbors)=="Cores"]<-"Cores1"

colnames(Neighbors)[2]<-"Cores"
Neighbors<-join(Neighbors, MAT, type= "inner", by= "Cores")
colnames(Neighbors)[colnames(Neighbors)=="Mean"]<-"Mean2"
colnames(Neighbors)[colnames(Neighbors)=="Cores"]<-"Cores2"

n<-nrow(Neighbors)

#lists the hotter core # as the "Origin" & cooler core # as the "Dest"
for (i in 1:n)
{
  if (Neighbors$Mean1[i]>Neighbors$Mean2[i])
  {Neighbors$Origin[i]<-Neighbors$Cores1[i]
   Neighbors$Dest[i]<-Neighbors$Cores2[i]}
  
  else {Neighbors$Origin[i]<-Neighbors$Cores2[i]
        Neighbors$Dest[i]<-Neighbors$Cores1[i]}
}

#Delete the temperatures used for det. origin & Dest.
Neighbors<-Neighbors[,(1:4)]

#Joins Mean MATs for Neighbor Origin & Dest Cores
colnames(Neighbors)[colnames(Neighbors)=="Origin"]<-"Cores"
Neighbors<-join(Neighbors, MAT, type= "inner", by= "Cores")
colnames(Neighbors)[colnames(Neighbors)=="Mean"]<-"MeanO"
colnames(Neighbors)[colnames(Neighbors)=="Cores"]<-"Origin"

colnames(Neighbors)[colnames(Neighbors)=="Dest"]<-"Cores"
Neighbors<-join(Neighbors, MAT, type= "inner", by= "Cores")
colnames(Neighbors)[colnames(Neighbors)=="Mean"]<-"MeanD"
colnames(Neighbors)[colnames(Neighbors)=="Cores"]<-"Dest"

Neighbors<-cbind(Neighbors, eucDist=0, lcDist=0, lcpLength=0, cwdToEucRatio=0, cwdToPathRatio=0)

##Next we will combine the table of adjacent connections with the table of corridor connections
##If you do not use corridor connectivity uncomment the following line, run it, and then skip to line 114
#LinkStep<-Neighbors

##uncomment this section to use a quantiles of corridor linkages or skip to line 80 to use all; quant is the desired quantile
#DataStep<-function(quant){
  
  ##If you want to exclude corridors based upon LCD:
  #pct<-quantile(Links$lcDist, probs= quant, na.rm=TRUE)
  #del<-which(Links$lcDist>=pct)
  #LinkStep<-Links[-del,]
  
  ##If you want to exclude based upon CWD/Path-dist
  #pct<-quantile(Links$cwdToPathRatio, probs= quant, na.rm=TRUE)
  #del<-which(Links$cwdToPathRatio>=pct)
  #LinkStep<-Links[-del,]
  
  ##If you want the entire set of linkages:
  LinkStep<-Links
#}

#renumber & reorder "Links", the file that contains corridor costs
LinkStep<-LinkStep[order(LinkStep$Origin,LinkStep$Dest),]
row.names(LinkStep)<-seq.int(1,nrow(LinkStep))

#Join the Mean MATs of "Links" Origin & Dest Cores
colnames(LinkStep)[colnames(LinkStep)=="Origin"]<-"Cores"
LinkStep<- join(LinkStep, MAT, type= "inner", by= "Cores")
colnames(LinkStep)[colnames(LinkStep)=="Mean"]<-"MeanO"
colnames(LinkStep)[colnames(LinkStep)=="Cores"]<-"Origin"

colnames(LinkStep)[colnames(LinkStep)=="Dest"]<-"Cores"  
LinkStep<- join(LinkStep, MAT, type= "inner", by= "Cores")
colnames(LinkStep)[colnames(LinkStep)=="Mean"]<-"MeanD"
colnames(LinkStep)[colnames(LinkStep)=="Cores"]<-"Dest"

#Prepare to merge neighbors & links; I use specific column names later
colnames(LinkStep)[colnames(LinkStep)=="CW_Dist"]<-"lcDist"
colnames(LinkStep)[colnames(LinkStep)=="Euc_Dist"]<-"eucDist"
colnames(LinkStep)[colnames(LinkStep)=="LCP_Length"]<-"lcpLength"

#Merge dataset of neighbors & links with mean MATs
LinkStep<-rbind(LinkStep, cbind(Origin=Neighbors$Origin, Dest=Neighbors$Dest, eucDist=Neighbors$eucDist, lcDist=Neighbors$lcDist, lcpLength=Neighbors$lcpLength, MeanO=Neighbors$MeanO, MeanD=Neighbors$MeanD))
LinkStep<-LinkStep[order(LinkStep$Origin,LinkStep$Dest),]
row.names(LinkStep)<-seq.int(1,nrow(LinkStep))

##can test for any duplicates once more
#LinkStep<-cbind(LinkStep, corecode=paste(as.character(LinkStep$Origin),as.character(LinkStep$Dest)))
#dupcorecode<-which(duplicated(LinkStep$corecode)==TRUE)

#Create some files!

#Create a list of connections from hot to cold + the costs for each if using corridors
connections<-as.data.frame(cbind(Origin=LinkStep$Origin,Dest=LinkStep$Dest, EucDist=LinkStep$eucDist, LeastCost=LinkStep$lcDist, LCP=LinkStep$lcpLength))
connections<-unique(connections)
#write.csv(connections, file="Output/connections.csv")

#Create a list of coreIDs
coreID<-unique(c(LinkStep$Origin,LinkStep$Dest))
coreID<-as.data.frame(coreID, colname="Cores")
coreID<-as.data.frame((coreID[order(coreID[,1]),]))
colnames(coreID)[1]<-"Cores"
#write.csv(coreID, file="Output/coreID_ALL.csv")

#Create a list of temps
temps<-as.data.frame(join(coreID,MAT, type="inner", by="Cores"))
#write.csv(temps, file="Output/temps_ALL.csv")

rm(CoreClimate, CorridClip, Dups, Links, LinkStep, Neighbors, Neighbors_Orig, cci, delrows, dupcorecodes, i, j, mini, n, r)
_________________________________
##How Much Function!
##This calculates the coolest destination reachable from any origin core

library (matlab)

howmuch<-function(connections,temps,coreID)
{
  #set up output file
  r<-nrow(temps)
  howfar<-as.data.frame(cbind(Core=coreID[,1],Final_Temp=rep(0,r),Final_Core=rep(0,r), Final_Cost=rep(0,r), EucDist=rep(0,r), PathDist=rep(0,r)))
  #set up variables
  runningcore<-coreID
  connectsto<-vector("list", r)
  uniquetemps<-sort(unique(temps$Mean))
  runningtemps<-temps
  n<-length(uniquetemps)
  runningcosts<-cbind(Cores=coreID, cost=rep(0,r))
  
  #populate "connectsto", which is a vector of the cores to which each origin core connects
  #each row is the list of cores to which the origin core (in order of coreID) connects
  for (i in 1:r)
  {
    inds<-which(connections$Origin==coreID[i,"Cores"])
    connectsto[[i]]= connections[inds,"Dest"]
  }
  
  #feeds through each of the unique temperatures colder to warmer
  #finds the list of cores that correspond with each unique temperature
  for (j in 1:n)
  {
    warmer<-uniquetemps[j]
    inds<-which(runningtemps$Mean==warmer)
    #feeds through the list of cores that corresponds with the active unique temperature
    for (k in 1:(length(inds)))
    {
      ii<-inds[k]
      #if the active core does not connect to any other core, populate howfar w/
      #Final_Temp with the active temperature
      #Final_Core with the current core # (itself)
      if (isempty (connectsto[[ii]]))
      {
        howfar$Final_Temp[ii]<-warmer
        howfar$Final_Core[ii]<-runningcore[ii,"Cores"]
      } 
      #if the active core does connect to another core
      #make an index of where the destination (colder) cores are in the initial core list
      #find the minimum temperature of those cores and figure out to which core it belongs
      else 
      {
        tocoresinds<-match(connectsto[[ii]],coreID[,"Cores"])
        t<-min(runningtemps$Mean[tocoresinds])
        a<-which(runningtemps$Mean[tocoresinds]==min(runningtemps$Mean[tocoresinds]))
       #if there is only one coldest core, index that destination (colder) core & 
       #make the cost of that link= the cost of the cold core to which it connects + the cost of th new link
        if (length(a)==1)
          {
            minind<-tocoresinds[a]
            #this figures out the cost of the new link
            b<-which(connections$Origin==coreID$Cores[ii]&connections$Dest==coreID$Cores[minind])
            linkcosts<-connections$LeastCost[b]
            #this figures out the previous summed cost for the destination (colder) core
            initcosts<-runningcosts$cost[minind]
            #this sums the two to create an updated cost
            cost<-linkcosts+initcosts
            }
       #if there is more than one colder core w/ = temperatures, select the path with the lowest cost
       else
        {
          #this figures out the cost of the new link
          b<-which(connections$Origin==coreID$Cores[ii])
          linkcosts<-connections$LeastCost[b]
          linkcosts<-linkcosts[a]
          #this figures out the previous summed cost for the destination (colder) core options
          c<-which(runningcosts$Cores %in% coreID$Cores[tocoresinds])
          initcosts<-runningcosts$cost[c]
          initcosts<-initcosts[a]
          d<-linkcosts+initcosts
          #this selects the path with the lowest cost & assigns that low cost to be the new cost
          cost<-min(d)
          e<-which(d==min(d))
          #some destination (colder) cores have the same low temp & cost; arbitrarily select the first one
          length(e)<-1
          #this indexes the destination (colder) core selected (w/ coldest temp & lowest cost)
          minind<-tocoresinds[a[e]]
        }
        #make the final temp of the active core the new minimum temperature
        #make the current listed temperature of that core now == the new min. temperature
        #make the final core of the active core the new colder (destination) core #
        #have new colder (destination) core # replace the core # in runningcore
        #update the running cosst & the final cost of the active core
        #sum the Euc. distance and path length of the old & new path
        howfar$Final_Temp[ii]<-t
        runningtemps$Mean[ii]<-t
        howfar$Final_Core[ii]<-runningcore[minind,]
        runningcore[ii,]<-runningcore[minind,]
        runningcosts$cost[ii]<-cost
        howfar$Final_Cost[ii]<-cost
        distind<-which(connections$Origin==coreID$Cores[ii]&connections$Dest==coreID$Cores[minind])
        howfar$EucDist[ii]<-(howfar$EucDist[minind]+connections$EucDist[distind])
        howfar$PathDist[ii]<-(howfar$PathDist[minind]+connections$LCP[distind])
      }
    }
  }
  howfar<-cbind(howfar, CostToEuc=(howfar$Final_Cost/howfar$EucDist), CostToPath=(howfar$Final_Cost/howfar$PathDist))
  return(howfar)
}

howmuch<-howmuch(connections,temps,coreID)

##Check for correctness
howmuch<-cbind(howmuch, wrong=0)

for (m in 1:nrow(howmuch))
{if(howmuch$Final_Temp[m]!=temps[which(temps$Cores==howmuch$Final_Core[m]),"Mean"])
{howmuch$wrong[m]<-1}
}
which(howmuch$wrong==1)

for (m in 1:nrow(howmuch))
{if(howmuch$Final_Cost[m]==0 & howmuch$EucDist[m]>0)
  {howmuch$wrong[m]<-1}
}

which(howmuch$wrong==1)
howmuch<-howmuch[,-9]

#howmuch<-subset(howmuch, select= -wrong)

howmuch <- replace(howmuch, is.na(howmuch), 0)

write.csv(howmuch, file="Output/howmuch.csv")

rm(m, corecode, temps, coreID, connections)
________________________________________________________

###Lets calculate variables! 
##This is to be done once you've done both an adjacency-only and corridor + adjacency analysis
##"F" is used for destination cores; "0" is used for adjacency analyses; "100" is used for corridor+adjacency analyses

##You can load a "howmuch example"
#howmuch<-read.csv("Data/EXhowmuch.csv")
AllData<-howmuch

##This dataset contains future MAT information about the cores; 
##it also contains the destination cores and destination temperatures for an adjacency-only analysis
BonusData<-read.csv("Data/BonusData.csv")

##create a future MAT layer
FutMAT<-as.data.frame(cbind(Cores=BonusData$Cores, Fut_T=BonusData$Mean_fut))

##Changing column names for consistency
colnames(AllData)[colnames(AllData)=="Final_Temp"]<-"TempF_100"
colnames(AllData)[colnames(AllData)=="Final_Core"]<-"CoreF_100"
colnames(AllData)[colnames(AllData)=="Core"]<-"Cores"

##Modern MAT for each core
AllData<-join(AllData, MAT, by="Cores", type="left")

#Join future MAT for each core
AllData<-join(AllData, FutMAT, by="Cores", type="left")

#This is to determine the future MAT for the destination cores
colnames(FutMAT)<-c("CoreF_100", "FTempF_100")
AllData<-join(AllData, FutMAT, by="CoreF_100", type="left")

#Pulling adjacency only data from this dataset
AdjData<-as.data.frame(cbind(Cores=BonusData$Cores, TempF_0=BonusData$TempF_0, CoreF_0=BonusData$CoreF_0))
AllData<-join(AllData, AdjData, by="Cores", type="left")

#using future MAT to determine the future MAT for the destination cores for adjacency-only
colnames(FutMAT)<-c("CoreF_0", "FTempF_0")
AllData<-join(AllData, FutMAT, by="CoreF_0", type="left")

#In case I want to use it again- change the names back
colnames(FutMAT)<-c("Cores", "Fut_T")

AllData<-cbind(AllData, dFut_T=(AllData$Fut_T-AllData$Mean), TempD_100=(AllData$Mean-AllData$TempF_100), 
               TempD_0=(AllData$Mean-AllData$TempF_0), FTempD_100=(AllData$Fut_T-AllData$FTempF_100),
               FTempD_0=(AllData$Fut_T-AllData$FTempF_0), HowFar_100=(AllData$Mean-AllData$FTempF_100),
               HowFar_0=(AllData$Mean-AllData$FTempF_0))

AllData<-AllData[c("Cores", "Mean", "Fut_T", "dFut_T", "CoreF_0", "TempF_0", "TempD_0",
                    "FTempF_0", "FTempD_0","CoreF_100", "TempF_100", "TempD_100", 
                    "FTempF_100", "FTempD_100", "HowFar_0", "HowFar_100", "Final_Cost",
                    "EucDist", "PathDist", "CostToEuc", "CostToPath")]

write.csv(AllData, file="Output/AllData.csv")

USClip<-read.csv(file="Data/USClip.csv")

NewUSData<-join(USClip, AllData, by="Cores", type="left")

write.csv(NewUSData, file="Output/NewUSData.csv")

rm(AdjData, BonusData, FutMAT, howmuch, USClip)
___________________________________________________________

###Statistics!

E_W_Cores <- read.csv("Data/E_W_Cores.csv", stringsAsFactors=FALSE)
Areas<-read.csv("Data/CoreAreas.csv")
USData<-NewUSData

USData<-subset(USData, USData$Mean != -9999)

E_W_Cores$E_W[E_W_Cores$E_W==-1]<-"W"
E_W_Cores$E_W[E_W_Cores$E_W==0]<-"E"

E_W<-E_W_Cores
E_W$Cores<-as.integer(E_W$Cores)
E_W$E_W.Area<-as.numeric(E_W$E_W.Area)

DataEW<-join(USData, E_W,type="left", by="Cores")
DataEW<-join(DataEW, Areas, type="left", by="Cores" )

DataEW<-DataEW[complete.cases(DataEW),]

W<-subset(DataEW, DataEW$E_W=="W")
E<-subset(DataEW, DataEW$E_W=="E")

Wsuccess100<-subset(W, W$HowFar_100>0)
Esuccess100<-subset(E, E$HowFar_100>0)
Tsuccess100<-subset(DataEW, DataEW$HowFar_100>0)
Wsuccess0<-subset(W, W$HowFar_0>0)
Esuccess0<-subset(E, E$HowFar_0>0)
Tsuccess0<-subset(DataEW, DataEW$HowFar_0>0)

Warea<-sum(W$Area)
Earea<-sum(E$Area)
Tarea<-sum(DataEW$Area)

Success<-as.data.frame(cbind(Adj.count=c(0,0,0), Adj.freq=c(0,0,0), Adj.freq.total=c(0,0,0), Adj.area=c(0,0,0), Adj.pctA=c(0,0,0), Adj.pctA.total=c(0,0,0), Corrid.count=c(0,0,0), Corrid.freq=c(0,0,0), Corrid.freq.total=c(0,0,0), Corrid.area=c(0,0,0), Corrid.pctA=c(0,0,0), Corrid.pctA.total=c(0,0,0)))
row.names(Success)<-c("West", "East", "Total")

#West Corridor
Success$Corrid.count[1]<-nrow(Wsuccess100)
Success$Corrid.freq[1]<-round((nrow(Wsuccess100)/nrow(W))*100, digits=1)
Success$Corrid.freq.total[1]<-round((nrow(Wsuccess100)/nrow(DataEW))*100, digits=1)
Success$Corrid.area[1]<-(sum(Wsuccess100$Area))
Success$Corrid.pctA[1]<-round((Success$Corrid.area[1]/Warea)*100, digits=1)
Success$Corrid.pctA.total[1]<-round((Success$Corrid.area[1]/Tarea)*100, digits=1)
#East Corridor
Success$Corrid.count[2]<-nrow(Esuccess100)
Success$Corrid.freq[2]<-round((nrow(Esuccess100)/nrow(E))*100, digits=1)
Success$Corrid.freq.total[2]<-round((nrow(Esuccess100)/nrow(DataEW))*100, digits=1)
Success$Corrid.area[2]<-(sum(Esuccess100$Area))
Success$Corrid.pctA[2]<-round((Success$Corrid.area[2]/Earea)*100, digits=1)
Success$Corrid.pctA.total[2]<-round((Success$Corrid.area[2]/Tarea)*100, digits=1)
#Total Corridor
Success$Corrid.count[3]<-nrow(Tsuccess100)
Success$Corrid.freq[3]<-round((nrow(Tsuccess100)/nrow(DataEW))*100, digits=1)
Success$Corrid.freq.total[3]<-round((nrow(Tsuccess100)/nrow(DataEW))*100, digits=1)
Success$Corrid.area[3]<-(sum(Tsuccess100$Area))
Success$Corrid.pctA[3]<-round((Success$Corrid.area[3]/Tarea)*100, digits=1)
Success$Corrid.pctA.total[3]<-round((Success$Corrid.area[3]/Tarea)*100, digits=1)

#West Adjacent
Success$Adj.count[1]<-nrow(Wsuccess0)
Success$Adj.freq[1]<-round((nrow(Wsuccess0)/nrow(W))*100, digits=1)
Success$Adj.freq.total[1]<-round((nrow(Wsuccess0)/nrow(DataEW))*100, digits=1)
Success$Adj.area[1]<-(sum(Wsuccess0$Area))
Success$Adj.pctA[1]<-round((Success$Adj.area[1]/Warea)*100, digits=1)
Success$Adj.pctA.total[1]<-round((Success$Adj.area[1]/Tarea)*100, digits=1)
#East Adjacent
Success$Adj.count[2]<-nrow(Esuccess0)
Success$Adj.freq[2]<-round((nrow(Esuccess0)/nrow(E))*100, digits=1)
Success$Adj.freq.total[2]<-round((nrow(Esuccess0)/nrow(DataEW))*100, digits=1)
Success$Adj.area[2]<-(sum(Esuccess0$Area))
Success$Adj.pctA[2]<-round((Success$Adj.area[2]/Earea)*100, digits=1)
Success$Adj.pctA.total[2]<-round((Success$Adj.area[2]/Tarea)*100, digits=1)
#Total Adjacent
Success$Adj.count[3]<-nrow(Tsuccess0)
Success$Adj.freq[3]<-round((nrow(Tsuccess0)/nrow(DataEW))*100, digits=1)
Success$Adj.freq.total[3]<-round((nrow(Tsuccess0)/nrow(DataEW))*100, digits=1)
Success$Adj.area[3]<-(sum(Tsuccess0$Area))
Success$Adj.pctA[3]<-round((Success$Adj.area[3]/Tarea)*100, digits=1)
Success$Adj.pctA.total[3]<-round((Success$Adj.area[3]/Tarea)*100, digits=1)

write.csv(Success, file="Output/Success.csv")

###Redo if I want it. Incorrect
##This compares the percentage of E natural landscape patches v. W natural landscape patches
#E.Area.Improve<-Esuccess100.A-Esuccess0.A
#W.Area.Improve<-Wsuccess100.A-Wsuccess0.A
#EW.Area.Diff100<-Esuccess100.A-Wsuccess100.A
#EW.Area.Diff0<-Wsuccess0.A-Esuccess0.A

##This compares the absolute differences between natural landscape patch areas that succeed in the W v. E
#EW.Area.Diff100a<-Wsuccess100.Aa-Esuccess100.Aa
#EW.Area.Diff100a_div<-EW.Area.Diff100a/(Earea+Warea)
#EW.Area.Diff0a<-Wsuccess0.Aa-Esuccess0.Aa
#EW.Area.Diff0a_div<-EW.Area.Diff0a/(Earea+Warea)

TempChange<-as.data.frame(cbind(FTempD100=c(0,0,0), FTempD100.sd=c(0,0,0), TempD100=c(0,0,0), TempD100.sd=c(0,0,0)))
rownames(TempChange)<-c("West", "East", "Total")


TempChange$FTempD100[1]<-mean(W$FTempD_100, na.rm=TRUE)
TempChange$FTempD100[2]<-mean(E$FTempD_100, na.rm=TRUE)
TempChange$FTempD100[3]<-mean(DataEW$FTempD_100, na.rm=TRUE)
TempChange$FTempD0[1]<-mean(W$FTempD_0, na.rm=TRUE)
TempChange$FTempD0[2]<-mean(E$FTempD_0, na.rm=TRUE)
TempChange$FTempD0[3]<-mean(DataEW$FTempD_0, na.rm=TRUE)

TempChange$FTempD100.sd[1]<-sd(W$FTempD_100, na.rm=TRUE)
TempChange$FTempD100.sd[2]<-sd(E$FTempD_100, na.rm=TRUE)
TempChange$FTempD100.sd[3]<-sd(DataEW$FTempD_100, na.rm=TRUE)
TempChange$FTempD0.sd[1]<-sd(W$FTempD_0, na.rm=TRUE)
TempChange$FTempD0.sd[2]<-sd(E$FTempD_0, na.rm=TRUE)
TempChange$FTempD0.sd[3]<-sd(DataEW$FTempD_0, na.rm=TRUE)

TempChange$TempD100[1]<-mean(W$TempD_100, na.rm=TRUE)
TempChange$TempD100[2]<-mean(E$TempD_100, na.rm=TRUE)
TempChange$TempD100[3]<-mean(DataEW$TempD_100, na.rm=TRUE)
TempChange$TempD0[1]<-mean(W$TempD_0, na.rm=TRUE)
TempChange$TempD0[2]<-mean(E$TempD_0, na.rm=TRUE)
TempChange$TempD0[3]<-mean(DataEW$TempD_0, na.rm=TRUE)

TempChange$TempD100.sd[1]<-sd(W$TempD_100, na.rm=TRUE)
TempChange$TempD100.sd[2]<-sd(E$TempD_100, na.rm=TRUE)
TempChange$TempD100.sd[3]<-sd(DataEW$TempD_100, na.rm=TRUE)
TempChange$TempD0.sd[1]<-sd(W$TempD_0, na.rm=TRUE)
TempChange$TempD0.sd[2]<-sd(E$TempD_0, na.rm=TRUE)
TempChange$TempD0.sd[3]<-sd(DataEW$TempD_0, na.rm=TRUE)

TempChange<-round(TempChange, digits=1)

write.csv(TempChange, file="Output/TempChange.csv")

rm(CoreAreas, DataEW, E, E_W_Cores, W, Esuccess0, Esuccess100, Wsuccess0, Wsuccess100, Tsuccess0, Tsuccess100, Earea, Tarea, Warea)
_____________________________________

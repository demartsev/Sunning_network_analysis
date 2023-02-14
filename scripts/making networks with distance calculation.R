library(gtools)
library(asnipe)
library(igraph)
library(CINNA)
library(reshape2)

setwd("C:/Users/vdemartsev/My Cloud/Sunning_network_analysis")

#-------------------------
#NETWORK FROM SUNNING DATA

#read in file with social association data
#the file should include GROUP, DATE, YEAR, ID-CODES, BEHAVIORAL STATE
sunningVlad <- read.csv(file="assosiations_raw_all.csv",h=T)


#this loads grooming networks data (in progress)
#sunningVlad <- read.csv(file="grooming.csv",h=T)


#filter only sunning interactions
sunningVlad <- subset(sunningVlad, sunningVlad$Behaviour == "sunning")

#colnames(sunningVlad) <- c("#", "date", "dye", "code", "Behaviour", "Group", "Year")
directed = F


#get years when the data ws collected
years <- unique(sunningVlad$Year)
all_ties <- data.frame()

for (year in years) {
  year_select <- sunningVlad[which(sunningVlad$Year == year),] 
  
  #get groups in which the data was collected
  groups <- unique(year_select$Group)
  for (group in groups) {
    group_select <- year_select[which(year_select$Group == group) ,]
    indInfo <- read.delim(paste(getwd(), "/groups_info/", group,"_", year, "_IND_INFO.txt", sep = ""))
#make empty matrix with names of group members
gbiMat <- matrix(0,nrow=nrow(group_select),ncol=nrow(indInfo))
row.names(gbiMat) <- group_select$Date
colnames(gbiMat) <- indInfo$code

#transform the raw assosiation into  matrix
for (j in 1:nrow(group_select))
{
  #strip observation days into single vectors
  inds <- strsplit(as.character(group_select$CODES[j])," ")[[1]]
  #populate matrix with observation instances 
  gbiMat[ j, match(inds,as.character(indInfo$code))] <- 1
}
networkSunning <- get_network(gbiMat, data_format = "GBI",
                              association_index = "SRI",identities = indInfo$code)
 
#diag(networkSunning)<-NA
#transform tie strenghts into distances
#inverse <- round(networkSunning/mean(networkSunning), digits = 3)
#inverse <- round(1/networkSunning, digits = 3)

library(sna)
deg_weighted <- degree(networkSunning, gmode="graph", g=c(1:10000), ignore.eval=FALSE)
network1_perm <- network_permutation(gbiMat, permutations = 10000)
## calculate the weighted degree for each permutation
deg_weighted_perm1 <- degree(network1_perm,gmode="graph", g=c(1:10000), ignore.eval=FALSE)
## plot the distribution of permutations with the original data overlaid
par(mfrow=c(1,2))
hist(colMeans(deg_weighted_perm1),breaks=100,
     main=paste(group, "P = ",
                sum(mean(deg_weighted) < colMeans(deg_weighted_perm1))/ncol(deg_weighted_perm1)),
     xlab="Weighted degree", ylab="Probability")
abline(v=mean(deg_weighted), col='red', xpd = FALSE)



#geodesic distance with ANTs package

library(ANTs)

short <- met.geodesic(
  networkSunning,
  weighted = TRUE,
  shortest.weight = FALSE,
  normalization = TRUE,
  directed = F,
  out = F
)

##  #make network for igraph
  net2 <- graph_from_adjacency_matrix(networkSunning , mode = "upper", weighted = T)
 
#remove self loops
#net2<- simplify(net2, remove.multiple = F, remove.loops = TRUE )
plot.igraph(net2, main = paste(group, year, sep= "_"))
diag(networkSunning)<-NA
#transform into paired interactions
netw_tbl <- melt(networkSunning)

#net3 <- graph_from_adjacency_matrix(inverse , mode = "upper", weighted = T)
#net3<- simplify(net3, remove.multiple = F, remove.loops = TRUE )
 
 ## #calculate all shortest paths in a network (weighted)
 ## short <- shortest.paths(net3, to = V(net3), weights = NULL, mode = "all", algorithm = "dijkstra")
  #short <- 1/short
  diag(short)<-NA
  short_tbl <- melt(short)
 
  
  #combine tie strenght and shortest path lenght
  netw_tbl <- cbind(netw_tbl, short_tbl$value)
  colnames(netw_tbl) <- c("ID1", "ID2", "Tie strenght", "Tie lenght")
  netw_tbl$group <- group
  netw_tbl$year <- year
  
  netw_tbl$ID1_dom <- indInfo[match(netw_tbl$ID1, indInfo$code) , "status"]
  netw_tbl$ID2_dom <- indInfo[match(netw_tbl$ID2, indInfo$code) , "status"]
  netw_tbl$ID1_sex <- indInfo[match(netw_tbl$ID1, indInfo$code) , "sex"]
  netw_tbl$ID2_sex <- indInfo[match(netw_tbl$ID2, indInfo$code) , "sex"]
  
  all_ties <- rbind(all_ties, netw_tbl)
  

  }
}
#write.csv(all_ties, "all_network_ties.csv")
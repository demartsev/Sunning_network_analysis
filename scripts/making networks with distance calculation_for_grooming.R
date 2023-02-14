library(gtools)
library(asnipe)
library(igraph)
library(CINNA)
library(reshape2)
library(ANTs)
library(sna)

setwd("C:/Users/vdemartsev/My Cloud/Sunning_network_analysis")

#-------------------------
#NETWORK FROM SUNNING DATA

#read in file with social association data
#the file should include GROUP, DATE, YEAR, ID-CODES, BEHAVIORAL STATE
 
#this loads grooming networks data (in progress)
sunningVlad <- read.csv(file="grooming.csv",h=T)

colnames(sunningVlad) <- c("#", "date", "dye", "code", "Behaviour", "Group", "Year")
directed = F
sunningVlad$Year <- paste(20, sunningVlad$Year, sep = "")

#get years when the data was collected
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
row.names(gbiMat) <- group_select$date
colnames(gbiMat) <- indInfo$code

#transform the raw assosiation into  matrix
for (j in 1:nrow(group_select))
{
  #strip observation days into single vectors
  inds <- strsplit(as.character(group_select$code[j])," ")[[1]]
  #populate matrix with observation instances 
  gbiMat[ j, match(inds,as.character(indInfo$code))] <- 1
}
networkSunning <- get_network(gbiMat, data_format = "GBI",
                              association_index = "SRI",identities = indInfo$code)
#diag(networkSunning)<-NA



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

#write.csv(networkSunning, "JAXX_sunningnetwork.csv")

#metrics <- data.frame(indDegree=sna::degree(networkSunning))

#row.names(metrics) <- indInfo$code

#make network for igraph
net2 <- graph_from_adjacency_matrix(networkSunning , mode = "undirected", weighted = T)

#calculate the number of edges in each shortest path
link_count <- distances(
  net2,
  mode = "all",
  weights = NA,
  algorithm =  "dijkstra"
)

#get the path adjusted for number of links
short_adj <-  met.geodesic(
  networkSunning,
  weighted = TRUE,
  shortest.weight = FALSE, #considers the highest met.strength as the shortest path.
  normalization = T,
  directed = F,
  out = T
)*link_count


#remove self loops
net2<- igraph::simplify(net2, 
                remove.loops = TRUE, remove.multiple = T)
plot(net2, edge.width = 10*(edge_attr(net2)$weight), main = paste(group, year, sep= "_"))
#transform into paired interactions
netw_tbl <- melt(networkSunning)
#netw_tbl <- melt(inverse)


#calculate all shortest paths in a network (weighted)
short <- met.geodesic(
  networkSunning,
  weighted = TRUE,
  shortest.weight = FALSE, #considers the highest met.strength as the shortest path.
  normalization = T,
  directed = F,
  out = T
)

networkSunning_1 <- networkSunning
#remove diagonal from direct tie strength network matrix
diag(networkSunning_1)<-NA
#melt into paired interactions
netw_tbl <- melt(networkSunning_1)

#remove diagonal from shortest path network matrix
diag(short)<-NA
#melt into paired interactions
short_tbl <- melt(short)

#remove diagonal from shortest path adjusted network matrix
diag(short_adj)<-NA
#melt into paired interactions
short_adj_tbl <- melt(short_adj)

#remove diagonal from number of links path network matrix
diag(link_count)<-NA
#melt into paired interactions
link_count_tbl <- melt(link_count)

#combine tie strength and shortest path length
netw_tbl <- cbind(netw_tbl, short_tbl$value, short_adj_tbl$value,  link_count_tbl$value)
colnames(netw_tbl) <- c("ID1", "ID2", "Tie strenght", "Tie lenght", "Tie lenght adj", "Link count")

#fill some additional data on the individuals
netw_tbl$group <- group
netw_tbl$year <- year
netw_tbl$ID1_dom <- indInfo[match(netw_tbl$ID1, indInfo$code) , "status"]
netw_tbl$ID2_dom <- indInfo[match(netw_tbl$ID2, indInfo$code) , "status"]
netw_tbl$ID1_sex <- indInfo[match(netw_tbl$ID1, indInfo$code) , "sex"]
netw_tbl$ID2_sex <- indInfo[match(netw_tbl$ID2, indInfo$code) , "sex"]

all_ties <- rbind(all_ties, netw_tbl)


  }
}
write.csv(all_ties, "all_network_groom_ties.csv")

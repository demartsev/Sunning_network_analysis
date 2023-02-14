#this is a script for calculating sunning social association and minimal path length

#not sure if all those are actually needed but I lost track at what function cones from where at this point.
#dont think it creates conflicts so will just leave everything here
library(gtools)
library(asnipe)
library(igraph)
library(CINNA)
library(reshape2)
library(sna)
library(ANTs)

#load the sunning social data
load("sunningVlad.RData")

#filter the data to include only sunning proximity
sunningVlad <- subset(sunningVlad, sunningVlad$Behaviour == "sunning")

#set parameters for the network being directed or not
directed = F

#get years when the data ws collected
years <- unique(sunningVlad$Year)

#make empty data frame for collecting the social summaries later
all_ties <- data.frame()

##### here we restruct the data and calculate the social metrics ####


for (year in years) {
  year_select <- sunningVlad[which(sunningVlad$Year == year),] #get a single year of data
  
  #get groups in which the data was collected in the selected year
  groups <- unique(year_select$Group)
  
  for (group in groups) {
    group_select <- year_select[which(year_select$Group == group) ,] #get one group
    
    #load individual info file for the group of interest
    indInfo <- get(paste(group,"_", year, "_IND_INFO.txt", sep = "")) 
    
    #make empty matrix with codes of group members and dates of data collection
    gbiMat <- matrix(0,nrow=nrow(group_select),ncol=nrow(indInfo))
    row.names(gbiMat) <- group_select$Date
    colnames(gbiMat) <- indInfo$code
    
    
    #### here we transform the raw association into  matrix. 
    for (j in 1:nrow(group_select))
    {
      #strip observation days into single vectors
      inds <- strsplit(as.character(group_select$CODES[j])," ")[[1]]
      
      #populate matrix with observation instances 
      gbiMat[ j, match(inds,as.character(indInfo$code))] <- 1
    }
    
    #here I just use the get_network function. Assume it works. 
    # at least the numbers seem to add up
    networkSunning <- get_network(gbiMat, data_format = "GBI",
                                  association_index = "SRI",identities = indInfo$code)
    
    
    ### TESTING THE NETWORK ####
    ## doing permutations -  generating 10000 random networks
    network1_perm <- network_permutation(gbiMat, permutations = 10000)
    
    ## calculate the weighted degree for each permutation
    deg_weighted_perm1 <- degree(network1_perm,gmode="graph", g=c(1:10000), ignore.eval=FALSE)
    ## calculate the weighted degree for the real network
    deg_weighted <- degree(networkSunning, gmode="graph", ignore.eval=FALSE)
    
    ## plot the distribution of permutations with the original data overlaid
    par(mfrow=c(1,2))
    hist(colMeans(deg_weighted_perm1),breaks=100,
         main=paste(group, "P = ",
                    sum(mean(deg_weighted) < colMeans(deg_weighted_perm1))/ncol(deg_weighted_perm1)),
         xlab="Weighted degree", ylab="Probability")
    abline(v=mean(deg_weighted), col='red', xpd = FALSE)
    
    
    
    ####getting geodesic distance with ANTs package###
    
    #finding the "shortest path between any two individuals
    
    short <- met.geodesic(
      networkSunning,
      weighted = TRUE,
      shortest.weight = FALSE, #considers the highest met.strength as the shortest path.
      normalization = T,
      directed = F,
      out = T
    )
    
    ##  #make network for igraph
    net2 <- graph_from_adjacency_matrix(networkSunning , mode = "upper", weighted = T)
 
    
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
    
    
    #plot
    plot.igraph(net2, edge.width = 10*(edge_attr(net2)$weight), main = paste(group, year, sep= "_"))
   
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
    
    #bind all data together. this goes to the final analysis
    all_ties <- rbind(all_ties, netw_tbl)
    
    
  }
}

all_ties <- jsonlite::flatten(all_ties, recursive = TRUE)
#write.csv(all_ties, "all_network_ties_1.csv")

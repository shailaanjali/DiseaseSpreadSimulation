# K imporant Nodes, Infection 

library(igraph)
library(rpart)
library(ggplot2)

############ EDGE LISTS ######################

#import populaiton data
populationMatrix <-read.csv("foo.csv")
populationMatrix <- populationMatrix[, -c(1)]

hist(populationMatrix$Age, main="Simulation Age", col="mediumslateblue", xlab="age")
hist(populationMatrix$workLoc, main="Simulation Workplaces", col="coral2", xlab="workplace")



##### FAMILY NETWORK #############

#connections within family 
famMat <- data.frame(populationMatrix[, 1:2])
colnames(famMat) <- c("famID", "pID")

#starter matrix (later deleted)
elFamily <- c(0,0)

#isolate list of families 
x <- famMat[nrow(famMat), 1]
families <- c(1:as.numeric(as.character(x)))


#iterate through all families
for (a in families)
{
  #single family to analyze (Network A)
  famMatTemp <- famMat[famMat[,1]==a,]
  fFrame <- data.frame(famMatTemp$pID)
  
  #fNet = all posible combinations of interactions within Network A
  fNet <- graph.full(nrow(fFrame))
  vec <- c(as.character(fFrame$famMatTemp.pID))
  V(fNet)$name <- vec
  
  #use Network A Density and Time Together to calc # meaningful interactions
  densityFam <- (1/(nrow(fFrame)))
  timeFam <- 120
  numMeaningful <- sum(rbinom(timeFam*(nrow(fFrame)), 1, prob=densityFam))
  
  #randomly choose meaninful interactions from all possible interactions
  if(nrow(as_edgelist(fNet))>0)
  {
    elRows <- sample(c(1:nrow(as_edgelist(fNet))), min(numMeaningful,nrow(as_edgelist(fNet))) , replace = FALSE)
    elTempF <- as_edgelist(fNet)[elRows, ]
    elFamily <- rbind(elFamily, elTempF)
  }
}

#final matrix
elFamily <- elFamily[-c(1), ]
elFamMat <- (matrix(sapply(elFamily, as.numeric), ncol=2))


##### TRACT NETWORK #############


#connections within tracts
tractMat <- data.frame(populationMatrix[, c(2,4)])
colnames(tractMat) <- c("pID", "tract")
elTract <- c(0,0)

#tracts, by number
tracts <- c(51.01, 51.02, 52, 53, 54, 55, 102.03, 102.04, 103, 106, 
            1, 2, 3, 4, 7, 8, 10, 11, 12, 13,
            14, 15.01, 15.02, 16, 17, 18, 19, 107, 108, 109.01,
            109.02, 111)

#number of households per tract - used to calculate tract density
tHH <- c(1527, 1815, 2072, 1332, 1897, 1721, 3149, 4954, 376, 2011,
         1053, 735, 1318, 2074, 1428, 827, 631, 1387, 1308, 2019,
         1308, 1774, 2525, 4097, 3177, 1503, 1719, 1579, 1859, 2632,
         1386, 2050)


for (a in 1:32)
{
  tractMatTemp <- tractMat[tractMat[,2]==tracts[a],]
  tFrame <- data.frame(tractMatTemp$pID)
  tNet <- graph.full(nrow(tFrame))
  vec1 <- c(as.character(tFrame$tractMatTemp.pID))
  V(tNet)$name <- vec1 
  densityTract <- (1/(tHH[a]))
  timeTract <- 240
  numMeaningful <- sum(rbinom(timeTract*(nrow(tFrame)), 1, prob=densityTract))
  if(nrow(as_edgelist(tNet))>0)
  {
    elRows <- sample(c(1:nrow(as_edgelist(tNet))), min(numMeaningful,nrow(as_edgelist(tNet))) , replace = FALSE)
    elTempT <- as_edgelist(tNet)[elRows, ]
    elTract <- rbind(elTract, elTempT)
  }
}

elTract <- elTract[-c(1), ]
elTractMat <- (matrix(sapply(elTract, as.numeric), ncol=2))






#connections within the workplace
jobMat <- data.frame(populationMatrix[, c(2,9)])
colnames(jobMat) <- c("pID", "job")
elJob <- c(0,0)
jobs <- c(1:1076)
for (a in jobs)
{
  jobMatTemp <- jobMat[jobMat[,2]==a,]
  jFrame <- data.frame(jobMatTemp$pID)
  jNet <- graph.full(nrow(jFrame))
  vec2 <- c(as.character(jFrame$jobMatTemp.pID))
  V(jNet)$name <- vec2
  
  densityJob <- (1/(nrow(jFrame)))
  timeJob <- 377
  numMeaningful <- sum(rbinom(timeJob*(nrow(jFrame)), 1, prob=densityJob))
  if(nrow(as_edgelist(jNet))>0)
  {
    elRows <- sample(c(1:nrow(as_edgelist(jNet))), min(numMeaningful,nrow(as_edgelist(jNet))) , replace = FALSE)
    elTempJ <- as_edgelist(jNet)[elRows, ]
    elJob <- rbind(elJob, elTempJ)
  }
}
elJob <- elJob[-c(1), ]
elJobMat <- (matrix(sapply(elJob, as.numeric), ncol=2))



#make the final edge list
edgeList <- rbind(elFamMat, elTractMat, elJobMat)
write.csv(edgeList, file="foo2.csv")








############ RUN SIMULATIONS ############# 


edgeList <-read.csv("foo2.csv")
edgeList <- as.matrix(edgeList)
edgeList <- edgeList[, c(-1)]

testG <- as.undirected(graph_from_edgelist(edgeList))
order(degree(testG), decreasing = TRUE)[1:20]
#vertex_connectivity(iP)


#infection simulations: 3 weeks per simulation, ie 21 days

# node Latino Man
sickVerts <- ego(graph_from_edgelist(edgeList), 21, "40215")
sickVerts <- (matrix(sapply(sickVerts, as.numeric), ncol=1))
infectedPpl <- subgraph(graph_from_edgelist(edgeList), sickVerts)
iP <- as.undirected(infectedPpl)

# node White Man
sickVerts <- ego(graph_from_edgelist(edgeList), 21, "40217")
sickVerts <- (matrix(sapply(sickVerts, as.numeric), ncol=1))
infectedPpl <- subgraph(graph_from_edgelist(edgeList), sickVerts)
iP <- as.undirected(infectedPpl)

# node Black Male
sickVerts <- ego(graph_from_edgelist(edgeList), 21, "14105")
sickVerts <- (matrix(sapply(sickVerts, as.numeric), ncol=1))
infectedPpl <- subgraph(graph_from_edgelist(edgeList), sickVerts)
iP <- as.undirected(infectedPpl)

# node Asian Male
sickVerts <- ego(graph_from_edgelist(edgeList), 21, "80672")
sickVerts <- (matrix(sapply(sickVerts, as.numeric), ncol=1))
infectedPpl <- subgraph(graph_from_edgelist(edgeList), sickVerts)
iP <- as.undirected(infectedPpl)

# node TwoOrMore Male
sickVerts <- ego(graph_from_edgelist(edgeList), 21, "80675")
sickVerts <- (matrix(sapply(sickVerts, as.numeric), ncol=1))
infectedPpl <- subgraph(graph_from_edgelist(edgeList), sickVerts)
iP <- as.undirected(infectedPpl)

# node Hawaiian Male
sickVerts <- ego(graph_from_edgelist(edgeList), 21, "9601")
sickVerts <- (matrix(sapply(sickVerts, as.numeric), ncol=1))
infectedPpl <- subgraph(graph_from_edgelist(edgeList), sickVerts)
iP <- as.undirected(infectedPpl)

# node Other Male
sickVerts <- ego(graph_from_edgelist(edgeList), 21, "8798")
sickVerts <- (matrix(sapply(sickVerts, as.numeric), ncol=1))
infectedPpl <- subgraph(graph_from_edgelist(edgeList), sickVerts)
iP <- as.undirected(infectedPpl)

# node Native Male
sickVerts <- ego(graph_from_edgelist(edgeList), 21, "2354")
sickVerts <- (matrix(sapply(sickVerts, as.numeric), ncol=1))
infectedPpl <- subgraph(graph_from_edgelist(edgeList), sickVerts)
iP <- as.undirected(infectedPpl)

# node Black Female
sickVerts <- ego(graph_from_edgelist(edgeList), 21, "16132")
sickVerts <- (matrix(sapply(sickVerts, as.numeric), ncol=1))
infectedPpl <- subgraph(graph_from_edgelist(edgeList), sickVerts)
iP <- as.undirected(infectedPpl)

# node Asian Female
sickVerts <- ego(graph_from_edgelist(edgeList), 21, "17492")
sickVerts <- (matrix(sapply(sickVerts, as.numeric), ncol=1))
infectedPpl <- subgraph(graph_from_edgelist(edgeList), sickVerts)
iP <- as.undirected(infectedPpl)

# node White Female
sickVerts <- ego(graph_from_edgelist(edgeList), 21, "13832")
sickVerts <- (matrix(sapply(sickVerts, as.numeric), ncol=1))
infectedPpl <- subgraph(graph_from_edgelist(edgeList), sickVerts)
iP <- as.undirected(infectedPpl)

# node Latina Female
sickVerts <- ego(graph_from_edgelist(edgeList), 21, "13905")
sickVerts <- (matrix(sapply(sickVerts, as.numeric), ncol=1))
infectedPpl <- subgraph(graph_from_edgelist(edgeList), sickVerts)
iP <- as.undirected(infectedPpl)

# node TwoOrMore Female
sickVerts <- ego(graph_from_edgelist(edgeList), 21, "67201")
sickVerts <- (matrix(sapply(sickVerts, as.numeric), ncol=1))
infectedPpl <- subgraph(graph_from_edgelist(edgeList), sickVerts)
iP <- as.undirected(infectedPpl)

# node Hawaiian Female
sickVerts <- ego(graph_from_edgelist(edgeList), 21, "29089")
sickVerts <- (matrix(sapply(sickVerts, as.numeric), ncol=1))
infectedPpl <- subgraph(graph_from_edgelist(edgeList), sickVerts)
iP <- as.undirected(infectedPpl)

# node Other Female
sickVerts <- ego(graph_from_edgelist(edgeList), 21, "1782")
sickVerts <- (matrix(sapply(sickVerts, as.numeric), ncol=1))
infectedPpl <- subgraph(graph_from_edgelist(edgeList), sickVerts)
iP <- as.undirected(infectedPpl)

# node Native Female
sickVerts <- ego(graph_from_edgelist(edgeList), 21, "3586")
sickVerts <- (matrix(sapply(sickVerts, as.numeric), ncol=1))
infectedPpl <- subgraph(graph_from_edgelist(edgeList), sickVerts)
iP <- as.undirected(infectedPpl)

#dense areas of graph 
vecSick <- c(order(degree(iP), decreasing = TRUE)[1:10])
mostConnected <- sickVerts[vecSick, 1]
mostConnected


#write.csv(edgeList, file="foo2.csv")

############ Tree ############# 

#populationMatrix
pM2 <- populationMatrix[c(mostConnected), ]

id <- pM2[,2]
sex <- pM2[,3]
tract <- as.character(pM2[,4])
race <- pM2[,5]
age <- pM2[,6]
insured <- pM2[,7]
job <- pM2[,8]
jobLoc <- as.character(pM2[,9])
transport <- pM2[,10]
income <- pM2[,11]

tree <- rpart(id ~  sex  + race  + insured  + job +tract + income + jobLoc
                , data=pM2, method = "class", control=rpart.control(minsplit=3, cp=0.001))



#printcp(tree)
#plotcp(tree)
#summary(tree)

plot(tree, uniform = TRUE, main = "Tree Diagram for Infected Persons")
text(tree, all=TRUE, cex=.8)

post(tree, file = "treeNativeFemale.ps", title = "Tree Diagram for Infected Persons")

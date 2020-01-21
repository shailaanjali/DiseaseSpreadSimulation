# 332 Project


# Shaila Sundram
# Population Generation File


#Contents:
### Import/Organize Data
### Job/Workplace Assignments
### Create People
### Fill Neighborhoods




############# Import/Organize Data ###################



# import tract data
locdata <-read.csv("incomeLocation.csv")
locFrame <- data.frame(locdata$Tract.Number, locdata$percentWL, locdata$bin)
locFrame <- locFrame[-c(1, 12, 35, 36, 37, 38), ]


# separate tract data by city 
wlTracts <- locFrame[c(1:10), ]
lafTracts <- locFrame[c(11:32), ]


# import age data
agedata <- read.csv("agefile.csv")
ageFrame <- data.frame(agedata$Age, agedata$Final.Frac)


# age function 
ageBracket <- function(){
  drawsAge <- sample(agedata$Age, size=1,prob=agedata$Final.Frac)
  return (drawsAge)}

# parent age data (over 18)
ageFrame2 <- data.frame(agedata$Age, agedata$Parent)
ageFrame2 <- ageFrame2[-c(1,2), ]


# parent age function
ageBracketParent <- function(){
  drawsAge <- sample(ageFrame2$agedata.Age, size=1,prob= ageFrame2$agedata.Parent)
  return (drawsAge)}


# import insurance coverage data
coverdata <- read.csv("nocover.csv")
coverFrame <- data.frame(coverdata$age_bucket, coverdata$noCover)
coverFrame <- coverFrame[-c(1, 3, 5, 7, 9, 11, 13, 15, 17), ] 
MatCover <- as.matrix(coverFrame)
MatCover <- cbind(1-as.numeric(MatCover[,2]), as.numeric(MatCover[,2]))
coverage <- c('yes', 'no')


# coverage function
# assigns coverage based on age & choose age from age range 
isInsured <- function(x){
  if(x=="under5") 
    c(sample((0:5), 1, replace=T), sample(coverage, 1, replace=T, prob=MatCover[1,]))
  else if (x=="5-17") 
    c(sample((5:17), 1, replace=T), sample(coverage, 1, replace=T, prob=MatCover[2,]))
  else if(x=="18-24") 
    c(sample(18:24, 1, replace=T), sample(coverage, 1, replace=T, prob=MatCover[3,]))
  else if(x=="25-34") 
    c(sample(25:34, 1, replace=T), sample(coverage, 1, replace=T, prob=MatCover[4,]))
  else if(x=="35-44") 
    c(sample(35:44, 1, replace=T), sample(coverage, 1, replace=T, prob=MatCover[5,]))
  else if(x=="45-54") 
    c(sample(45:54, 1, replace=T), sample(coverage, 1, replace=T, prob=MatCover[6,]))
  else if(x=="55-59") 
    c(sample(55:59, 1, replace=T), sample(coverage, 1, replace=T, prob=MatCover[7,]))
  else if(x=="55-59") 
    c(sample(55:59, 1, replace=T), sample(coverage, 1, replace=T, prob=MatCover[7,]))
  else if(x=="60-61") 
    c(sample(60:61, 1, replace=T), sample(coverage, 1, replace=T, prob=MatCover[7,]))
  else if(x=="62-64") 
    c(sample(62:64, 1, replace=T), sample(coverage, 1, replace=T, prob=MatCover[7,]))
  else if(x=="65-74") 
    c(sample(65:74, 1, replace=T), sample(coverage, 1, replace=T, prob=MatCover[8,]))
  else if(x=="over75") 
    c(sample(75:85, 1, replace=T), sample(coverage, 1, replace=T, prob=MatCover[9,]))}


#import race data
racedata <- read.csv("race.csv")
raceFrame <- data.frame(racedata$Race, racedata$percentRace)
raceFrame <- raceFrame[-c(9), ]

#race function
raceFunc <- function(){
  r1 <- sample(raceFrame$racedata.Race, 1, prob=raceFrame$racedata.percentRace)
  return(r1)}

#import employment data
employdata <- read.csv("employment.csv")
employFrame <- data.frame(employdata$acs_occ_name, employdata$pcent)
employFrame <- employFrame[-c(25, 26), ]

#employment function
employFunc <- function(){
  e1 <- sample(employFrame$employdata.acs_occ_name, 1, prob=employFrame$employdata.X)
  return(e1)
}

#transportation function
transportFunc <- function(){
  transportYN <- 0
  x <- rbinom(n=1, size=1, prob=0.07)
  if(x==1)
  {
    transportYN <- 1
  }
  return(transportYN)
}


################# Job/Workplace Assignments #############


# assign workplace based on career
personWorkplaces <- function(pEmploy)
{
  if (pEmploy == "Unemployed"){
    pLoc <- 0
  } else if(pEmploy == "Education, Training, & Library"){
    pLoc <- sample(1:101, 1)
  } else if (pEmploy == "Fire Fighting Supervisors"){
    pLoc <- sample(102:115, 1)
  } else if (pEmploy == "Healthcare"){
    pLoc <- sample(116:191, 1)
  } else if (pEmploy == "Law Enforcement Supervisors"){
    pLoc <- sample(76:101, 1)
  } else if (pEmploy == "Farming, Fishing, & Forestry"){
    pLoc <- sample(192:201, 1)
  } else if (pEmploy == "Architecture & Engineering"){
    pLoc <- sample(202:347, 1)
  } else if (pEmploy == "Community & Social Service"){
    pLoc <- sample(348:429, 1)
  } else{
    pLoc <- sample(1:1076, 1)
  }
  return(pLoc)
}



# assign school to student
studentSchool <- function(age, tract)
{
  if(20<tract & tract<107){ #west lafayette
    if(age<11){
      pLoc <- sample(1:5, 1)
    } else if (age<14){
      pLoc <- sample(6:9, 1)
    } else {
      pLoc <- sample(10:12, 1)
    }
  }else {
    if(age<11){
      pLoc <- sample(13:36, 1)
    } else if (age<14){
      pLoc <- sample(37:50, 1)
    } else {
      pLoc <- sample(51:59, 1)
    }
  }
  return(pLoc)
}

##workplace key

#1-5: west lafayette elementary
#6-9: west lafayette middle
#10-12: west lafayette high 
#13-36: lafayette elementary
#37-50: lafayette middle
#51-59: lafayette high
#60-101: other education & government
#102-115: fire stations
#116-191: healthcare
#192-201: agriculture
#202-288: networks
#289-347: industry, manufacturing
#348-429: community, non-profit
#430-1076: business, retail, entertainment, consumer, tourism, etc.


################# Create People ##########################


# add person
addPerson <- function(idNum, tNum)
{
  pID <- idNum+1;
  pSex <- sample(c('male', 'female'), size=1)
  pAddress <- tNum;
  pRace <- raceFunc()
  pInsureMat <- isInsured(ageBracket())
  pAge <- pInsureMat[1]
  pInsure <- pInsureMat[2]
  pTrans <- transportFunc()
  
  if ((as.numeric(pAge))<5){
    pEmploy <- "N/A"
    pLoc <- 0}
  else if ((as.numeric(pAge))<18){
    pEmploy <- "student"
    pLoc <- studentSchool(pAge, tNum)
  } else if ((as.numeric(pAge))>65){
    pEmploy <- "retired"
    pLoc <- 0
  } else {
    pEmploy <- employFunc()
    pLoc <- personWorkplaces(pEmploy)
  }
  newMat <- c(pID, pSex, pAddress, as.character(pRace), pAge, pInsure, as.character(pEmploy), pLoc, pTrans)
  return(newMat)
}


# add parent 
addParent <- function(idNum, tNum)
{
  pID <- idNum+1;
  pSex <- sample(c('male', 'female'), size=1)
  pAddress <- tNum;
  pRace <- raceFunc()
  pInsureMat <- isInsured(ageBracketParent())
  pEmploy <- employFunc()
  pAge <- pInsureMat[1]
  pInsure <- pInsureMat[2]
  pTrans <- transportFunc()
  pLoc <- personWorkplaces(pEmploy)
  newMat <- c(pID, pSex, pAddress, as.character(pRace), pAge, pInsure, as.character(pEmploy), pLoc, pTrans)
  return(newMat)
}





############# FILL NEIGHBORHOODS ###################


# initialize number of families to zero, and people to zero 
familyNum <- 0;
personNum <- 0;

# initialize population matrix
populationMatrix <- c("5", "0", "male", "55", "White", "20", "yes", "farmer", "1", "1", "1")#, busMatrix)
testMat <- c("5", "0", "male", "55", "White", "20", "yes", "farmer", "1", "1", "1")#, busMatrix)

# create starter matrix (removed later)
populationMatrix <- rbind(populationMatrix, testMat)
colnames(populationMatrix) <- c("FamilyID", "ID", "Sex", "Tract", "Race", "Age", "Insured?", "Job", "workLoc", "transport", "HH Income")#, busNums)




# iterate through West Lafayette tracts
for (a in (1:nrow(wlTracts)))
{
  tractNum <- wlTracts$locdata.Tract.Number[a]
  numPpl <- round(43999 * wlTracts$locdata.percentWL[a]) #43999
  incomeBracket <- wlTracts$locdata.bin[a]
  k <- 0
  
  while(k < numPpl)
  {
    familyNum <- familyNum+1;
    
    # set fam size (must be greater than 1)
    famSize <- 0
    while(famSize<1){
      famSize <- round(rnorm(1, 2.26))}
    
    # initialize family info to 0 
    kidHS <- 0
    adultHS <- 0
    hhIncome <- 0

    
    for (j in (1:famSize))
    {
      
      # attributes
      personMat <- addPerson(personNum, tractNum)
      personNum <- personNum+1;
      
      # household income
      if (j == 1){
        pIncome <- incomeBracket
        hhIncome <- pIncome}
      else{
        pIncome <- hhIncome}
      

      
      # add to matrix
      populationMatrix <- rbind(populationMatrix, c(familyNum, personMat, as.character(pIncome)))
      
      # household with children?
      if (personMat[5]<=18){
        kidHS <- kidHS+1
        } else {
        adultHS <- adultHS+1 }
      
      # no minors without adults
      if ((personMat[5] <=18) & kidHS == 1 & adultHS == 0)
      {
        if (famSize < 2)
          {
          famSize <- famSize+1;
          }
        parentMat <-addParent(personNum, tractNum)
        populationMatrix <- rbind(populationMatrix, c(familyNum, parentMat, as.character(pIncome)))
        j <- j+1;
        personNum <- personNum+1
      }
      
    }#family filled
    k <- k+famSize
    
  }#tract filled
}#west lafayette filled



#remove first two lines
populationMatrix <- populationMatrix[-c(1,2),]


pM <- populationMatrix[c(65000:70000), ]
#[100:200, ]
write.csv(populationMatrix, file="foo.csv")





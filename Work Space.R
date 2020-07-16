#index <- which((duplicated(test$PlayerID) == FALSE) & test$Level %in% c("Tutorial", "Paired") == TRUE)
#test <- test[-index,]



#pairedData <- pairedData %>% arrange( Level, Track, Order)


for(i in 1:(nrow(pairedData))){
  
  if(i == nrow(pairedData)){
    
    break
  }
  
  if(IsOdd(pairedData$Order[i]) == TRUE & (pairedData$Level[i] %in% c("Tutorial", "Paired")) == TRUE){
    
    if(pairedData$PlayerID[i] != pairedData$PlayerID[i + 1])
      #pairedData$Level[i] != pairedData$Level[i + 1]){
      {
      pairedData <- pairedData[-i,]
      
    }
  }
   
}



data.all <- data.all %>% select(PlayerID, Level, TimeOffTrack, PlayerID2, Order2)


# data.all$TempColumn <- 0
# 
# for(i in 1:nrow(data.all)){
#   
#   if(i == nrow(data.all)){
#     
#     break
#   }
#   
#   
#   if((data.all$TimeOffTrack[i] > 0.3 | 
#       data.all$TimeOffTrack[i + 1] > 0.3) & 
#      (data.all$PlayerID2[i] == data.all$PlayerID2[i+1] &
#       data.all$Order2[i] < data.all$Order2[i + 1])){
#     
#     if(data.all$TempColumn[i] == 0){
#       
#       data.all$TempColumn[i] <- 1
#       
#     }
#     
#     if(data.all$TempColumn[i + 1] == 0){
#       
#       data.all$TempColumn[i + 1] <- 1
#     }
#     
#     
#   } else {
#     
#     data.all$TempColumn[i] <- 2
#     
#   }
# }

data.all$TempColumn <- 0


for(i in 1:nrow(data.all)){
  
  if(data.all$TimeOffTrack[i] > 0.3){
    
    data.all$TempColumn[i] <- 1
  }
}

for(i in 1:nrow(data.all)){
  
  if(i == nrow(data.all)){
    
    break
  }
  
  if(data.all$PlayerID2[i] != data.all$PlayerID2[i + 1]){}
  
  
  
  if(data.all$PlayerID2[i] == data.all$PlayerID2[i + 1] &
     (data.all$TempColumn[i] == 1 |
     data.all$TempColumn[i + 1] == 1)){
    
    data.all$TempColumn[i] <-  1
    data.all$TempColumn[i + 1] <-  1
    
  }

}

data.all <- data.all %>% filter(TempColumn == 0)


















if(input$filterPData == "TRUE"){
  
    
    plotData <- plotData()
    
    plotData$TempColumn <- 0
    
    
    for(i in 1:nrow(plotData)){
      
      if(plotData$TimeOffTrack[i] > 0.3){
        
        plotData$TempColumn[i] <- 1
      }
    }
    
    for(i in 1:nrow(plotData)){
      
      if(i == nrow(plotData)){
        
        break
      }
      
      if(plotData$PlayerID2[i] != plotData$PlayerID2[i + 1]){}
      
      
      
      if(plotData$PlayerID2[i] == plotData$PlayerID2[i + 1] &
         (plotData$TempColumn[i] == 1 |
          plotData$TempColumn[i + 1] == 1)){
        
        plotData$TempColumn[i] <-  1
        plotData$TempColumn[i + 1] <-  1
        
      }
      
    }
    
    plotData <- plotData %>% filter(TempColumn == 0)
    
} 







data.all <- data.all %>% filter(GroupID %in% c("001", "2", "1324","2019", "2020research", "cowabunga", "cooldudes33", "12453235")) %>%
  filter(Level == "Tutorial", Track == "Tutorial")

nlevels(droplevels(as.factor(data.all$Body)))




droplevels(as.factor(data.all$PlayerID))

summary(aov(FinishTime ~ PlayerID, data = data.all))





test <- data.all %>% filter(Track == "Tutorial", GroupID %in% c("stest", "1", "12345"))


nlevels(drop.levels(test$Engine))









#Running the Paired Randomization Test

#Differences Vector
diffvec <- group1vec - group2vec

group1vec <- c(4,5,6)
group2vec <- c(3,3,-4)

mean(group1vec) - mean(group2vec)

diffvec <- c(1,2,10,-6,-1,2,-3,4,1,1,-1,-3,5,6,1,2,-3,4,3,-7,-5,1,2,4,6,4,10,-20,-4,4,1,20,3,4,1)*3
#diffvec <- c(1,2,10,2,5,12,2,5,14,3,10)

diffvec <- c(-2,4,8)

diffvec <- c(1,2,10)

#diffvec <- c(-1,2,-10,2,5,-12,-2,5,14,3,10)

#diffvec <- c(20.06 - 19.3, 20.84 -19.88, 20.02 - 19.11)


#Setting up
meandiff <- mean(diffvec)
R <- 100000
results <- numeric()
results1 <- numeric()

#Adjusting Data to be Under Null
diffvecadj <- diffvec - rep(mean(diffvec), length(diffvec))

for(i in 1:R){
  samp <- sample(c(-1,1), size = length(diffvec), replace = TRUE)
  nextt <- samp * diffvec
  results[i] <- mean(nextt)
  
  samp <- base::sample(diffvecadj, length(diffvecadj), replace = T)
  t <- mean(samp)
  results1[i] <- t
  
}

lower <- sum(results <= -abs(meandiff))
upper <- sum(results >= abs(meandiff))

lower1 <- sum(results1 <= -abs(meandiff))
upper1 <- sum(results1 >= abs(meandiff))

pvalue <- (1+lower+upper) /(R+1)

pvalue1 <- (1+lower1+upper1) /(R+1)


pvalue <- (1 + sum(results1 >= abs(meandiff)) + sum(results1 <= -abs(meandiff))) / (R+1)

return(paste("P Value:", round(pvalue,4)))







#Notes for Friday

#Two Sample T Test residuals
  #Observed - Predicted
  #For group 1, observed is values for group 1 and predicted is the mean for group 1
  #For group 2, obvserved is values for group2, and predicted is the mean for group2

#Paired T Test residuals
  #Observed - Predicted
  #Observed is the differences from our sample and predicted is the mean difference
  #Since we are looking at differences (paired), there should be half as many points
  
#Residuals for Two Sample Randimization Test should be the same as the Two Sample T Test
#Residuals for Paired Randomization Test should be the same as the Paired T Test

#Two Sample Randomization Test is working well
#Paired Randimization Test - Use code that is online

#Make sure to compare residual plots on minitab vs the shiny app






















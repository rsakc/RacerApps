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











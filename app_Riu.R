library(shiny)
library(readr)
library(ggplot2)
library(dplyr)
library(broom)
library(tidyr)
library(purrr)
library(gdata)
library(DescTools)
library(schoolmath)

##RACER1 DATA

#Raw Data
#data.all <-read.csv("https://www.stat2games.sites.grinnell.edu/data/racer/getdata.php") 

#Cleaner Data
data.all <-read.csv("data/RacerClean.csv") 


#Reordering Rows if Needed
for(i in 1:nrow(data.all)){
  
  if(i == nrow(data.all)){
    break
  }
  if(data.all$PlayerID[i] == data.all$PlayerID[i + 1] &
     data.all$Level[i] == data.all$Level[i + 1] &
     data.all$Track[i] == data.all$Track[i + 1]){
    
    if(data.all$Order[i] > data.all$Order[i + 1]){
      
      temp <- data.all$Order[i + 1]
      data.all$Order[i + 1] <- data.all$Order[i]
      data.all$Order[i] <- temp
      
      
    }
  }
}

#Only Keeping Paired Data
for(i in 1:(nrow(data.all))){
  
  if(i == nrow(data.all)){
    
    break
  }
  
  if(IsOdd(data.all$Order[i]) == TRUE){
    
    if(data.all$PlayerID[i] != data.all$PlayerID[i + 1]){
      data.all <- data.all[-i,]
      
    }
  }
  
  if(i != 1){
    
    if(is.even(data.all$Order[i]) == TRUE &
       data.all$PlayerID[i] != data.all$PlayerID[i + 1] &
       data.all$PlayerID[i] != data.all$PlayerID[i - 1]){
      
      data.all <- data.all[-i,]
    }
  }
}

#data.all$Level <- as.factor(data.all$Level)
data.all$GroupID <- as.character(data.all$GroupID)
data.all$PlayerID <- as.character(data.all$PlayerID)
data.all$Track<- as.factor(data.all$Track)

data.all <- filter(data.all, FinishTime < 100)
data.all <- filter(data.all, Body == "Bayes" | Body == "Nightingale" | Body == "Gauss")
data.all <- filter(data.all, Level == "Tutorial" | Level == "Paired")
data.all <- filter(data.all, Track == "Tutorial" | Track == "StraightTrack" | Track == "OvalTrack" | Track == "8Track" | Track == "ComplexTrack" | Track == "VeryComplexTrack")

#Filtering Date
#data.all <- data.all %>% separate(GameDate, c("Date", "Time"), " ")
#data.all$Date <- as.Date(data.all$Date, format = "%m/%d/%Y")
#data.all <- data.all %>% filter(Date >= as.Date("01/01/2020", format = "%m/%d/%Y"))



#Adding Order2 Column
data.all$Order2 <- NA

for(i in 1:nrow(data.all)){
  
  if(IsOdd(data.all$Order[i]) == TRUE){
    data.all$Order2[i] <- 1
 
  } else{
       
    data.all$Order2[i] <- 2
    }
}

#Adding Player2 Column
data.all$PlayerID2 <- NA

counter <- 1
for(i in 1:nrow(data.all)){

  if(i == nrow(data.all)){
    break
  }
  
  if(data.all$PlayerID[i] == data.all$PlayerID[i+1] &
     data.all$Track[i] == data.all$Track[i + 1] &
     data.all$Level[i] == data.all$Level[i + 1]){
  
    if(IsOdd(data.all$Order2[i]) == TRUE &
      is.even(data.all$Order2[i + 1]) == TRUE &
      data.all$Order2[i+1] == data.all$Order2[i] + 1){
      
      data.all$PlayerID2[i] <- paste(data.all$PlayerID[i], counter, sep = "")
      data.all$PlayerID2[i + 1] <- paste(data.all$PlayerID[i], counter, sep = "")
  }
    
    if(is.even(data.all$Order2[i]) == TRUE &
      IsOdd(data.all$Order2[i + 1] == TRUE)){
     
       counter <- counter + 1
      } 
  
  } else {
   counter <- 1
  }
   
   
}

data.all <- data.all %>% filter(!(is.na(data.all$PlayerID2)))

#Data for Checkbox
removed.data <- data.all

removed.data$TempColumn <- 0
  
  for(i in 1:nrow(removed.data)){
    
    if(removed.data$TimeOffTrack[i] > 0.3){
      
      removed.data$TempColumn[i] <- 1
    }
  }
  
  for(i in 1:nrow(removed.data)){
    
    if(i == nrow(removed.data)){
      
      break
    }
    
    if(removed.data$PlayerID2[i] != removed.data$PlayerID2[i + 1]){}
    
    
    
    if(removed.data$PlayerID2[i] == removed.data$PlayerID2[i + 1] &
       (removed.data$TempColumn[i] == 1 |
        removed.data$TempColumn[i + 1] == 1)){
      
      removed.data$TempColumn[i] <-  1
      removed.data$TempColumn[i + 1] <-  1
      
    }
    
  }
  
  removed.data <- removed.data %>% filter(TempColumn == 0)
  



#For Inputs
all_groups <- sort(unique(data.all$GroupID))
all_players <- sort(unique(data.all$PlayerID))
all_tracks <- sort(unique(data.all$Track))


ui <- fluidPage(
  # App title ----
  titlePanel("Racer Hypothesis Tests"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput(inputId = "groupID",
                  label = "Group ID:", 
                  choices =  c(all_groups),
                  multiple = TRUE,
                  selectize = TRUE,
                  selected = "stest"),
      
      selectInput("levels", "Level",
                  choices = c("Tutorial", "Paired"),
                  multiple = TRUE,
                  selectize = TRUE,
                  selected = "Tutorial"),
      
      selectInput(inputId = "playerID",
                  label = "Player ID:",
                  choices =  c("all", all_players),
                  multiple = TRUE,
                  selectize = TRUE,
                  selected = "all"),
      
      selectInput(inputId = "tracks",
                  label = "Track:",
                  choices =  c("Tutorial", "StraightTrack", "OvalTrack", "8Track", "ComplexTrack"),
                  multiple = FALSE,
                  selectize = TRUE,
                  selected = "Tutorial"),
      
      selectInput(inputId = "xvar",
                  label = "X Axis:",
                  #columns of the dataset
                  choices = c("Body", "Engine", "Tire", "Track", "Order", "PlayerID"),
                  selected = "Body",
                  multiple = FALSE),
      
      selectInput(inputId = "yvar",
                  label = "Y Axis:",
                  #columns of the dataset
                  choices = c("FinishTime", "TimeTo30", "TimeTo60", "TopSpeedReached"),
                  selected = "FinishTime",
                  multiple = FALSE),
      
      checkboxInput('bplot',"Add boxplot",FALSE),
      
      selectInput(inputId = "color",
                  label = "Color by",
                  choices = c("Body", "Engine", "Tire", "Track", "Order", "PlayerID"),
                  selected = "Body",
                  multiple = FALSE),
      
      selectInput(inputId = "tests",
                  label = "Statistic Tests",
                  choices = c("None", "two-sample t-test", "paired t-test", "ANOVA", "Block Design"),
                  selected = "None",
                  multiple = FALSE),
      
      checkboxInput("filterPData","Filter Paired Data",FALSE),
      
      
      
      downloadButton('downloadData', label = "Racer Data")
      
    ),
    
    mainPanel(
      
      plotOutput(outputId = "Plot"),
      #### Change to t_test not "Why"
      verbatimTextOutput("why"),
      verbatimTextOutput("why2"),
      tableOutput("tbl1"),
      tableOutput("blocked"),
      h3(textOutput("caption")),
      verbatimTextOutput("why3"),
      verbatimTextOutput("why4")
      
    )
  )
)

server <- function(input, output,session) {
  
  plotData <- reactive({
    if("all" %in% input$playerID)
    {filter(data.all, GroupID %in% input$groupID, Level %in% input$levels)}
    else{filter(data.all, GroupID %in% input$groupID, Level %in% input$levels, PlayerID %in% input$playerID)}
    
  })  
  
  plotDataR <- reactive({
    if("all" %in% input$playerID)
    {filter(removed.data, GroupID %in% input$groupID, Level %in% input$levels)}
    else{filter(removed.data, GroupID %in% input$groupID, Level %in% input$levels, PlayerID %in% input$playerID)}
    
  })  
  
  
  
  
  # Updates PlayerID based upon GroupID
  observe({
    # req() requires a selection from GroupID before any output
    # or reactivity occurs (keeps the app from crashing)
    req(input$groupID)   
    
    if(input$filterPData == "FALSE"){
    
    
    if ("all" %in% input$groupID) {gamedata <- data.all}
    else{gamedata <- filter(data.all, GroupID %in% input$groupID)}
    
    updateSelectInput(session, 
                      "playerID",
                      choices = c("all", sort(unique(gamedata$PlayerID))),
                      selected = "all")
    
    } else {
      
      if ("all" %in% input$groupID) {gamedata <- removed.data}
      else{gamedata <- filter(removed.data, GroupID %in% input$groupID)}
      
      updateSelectInput(session, 
                        "playerID",
                        choices = c("all", sort(unique(gamedata$PlayerID))),
                        selected = "all")
      
      
    }
    
  })

  
  
  # Creates Plot 
  output$Plot <- renderPlot({
    req(input$groupID)
    
    if(input$filterPData == "TRUE"){
      
      plotData <- plotDataR()
    
    } else {
        
      plotData <- plotData()
     
      }
   
    plotData <- plotData[plotData$Level %in% input$levels, ]
    plotData <- plotData[plotData$Track %in% input$tracks, ]
    
    if("all" %in% input$playerID) {
      plotData <- plotData[plotData$GroupID %in% input$groupID, ]
    }
    else{
      plotData <- plotData[plotData$GroupID %in% input$groupID, ]
      plotData <-  plotData[plotData$PlayerID %in% input$playerID, ]
    }
    
    
    if (input$bplot == "TRUE"){
      
      cols <- c("Bayes" = "blue", "Gauss" = "red", "Nightingale" = "orange")
      myplot <- ggplot(data = plotData, aes_string(x = input$xvar, y = input$yvar, color=input$color)) +
        geom_boxplot() +
        geom_point(position=position_dodge(0.8)) +
        #geom_dotplot(binaxis='y', stackdir='center', dotsize = .5, position=position_dodge(0.8)) + 
        #stat_summary(fun.y=mean, geom="point", shape = 18,
        #             size=3, color="red") +
        labs(x = input$xvar, y = input$yvar, title = paste("Plot of",input$yvar, "by",input$xvar, "and colored by", input$color)) +
        theme(axis.text.x = element_text(size = 14), 
              axis.title = element_text(size = 16), 
              plot.title = element_text(size = 18, face = "bold"),
              legend.title = element_text(size = 14), 
              legend.text = element_text(size = 12), 
              axis.text.y = element_text(size = 11)) +
        scale_color_manual(values = cols)
      
   
       } else {
      
      cols <- c("Bayes" = "blue", "Gauss" = "red", "Nightingale" = "orange")   
      myplot <- ggplot(data = plotData, aes_string(x = input$xvar, y = input$yvar, color=input$color), plot.title = element_text(size = 18)) +
        #geom_boxplot()+
        geom_point(position=position_dodge(0.8)) +
        #geom_dotplot(binaxis='y', stackdir='center', dotsize = .5, position=position_dodge(0.8)) + 
        #stat_summary(fun.y=mean, geom="point", shape = 18,
        #             size=3, color="red") +
        labs(x = input$xvar, y = input$yvar, title = paste("Plot of",input$yvar, "by",input$xvar, "and colored by", input$color)) +
        theme(axis.text.x = element_text(size = 14), 
              axis.title = element_text(size = 16), 
              plot.title = element_text(size = 18, face = "bold"),
              legend.title = element_text(size = 14), 
              legend.text = element_text(size = 12), 
              axis.text.y = element_text(size = 11)) +
        scale_color_manual(values = cols)
      
      
    }
    
    
    
    output$why3 = renderPrint({
      
      if(input$filterPData == "TRUE"){
        
        plotData <- plotDataR()
        
      } else {
        
        plotData <- plotData()
        
      }
      
      YVariable = plotData %>% pull(input$yvar)
      XVariable = plotData %>% pull(input$xvar)
      ColorVariable = plotData %>% pull(input$color)
      ColorVariable = droplevels(ColorVariable)
      XVariable = drop.levels(XVariable)
      YVariable = drop.levels(YVariable)
      if (input$tests == "ANOVA") {
        if(nlevels(ColorVariable) > 1){
          anovatest = anova(aov(YVariable ~ XVariable + ColorVariable + XVariable*ColorVariable))
        }
        
        else{
          anovatest = anova(aov(YVariable ~ XVariable))
          
        }
        
        check2 = tidy(anovatest)
        sum_df = sum(check2$df)
        sum_ss = sum(check2$'sumsq')
        sum_df
        sum_ss
        check2 = add_row(check2,term = "Total", df = sum_df, sumsq = sum_ss)
        check2$sumsq = round(check2$sumsq, digits = 2)
        check2$meansq = round(check2$meansq, digits = 2)
        check2$statistic = round(check2$statistic, digits = 2)
        # check2[is.na(check2)] = " "
        for(i in 1:(length(check2$p.value) - 2)){
          if(check2$p.value[i] < 0.001){
            check2$p.value[i] = "<0.001"
          } 
          
          else{
            check2$p.value[i] = round(check2$p.value[i], digits = 4)
          } 
          
        }
        
        check2
      }
    })
    
    output$why4 = renderPrint({
      
      if(input$filterPData == "TRUE"){
        
        plotData <- plotDataR()
        
      } else {
        
        plotData <- plotData()
        
      }
      
      YVariable = plotData %>% pull(input$yvar)
      XVariable = plotData %>% pull(input$xvar)
      ColorVariable = plotData %>% pull(input$color)
      ColorVariable = droplevels(ColorVariable)
      PlayerID = plotData$PlayerID
      if (input$tests == "Block Design") {
        
        if(nlevels(ColorVariable) > 1){
          anovatest = aov(YVariable ~ PlayerID + XVariable + ColorVariable + XVariable*ColorVariable)
          
        }
        
        else{
          anovatest = aov(YVariable ~ PlayerID + XVariable)
          
        }
        check2 = tidy(anovatest)
        options(digits = 3)
        sum_df = sum(check2$df)
        sum_ss = sum(check2$'sumsq')
        sum_df
        sum_ss
        check2$sumsq = round(check2$sumsq, digits = 2)
        check2$meansq = round(check2$meansq, digits = 2)
        check2$statistic = round(check2$statistic, digits = 2)
        check2 = add_row(check2,term = "Total", df = sum_df, sumsq = sum_ss)
        ## check2[is.na(check2)] = " "
        for(i in 1:(length(check2$p.value) - 2)){
          if(check2$p.value[i] < 0.005){
            check2$p.value[i] = "<0.005"
          } 
          
          else{
            check2$p.value[i] = round(check2$p.value[i], digits = 4)
          } 
          
        }
        
        check2
      }
    })
    
    output$why = renderPrint({
      
      if(input$filterPData == "TRUE"){
        
        plotData <- plotDataR()
        
      } else {
        
        plotData <- plotData()
        
      }
      
      YVariable = plotData %>% pull(input$yvar)
      XVariable = plotData %>% pull(input$xvar)
      ColorVariable = plotData %>% pull(input$color)
      ColorVariable = droplevels(ColorVariable)
      colorlevel = nlevels(ColorVariable)
      if (input$tests == "two-sample t-test"){
        
        if(input$xvar == input$color) {
          dropped = droplevels(XVariable)
          
          if(nlevels(dropped) == 2) {
            t.test(YVariable ~ XVariable)
          }
          else{
            "t-tests are only valid with there are exactly two groups."
          }
        }  
        else{
          "The X-axis and the Color variable should be the same for a t-test."
        }
      }
    })
    
    
    output$why2 = renderPrint({
      
      if(input$filterPData == "TRUE"){
        
        plotData <- plotDataR()
        
      } else {
        
        plotData <- plotData()
        
      }
      
      YVariable = plotData %>% pull(input$yvar)
      XVariable = plotData %>% pull(input$xvar)
      ColorVariable = plotData %>% pull(input$color)
      ColorVariable = droplevels(ColorVariable)
      colorlevel = nlevels(ColorVariable)
      if (input$tests == "paired t-test"){
        
        if(input$xvar == input$color) {
          dropped = droplevels(XVariable)
          
          if(input$levels =="Tutorial" & input$tracks =="Tutorial") {
            ###In the following we want Car to be input$xvar and FinishTime to be input$yvar, but it won't run then
            plotData = arrange(plotData, PlayerID, Order, Car)
            #Xvar <- as.data.frame(plotData$input$xvar)
            #names(Xvar) <- ("Xvar")
            #Yvar <- as.data.frame(plotData$input$yvar)
            #names(Xvar) <- ("Yvar")
            #pairs2 <- bind_cols(Xvar, Yvar, plotData)
            ### If Order is odd and Order +1 is odd, then drop Order (not an even number of tests)
            pairs2 <- mutate(plotData, odds2 = ifelse(Order%% 2 != 0 & lead(Order)%% 2 != 0, 0,1))
            pairs2 <- filter(pairs2, odds2 == 1)
            pairs2 = arrange(pairs2, Car, PlayerID, Order)
            
            t.test(FinishTime ~ Car, data = pairs2, paired = TRUE)
          }
          else{
            "Paired tests can only be calculated for races completed in the Tutorial or Paired t-test game options."
          } 
        }  
        else{
          "The X-axis and the Color variable should be the same for a t-test."
        }
      }
    })
    
    
    myplot
     
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('Data-', Sys.Date(), '.csv', sep="")
    },
    content = function(con) {
      write.csv(plotData(), con)
      
    })
  
}

shinyApp(ui = ui, server = server)



#Loading Libraries
library(shiny)
library(shinythemes)
library(broom)
library(dplyr)
library(gdata)
library(ggplot2)
library(stringr)
 
#Importing Data
data.all <-read.csv("https://www.stat2games.sites.grinnell.edu/data/racer/getdata.php") 

#Filtering Data
data.all <- filter(data.all, Body == "Bayes" | Body == "Nightingale" | Body == "Gauss")
data.all <- filter(data.all, Level == "Tutorial" | Level == "Paired" | Level == "ChooseCar")
data.all <- filter(data.all, Track == "Tutorial" | Track == "StraightTrack" | Track == "OvalTrack" | Track == "8Track" | Track == "ComplexTrack" )
data.all <- drop.levels(data.all)

## 2020 Data Only (Note there was one bad game in Jan so we start on Feb 1)
## This should now work for any date:
data.all <- data.all %>% mutate(Date = str_sub(GameDate, 1, 10))
data.all$Date <- as.Date(data.all$Date, format = "%m/%d/%Y")
data.all <- data.all %>% filter(Date >= as.Date("02/01/2020", format = "%m/%d/%Y"))

#Making GroupID and PlayerID a character vector
data.all$GroupID <- as.character(data.all$GroupID)
data.all$PlayerID <- as.character(data.all$PlayerID)

#Sort Data in order to create an accurate Order column
data.all <- data.all %>% arrange(Level, Track, GroupID, PlayerID, Game)


#Make a working Order column called Order2
data.all$Order2 <- 0
data.all$Order2[1] <- 1 
for(i in 2:nrow(data.all)){
  if(data.all$PlayerID[i] == data.all$PlayerID[i - 1] &
     data.all$GroupID[i] == data.all$GroupID[i - 1] &
     data.all$Track[i] == data.all$Track[i - 1] &
     data.all$Level[i] == data.all$Level[i - 1]){
    
    data.all$Order2[i] <- data.all$Order2[i - 1] + 1
  } 
  
  else {
    data.all$Order2[i] <- 1
  }
}

## We only kept the first two clean races for data.clean
data.clean <- data.all
data.clean <- filter(data.clean, Order2 < 3)

## Don't keep if they only played one race
data.clean$Clean <- "Yes"
for(i in 1:nrow(data.clean)){
  
  if(i == nrow(data.clean)){
    if(data.clean$Order2[i] == 1){
      data.clean$Clean[i] <- "No"
    }
  
  }
  
  else if(i != nrow(data.clean)){
    if(data.clean$Order2[i] == 1 & data.clean$Order2[i+1] == 1){
    data.clean$Clean[i] <- "No"
  }
 }
}

# Don't keep if they played the same car twice
for(i in 2:nrow(data.clean)){
  if(data.clean$PlayerID[i] == data.clean$PlayerID[i - 1] &
     data.clean$Car[i] == data.clean$Car[i - 1]){
    data.clean$Clean[i - 1] <- "No" 
    data.clean$Clean[i] <- "No"
  }
}

## We need a checkbox (Use "Only Clean Data)
data.clean <- filter(data.clean, Clean == "Yes")

## ONLY if the data is clean, we can then filter to eliminate BadDrivers
## If the "Only Clean Data" is checked, then we can also checkbox "Only Good Drivers"

## We need to take the track into account for bad drivers.
data.clean$BadDriver <- "No"
for(i in 2:nrow(data.clean)){
  if(data.clean$Track[i] == "OvalTrack" &
     data.clean$PlayerID[i] == data.clean$PlayerID[i - 1] &
     (data.clean$FinishTime[i] + data.clean$FinishTime[i - 1] > 60 |
      data.clean$TimeOffTrack[i] + data.clean$TimeOffTrack[i - 1] > 3)){
    data.clean$BadDriver[i - 1] <- "Yes"
    data.clean$BadDriver[i] <- "Yes"
  }
}

for(i in 2:nrow(data.clean)){
  if(data.clean$Track[i] == "Tutorial" &
     data.clean$PlayerID[i] == data.clean$PlayerID[i - 1] &
     (data.clean$FinishTime[i] + data.clean$FinishTime[i - 1] > 50 |
      data.clean$TimeOffTrack[i] + data.clean$TimeOffTrack[i - 1] > 1)){
    data.clean$BadDriver[i - 1] <- "Yes"
    data.clean$BadDriver[i] <- "Yes"
  }
}

for(i in 2:nrow(data.clean)){
  if(data.clean$Track[i] == "StraightTrack" &
     data.clean$PlayerID[i] == data.clean$PlayerID[i - 1] &
     (data.clean$FinishTime[i] + data.clean$FinishTime[i - 1] > 40 |
      data.clean$TimeOffTrack[i] + data.clean$TimeOffTrack[i - 1] > 1)){
    data.clean$BadDriver[i - 1] <- "Yes"
    data.clean$BadDriver[i] <- "Yes"
  }
}

for(i in 2:nrow(data.clean)){
  if((data.clean$Track[i] == "8Track" | data.clean$Track[i] == "ComplexTrack") &
     data.clean$PlayerID[i] == data.clean$PlayerID[i - 1] &
     data.clean$TimeOffTrack[i] + data.clean$TimeOffTrack[i - 1] > 1){
    data.clean$BadDriver[i - 1] <- "Yes"
    data.clean$BadDriver[i] <- "Yes"
  }
}

##Checkbox for "Only Good Drivers"
data.good = filter(data.clean, BadDriver == "No")

#Making Order 2 a factor for the three data sets
data.all$Order2 <- as.factor(data.all$Order2)
data.clean$Order2 <- as.factor(data.clean$Order2)
data.good$Order2 <- as.factor(data.good$Order2)


#To use for Inputs
all_groups <- sort(unique(data.all$GroupID))
all_players <- sort(unique(data.all$PlayerID))
all_tracks <- sort(unique(data.all$Track))


#UI
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  # App title ----
  titlePanel("Racer Hypothesis Tests"),
  
  fluidRow(
    column(2,
           
           selectInput(inputId = "groupID",
                       label = "Group ID:", 
                       choices =  c(all_groups),
                       multiple = TRUE,
                       selectize = TRUE,
                       selected = "stest"),
           
           selectInput(inputId = "playerID",
                       label = "Remove Player ID:",
                       choices =  c(all_players),
                       multiple = TRUE,
                       selectize = TRUE),
           
           selectInput("levels", "Level",
                       choices = c("Tutorial", "Paired", "ChooseCar"),
                       multiple = FALSE,
                       selectize = TRUE,
                       selected = "Tutorial"),
           
           selectInput(inputId = "tracks",
                       label = "Track:",
                       choices =  c("Tutorial", "StraightTrack", "OvalTrack", "8Track", "ComplexTrack"),
                       multiple = FALSE,
                       selectize = TRUE,
                       selected = "Tutorial"),
           
           selectInput(inputId = "xvar",
                       label = "X Variable:",
                       #columns of the dataset
                       choices = c("Body", "Engine", "Tire", "Track", "Order2", "PlayerID"),
                       selected = "Body",
                       multiple = FALSE),
           
           selectInput(inputId = "yvar",
                       label = "Y Variable:",
                       #columns of the dataset
                       choices = c("FinishTime", "TopSpeedReached", "TimeTo30", "TimeTo60"),
                       selected = "FinishTime",
                       multiple = FALSE)
           
           
    ),
    
    column(2, 
           selectInput(inputId = "color",
                       label = "Color by",
                       choices = c("Body", "Engine", "Tire", "Track", "Order2", "PlayerID"),
                       selected = "Body",
                       multiple = FALSE),
           
           selectInput(inputId = "tests",
                       label = HTML("Statistical Test <br/> (for X Variable)"),
                       choices = c("None", "two-sample t-test", "paired t-test", "ANOVA", "Block Design"),
                       selected = "None",
                       multiple = FALSE),
           
           checkboxInput('bplot',"Add boxplot",FALSE),
           checkboxInput("summary", "Show Summary Statistics", FALSE),
           
           radioButtons(inputId = "data",
                        label = "Choose Data", 
                        choices = c("All Data", "Clean Data"),
                        selected = "All Data",
                        inline = TRUE),
           uiOutput("gooddriver"),
           
           downloadButton('downloadData', label = "Racer Data"),
           
           
           # a(h5("Tutorial Video"),
           #   href="https://www.youtube.com/watch?v=JZDQVHVNC10",
           #   aligh= "left", target="_blank"),
           
           a(h5("Instructor Details"),
             href="https://stat2labs.sites.grinnell.edu/racer.html", 
             align="left", target = "_blank")
           
           
           
           
    ),
    column(8,
           
           #Outputs
           plotOutput(outputId = "Plot"),
           verbatimTextOutput("twosamp"),
           verbatimTextOutput("paired"),
           verbatimTextOutput("anova"),
           verbatimTextOutput("blocked"),
           tableOutput("summarytable"),
           uiOutput("summarytext")
           
    ))
  
  
)



#Server
server <- function(input, output,session) {
  
  
  
  #Dynamic Input to filter only the good drivers
  output$gooddriver <- renderUI({

    req(input$data)

    if(input$data == "Clean Data"){

      checkboxInput(inputId = "gooddata",
                    label = "Good Driver Data",
                    value = FALSE)
    }
  })
  
  
  #Reactive Data for all three data types
  plotDataR <- reactive({
    
    req(input$data)
    
    if(input$data == "All Data"){
      data <- filter(data.all, GroupID %in% input$groupID, Level %in% input$levels, Track %in% input$tracks, !(PlayerID %in% input$playerID))
 
    } else if(input$data == "Clean Data"){
     data <-  filter(data.clean, GroupID %in% input$groupID, Level %in% input$levels, Track %in% input$tracks, !(PlayerID %in% input$playerID))
      
      if(input$gooddata == "TRUE"){
      data <- filter(data.good, GroupID %in% input$groupID, Level %in% input$levels, Track %in% input$tracks, !(PlayerID %in% input$playerID))
    
      } 
   
    } 
    return(data)
    
  })
  
  
  # Making Remove PlayerID Input dynamic
  observe({
    
    # req() requires a selection from GroupID before any output
    # or reactivity occurs (keeps the app from crashing)
    req(input$groupID) 
    
    if(input$data == "All Data"){
      
      gamedata <- filter(data.all, GroupID %in% input$groupID, Level %in% input$levels, Track %in% input$tracks)
      
      updateSelectInput(session, 
                        "playerID",
                        choices = c(sort(unique(gamedata$PlayerID))))
      
      
    } else if(input$data == "Clean Data"){
      
      gamedata <- filter(data.clean, GroupID %in% input$groupID, Level %in% input$levels, Track %in% input$tracks)
      
      updateSelectInput(session, 
                        "playerID",
                        choices = c(sort(unique(gamedata$PlayerID))))
      
    }
    
    
    #Doesn't work right now
    else if(input$gooddata == "TRUE"){
      gamedata <- filter(data.good, GroupID %in% input$groupID, Level %in% input$levels, Track %in% input$tracks)
      
      updateSelectInput(session, 
                        "playerID",
                        choices = c(sort(unique(gamedata$PlayerID))))
    }
    
  })
  

  # Creating Vizualizations
  output$Plot <- renderPlot({
    
    #Requiring inputs
    req(input$data)
    req(input$groupID)
    
    #Using Reactive Data
    plotData <- plotDataR()
    
    #If boxplot option is selected
    if (input$bplot == "TRUE"){
      
      #ggplot with manual colors if color by option is Body, Engine, or Tire
      if(input$color %in% c("Body", "Engine", "Tire") == TRUE){
        cols <- c("Bayes" = "blue", "Gauss" = "red", "Nightingale" = "orange")
        myplot <- ggplot(data = plotData, aes_string(x = input$xvar, y = input$yvar, color=input$color)) +
          geom_boxplot() +
          geom_point(position=position_dodge(width = 0.75), size = 3) +
          labs(x = input$xvar, y = input$yvar, title = paste("Plot of",input$yvar, "by",input$xvar, "and colored by", input$color)) +
          theme_bw() +
          theme(axis.text.x = element_text(size = 18, angle = 50, hjust = 1), 
                axis.title = element_text(size = 20), 
                plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
                legend.title = element_text(size = 18), 
                legend.text = element_text(size = 16), 
                axis.text.y = element_text(size = 14)) +
          scale_color_manual(values = cols)
        
        #Using automatic colors
      } else {
        
        myplot <- ggplot(data = plotData, aes_string(x = input$xvar, y = input$yvar, color=input$color)) +
          geom_boxplot() +
          geom_point(position=position_dodge(width = 0.75), size = 3) +
          labs(x = input$xvar, y = input$yvar, title = paste("Plot of",input$yvar, "by",input$xvar, "and colored by", input$color)) +
          theme_bw() +
          theme(axis.text.x = element_text(size = 18, angle = 50, hjust = 1), 
                axis.title = element_text(size = 20), 
                plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
                legend.title = element_text(size = 18), 
                legend.text = element_text(size = 16), 
                axis.text.y = element_text(size = 14)) 
        
        
      }
      
      #If boxplot option is not selected  
    } else {
      
      #ggplot with manual colors if color by option is Body, Engine, or Tire
      if(input$color %in% c("Body", "Engine", "Tire") == TRUE){
        cols <- c("Bayes" = "blue", "Gauss" = "red", "Nightingale" = "orange")   
        myplot <- ggplot(data = plotData, aes_string(x = input$xvar, y = input$yvar, color=input$color), plot.title = element_text(size = 18)) +
          geom_point(position= position_dodge(width = 0.1), size = 3) +
          labs(x = input$xvar, y = input$yvar, title = paste("Plot of",input$yvar, "by",input$xvar, "and colored by", input$color)) +
          theme_bw() +
          theme(axis.text.x = element_text(size = 18, angle = 50, hjust = 1), 
                axis.title = element_text(size = 20), 
                plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
                legend.title = element_text(size = 18), 
                legend.text = element_text(size = 16), 
                axis.text.y = element_text(size = 14)) +
          scale_color_manual(values = cols)
        
        #Using automatic colors
      } else{
        myplot <- ggplot(data = plotData, aes_string(x = input$xvar, y = input$yvar, color=input$color), plot.title = element_text(size = 18)) +
          geom_point(position =position_dodge(width = 0.1), size = 3) +
          labs(x = input$xvar, y = input$yvar, title = paste("Plot of",input$yvar, "by",input$xvar, "and colored by", input$color)) +
          theme_bw() +
          theme(axis.text.x = element_text(size = 18, angle = 50, hjust = 1), 
                axis.title = element_text(size = 20), 
                plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
                legend.title = element_text(size = 18), 
                legend.text = element_text(size = 16), 
                axis.text.y = element_text(size = 14))
        
      }
    }
    
    
    #ANOVA Output
    output$anova = renderPrint({

      #Using Reactive Data
      plotData <- plotDataR()
      
      #Setting Up
      YVariable = plotData %>% pull(input$yvar)
      XVariable = plotData %>% pull(input$xvar)
      ColorVariable = plotData %>% pull(input$color)
      ColorVariable = drop.levels(ColorVariable)
      XVariable = drop.levels(as.factor(XVariable))
      
      if(input$tests == "ANOVA") {
        
        #Two way ANOVA
        if(nlevels(ColorVariable) > 1){
          anovatest = anova(aov(YVariable ~ XVariable + ColorVariable + XVariable*ColorVariable))
        }
        
        #One way ANOVA
        else{
          anovatest = aov(YVariable ~ XVariable)
        }
        
        #Making Tidy table and adding columns/rows
        check2 = tidy(anovatest)
        sum_df = sum(check2$df)
        sum_ss = sum(check2$'sumsq')
        sum_df
        sum_ss
        check2 = add_row(check2,term = "Total", df = sum_df, sumsq = sum_ss)
        check2$sumsq = round(check2$sumsq, digits = 2)
        check2$meansq = round(check2$meansq, digits = 2)
        check2$statistic = round(check2$statistic, digits = 2)
        
        
        return(check2)
      }
    })
    
    #Blocked Design
    output$blocked = renderPrint({
      
      #Using Reactive Data
      plotData <- plotDataR()
      
      #Setting Up
      YVariable = plotData %>% pull(input$yvar)
      XVariable = plotData %>% pull(input$xvar)
      ColorVariable = plotData %>% pull(input$color)
      ColorVariable = drop.levels(ColorVariable)
      PlayerID = plotData$PlayerID
      
      if (input$tests == "Block Design") {
        
        #Error Message if PlayerID is selected as X-axis or Color
        if(input$xvar == "PlayerID" | input$color == "PlayerID"){
          
          "When using the Block Design, the X-axis/Color Variable cannot be PlayerID"
          
        } else {
          
          #Two Way Blocked ANOVA
          if(nlevels(ColorVariable) > 1){
            anovatest = aov(YVariable ~ PlayerID + XVariable + ColorVariable + XVariable*ColorVariable)
            
          }
          
          #One Way Blocked
          else{
            anovatest = aov(YVariable ~ PlayerID + XVariable)
            
          }
          
          #Making Tidy table and adding columns/rows
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
          
          return(check2)
        }
      }
    })
    
  
    
    #Two Sample T-Test
    output$twosamp = renderPrint({
      
      #Using Reactive Data
      plotData <- plotDataR()
      
      #Setting up
      YVariable = plotData %>% pull(input$yvar)
      XVariable = plotData %>% pull(input$xvar)
      ColorVariable = plotData %>% pull(input$color)
      ColorVariable = drop.levels(as.factor(ColorVariable))
      
      if (input$tests == "two-sample t-test"){
        
        #X-axis and Color option must be the same
        if(input$xvar == input$color) {
          dropped = drop.levels(as.factor(XVariable))
          
          #If there are two levels for the X-axis option, run the test
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
    
    
    #Paired T-Test
    output$paired = renderPrint({
      
      #Using Reactive Data
      plotData <- plotDataR()
      
      #Setting Up
      YVariable = plotData %>% pull(input$yvar)
      XVariable = plotData %>% pull(input$xvar)
      ColorVariable = plotData %>% pull(input$color)
      ColorVariable = drop.levels(as.factor(ColorVariable))
      
      
      if (input$tests == "paired t-test"){
        
        #Users need to use the Clean Data to run Paired T-Test
        if(input$data == "Clean Data"){
          
          #X-axis and Color option must be the same
          if(input$xvar == input$color) {
            dropped = drop.levels(as.factor(XVariable))
            
            #If there are two levels for the X-axis option, run the test
            if(nlevels(dropped) == 2) {
              t.test(FinishTime ~ Car, data = plotData, paired = TRUE)
              
            } else {
              "paired t-tests are only valid with there are exactly two groups."
            }
            
            
          } else{
            "The X-axis and the Color variable should be the same for a t-test."
          }
          
        } else{
          "Only Clean Data can be used for the paired t-test"
        }
        
      }
      
    })
    
    #Summary Table Output
    output$summarytable <- renderTable({
      
      #Using reactive data
      plotData <- plotDataR()
      
      if(input$summary == "TRUE"){
        
        #If there is data
        if(nrow(plotData) != 0){
        
        #Creating summary table
        stable <- plotData %>% select(input$xvar, input$yvar) %>% 
          rename(`X Variable` = input$xvar, Yvar = input$yvar) %>%
          group_by(`X Variable`) %>%
          summarize(N = n(), Mean = mean(Yvar), SD = sd(Yvar))
        
        #Removing dynamic help text
        output$summarytext <- renderUI({""})
        
        #If there is no data
        } else{
        
        #Empty data frame to return  
        stable <- data.frame()
        
        #Help Text
        output$summarytext <- renderUI(HTML(paste(
          em("There is no data"))))
        }
        
        return(stable)

      }
    
    })
    
    #Making sure help text goes away if checkbox is unchecked
    observeEvent(input$summary, {
     
       if(input$summary == "FALSE"){
        output$summarytext <- renderUI({""})
      }
    })
  
    return(myplot)
 
     })
  
  #Download Data
  output$downloadData <- downloadHandler(
    
    filename = function() {
      paste('Data-', Sys.Date(), '.csv', sep="")
    },
    content = function(con) {
      write.csv(plotDataR(), con)
    })
  
}

#Creating Shiny App
shinyApp(ui = ui, server = server)



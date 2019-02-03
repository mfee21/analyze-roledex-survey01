library(googlesheets)
library(ggplot2)
library(dplyr)
library(stringr)

# Functions

# Survey Variables
gs <- gs_title("TypeFormResult Survey 1.0") # Get specific google sheet
df <- gs_read(gs, ws="Survey 1.0")
questionChoices <- names(df[3:24])
questionChoices <- c(questionChoices, names(df[28:29]))
questionChoices <- c("Survey and Demographics", questionChoices)
observations <- nrow(df)

# Age Variables
  ageVector <- (df[['Age']])
  ageVector <- ageVector[!is.na(ageVector)]
  ageMean <- format((mean(ageVector)),digits=2,nsmall=2)
  ageRange <- (range(ageVector))
  ageRange <- paste(ageRange[1],ageRange[2], sep = "-")
 
# Genre Variables **********NEEDS MAJOR REFACTOR****************
  genreVector <- (df['What are your favorite role-playing genres?'])
   
  adventureCount  <-str_count(genreVector, "Adventure")
  fandomCount     <-str_count(genreVector, "Fandom") 
  fantasyCount    <-str_count(genreVector, "Fantasy") 
  historicalCount <-str_count(genreVector, "Historical")
  horrorCount     <-str_count(genreVector, "Horror") 
  modernCount     <-str_count(genreVector, "Modern") 
  mysteryCount    <-str_count(genreVector, "Mystery")
  romanceCount    <-str_count(genreVector, "Romance")
  scienceCount    <-str_count(genreVector, "Science")
  smutCount       <-str_count(genreVector, "Smut")
  
  genreCounts <- data.frame("answer" = c("Adventure","Fandom","Fantasy","Historical Fiction",
                                         "Horror/Supernatural/Survival","Modern/Contemporary/Realistic Fiction",
                                         "Mystery/Thriller/Suspense","Romance","Science Fiction","Smut/Erotic Fiction"),
                            "count" = c(adventureCount,fandomCount,fantasyCount,historicalCount,horrorCount,
                                        modernCount,mysteryCount,romanceCount,scienceCount,smutCount))

  genreCounts <- arrange(genreCounts,desc(count))

# Character Variables
  characterVector <- (df[['Describe your favorite type of character.']])
  characterVector <- characterVector[!is.na(characterVector)]
  maxCharacter <- length(characterVector)
  print(characterVector)
  
function(input,output) {
  
    # Reactive Variables
    
      counter <- reactiveValues(counterValue = 1)
      print(counter)
  
      # ========== Sidebar - Output Variables ============
      output$selectDisplayOptions <- renderUI ({
        selectInput(inputId = "displayChoice", label = "What data would you like to visualize?",choices = questionChoices)
      })

      # ============== Main Content - Output Variables ================== 
      
      output$title <- renderText(input$displayChoice)
     
      
       # ++++++++++++++++++++++++++++++ new stuffff +++++++++++++++++++++++++++++++
      output$body <- renderUI ({
      
          switch(input$displayChoice,
             
             # Case 1 -------------------------------------------
             "Survey and Demographics" = 
               box( 
                 title = textOutput("title"), width = 12, solidHeader = TRUE,
                 box(
                   title = "Summary", width = 12, solidHeader = TRUE,
                   infoBoxOutput("completedBox"),
                   infoBoxOutput("ageMeanBox"),
                   infoBoxOutput("ageRangeBox")
                 ),
                 box(
                   title = "Plot", width = 12, solidHeader = TRUE,
                   plotOutput("agePlot")
                 )
               ),
            
             # Case 2 -------------------------------------------
             "What are your favorite role-playing genres?" =
               box( 
                 title = textOutput("title"), width = 12, solidHeader = TRUE,
                 box(
                   title = "Summary", width = 12, solidHeader = TRUE,
                   infoBoxOutput("completedBox"),
                   infoBoxOutput("ageMeanBox"),
                   infoBoxOutput("ageRangeBox")
                 ),
                 box(
                   title = "Plot", width = 12, solidHeader = TRUE,
                   plotOutput("genrePlot")
                 )
               ),
             
             # Case 3 -------------------------------------------
             "Describe your favorite type of character." =
               box( 
                 title = textOutput("title"), width = 12, solidHeader = TRUE,
                 box(
                   title = "Summary", width = 12, solidHeader = TRUE,
                   infoBoxOutput("completedBox"),
                   infoBoxOutput("ageMeanBox"),
                   infoBoxOutput("ageRangeBox")
                 ),
                 box(
                   title = "Plot", width = 12, solidHeader = TRUE,
                    actionButton("sub1", width = '40%', label = "Previous", icon("step-backward")),
                    actionButton("add1", width = '40%', label = "Next", icon("step-forward")),
                   textOutput("characterText")
                   )                 
                 ),
             # Case 1 -------------------------------------------
             "What tools do you use?" = 
               box( 
                 title = textOutput("title"), width = 12, solidHeader = TRUE,
                 box(
                   title = "Summary", width = 12, solidHeader = TRUE,
                   infoBoxOutput("completedBox"),
                   infoBoxOutput("ageMeanBox"),
                   infoBoxOutput("ageRangeBox")
                 ),
                 box(
                   title = "Plot", width = 12, solidHeader = TRUE,
                   plotOutput("toolsPlot")
                 )
               )
                 
          )
        })
      
       observeEvent(input$add1, {
         if (counter$counterValue == maxCharacter) 
           {counter$counterValue <- 1}
         else 
           {counter$counterValue <- counter$counterValue + 1
            print(counter$counterValue)}
       })
       
       observeEvent(input$sub1, {
         if (counter$counterValue == 1) 
            {counter$counterValue <- maxCharacter}
         else 
          {counter$counterValue <- counter$counterValue - 1
           print(counter$counterValue)}
       })
      
      
      output$characterText <- renderText(paste("\"",characterVector[counter$counterValue],"\"", sep=""))
      
      
      # ouput$toolsPlot <- renderPlot({
      #   
      # })
      
      output$genrePlot <- renderPlot({
        ggplot(data = genreCounts, aes(x=answer, y = count)) +
          
          geom_bar(color = "#01082A", fill = "#2EB6B1", stat = "identity", position = position_stack(reverse = TRUE)) +
          coord_flip() +
          theme(text = element_text(size=20, face = "bold"))
      })
      
      output$agePlot <- renderPlot({ggplot(data = df, aes(df$Age)) +
          geom_histogram(color = "#01082A", fill = "#2EB6B1") + 
          labs(x="Age", y="Count") +
          theme(text = element_text(size=20, face = "bold"))
      }) 
      
      output$completedBox <- renderValueBox({ # Value Box - Completed Surveys
        infoBox("Surveys", observations, icon = icon("tasks"),
                color = "orange", fill = TRUE)
      })
      
      output$ageMeanBox <- renderValueBox({ # Value Box - Average Age
        infoBox("Mean Age", ageMean, icon = icon("users"),
                color = "blue", fill = TRUE)
      })
      
      output$ageRangeBox <- renderValueBox({ # Value Box - Age Range
        infoBox("Age Range", ageRange, icon = icon("user-friends"),
          color = "purple", fill = TRUE)
      })
    
  }

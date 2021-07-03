#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(caret)
setwd("C:\\Users\\acecil\\Desktop\\IFB Segmentation")
data <- read.csv("Final_Data_joined3.CSV", header = TRUE)
data<-data[complete.cases(data),]
#KNN_Model<-load(file="KNN_model.rda")
index = createDataPartition(data$Cluster, p = 0.7, list = F )
#head(index)
train = data[index,]
#head(train)
validation = data[-index,]



# convert training target variable to factor
train[["Cluster"]]=factor(train[["Cluster"]])
validation[["Cluster"]]=factor(validation[["Cluster"]])

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3333)
knn_model<-knn_fit <- train(Cluster ~., data = train, method = "knn",
                            trControl=trctrl,
                            preProcess = c("center", "scale"),
                            tuneLength = 10)


# Define UI 
ui <- fluidPage(
   
# Application title
   titlePanel("Getting to Know Our Customers"),
   
   
   
   
# Select Box Age
selectInput(inputId="age", label = h3("Age"), 
               choices = list("Under 18"=1, "18 to 24"=2, "25 to 34"=3, "35 to 44"=4, "45 to 54"=5, "55 to 64"=6, "65 to older"=7, "Select a Response"=""), 
               selected =""),
   

  
#Select Box Playing at park
selectInput(inputId="park", label = h3("How Important is playing at the park with your pet?"), 
            choices = list("Extremely Important" = 1, "Moderately Important" = 2, "Slightly Important" = 3, "Neither Important nor Unimportant" = 4, "Slightly Unimportant" = 5, "Moderately Unimportant" = 6, "Extremely Unimportant" =7, "Select a Response"=""), 
            selected = ""),

#Select Box Playing 
selectInput(inputId="games", label = h3("How Important is playing games with your pet (like fetch, frisbee, etc.)?"), 
            choices = list("Extremely Important" = 1, "Moderately Important" = 2, "Slightly Important" = 3, "Neither Important nor Unimportant" = 4, "Slightly Unimportant" = 5, "Moderately Unimportant" = 6, "Extremely Unimportant" =7, "Select a Response"=""), 
            selected = ""),


#Select Box new tricks
selectInput(inputId="tricks", label = h3("How Important is teaching your pet new tricks/commands?"), 
            choices = list("Extremely Important" = 1, "Moderately Important" = 2, "Slightly Important" = 3, "Neither Important nor Unimportant" = 4, "Slightly Unimportant" = 5, "Moderately Unimportant" = 6, "Extremely Unimportant" =7, "Select a Response"=""), 
            selected = ""),

#Select Box Enjoy pet interacting with other pets
selectInput(inputId="pets", label = h3("Do you enjoy seeing your pet interact with other pets?"), 
            choices = list("Strongly Agree" = 1, "Agree" = 2, "Slightly Important" = 3, "Somewhat Agree" = 4, "Somewhat Disagree" = 5, "Disagree" = 6, "Strongly Disagree" =7, "Select a Response"=""), 
            selected = ""),

#action button
submitButton( "Enter Values"),

h3("Clusters 0-3 and their Probabilities"),
mainPanel(verbatimTextOutput("prediction")),

fluidRow(column(4, verbatimTextOutput("value")))

)
      
 

# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
new_answer<-reactive({
    data.frame("Age"=as.integer(input$age),"playing_at_park_importance"=as.integer(input$park),"playing_importance"=as.integer(input$games),"new_tricks_importance"=as.integer(input$tricks), "enjoy_pet_interacting_w_other_pets"=as.integer(input$pets))})

   #output$selectedData<-renderPrint({ predict(knn_fit,new_answer(), "prob") })

 
    #observeEvent(input$Enter, isolate({print(predict(knn_fit,new_answer(), "prob"))}))
     output$prediction<- renderPrint({format(round(predict(knn_fit,new_answer(), "prob")*100,1), nsmall=1)})
}

# Run the application 
shinyApp(ui = ui, server = server)

#
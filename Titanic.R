title_4 <- titlePanel("Data Exploration of Dataset \"Titanic\"")

sidebarpanel_4 <- tabsetPanel(
      tabPanel("Overview",
               sidebarLayout(
                 sidebarPanel(
                    h4("Structure of Dataset Titanic"),
                    verbatimTextOutput("str_4"),
                    h4("Chi-square Test of Dataset Titanic"),
                    verbatimTextOutput("summary_4")
                    ),
                 mainPanel(plotOutput("titanic"))
                 )
               ),
      tabPanel("Passenger Structure", plotOutput("titanic_passengers")),
              
      tabPanel("Survival with 2 parameters", 
               selectInput("param_2_a", "Select Parameter 1", c("Sex","Age", "Class"), selected = "Age"),
               selectInput("param_2_b", "Select Parameter 2", c("Sex","Age", "Class"), selected = "Class"),
               plotOutput("param_2")
               ),
      
      tabPanel("Survival with 1 parameter", 
               selectInput("param_1", "Select Parameter", c("Sex","Age", "Class")),
               plotOutput("param_1")),
      
      tabPanel("Survival Rates", 
               h4("Test one parameter against the other in terms of Survival Rate"),
               sidebarLayout(
                 sidebarPanel(
                   selectInput("sim_2_a", "Select Parameter 1", c("Sex","Age", "Class"), selected = "Class"),
               selectInput("sim_2_b", "Select Parameter 2", c("Sex","Age", "Class"), selected = "Sex")
                 ),
               mainPanel(
                 h3("Survival rates [%]"),
               verbatimTextOutput("simpson")
               
               )
                 
               )
               )
)
    

title_3 <- titlePanel("Data Exploration of Dataset \"LakeHuron\"")

sidebarpanel_3 <- tabsetPanel(
  tabPanel("Overview",
           sidebarLayout(
             sidebarPanel( 
               selectInput("color3", "Select plot color", c("darkgrey" ,"darkgreen", "orange", "red", "blue")),
               
                h4("Time Series Settings"),
               splitLayout(
                   radioButtons("mean_3", "Show mean?", choices=c("yes", "no"), selected="yes"),
                   radioButtons("trend_3", "Show regression?", choices=c("yes", "no"), selected="yes")
               ),
               
               
               h4("Histogram Settings"),
               sliderInput("bins3",
                           "Number of histogram bins:",
                           min = 1,
                           max = 11,
                           value = 7,
                           step=2),
               splitLayout(
                 radioButtons("density3", "Show density line?", choices=c("yes", "no"), selected="yes"),
                 radioButtons("abline_mean3", "Show abline mean?", choices=c("yes", "no"), selected="yes"),
                 radioButtons("abline_median3", "Show abline median?", choices=c("yes", "no"), selected="yes"),
                 radioButtons("line_mean3", "Show norm. curve?", choices=c("yes", "no"), selected="yes")
               )
              
             ),
               mainPanel(
                   plotOutput("plot_3"), 
                    splitLayout(
                        plotOutput("hist_3"),
                        plotOutput("qq3")
                      )
                 
               )
             
           )
          
            
           ),
  tabPanel("Smoothed Curve",
           sidebarLayout(
             sidebarPanel(
               sliderInput("alpha",
                           "alpha value for plot smoothing:",
                           min = 0.1,
                           max = 1,
                           value = 0.5,
                           step=0.1)
               
             ),
             mainPanel(
               plotOutput("smooth"),
               verbatimTextOutput("forecast")
               
             )
             
             
             
           )
           
           )
  
  
  
  
)
  
  
  
  
    #tableOutput("head_3")
  

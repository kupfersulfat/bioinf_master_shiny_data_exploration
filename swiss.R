# Sidebar with a slider input for number of bins 
title_1 <- titlePanel("Data Exploration of Dataset \"swiss\"")

#group_choice_1 <- selectInput("group_1", "Select group to visualise", c("Overview", "Fertility", "Agriculture", "Education", "Catholic", "Infant. Mortality", "Correlation"))

#sidebarpanel_1 <- verticalLayout(
sidebarpanel_1 <-  sidebarLayout(
  sidebarPanel( 
    selectInput("color", "Select plot color", c("darkgrey" ,"darkgreen", "orange", "red", "blue")),
    
    h4("Histogram Settings"),
    sliderInput("bins",
                "Number of histogram bins:",
                min = 1,
                max = 20,
                value = 13,
                step=3),
    splitLayout(
      radioButtons("density", "Show density line", choices=c("yes", "no"), selected="yes"),
      radioButtons("abline_mean", "Show abline mean", choices=c("yes", "no"), selected="yes"),
      radioButtons("abline_median", "Show abline median", choices=c("yes", "no"), selected="yes"),
      radioButtons("line_mean", "Show norm. curve?", choices=c("yes", "no"), selected="yes")
    ),
    
    h4("Correlation Settings"),
    selectInput("cor_1_a", "Choose first category", choices = c("Fertility", "Agriculture", "Education", "Catholic", "Infant.Mortality"), selected = "Fertility"),
    selectInput("cor_1_b", "Choose second category", choices = c("Fertility", "Agriculture", "Education", "Catholic", "Infant.Mortality"), selected = "Education")
    
  ),
  mainPanel(
      tabsetPanel(
  
  
    tabPanel("Overview",
             DT::DTOutput("head_1"),
            h3("Summary of Statistics"),
             verbatimTextOutput("sum_1"),
    ),
    tabPanel("Fertility",
             splitLayout(
               plotOutput("histogram_1_fert"), 
               plotOutput("qqplot_1_fert")
               ),
             fluidRow(
                 column(width = 5, plotOutput("vioplot_1_fert")),
                 column(width = 6, offset = 1,
                  verticalLayout(  
                      p(""),p(""),p(""),p(""),
                   p(strong("Mittelwert: "), mean(swiss$Fertility)),
                    p(strong("Median: "), median(swiss$Fertility)),
                    p(strong("Standardabweichung: "), sd(swiss$Fertility)),
                    p(strong("IQD: "), IQR(swiss$Fertility)),
                    p(strong("Spannweite: "), (max(swiss$Fertility)+min(swiss$Fertility)/2)),
                    p(strong("Modalitaet: "), "unimodal"),  
                    p(strong("Schiefe: "), "linksschief durch Ausreisser", skewness(swiss$Fertility)),
                    p(strong("Kurtosis: "), "Schwerer Rand links ", kurtosis(swiss$Fertility)),
                    p(strong("Normalverteilung: "), "ohne Ausreisser -> vermutlich ja"))      
                  )
                 
             )
             
              
    ),
   tabPanel("Agriculture",
            splitLayout(
              plotOutput("histogram_1_agri"), 
              #plotOutput("boxplot_1_agri"), 
              plotOutput("qqplot_1_agri") 
              #,plotOutput("vioplot_1_agri")
              ),
            fluidRow(
                column(width = 5, plotOutput("vioplot_1_agri")),
                column(width = 6, offset = 1,
                       verticalLayout(  
                           p(""),p(""),p(""),p(""),
                    p(strong("Mittelwert: "), mean(swiss$Agriculture)),
                    p(strong("Median: "), median(swiss$Agriculture)),
                    p(strong("Standardabweichung: "), sd(swiss$Agriculture)),
                    p(strong("IQD: "), IQR(swiss$Agriculture)),
                    p(strong("Spannweite: "), (max(swiss$Agriculture)+min(swiss$Agriculture)/2)),
                    p(strong("Modalitaet: "), "bimodal"),  
                    p(strong("Schiefe: "), "leicht linksschief", skewness(swiss$Agriculture)),
                    p(strong("Kurtosis: "), kurtosis(swiss$Agriculture)),
                    p(strong("Normalverteilung: "), "nein")))
            )
    ),
    tabPanel("Education",
             splitLayout(
               plotOutput("histogram_1_educ"),
               #plotOutput("boxplot_1_educ"),  
               plotOutput("qqplot_1_educ")
              #, plotOutput("vioplot_1_educ")
              ),
             fluidRow(
                 column(width = 5, plotOutput("vioplot_1_educ")),
                 column(width = 6, offset = 1,
                        verticalLayout(  
                            p(""),p(""),p(""),p(""),
                     p(strong("Mittelwert: "), mean(swiss$Education)),
                     p(strong("Median: "), median(swiss$Education)),
                     p(strong("Standardabweichung: "), sd(swiss$Education)),
                     p(strong("IQD: "), IQR(swiss$Education)),
                     p(strong("Spannweite: "), (max(swiss$Education)+min(swiss$Education)/2)),
                     p(strong("Modalitaet: "), "unimodal"),  
                     p(strong("Schiefe: "), "rechtsschief", skewness(swiss$Education)),
                     p(strong("Kurtosis: "), "Schwerer Rand rechts ", kurtosis(swiss$Education)),
                     p(strong("Normalverteilung: "), "nein")))
             )
    ),
    tabPanel("Catholic",
             splitLayout(
               plotOutput("histogram_1_cath"), 
               #plotOutput("boxplot_1_cath"), 
               plotOutput("qqplot_1_cath") 
               #,plotOutput("vioplot_1_cath")
               ),
             fluidRow(
                 column(width = 5, plotOutput("vioplot_1_cath")),
                 column(width = 6, offset = 1,
                        verticalLayout(  
                            p(""),p(""),p(""),p(""),
                     p(strong("Mittelwert: "), mean(swiss$Catholic)),
                     p(strong("Median: "), median(swiss$Catholic)),
                     p(strong("Standardabweichung: "), sd(swiss$Catholic)),
                     p(strong("IQD: "), IQR(swiss$Catholic)),
                     p(strong("Spannweite: "), (max(swiss$Catholic)+min(swiss$Catholic)/2)),
                     p(strong("Modalitaet: "), "bimodal"),  
                     p(strong("Schiefe: "), skewness(swiss$Catholic)),
                     p(strong("Kurtosis: "), kurtosis(swiss$Catholic)),
                     p(strong("Normalverteilung: "), "nein, da bimodal")))
             )
    ),
    tabPanel("Infant Mortality",
             splitLayout(
               plotOutput("histogram_1_infant"), 
               #plotOutput("boxplot_1_infant"), 
               plotOutput("qqplot_1_infant")
               #,plotOutput("vioplot_1_infant")
               ),
             fluidRow(
                 column(width = 5, plotOutput("vioplot_1_infant")),
                 column(width = 6, offset = 1,
                        verticalLayout(  
                            p(""),p(""),p(""),p(""),
                     p(strong("Mittelwert: "), mean(swiss$Infant.Mortality)),
                     p(strong("Median: "), median(swiss$Infant.Mortality)),
                     p(strong("Standardabweichung: "), sd(swiss$Infant.Mortality)),
                     p(strong("IQD: "), IQR(swiss$Infant.Mortality)),
                     p(strong("Spannweite: "), (max(swiss$Infant.Mortality)+min(swiss$Infant.Mortality)/2)),
                     p(strong("Modalitaet: "), "unimodal"),  
                     p(strong("Schiefe: "), skewness(swiss$Infant.Mortality)),
                     p(strong("Kurtosis: "), kurtosis(swiss$Infant.Mortality)),
                     p(strong("Normalverteilung: "), "ja")))
             )
    ),
    tabPanel("Correlation",
             splitLayout(
                 plotOutput("corrgram_1"),
                 plotOutput("corplot_1")
             ),
             verbatimTextOutput("cor_1")
    )
   

  
)

    
  )
)
  
  
  
  

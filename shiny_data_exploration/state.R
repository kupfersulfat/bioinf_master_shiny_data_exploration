title_2 <- titlePanel("Data Exploration of Dataset \"state\"")

state <- as.data.frame(state.x77)

sidebarpanel_2 <- sidebarLayout(
  sidebarPanel( 
    selectInput("color2", "Select plot color", c("darkgrey" ,"darkgreen", "orange", "red", "blue")),
    
    h4("Histogram Settings"),
    sliderInput("bins2",
                "Number of histogram bins:",
                min = 1,
                max = 20,
                value = 13,
                step=3),
    splitLayout(
      radioButtons("density2", "Show density line", choices=c("yes", "no"), selected="yes"),
      radioButtons("abline_mean2", "Show abline mean", choices=c("yes", "no"), selected="yes"),
      radioButtons("abline_median2", "Show abline median", choices=c("yes", "no"), selected="yes"),
      radioButtons("line_mean2", "Show norm. curve?", choices=c("yes", "no"), selected="yes")
    ),
    
    h4("Correlation Settings"),
    selectInput("cor_2_a", "Choose first category", choices = c("Population", "Income", "Illiteracy", "Life Exp", "Murder", "HS Grad", "Frost"), selected = "Murder"),
    selectInput("cor_2_b", "Choose second category", choices = c("Population", "Income", "Illiteracy", "Life Exp", "Murder", "HS Grad", "Frost"), selected = "Illiteracy")
    
    
  ),
  mainPanel( 
  
  
  
  tabsetPanel(
  
  tabPanel("Overview",
           DT::DTOutput("head_2"),
           h3("Summary of Statistics"),
           verbatimTextOutput("sum_2")
  ),
  tabPanel("Population",
           splitLayout(
             plotOutput("histogram_2_pop"), 
             plotOutput("qqplot_2_pop")
           ),
           fluidRow(
               column(width = 5, plotOutput("vioplot_2_pop")),
               column(width = 6, offset = 1,
                      verticalLayout(  
                          p(""),p(""),p(""),p(""),
                          p(strong("Mittelwert: "), mean(state$Population)),
                          p(strong("Median: "), median(state$Population)),
                          p(strong("Standardabweichung: "), sd(state$Population)),
                          p(strong("IQD: "), IQR(state$Population)),
                          p(strong("Spannweite: "), (max(state$Population)+min(state$Population)/2)),
                          p(strong("Modalitaet: "), "unimodal"),  
                          p(strong("Schiefe: "), "rechtsschief", skewness(state$Population)),
                          p(strong("Kurtosis: "), "schwerer rechter Rand", kurtosis(state$Population)),
                          p(strong("Normalverteilung: "), "nein")))
           )
  ),
  tabPanel("Income",
           splitLayout(
             plotOutput("histogram_2_inc"), 
             plotOutput("qqplot_2_inc")
           ),
           fluidRow(
               column(width = 5, plotOutput("vioplot_2_inc")),
               column(width = 6, offset = 1,
                      verticalLayout(  
                          p(""),p(""),p(""),p(""),
                          p(strong("Mittelwert: "), mean(state$Income)),
                          p(strong("Median: "), median(state$Income)),
                          p(strong("Standardabweichung: "), sd(state$Income)),
                          p(strong("IQD: "), IQR(state$Income)),
                          p(strong("Spannweite: "), (max(state$Income)+min(state$Income)/2)),
                          p(strong("Modalitaet: "), "bimodal"),  
                          p(strong("Schiefe: "), "leichter Rand beidseitig", skewness(state$Income)),
                          p(strong("Kurtosis: "), "linksschief ohne Ausreisser", kurtosis(state$Income)),
                          p(strong("Normalverteilung: "), "ja")))
           )
  ),
  tabPanel("Illiteracy",
           splitLayout(
             plotOutput("histogram_2_ill"), 
             plotOutput("qqplot_2_ill")
           ),
           fluidRow(
               column(width = 5, plotOutput("vioplot_2_ill")),
               column(width = 6, offset = 1,
                      verticalLayout(  
                          p(""),p(""),p(""),p(""),
                          p(strong("Mittelwert: "), mean(state$Illiteracy)),
                          p(strong("Median: "), median(state$Illiteracy)),
                          p(strong("Standardabweichung: "), sd(state$Illiteracy)),
                          p(strong("IQD: "), IQR(state$Illiteracy)),
                          p(strong("Spannweite: "), (max(state$Illiteracy)+min(state$Illiteracy)/2)),
                          p(strong("Modalitaet: "), "unimodal"),  
                          p(strong("Schiefe: "),"rechtsschief", skewness(state$Illiteracy)),
                          p(strong("Kurtosis: "), kurtosis(state$Illiteracy)),
                          p(strong("Normalverteilung: "), "nein")))
           )
  ),
  tabPanel("Life Exp",
           splitLayout(
             plotOutput("histogram_2_life"), 
             plotOutput("qqplot_2_life")
           ),
           fluidRow(
               column(width = 5, plotOutput("vioplot_2_life")),
               column(width = 6, offset = 1,
                      verticalLayout(  
                          p(""),p(""),p(""),p(""),
                          p(strong("Mittelwert: "), mean(state$'Life Exp')),
                          p(strong("Median: "), median(state$'Life Exp')),
                          p(strong("Standardabweichung: "), sd(state$'Life Exp')),
                          p(strong("IQD: "), IQR(state$'Life Exp')),
                          p(strong("Spannweite: "), (max(state$'Life Exp')+min(state$'Life Exp')/2)),
                          p(strong("Modalitaet: "), "bimodal"),  
                          p(strong("Schiefe: "), skewness(state$'Life Exp')),
                          p(strong("Kurtosis: "), kurtosis(state$'Life Exp')),
                          p(strong("Normalverteilung: "), "nein")))
           )
  ),
  tabPanel("Murder",
           splitLayout(
             plotOutput("histogram_2_mur"), 
             plotOutput("qqplot_2_mur")
           ),
           fluidRow(
               column(width = 5, plotOutput("vioplot_2_mur")),
               column(width = 6, offset = 1,
                      verticalLayout(  
                          p(""),p(""),p(""),p(""),
                          p(strong("Mittelwert: "), mean(state$Murder)),
                          p(strong("Median: "), median(state$Murder)),
                          p(strong("Standardabweichung: "), sd(state$Murder)),
                          p(strong("IQD: "), IQR(state$Murder)),
                          p(strong("Spannweite: "), (max(state$Murder)+min(state$Murder)/2)),
                          p(strong("Modalitaet: "), "fake bimodal?"),  
                          p(strong("Schiefe: "), "",skewness(state$Murder)),
                          p(strong("Kurtosis: "),"leichter Rand beidseitig", kurtosis(state$Murder)),
                          p(strong("Normalverteilung: "), "wenn unimodal, dann ja")))
           )
  ),
  tabPanel("HS Grad",
           splitLayout(
             plotOutput("histogram_2_hs"), 
             plotOutput("qqplot_2_hs")
           ),
           fluidRow(
               column(width = 5, plotOutput("vioplot_2_hs")),
               column(width = 6, offset = 1,
                      verticalLayout(  
                          p(""),p(""),p(""),p(""),
                          p(strong("Mittelwert: "), mean(state$'HS Grad')),
                          p(strong("Median: "), median(state$'HS Grad')),
                          p(strong("Standardabweichung: "), sd(state$'HS Grad')),
                          p(strong("IQD: "), IQR(state$'HS Grad')),
                          p(strong("Spannweite: "), (max(state$'HS Grad')+min(state$'HS Grad')/2)),
                          p(strong("Modalitaet: "), "bimodal"),  
                          p(strong("Schiefe: "), "leicht linksschief",skewness(state$'HS Grad')),
                          p(strong("Kurtosis: "), kurtosis(state$'HS Grad')),
                          p(strong("Normalverteilung: "), "nein, da bimodal")))
           )
  ),
  tabPanel("Frost",
           splitLayout(
             plotOutput("histogram_2_fr"), 
             plotOutput("qqplot_2_fr")
           ),
           fluidRow(
               column(width = 5, plotOutput("vioplot_2_fr")),
               column(width = 6, offset = 1,
                      verticalLayout(  
                          p(""),p(""),p(""),p(""),
                          p(strong("Mittelwert: "), mean(state$Frost)),
                          p(strong("Median: "), median(state$Frost)),
                          p(strong("Standardabweichung: "), sd(state$Frost)),
                          p(strong("IQD: "), IQR(state$Frost)),
                          p(strong("Spannweite: "), (max(state$Frost)+min(state$Frost)/2)),
                          p(strong("Modalitaet: "), "bimodal"),  
                          p(strong("Schiefe: "), "leicht linksschief",skewness(state$Frost)),
                          p(strong("Kurtosis: "), kurtosis(state$Frost)),
                          p(strong("Normalverteilung: "), "nein, da bimodal")))
           )
  ),
  tabPanel("Correlation",
           splitLayout(
               plotOutput("corrgram_2"),
               plotOutput("corplot_2")
           ),
           
           verbatimTextOutput("cor_2")
  )
  
)   
)
)
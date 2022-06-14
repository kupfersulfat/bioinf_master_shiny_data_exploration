#libraries and data
library(shiny)
library(shinythemes)
library(ggplot2)
library(utils)
library(vioplot)
library(sm)
library(here)
library(DT)
library(dplyr)
library(rsconnect)
library(e1071)
library(corrgram)

data("swiss")
data("state")
data("LakeHuron")
data("Titanic")

#setting work directory and include files
here::i_am("app.R")
source(here("bsp1.R"), encoding = "utf-8")
source(here("bsp2.R"), encoding = "utf-8")
source(here("bsp3.R"), encoding = "utf-8")
source(here("bsp4.R"), encoding = "utf-8")

ui <- fluidPage(
  theme = shinytheme("yeti"),
  tabsetPanel(
    tabPanel("Bsp. 1", title_1,
             sidebarpanel_1
    ),
    
    tabPanel("Bsp. 2", title_2,
             sidebarpanel_2
    ),
    
    tabPanel("Bsp. 3", title_3,
             sidebarpanel_3
    ),
    
    tabPanel("Bsp. 4", title_4,
             sidebarpanel_4
    )
  )
)

server <- function(input, output, session) {
  
  #1-SWISS
  #group_1 <- input$group_1
  
  output$head_1 <- DT::renderDT({swiss[-3]})
  output$sum_1 <- renderPrint({summary(swiss[-3])})
  
  
  
  output$histogram_1_fert <- renderPlot({
    nf <- layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(3,1))
    par(mar=c(2.1, 1.1, 1.1, 2.1))
    hist(swiss$Fertility, main=paste("Histogram of Fertility"), freq = FALSE, xlab = paste("Fertility measure [%]"), breaks = (input$bins), col=input$color, border = "black") 
    if(input$line_mean == "yes"){lines(seq(min(swiss$Fertility),max(swiss$Fertility),by=0.1),dnorm(seq(min(swiss$Fertility),max(swiss$Fertility),by=0.1),mean=mean(swiss$Fertility),sd=sd(swiss$Fertility)),col=2)}
    if(input$density == "yes"){lines(density(swiss$Fertility), col=4)}
    if(input$abline_mean == "yes"){abline(v=mean(swiss$Fertility),col=2)}
    if(input$abline_median == "yes"){abline(v=median(swiss$Fertility),col=3)}
    boxplot(swiss$Fertility, horizontal = TRUE, main = "Boxplot of Fertility", xlab="Fertility measure[%]", col=input$color)
  })
  #output$boxplot_1_fert <- renderPlot({boxplot(swiss$Fertility, horizontal = TRUE, main = "Boxplot of Fertility", xlab="Fertility measure[%]")})
  output$qqplot_1_fert <- renderPlot({
    #nf <- layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(3,1))
    par(mar=c(2.1, 2.1, 2.1, 2.1),mfrow=c(1,2))
    qqnorm(swiss$Fertility, main = "QQ-plot of Fertility"); qqline(swiss$Fertility, col = 2)
    abline(h=median(swiss$Fertility), v=0, col = "darkgrey")
    plot(ecdf(swiss$Fertility), main = "ECDF plot of Fertility", verticals=TRUE)
    })
  output$vioplot_1_fert <- renderPlot({vioplot(swiss$Fertility, main="Violinplot of Fertility",horizontal = TRUE, xlab= "Fertility measure [%]", col=input$color)})
  
  
  
  output$histogram_1_agri <- renderPlot({
    nf <- layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(3,1))
    par(mar=c(2.1, 1.1, 1.1, 2.1))
    hist(swiss$Agriculture, main=paste("Histogram of Agriculture"), freq = FALSE, xlab = paste("Agriculture measure [%]"), breaks = (input$bins), col=input$color) 
    if(input$line_mean == "yes"){lines(seq(min(swiss$Agriculture),max(swiss$Agriculture),by=0.1),dnorm(seq(min(swiss$Agriculture),max(swiss$Agriculture),by=0.1),mean=mean(swiss$Agriculture),sd=sd(swiss$Agriculture)),col=2)}
    if(input$density == "yes"){lines(density(swiss$Agriculture), col=4)}
    if(input$abline_median == "yes"){abline(v=median(swiss$Agriculture),col=3)}
    if(input$abline_mean == "yes"){abline(v=mean(swiss$Agriculture),col=2)}
    boxplot(swiss$Agriculture, horizontal = TRUE, main = "Boxplot of Agriculture", xlab="Agriculture measure[%]", col=input$color)
  })
  #output$boxplot_1_agri <- renderPlot({boxplot(swiss$Agriculture, horizontal = TRUE, main = "Boxplot of Agriculture", xlab="Agriculture measure[%]")})
  output$qqplot_1_agri <- renderPlot({
    par(mar=c(2.1, 2.1, 2.1, 2.1),mfrow=c(1,2))
    qqnorm(swiss$Agriculture, main = "QQ-plot of Agriculture")
    qqline(swiss$Agriculture, col = 2)
    abline(h=median(swiss$Agriculture), v=0, col = "darkgrey")
    plot(ecdf(swiss$Agriculture), main = "ECDF plot of Agriculture", verticals=TRUE)
    })
  output$vioplot_1_agri <- renderPlot({vioplot(swiss$Agriculture, main="Violinplot of Agriculture",horizontal = TRUE, xlab= "Agriculture measure [%]", col=input$color)})
  
  
  
  
  
  output$histogram_1_educ <- renderPlot({
    nf <- layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(3,1))
    par(mar=c(2.1, 1.1, 1.1, 2.1))
    hist(swiss$Education, main=paste("Histogram of Education"), freq = FALSE, xlab = paste("Education measure [%]"), breaks = (input$bins), col=input$color) 
    if(input$line_mean == "yes"){lines(seq(min(swiss$Education),max(swiss$Education),by=0.1),dnorm(seq(min(swiss$Education),max(swiss$Education),by=0.1),mean=mean(swiss$Education),sd=sd(swiss$Education)),col=2)}
    if(input$density == "yes"){lines(density(swiss$Education), col=4)}
    if(input$abline_median == "yes"){abline(v=median(swiss$Education),col=3)}
    if(input$abline_mean == "yes"){abline(v=mean(swiss$Education),col=2)}
    boxplot(swiss$Education, horizontal = TRUE, main = "Boxplot of Education", xlab="Education measure[%]", col=input$color)
  })
  #output$boxplot_1_educ <- renderPlot({boxplot(swiss$Education, horizontal = TRUE, main = "Boxplot of Education", xlab="Education measure[%]")})
  output$qqplot_1_educ <- renderPlot({
    par(mar=c(2.1, 2.1, 2.1, 2.1),mfrow=c(1,2))
    qqnorm(swiss$Education, main = "QQ-plot of Education")
    qqline(swiss$Education, col = 2)
    abline(h=median(swiss$Education), v=0, col = "darkgrey")
    plot(ecdf(swiss$Education), main = "ECDF plot of Education", verticals=TRUE)
    })
  output$vioplot_1_educ <- renderPlot({vioplot(swiss$Education, main="Violinplot of Education",horizontal = TRUE, xlab= "Education measure [%]", col=input$color)})
  
  
  
  
  
  output$histogram_1_cath <- renderPlot({
    nf <- layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(3,1))
    par(mar=c(2.1, 1.1, 1.1, 2.1))
    hist(swiss$Catholic, main=paste("Histogram of Catholic"), freq = FALSE, xlab = paste("Catholic measure [%]"), breaks = (input$bins), col=input$color) 
    if(input$line_mean == "yes"){lines(seq(min(swiss$Catholic),max(swiss$Catholic),by=0.1),dnorm(seq(min(swiss$Catholic),max(swiss$Catholic),by=0.1),mean=mean(swiss$Catholic),sd=sd(swiss$Catholic)),col=2)}
    if(input$density == "yes"){lines(density(swiss$Catholic), col=4)}
    if(input$abline_median == "yes"){abline(v=median(swiss$Catholic),col=3)}
    if(input$abline_mean == "yes"){abline(v=mean(swiss$Catholic),col=2)}
    boxplot(swiss$Catholic, horizontal = TRUE, main = "Boxplot of Catholic", xlab="Catholic measure[%]", col=input$color)
  })
  #output$boxplot_1_cath <- renderPlot({boxplot(swiss$Catholic, horizontal = TRUE, main = "Boxplot of Catholic", xlab="Catholic measure[%]")})
  output$qqplot_1_cath <- renderPlot({
    par(mar=c(2.1, 2.1, 2.1, 2.1),mfrow=c(1,2))
    qqnorm(swiss$Catholic, main = "QQ-plot of Catholic")
    qqline(swiss$Catholic, col = 2)
    abline(h=median(swiss$Catholic), v=0, col = "darkgrey")
    plot(ecdf(swiss$Catholic), main = "ECDF plot of Catholic", verticals=TRUE)
    })
  output$vioplot_1_cath <- renderPlot({vioplot(swiss$Catholic, main="Violinplot of Catholic",horizontal = TRUE, xlab= "Catholic measure [%]", col=input$color)})
  
  
  
  
  
  output$histogram_1_infant <- renderPlot({
    nf <- layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(3,1))
    par(mar=c(2.1, 1.1, 1.1, 2.1))
    hist(swiss$Infant.Mortality, main=paste("Histogram of Infant.Mortality"), freq = FALSE, xlab = paste("Infant.Mortality measure [%]"), breaks = (input$bins), col=input$color) 
    if(input$line_mean == "yes"){lines(seq(min(swiss$Infant.Mortality),max(swiss$Infant.Mortality),by=0.1),dnorm(seq(min(swiss$Infant.Mortality),max(swiss$Infant.Mortality),by=0.1),mean=mean(swiss$Infant.Mortality),sd=sd(swiss$Infant.Mortality)),col=2)}
    if(input$density == "yes"){lines(density(swiss$Infant.Mortality), col=4)}
    if(input$abline_median == "yes"){abline(v=median(swiss$Infant.Mortality),col=3)}
    if(input$abline_mean == "yes"){abline(v=mean(swiss$Infant.Mortality),col=2)}
    boxplot(swiss$Infant.Mortality, horizontal = TRUE, main = "Boxplot of Infant.Mortality", xlab="Infant.Mortality measure[%]", col=input$color)
  })
  #output$boxplot_1_infant <- renderPlot({boxplot(swiss$Infant.Mortality, horizontal = TRUE, main = "Boxplot of Infant.Mortality", xlab="Infant.Mortality measure[%]", col=input$color)})
  output$qqplot_1_infant <- renderPlot({
    par(mar=c(2.1, 2.1, 2.1, 2.1),mfrow=c(1,2))
    qqnorm(swiss$Infant.Mortality, main = "QQ-plot of Infant Mortality")
    qqline(swiss$Infant.Mortality, col = 2)
    abline(h=median(swiss$Infant.Mortality), v=0, col = "darkgrey")
    plot(ecdf(swiss$Infant.Mortality), main = "ECDF plot of Infant Mortality", verticals=TRUE)
    })
  output$vioplot_1_infant <- renderPlot({vioplot(swiss$Infant.Mortality, main="Violinplot of Catholic",horizontal = TRUE, xlab= "Infant.Mortality measure [%]", col=input$color)})
  
  
  
  
  output$corplot_1 <- renderPlot({
      switch(input$cor_1_a,
             Fertility = {var1 <- 1},
             Agriculture = {var1 <- 2},
             Education = {var1 <- 4},
             Catholic = {var1 <-5},
             Infant.Mortality = {var1 <- 6}
             
      )
      switch(input$cor_1_b,
             Fertility = {var2 <- 1},
             Agriculture = {var2 <- 2},
             Education = {var2 <- 4},
             Catholic = {var2 <-5},
             Infant.Mortality = {var2 <- 6}
      )
      #corrgram::corrgram(swiss[-3], order=TRUE, lower.panel=panel.fill, upper.panel=panel.cor, text.panel=panel.txt, col.regions=colorRampPalette(c("red4", "salmon", "white", "lightgreen", "forestgreen")))
      plot(swiss[,c(var1,var2)])
      lines(lowess(swiss[,c(var1,var2)]), col=2)
    
  })
  
  output$corrgram_1 <- renderPlot({corrgram::corrgram(swiss[-3], order=TRUE, lower.panel=panel.fill, upper.panel=panel.cor, text.panel=panel.txt, col.regions=colorRampPalette(c("red4", "salmon", "white", "lightgreen", "forestgreen")))})
  
  output$cor_1 <- renderPrint({
    round(cor(swiss[, c(1,2,4:6)]),digits=2)
  })
  #output$vioplot_1_infant <- renderPlot({vioplot(swiss$Infant.Mortality, main="Violinplot of Infant.Mortality",horizontal = TRUE, xlab= "Infant.Mortality measure [%]", col=input$color)})
  
  
    #boxplot(swiss$group, horizontal = TRUE, ylim=c(20,120), xlab=paste(group_1, "measure [%]")
    
    #qqnorm(swiss$group_1); qqline(swiss$group_1, col = 2)
    #vioplot(swiss$group_1, main="Violinplot of Fertility",horizontal = TRUE, xlab= paste(group_1, "measure [%]")
  
  

  
  #output$boxplot_1 <- renderPlot({})
  #output$qqplot_1 <- renderPlot({})
  #output$violinplot_1 <- renderPlot({})
  
  
  #state
  state <- as.data.frame(state.x77[,1:7])
  output$head_2 <- DT::renderDT({
    state
  })
  output$sum_2 <- renderPrint({
    summary(state)
  })
  
  
  output$histogram_2_pop <- renderPlot({
    nf <- layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(3,1))
    par(mar=c(2.1, 1.1, 1.1, 2.1))
    hist(state$Population, main="Histogram of Population", freq = FALSE, xlab = paste("Population measure [%]"), breaks = (input$bins2), col=input$color2, border = "black") 
    if(input$line_mean2 == "yes"){lines(seq(min(state$Population),max(state$Population),by=0.1),dnorm(seq(min(state$Population),max(state$Population),by=0.1),mean=mean(state$Population),sd=sd(state$Population)),col=2)}
    if(input$density2 == "yes"){lines(density(state$Population), col=4)}
    if(input$abline_mean2 == "yes"){abline(v=mean(state$Population),col=2)}
    if(input$abline_median2 == "yes"){abline(v=median(state$Population),col=3)}
    boxplot(state$Population, horizontal = TRUE, main = "Boxplot of Population", xlab="Population measure[%]", col=input$color2)
  })
  output$qqplot_2_pop <- renderPlot({
    par(mar=c(2.1, 2.1, 2.1, 2.1),mfrow=c(1,2))
    qqnorm(state$Population, main = "QQ-plot of Population")
    qqline(state$Population, col = 2)
    abline(h=median(state$Population), v=0, col = "darkgrey")
    plot(ecdf(state$Population), main = "ECDF plot of Population", verticals=TRUE)})
  
  output$vioplot_2_pop <- renderPlot({vioplot(state$Population, main="Violinplot of Population",horizontal = TRUE, col=input$color2)})
  
  
  
  output$histogram_2_inc <- renderPlot({
    nf <- layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(3,1))
    par(mar=c(2.1, 1.1, 1.1, 2.1))
    hist(state$Income, main="Histogram of Income", freq = FALSE, xlab = paste("Income measure [%]"), breaks = (input$bins2), col=input$color2, border = "black") 
    if(input$line_mean2 == "yes"){lines(seq(min(state$Income),max(state$Income),by=0.1),dnorm(seq(min(state$Income),max(state$Income),by=0.1),mean=mean(state$Income),sd=sd(state$Income)),col=2)}
    if(input$density2 == "yes"){lines(density(state$Income), col=4)}
    if(input$abline_mean2 == "yes"){abline(v=mean(state$Income),col=2)}
    if(input$abline_median2 == "yes"){abline(v=median(state$Income),col=3)}
    boxplot(state$Income, horizontal = TRUE, main = "Boxplot of Income", xlab="Income measure[%]", col=input$color2)
  })
  output$qqplot_2_inc <- renderPlot({
    par(mar=c(2.1, 2.1, 2.1, 2.1),mfrow=c(1,2))
    qqnorm(state$Income, main = "QQ-plot of Income")
    qqline(state$Income, col = 2)
    abline(h=median(state$Income), v=0, col = "darkgrey")
    plot(ecdf(state$Income), main = "ECDF plot of Income", verticals=TRUE)})
  
  output$vioplot_2_inc <- renderPlot({vioplot(state$Income, main="Violinplot of Income",horizontal = TRUE, col=input$color2)})
  
  
  output$histogram_2_ill <- renderPlot({
    nf <- layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(3,1))
    par(mar=c(2.1, 1.1, 1.1, 2.1))
    hist(state$Illiteracy, main="Histogram of Illiteracy", freq = FALSE, xlab = paste("Illiteracy measure [%]"), breaks = (input$bins2), col=input$color2, border = "black") 
    if(input$line_mean2 == "yes"){lines(seq(min(state$Illiteracy),max(state$Illiteracy),by=0.1),dnorm(seq(min(state$Illiteracy),max(state$Illiteracy),by=0.1),mean=mean(state$Illiteracy),sd=sd(state$Illiteracy)),col=2)}
    if(input$density2 == "yes"){lines(density(state$Illiteracy), col=4)}
    if(input$abline_mean2 == "yes"){abline(v=mean(state$Illiteracy),col=2)}
    if(input$abline_median2 == "yes"){abline(v=median(state$Illiteracy),col=3)}
    boxplot(state$Illiteracy, horizontal = TRUE, main = "Boxplot of Illiteracy", xlab="Illiteracy measure[%]", col=input$color2)
  })
  output$qqplot_2_ill <- renderPlot({
    par(mar=c(2.1, 2.1, 2.1, 2.1),mfrow=c(1,2))
    qqnorm(state$Illiteracy, main = "QQ-plot of Illiteracy")
    qqline(state$Illiteracy, col = 2)
    abline(h=median(state$Illiteracy), v=0, col = "darkgrey")
    plot(ecdf(state$Illiteracy), main = "ECDF plot of Illiteracy", verticals=TRUE)})
  
  output$vioplot_2_ill <- renderPlot({vioplot(state$Illiteracy, main="Violinplot of Illiteracy",horizontal = TRUE, col=input$color2)})
  
  
  
  output$histogram_2_life <- renderPlot({
    nf <- layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(3,1))
    par(mar=c(2.1, 1.1, 1.1, 2.1))
    hist(state$'Life Exp', main="Histogram of Life Exp", freq = FALSE, xlab = paste("Life Exp measure [%]"), breaks = (input$bins2), col=input$color2, border = "black") 
    if(input$line_mean2 == "yes"){lines(seq(min(state$'Life Exp'),max(state$'Life Exp'),by=0.1),dnorm(seq(min(state$'Life Exp'),max(state$'Life Exp'),by=0.1),mean=mean(state$'Life Exp'),sd=sd(state$'Life Exp')),col=2)}
    if(input$density2 == "yes"){lines(density(state$'Life Exp'), col=4)}
    if(input$abline_mean2 == "yes"){abline(v=mean(state$'Life Exp'),col=2)}
    if(input$abline_median2 == "yes"){abline(v=median(state$'Life Exp'),col=3)}
    boxplot(state$'Life Exp', horizontal = TRUE, main = "Boxplot of Life Exp", xlab="Life Exp measure[%]", col=input$color2)
  })
  output$qqplot_2_life <- renderPlot({
    par(mar=c(2.1, 2.1, 2.1, 2.1),mfrow=c(1,2))
    qqnorm(state$'Life Exp', main = "QQ-plot of Life Exp")
    qqline(state$'Life Exp', col = 2)
    abline(h=median(state$'Life Exp'), v=0, col = "darkgrey")
    plot(ecdf(state$'Life Exp'), main = "ECDF plot of Life Exp", verticals=TRUE)})
  
  output$vioplot_2_life <- renderPlot({vioplot(state$'Life Exp', main="Violinplot of Life Expectancy",horizontal = TRUE, col=input$color2)})
  
  
  output$histogram_2_mur <- renderPlot({
    nf <- layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(3,1))
    par(mar=c(2.1, 1.1, 1.1, 2.1))
    hist(state$Murder, main="Histogram of Murder", freq = FALSE, xlab = paste("Murder measure [%]"), breaks = (input$bins2), col=input$color2, border = "black") 
    if(input$line_mean2 == "yes"){lines(seq(min(state$Murder),max(state$Murder),by=0.1),dnorm(seq(min(state$Murder),max(state$Murder),by=0.1),mean=mean(state$Murder),sd=sd(state$Murder)),col=2)}
    if(input$density2 == "yes"){lines(density(state$Murder), col=4)}
    if(input$abline_mean2 == "yes"){abline(v=mean(state$Murder),col=2)}
    if(input$abline_median2 == "yes"){abline(v=median(state$Murder),col=3)}
    boxplot(state$Murder, horizontal = TRUE, main = "Boxplot of Murder", xlab="Murder measure[%]", col=input$color2)
  })
  output$qqplot_2_mur <- renderPlot({
    par(mar=c(2.1, 2.1, 2.1, 2.1),mfrow=c(1,2))
    qqnorm(state$Murder, main = "QQ-plot of Murder")
    qqline(state$Murder, col = 2)
    abline(h=median(state$Murder), v=0, col = "darkgrey")
    plot(ecdf(state$Murder), main = "ECDF plot of Murder", verticals=TRUE)})
  
  output$vioplot_2_mur <- renderPlot({vioplot(state$Murder, main="Violinplot of Murder",horizontal = TRUE, col=input$color2)})
  
  
  
  output$histogram_2_hs <- renderPlot({
    nf <- layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(3,1))
    par(mar=c(2.1, 1.1, 1.1, 2.1))
    hist(state$'HS Grad', main="Histogram of HS Grad", freq = FALSE, xlab = paste("HS Grad measure [%]"), breaks = (input$bins2), col=input$color2, border = "black") 
    if(input$line_mean2 == "yes"){lines(seq(min(state$'HS Grad'),max(state$'HS Grad'),by=0.1),dnorm(seq(min(state$'HS Grad'),max(state$'HS Grad'),by=0.1),mean=mean(state$'HS Grad'),sd=sd(state$'HS Grad')),col=2)}
    if(input$density2 == "yes"){lines(density(state$'HS Grad'), col=4)}
    if(input$abline_mean2 == "yes"){abline(v=mean(state$'HS Grad'),col=2)}
    if(input$abline_median2 == "yes"){abline(v=median(state$'HS Grad'),col=3)}
    boxplot(state$'HS Grad', horizontal = TRUE, main = "Boxplot of HS Grad", xlab="HS Grad measure[%]", col=input$color2)
  })
  output$qqplot_2_hs <- renderPlot({
    par(mar=c(2.1, 2.1, 2.1, 2.1),mfrow=c(1,2))
    qqnorm(state$'HS Grad', main = "QQ-plot of HS Grad")
    qqline(state$'HS Grad', col = 2)
    abline(h=median(state$'HS Grad'), v=0, col = "darkgrey")
    plot(ecdf(state$'HS Grad'), main = "ECDF plot of HS Grad", verticals=TRUE)})
  
  output$vioplot_2_hs <- renderPlot({vioplot(state$'HS Grad', main="Violinplot of HS Graduation",horizontal = TRUE, col=input$color2)})
  
  
  
  output$histogram_2_fr <- renderPlot({
    nf <- layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(3,1))
    par(mar=c(2.1, 1.1, 1.1, 2.1))
    hist(state$Frost, main="Histogram of Frost", freq = FALSE, xlab = paste("Frost measure [%]"), breaks = (input$bins2), col=input$color2, border = "black") 
    if(input$line_mean2 == "yes"){lines(seq(min(state$Frost),max(state$Frost),by=0.1),dnorm(seq(min(state$Frost),max(state$Frost),by=0.1),mean=mean(state$Frost),sd=sd(state$Frost)),col=2)}
    if(input$density2 == "yes"){lines(density(state$Frost), col=4)}
    if(input$abline_mean2 == "yes"){abline(v=mean(state$Frost),col=2)}
    if(input$abline_median2 == "yes"){abline(v=median(state$Frost),col=3)}
    boxplot(state$Frost, horizontal = TRUE, main = "Boxplot of Frost", xlab="Frost measure[%]", col=input$color2)
  })
  output$qqplot_2_fr <- renderPlot({
    par(mar=c(2.1, 2.1, 2.1, 2.1),mfrow=c(1,2))
    qqnorm(state$Frost, main = "QQ-plot of Frost")
    qqline(state$Frost, col = 2)
    abline(h=median(state$Frost), v=0, col = "darkgrey")
    plot(ecdf(state$Frost), main = "ECDF plot of Frost", verticals=TRUE)})
  
  output$vioplot_2_fr <- renderPlot({vioplot(state$Frost, main="Violinplot of Frost",horizontal = TRUE, col=input$color2)})
  
  
  output$corplot_2 <- renderPlot({
      
     
      
      switch(input$cor_2_a,
             Population = {var1 <- 1},
             Income = {var1 <- 2},
             Illiteracy = {var1 <- 3},
             'Life Exp' = {var1 <- 4},
             Murder = {var1 <- 5},
             'HS Grad' = {var1 <- 6},
             Frost = {var1 <- 7}
      )
      switch(input$cor_2_b,
             Population = {var2 <- 1},
             Income = {var2 <- 2},
             Illiteracy = {var2 <- 3},
             'Life Exp' = {var2 <- 4},
             Murder = {var2 <- 5},
             'HS Grad' = {var2 <- 6},
             Frost = {var2 <- 7}
      )
      
      plot(state[,c(var1,var2)])
      lines(lowess(state[,c(var1,var2)]), col=2)
    
  })
  
  output$corrgram_2 <- renderPlot({corrgram::corrgram(state, order=TRUE, lower.panel=panel.fill, upper.panel=panel.cor, text.panel=panel.txt, col.regions=colorRampPalette(c("red4", "salmon", "white", "lightgreen", "forestgreen")))})
  
  
  output$cor_2 <- renderPrint({
    round(cor(state[, c(1:7)]), digits=2)
  })
  
  
  
  #Lake Huron
  
  output$plot_3 <- renderPlot({
    plot.ts(LakeHuron, ylab="Level [ft.]", main = "Fuellhoehe des Sees nach Jahren")
      if(input$mean_3 == "yes"){abline(h=mean(LakeHuron),col=2)}
      if(input$trend_3 == "yes"){lines(lowess(LakeHuron), col = 3)}
  })
  
  output$forecast <- renderPrint(
    HoltWinters(LakeHuron, gamma=FALSE, alpha = input$alpha)
  )
  
  output$smooth <- renderPlot({
    #nf <- layout(mat = matrix(c(1,2),2,1, byrow=FALSE), height = c(3,3)) 
    #par(mar=c(4.1, 5.1, 2.1, 1.1))
    
    #levelforecasts <- HoltWinters(LakeHuron, gamma=FALSE, alpha = input$alpha) 
    #levelforecasts
    
    plot(HoltWinters(LakeHuron, gamma=FALSE, alpha = input$alpha))
  })
  
  output$hist_3 <- renderPlot({
    nf <- layout(mat = matrix(c(1,2),2,1, byrow=FALSE), height = c(3.5,3)) 
    par(mar=c(4.1, 5.1, 2.1, 1.1))
    hist(LakeHuron,main="Histogram", freq = FALSE, xlim=c(575,584),  
         ylim=c(0,0.5), xlab = NULL, col=input$color3, breaks = input$bins3)
    if(input$line_mean3 == "yes"){lines(seq(min(LakeHuron),max(LakeHuron),by=0.1),dnorm(seq(min(LakeHuron),max(LakeHuron),by=0.1),mean=mean(LakeHuron),sd=sd(LakeHuron)),col=2)}
    if(input$density3 == "yes"){lines(density(LakeHuron), col=4)}
    if(input$abline_mean3 == "yes"){abline(v=mean(LakeHuron),col=2)}
    if(input$abline_median3 == "yes"){abline(v=median(LakeHuron),col=3)}
    boxplot(LakeHuron, horizontal = TRUE, ylim=c(575,584), xlab="feet", col=input$color3)
  })
  
  output$qq3 <- renderPlot({
    qqnorm(LakeHuron, main = "Q-Q Plot", xlab = "Theoretical Quantiles",
           ylab = "Sample Quantiles", plot.it = TRUE, datax = FALSE)
    qqline(LakeHuron,col=2)
  })
  
  output$vio3 <- renderPlot({
    vioplot(LakeHuron, main = "Violin Plot", horizontal = TRUE, ylim = c(575,583))
  })
  
  output$head_3 <- renderTable({
    LakeHuron
  })
  
  
  
  
  
  
  
  
  
  
  #Titanic
  output$str_4 <- renderPrint({str(Titanic)})
  output$summary_4 <- renderPrint({summary(Titanic)})
  
  output$titanic <- renderPlot({
    mosaicplot(Titanic, main = "Survival on the Titanic",color=TRUE)
  })
  
  output$titanic_passengers <- renderPlot({ 
    mosaicplot(~ Sex + Age + Class , data = Titanic, color = TRUE)
  })
  output$param_1 <- renderPlot({
    switch(input$param_1,
           Sex = {mosaicplot(~ Sex + Survived, data = Titanic, main = "Sex and Survival", color=TRUE)},
           Age = {mosaicplot(~ Age + Survived, data = Titanic, main = "Age and Survival", color=TRUE)},
           Class = {mosaicplot(~ Class + Survived, data = Titanic, main = "Class and Survival", color=TRUE)}
           )
    #mosaicplot(~ input$param_1 + Survived, data = Titanic, color=FALSE)
  })
  output$param_2 <- renderPlot({
    switch(input$param_2_a,
           Sex = {switch(input$param_2_b,
                         Sex = {"Choose 2 different categories"},
                         Age = {mosaicplot(~ Sex + Age + Survived, data = Titanic, main = "Sex, Age and Survival", color=TRUE)},
                         Class = {mosaicplot(~ Sex + Class + Survived, data = Titanic, main = "Sex, Class and Survival", color=TRUE)} 
                         )},
           Age = {switch(input$param_2_b,
                         Sex = {mosaicplot(~ Age + Sex + Survived, data = Titanic, main = "Age, Sex and Survival", color=TRUE)},
                         Age = {"Choose 2 different categories"},
                         Class = {mosaicplot(~ Age + Class + Survived, data = Titanic, main = "Age, Class and Survival", color=TRUE)} 
                        )},
           Class = {switch(input$param_2_b,
                           Sex = {mosaicplot(~ Class + Sex + Survived, data = Titanic, main = "Class, Sex and Survival", color=TRUE)},
                           Age = {mosaicplot(~ Class + Age + Survived, data = Titanic, main = "Class, Age and Survival", color=TRUE)},
                           Class = {"Choose 2 different categories"} 
                          )}
    )
    
    
    #mosaicplot(~ input$param_2_a + input$param_2_b + Survived, data = Titanic, color=TRUE)
  })
  
  output$simpson <- renderPrint({
    tit <- as.data.frame(Titanic)
    men <- filter(tit, Sex=="Male")
    women <- filter(tit, Sex=="Female")
    child <- filter(tit, Age=="Child")
    adult <- filter(tit, Age=="Adult")
    first <- filter(tit, Class=="1st")
    second <- filter(tit, Class=="2nd")
    third <- filter(tit, Class=="3rd")
    crew <- filter(tit, Class=="Crew")
    
    SR <- round(100*sum(filter(tit, Survived == "Yes")$Freq)/sum(tit$Freq))
    SR_men <- round(100*sum(filter(men, Survived == "Yes")$Freq)/sum(men$Freq))
    SR_women <- round(100*sum(filter(women, Survived == "Yes")$Freq)/sum(women$Freq))
    SR_child <- round(100*sum(filter(child, Survived == "Yes")$Freq)/sum(child$Freq))
    SR_adult <- round(100*sum(filter(adult, Survived == "Yes")$Freq)/sum(adult$Freq))
    SR_first <- round(100*sum(filter(first, Survived == "Yes")$Freq)/sum(first$Freq))
    SR_second <- round(100*sum(filter(second, Survived == "Yes")$Freq)/sum(second$Freq))
    SR_third <- round(100*sum(filter(third, Survived == "Yes")$Freq)/sum(third$Freq))
    SR_crew <- round(100*sum(filter(crew, Survived == "Yes")$Freq)/sum(crew$Freq))
    
    SR_men_child <- round(100*sum(filter(men, Age == "Child", Survived == "Yes")$Freq)/sum(filter(men, Age == "Child")$Freq))
    SR_men_adult <- round(100*sum(filter(men, Age == "Adult", Survived == "Yes")$Freq)/sum(filter(men, Age == "Adult")$Freq))
    SR_men_first <- round(100*sum(filter(men, Class == "1st", Survived == "Yes")$Freq)/sum(filter(men, Class == "1st")$Freq))
    SR_men_second <- round(100*sum(filter(men, Class == "2nd", Survived == "Yes")$Freq)/sum(filter(men, Class == "2nd")$Freq))
    SR_men_third <- round(100*sum(filter(men, Class == "3rd", Survived == "Yes")$Freq)/sum(filter(men, Class == "3rd")$Freq))
    SR_men_crew <- round(100*sum(filter(men, Class == "Crew", Survived == "Yes")$Freq)/sum(filter(men, Class == "Crew")$Freq))
    SR_women_child <- round(100*sum(filter(women, Age == "Child", Survived == "Yes")$Freq)/sum(filter(women, Age == "Child")$Freq))
    SR_women_adult <- round(100*sum(filter(women, Age == "Adult", Survived == "Yes")$Freq)/sum(filter(women, Age == "Adult")$Freq))
    SR_women_first <- round(100*sum(filter(women, Class == "1st", Survived == "Yes")$Freq)/sum(filter(women, Class == "1st")$Freq))
    SR_women_second <- round(100*sum(filter(women, Class == "2nd", Survived == "Yes")$Freq)/sum(filter(women, Class == "2nd")$Freq))
    SR_women_third <- round(100*sum(filter(women, Class == "3rd", Survived == "Yes")$Freq)/sum(filter(women, Class == "3rd")$Freq))
    SR_women_crew <- round(100*sum(filter(women, Class == "Crew", Survived == "Yes")$Freq)/sum(filter(women, Class == "Crew")$Freq))
    SR_child_first <- round(100*sum(filter(child, Class == "1st", Survived == "Yes")$Freq)/sum(filter(child, Class == "1st")$Freq))
    SR_child_second <- round(100*sum(filter(child, Class == "2nd", Survived == "Yes")$Freq)/sum(filter(child, Class == "2nd")$Freq))
    SR_child_third <- round(100*sum(filter(child, Class == "3rd", Survived == "Yes")$Freq)/sum(filter(child, Class == "3rd")$Freq))
    SR_child_crew <- round(100*sum(filter(child, Class == "Crew", Survived == "Yes")$Freq)/sum(filter(child, Class == "Crew")$Freq))
    SR_adult_first <- round(100*sum(filter(adult, Class == "1st", Survived == "Yes")$Freq)/sum(filter(adult, Class == "1st")$Freq))
    SR_adult_second <- round(100*sum(filter(adult, Class == "2nd", Survived == "Yes")$Freq)/sum(filter(adult, Class == "2nd")$Freq))
    SR_adult_third <- round(100*sum(filter(adult, Class == "3rd", Survived == "Yes")$Freq)/sum(filter(adult, Class == "3rd")$Freq))
    SR_adult_crew <- round(100*sum(filter(adult, Class == "Crew", Survived == "Yes")$Freq)/sum(filter(adult, Class == "Crew")$Freq))
    
    
    switch(input$sim_2_a,
           Sex = {switch(input$sim_2_b,
                         Sex = {"Choose 2 different categories"},
                         Age = {rbind(cbind("All", SR_men, SR_women),
                                 cbind("Children", SR_men_child, SR_women_child),
                                 cbind("Adults", SR_men_adult, SR_women_adult))
                         },
                         Class = {
                           rbind(cbind("All", SR_men, SR_women),
                                 cbind("First Class", SR_men_first, SR_women_first),
                                 cbind("Second Class", SR_men_second, SR_women_second),
                                 cbind("Third Class", SR_men_third, SR_women_third),
                                 cbind("Crew", SR_men_crew, SR_women_crew))
                         } 
           )},
           Age = {switch(input$sim_2_b,
                         Sex = {
                           rbind(cbind("All", SR_child, SR_adult),
                                 cbind("Men", SR_men_child, SR_men_adult),
                                 cbind("Women", SR_women_child, SR_women_adult))
                         },
                         Age = {"Choose 2 different categories"},
                         Class = {
                           rbind(cbind("All", SR_child, SR_adult),
                                 cbind("First Class", SR_child_first, SR_adult_first),
                                 cbind("Second Class", SR_child_second, SR_adult_second),
                                 cbind("Third Class", SR_child_third, SR_adult_third),
                                 cbind("Crew", SR_child_crew, SR_adult_crew))
                         } 
           )},
           Class = {switch(input$sim_2_b,
                           Sex = {rbind(cbind("All", SR_first, SR_second, SR_third, SR_crew),
                                        cbind("Men", SR_men_first, SR_men_second, SR_men_third, SR_men_crew),
                                        cbind("Women", SR_women_first, SR_women_second, SR_women_third, SR_women_crew))
                             },
                           Age = {rbind(cbind("All", SR_first, SR_second, SR_third, SR_crew),
                                        cbind("Children", SR_child_first, SR_child_second, SR_child_third, SR_child_crew),
                                        cbind("Adults", SR_adult_first, SR_adult_second, SR_adult_third, SR_adult_crew))
                             },
                           Class = {"Choose 2 different categories"} 
           )}
    )
    

    
  })
  
    
    
  
  
}




# Run the application 
shinyApp(ui = ui, server = server)
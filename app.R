#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(giftedCalcs)
library(dplyr)
library(devtools)
library(mnormt)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
# fluid rows, define column, width is 12, each item in a coloumn will be on different row. 
  # then close fliud row and start a new bracket system(). the numbers in the row should equal to 12. sepertae with , in between elements 
   
  
  # Application title
   titlePanel("Conditional Probabilities of Identification"),
   
   
   fluidRow(
     column(5, 
             
       # Slider for Test Reliabiliy         
        sliderInput("r.test",
                    HTML("Test reliability"),
                    min = .00,
                    max = .999,
                    value = .95,
                    step=.01),
    
   
   # Slider for Nomin Validity
   sliderInput("valid",
               HTML("Nomination validity overall"),
               min = .01,
               max = .99,
               value = .6,
               step=.01),
   
   # Nomin Cutoff
   sliderInput("nom.cutoff",
               HTML("Nomination cutoff"),
               min = .00,
               max = .99,
               value = .9,
               step=.005),
   
   # Test Cutoff
   sliderInput("t.cutoff",
               HTML("Test cutoff overall"),
               min = .5,
               max = .99,
               value = .9,
               step=.005)),
   
   column(7,
          tableOutput("sensitive")
          
          ),
   

  
  fluidRow(
    column(4,
           
           plotOutput("plot")),    #div(plotOutput('plot'),style = "padding-right: 10%; padding-left: 10%")
  
    column(4,    
           plotOutput("plot2")),
           
  
    column(4,
        
          plotOutput("plot3"))),
  
   
  
   
  fluidRow(
    column(12,  
   helpText("Natasha L. Godkin, Dept of Psychology, East Tennessee State University."),
   helpText("godkin@etsu.edu, @redskiouros")))
  
  ))
  
  
  


# Define server logic required to create plots and reactivity
server <- function(input, output) {
 # data <- function(r.test, valid, nom.cutoff, t.cutoff) {
   
  
  ########################################### Probabilty figure ########################################
  
   output$plot <- renderPlot({
    title <- "Probabilty of identification"
    
   # conditions <- expand.grid(list(
      relyt <- input$r.test
    test.cutoff <- input$t.cutoff
    valid <- input$valid      # if else statement 
    nom.cutoff <- input$nom.cutoff
    true.score1 <- seq(0,3, length.out=100)
    
    
  p_id <-conditional_p_id(x=true.score1, relyt=relyt, test.cutoff=test.cutoff, 
                            nom.cutoff=nom.cutoff, valid=valid)
  
  data <-data.frame(cbind(relyt, test.cutoff, valid, nom.cutoff, true.score1, p_id))
    
  #  plot(x=true.score1, y=p_id, type="l", xlab="true score", ylab="p identified") **plain jane plot (WORKS)
  
 
    
    ggplot(data=data, aes(x=(true.score1*15)+100, y=p_id), fill="blue", shape=23, size=3.5)+theme_bw()+ 
      ggtitle("Probabilty of Identification")+ xlab("True Score")+ ylab("Probability of Identification")+ 
      geom_line(alpha=.6, size=.6, col="#377eb2", alpha=.8)+
      scale_x_continuous(breaks=seq(100,150, 10))+
      geom_vline(xintercept=qnorm(test.cutoff, 100, 15), 
                                              linetype="dashed", alpha=.35, size=.8)
  
    
   })
   
   
   
   ############################ Universal screening ##################################################
   output$plot2 <- renderPlot({
     title <- "Conditional probability density of true scores"
     
     # enter values for true score
     tscore <-seq(0,4, by=.01)
     
     # ractivity for app
      relyt <- input$r.test
     test.cutoff <- input$t.cutoff
    valid <- input$valid
     nom.cutoff <- input$nom.cutoff
    
     # use d_identified for density at each true score        
     d <-d_identified(x=tscore, relyt=relyt, test.cutoff=test.cutoff, 
                      valid=valid, nom.cutoff=nom.cutoff, normalize=F)
     
     
     data <-data.frame(cbind(tscore, relyt, test.cutoff, valid, nom.cutoff, d))
    
    # plot(x=tscore, y=d, type="l") ### basic plain jane plot (WORKS)
    
  
    
    #find the max value of the y-axis
     max.y <- max(d_identified(x=tscore, relyt=relyt, test.cutoff=test.cutoff, 
                           normalize=F))
     
    ggplot(data, aes(x=(tscore*15)+100, y=d))+geom_line(alpha=.6)+
      ggtitle("Conditional Probability Density of True Scores")+
      geom_polygon(alpha=.45, fill="#2b8cbe")+
      theme_bw()+
      geom_hline(yintercept=0, col="black")+
      coord_cartesian(ylim=c(0, max.y))+
      scale_x_continuous(breaks=seq(100,150, 10))+
      theme(panel.background = element_rect(fill="white"), legend.key = element_rect(fill="white"),
          strip.background = element_rect(fill="white"), 
            text=element_text(family="Times", size=12),
            legend.position="bottom",
            plot.title = element_text(face="plain", hjust = 0.5), 
          axis.text.y=element_blank(), axis.ticks.y=element_blank())+
      xlab("Confirmatory test true score")+ylab("")+
     geom_vline(xintercept=qnorm(test.cutoff, 100, 15), # reactive dashed line 
                 linetype="dashed", alpha=.35, size=.8) 
       
      
     
   })
   
 
   ############################## comparision of unnormalized probabilty densities #########################
   output$plot3 <- renderPlot({
     title <- "Comparision of ineffective and effective two-stage identification"    
   

     
     # ractivity for app
     relyt <- input$r.test
     test.cutoff <- input$t.cutoff
     valid <- input$valid
     nom.cutoff <- input$nom.cutoff
     x <- seq(0,4, by=.01)
     
     
     y.universal <- conditional_p_id(x=x, relyt=relyt, test.cutoff=test.cutoff) 
     
     
     y.decent <- conditional_p_id(x=x, relyt=relyt, test.cutoff=test.cutoff, 
                                  valid=valid, nom.cutoff=nom.cutoff)
     

     y.decent.vs.universal <- y.decent/y.universal
     
    
     
     data <- data.frame(
       cbind(
         x=rep(x,2),
         condition=rep(c(1,2), each=length(x)),
        y.decent.vs.universal)
     )
  
    
  # plot(x=x, y=y.decent.vs.universal, type="l") plain jane plot (WORKS)
   
   ggplot(data, aes(x=((x*15)+100), y=y.decent.vs.universal))+
     
     geom_line(alpha=.6, size=.6, col="#377eb2", alpha=.8)+
     ggtitle("Comparision of Ineffective and Effective Two-stage Identification")+
    theme_classic()+
     coord_cartesian(xlim=c(110, 150), ylim=c(-.007, 1))+
     geom_hline(yintercept=c(0,1), col="gray10", linetype="solid", alpha=.25)+
     theme(text=element_text(family="Times", size=12),
           legend.position="bottom")+
     xlab("Confirmatory test true score")+ylab("Relative identification probability \nversus universal screening")+
     labs(color="Nomination parameters")+
     guides(color = guide_legend(nrow = 2))+
     scale_y_continuous(breaks=seq(0,1, .1))+
     scale_x_continuous(breaks=seq(100,150, 10))+
     scale_color_brewer(palette="Set1")+
     geom_vline(xintercept=qnorm(.9, 100, 15), 
                linetype="dashed", alpha=.35, size=.8)+
     geom_vline(xintercept=qnorm(c(.95, .975, .99, .999), 100, 15), 
                linetype="dashed", alpha=.15)+
     geom_hline(yintercept=(seq(.1, .9)), linetype="dotted", alpha=.15)+
     geom_hline(yintercept=seq(.1, 1, .1), 
                linetype="dashed", alpha=.1, size=.5)#+
  #   annotate("text", x = qnorm(.90, 100, 15), y =.05, label = "90th",
           #  fontface="plain",  family="Times", size=3.3)+
 #    annotate("text", x = qnorm(.95, 100, 15), y =.05, label = "95th",
           #   fontface="plain",  family="Times", size=3.3)+
  #   annotate("text", x = qnorm(.975, 100, 15), y =.05, label = "97.5th",
          #    fontface="plain",  family="Times", size=3.3)+
  #   annotate("text", x = qnorm(.99, 100, 15), y =.05, label = "99th",
          #    fontface="plain", family="Times", size=3.3)+
  #   annotate("text", x = qnorm(.999, 100, 15), y =.05, label = "99.9th",
       #       fontface="plain", family="Times", size=3.3)  
   
   
   })

 ####################### TABLES ############################### 
   
   ## Use the marginal_psychometrics function from gifted calculations ##
   
  
   output$sensitive <- renderTable({
     title <- "Sensitivity"   
     
     relyt <- input$r.test
     test.cutoff <- input$t.cutoff
     valid <- input$valid
     nom.cutoff <- input$nom.cutoff
     
     performance <- marginal_psychometrics(relyt = r.test, test.cutoff=t.cutoff, valid=valid, nom.cutoff=nom.cutoff)
     performance <- data.frame(performance)
     names(performance) <- c("Sensitivity", "Incorrect Identification Rate", "Nomination rate", 
                             "Nomination pass rate", "Identification rate")
     
     performance
  
   
   })  
   
}

# Run the application 

shinyApp(ui = ui, server = server)


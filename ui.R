
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinyjs)
library(png)
library(plotrix)

## Images seem to need readin in both ui and server files

rmanr<-readPNG('./www/rmanr.png')
rmang<-readPNG('./www/rmang.png')
gmang<-readPNG('./www/gmang.png')
gmanr<-readPNG('./www/gmanr.png')

shinyUI(fluidPage(
  useShinyjs(),
  
# Application title
titlePanel("Quantifying Diagnostic Test Accuracy: An Interactive Primer (v1.0)"),

# Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      
## Eventually there could be a choice of distributions to choose from for the diseased
## and healthy populations.    
      
## Rather than change the mean and sd by sliders, it would be good if you could 
## click directly on a little distribution plot to drag to change mean / sd etc
      
      sliderInput("mean_healthy",
                  "Mean test value for healthy:",
                  min = 15,
                  max = 25,
                  value = 17, step=0.5),
      
      sliderInput("sd_healthy",
                  "Standard deviation of test values for healthy:",
                  min = 0.5,
                  max = 6,
                  value = 3),
  
## It would be nice if you could colour code the input, but there does not seem to be
## a way of colour coding(?)
      
      sliderInput("mean_diseased",
                  "Mean test value for diseased:",
                  min = 15,
                  max = 25,
                  value = 22, step=0.5),

## ** Is there a way of saying diseased must have a higher mean response than the 
## ** healthy dynamically - i.e. one silder's limit is a function of the other slider
## ** Not pursued here because it would probabaly be confusing

      sliderInput("sd_diseased",
                  "Standard deviation of test values for diseased:",
                  min = 0.5,
                  max = 6,
                  value = 3),

    sliderInput("threshold",
            "Test threshold: above which defines disease diagnosis by test",
            min = 0,
            max = 40,
            value = 20, step=1, animate = TRUE, 
            animationOptions(interval = 500, playButton = TRUE, 
                             pauseButton = TRUE)),

 
######## Below tick boxes are put in using tags to allow the image to be attached 
######## to them

tags$i("Click on the buttons below to change highlighted related selections accross plots"),

tags$div(class="form-group shiny-input-container",
  tags$div(class="checkbox",
  tags$label(tags$input(id="TP", type="checkbox", checked="checked"),
          tags$img(src='rmanr.png', height=40,
    tags$span("True Positive Fraction (Sensitivity) & Number TP")
       )
    ))),
 
tags$div(class="form-group shiny-input-container",
         tags$div(class="checkbox",
                  tags$label(tags$input(id="FN", type="checkbox"),
                             tags$img(src='rmang.png', height=40,
                                      tags$span("False Negative Fraction (1 - Sensitivity) & Number FN")
                             )
                  ))),


tags$div(class="form-group shiny-input-container",
         tags$div(class="checkbox",
                  tags$label(tags$input(id="TN", type="checkbox", checked="checked"),
                             tags$img(src='gmang.png', height=40,
                                      tags$span("True Negative Fraction (Specificity) & Number TN")
                             )
                  ))),


tags$div(class="form-group shiny-input-container",
         tags$div(class="checkbox",
                  tags$label(tags$input(id="FP", type="checkbox"),
                             tags$img(src='gmanr.png', height=40,
                                      tags$span("False Positive Fraction (1- Specificity) & Number FP")
                             )
                  )))



, width=4),


    mainPanel(
      
      
      tabsetPanel(
        tabPanel("Test Accuracy", 
                 
                 fluidRow(
                   column(10, offset=1,
                   
                   
                   ## How to hide / show explanation text - #########
                   actionButton("e1", "Hide/show explanation text"),
                   htmlOutput("expl1"))),
                 
                 
                 fluidRow (
                   column(6, 
                          plotOutput("overlay_threshold")
                         # plotOutput("overlay_threshold", width ="450px", height = "300px")
                   ),
                   column(6,
                          plotOutput("ROCplot")
                         # plotOutput("ROCplot", width = "300px", height = "300px")
                   )
                 ),
                 
                 fluidRow(
                   column(10, offset=1,
                   actionButton("e2", "Hide/show explanation text"),
                   htmlOutput("expl2")
                 )),
                 
                 fluidRow(column(6, 
                                 
                                plotOutput("table.sesp")
                                 
                             #   plotOutput("table.sesp", height = "300px")
                 ),
                 column(5,
                        
                        htmlOutput("threshold_selected"),
                        br(),
                        htmlOutput("percent_tp")
                       
                        
                 ) ), hr()
                 
                 ), 
        
        tabPanel("Prevalence", 
                 fluidRow( column(10, offset=1,
                   
                   tags$hr(),
                   
                   actionButton("e3", "Hide/show explanation text"),
                   htmlOutput("expl3")
                   
                 )),
                 
                 fluidRow(column(4,
                                 br(),
                                 sliderInput("prevalence",
                                             "Prevalence: % of tested population with disease",
                                             min = 5,
                                             max = 95,
                                             value = 50, step=2, animate = TRUE, 
                                             animationOptions(interval = 10, playButton = TRUE, 
                                                              pauseButton = TRUE))
                                 
                 ),
                 column(8,
                        
                        br(), br(),
                        
                        
                        htmlOutput("tot_posneg"))
                 
                 
                 ),
                 
                 
                 ## Look for R software (or JS ?) to put faces representation in
                 ## https://stackoverflow.com/questions/20673584/visualizing-crosstab-tables-with-a-plot-in-r
                 
                 
                 ### No idea why the below code does not work????
                 
                 # conditionalPanel(condition = "input.Ptype == 'Ap'",
                 #                 plotOutput("tree1")),
                 
                 # conditionalPanel(condition = "input.Ptype == 'Bp'",
                 #                plotOutput("tree2"))
                 
                 
                 #### https://stackoverflow.com/questions/27716261/create-inputselection-to-subset-data-and-radiobuttons-to-choose-plot-type-in-sh?noredirect=1&lq=1
                 #### above website probably has the answer without using conditional panels ....
                 #)
                 #,
                 
                 fluidRow(column(9,
                                 
                                 plotOutput("Bp")),
                                 
                                 #   plotOutput("Bp", width ="700px", height = "400px")),
                          
                          (column(3, 
                                  htmlOutput("expl4")
                                  
                          )),
                          tags$hr()
                          
                 ),
                 
                 fluidRow(column(2,
                                 
                                 radioButtons("pType", "Choose format to view expected results of using the test:",
                                              c("Bar Chart" = "Ap",
                                                "Tree 1" = "Cp",
                                                "Tree 2" = "Dp"))
                 ),
                 
                 column(10,
                        
                        plotOutput("optionplot")
                        
                 )
                 ),
                 
                 fluidRow(column(10, offset=1, "In practice, the prevalence is a characteristic of the patient population and cannot be altered.", br(), br(),
                   "You can explore which test thresholds maximise 
                   the number of patients diagnosed correctly for a given prevalence and set of test score distributions, but
                   it is important to appreciate that in reality further factors need to be taken into account when deciding on a test operation threshold.
                   These include the consequences of false positive and false negative errors, which may 
                   be very different from each other.",  
                   tags$hr(), br(), br())
                 
                 
                 
                 
                 )), 
        tabPanel("Questions",
                 
                 fluidRow(column(10, offset=1,
                   
                   tags$h3("Questions:"),
                   
                   "1.) Which points does every ROC curve pass through? Describe the threshold 
                   values associated with these ponts.",
                   
                   actionButton("a1", "Show/hide answer"),
                   span(hidden( htmlOutput("ans1")),  style="color:red; font-style:italic"),
                   br(),br(),
                   
                   "2.) Describe how the distributions for test results have to be set, for healthy and diseased patients, 
                   for the ROC curve to follow the (dashed) line of no accuracy?",
                   
                   actionButton("a2", "Show/hide answer"),
                   actionButton("sl2", "Show example settings for no accuracy"),
                   span(hidden( htmlOutput("ans2")), style="color:red; font-style:italic"),
                   br(),br(),
                   
                   
                   "3.) What happens to the ROC curve if the mean of the diseased patients test scores is lower than that of the 
                   healthy? What does this imply?", 
                   
                   actionButton("a3", "Show/hide answer"),
                   actionButton("sl3", "Show example settings"),
                   span(hidden( htmlOutput("ans3")),  style="color:red; font-style:italic"),
                   br(), br(),
                   
                   
                   "4.) In terms of the distributions for test results, what characteristics generally improve the accuracy of the test? What would the ROC look like for a perfect test?",
                   
                   actionButton("a4", "Show/hide answer"),
                   actionButton("sl1", "Show perfect test settings"),
                   
                   
                   ## Below trying to use a bit of javascript to move page to top after button pressed 
                   ## but not sure I can include javascript like I can raw html.....
                   
                   #tags$button(id='sl1', type='button', class='btn btn-default action-button',
                   #onClick='document.getElementById('top').scrollIntoView()', 'Show perfect test settings'),
                   
                   # <button id="sl1" type="button" class="btn btn-default action-button"></button>
                   
                   
                   span(hidden( htmlOutput("ans4")), style="color:red; font-style:italic"),
                   br(), br(),
                   
                   
                   "5.) Set the distribution sliders to any values you wish and make a note of the ROC curve. Now increase
                   the mean of both the healthy and diseased distributions by 3 each. What do you notice about the 
                   resulting ROC curve?",
                   
                   actionButton("a5", "Show/hide answer"),
                   actionButton("sl4", "Example initial settings"),
                   actionButton("sl5", "Example changed settings"),
                   span(hidden( htmlOutput("ans5")),  style="color:red; font-style:italic"),
                   br(), br(),
                   
                   "6.) Set the mean of the healthy to 16 with a standard deviation of 3.1, and 
                   the mean of the diseased to 22 with a standard deviation of 3.7 ", 
                   actionButton("sl7", "Or use this button to do it for you"), 
                   " What is the maximum specificity that can be obtained while ensuring specificity is at least 80%;
                   what threshold should the test be operated at to obtain this? What is the minimum disease
                   prevalence that a population would have to have to ensure that, on average, at least 800 per 1000 patients would be correctly diagnosed?",
                   
                   actionButton("a8", "Show/hide answer"),
                   actionButton("sl8", "Show correct settings"),
                   
                   span(hidden( htmlOutput("ans8")),  style="color:red; font-style:italic"),
                   br(), br(),
                   
                   "7.) (Harder) What feature do ROC curves have when both the distributions for 
                   diseased and healthy test response are set to the same standard deviation?",
                   
                   actionButton("a6", "Show/hide answer"),
                   span(hidden( htmlOutput("ans6")), style="color:red; font-style:italic"),
                   br(), br(),
                   
                   "8.) (Advanced) Can you create ROC curves that are not completely concave with a portion
                   of the curve going below the line of no diagnostic effect? (Hint: Make the standard deviation of the healthy
                   group considerably larger than for the healthy group.)",
                   
                   actionButton("a7", "Show/hide answer"),
                   actionButton("sl6", "Example settings"),
                   span(hidden( htmlOutput("ans7")), style="color:red; font-style:italic"),
                   
                   hr()
                   
                 )
                 
                 
                 
                 )),
     
      
      
      tabPanel("Further Resources",  

        fluidRow(column(10, offset=1,

  tags$h3("Further Resources:"),
  "Excellent ",
  tags$a("interactive animations", href="https://understandinguncertainty.org/screening"), 
  " showing the impact of applying testing methods in realistic screening contexts (requires flash player)
  by Professor David Spiegelhalter's team at Cambridge.",
  br(),br(),
 "Two textbooks giving more extensive and mathematically rigorous coverage of the material presented here:", br(), br(),
  tags$li("Pepe, M.S. The Statistical Evaluation of Medical Tests for Classification and Prediction
  (Oxford Statistical Science Series No.31) Oxford University Press, New York. 2003"),
  br(),
  tags$li("Zhou,X-H., Obuchowski, N.A., McClish, D.K. Statistical Methods in Diagnostic Medicine
  (Wiley Series In Probability And Statistics) John Wiley and Sons, New York. 2002"),
  br(), br(), br(),
  tags$i("This webpage was created by Professor Alex Sutton, University of Leicester. 
         If you have any feedback I would love to hear from you. E-mail:"),
  tags$a("ajs22@le.ac.uk", href="mailto:ajs22@le.ac.uk"), hr(),
  br(), br()))),
  
  tabPanel("Thanks To", 
           fluidRow(column(10, offset=1,
  
 tags$h3("Thanks To:"),
  "Bret Victor for his incredibly inspirational ",
  tags$a("website", href="http://worrydream.com/"), 
 " and ",
 tags$a("explorable explanations", href="http://worrydream.com/ExplorableExplanations/"),
 " in particular.",
  br(), br(),
  tags$a("Nicky Case", href="http://ncase.me/"), 
  " for being the current torch bearer of explorable explanations whose ",
  tags$a("website", href="http://explorabl.es/"), 
"motivated me to create this page.",
  br(),
  br(),
  "Professor Joanne Lord for providing me with her slides on diagnostic test accuracy (many years ago!) out of which this webpage grew.",
  br(), br(),
  "Stephanie Hubbard for very helpful feedback after carefully testing and proofing the website",
  br(), br(),
  "RStudio for creating the ",
tags$a("Shiny package", href="https://shiny.rstudio.com/"), 
" which was used to build this page.", hr()
)


)
))
)

)


))

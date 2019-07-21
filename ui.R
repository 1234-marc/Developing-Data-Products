library(shiny)

strJavaScript01 <- "function showHr(obj){ dataViz.style.display = 'none'; graphViz.style.display = 'none'; modelViz.style.display = 'none'; predictViz.style.display = 'none'; document.getElementById(obj.id.substring(0,(obj.id.length-1))).style.display = 'block'; }"
strJavaScript02 <- "function blink(){ var blinks = document.getElementsByTagName('blink'); for (var i = blinks.length - 1; i >= 0; i--){var s = blinks[i]; s.style.visibility = (s.style.visibility === 'visible') ? 'hidden' : 'visible';} window.setTimeout(blink, 1000);} if (document.addEventListener) document.addEventListener('DOMContentLoaded', blink, false); else if (window.addEventListener) window.addEventListener('load', blink, false); else if (window.attachEvent) window.attachEvent('onload', blink); else window.onload = blink;"
strJavaScript03 <- ""
strJavaScript <- paste(strJavaScript01,strJavaScript02,strJavaScript03)



# Define UI for application
shinyUI(
    
    fluidPage(

    # Application title
    titlePanel(htmlOutput("title")),

    # Sidebar
    sidebarLayout(
        sidebarPanel(
            
            tags$div(id="dataViz"
                    ,style = "background-color: red; padding: 5px"
                    ,tags$code("Settings for data sample and vizualization")
            ),
            
            sliderInput("sidl"
                        ,htmlOutput("titleslider01")
                        ,min = 15
                        ,max = nrow(iris)
                        ,value = round(nrow(iris)/2,0)
            ),
            
            tags$div(id="graphViz"
                     ,style = "display:none; background-color: green; padding: 5px"
                     ,tags$code("Settings for graph vizualization")
            ),
            
            selectInput("graphtype"
                        ,htmlOutput("titleselectInput01")
                        ,c("[Correlation] with 'plot'","[Correlation] with 'colored pairs'","[Correlation] with 'psych'","[Correlation] with 'scatterplot3d'","[Density Plot] with 'ggplot2'","[Box Plot] with 'ggplot2'") 
                        ,selected = NULL
            ),

            
            tags$div(id="modelViz"
                     ,style = "display:none; background-color: blue; padding: 5px"
                     ,tags$code("Settings for model selection")
            ),
            

            selectInput("modeltype"
                        ,htmlOutput("titleselectInput02")
                        ,c("Linear Discriminant Analysis (LDA)","Classification and Regression Trees (CART)","k-Nearest Neighbors (kNN)","Support Vector Machines (SVM) with a linear kernel","Random Forest (RF)")
                        ,selected = NULL
            ),
            

            sliderInput("sdsize"
                        ,htmlOutput("titleslider02")
                        ,min = 0.5
                        ,max = 0.95
                        ,value = 0.7
                        ,step = 0.05
            ),            

            sliderInput("scvfold"
                        ,htmlOutput("titleslider03")
                        ,min = 2
                        ,max = 15
                        ,value = 3
                        ,step = 1
            ),            
            
            sliderInput("scvrepeat"
                        ,htmlOutput("titleslider04")
                        ,min = 2
                        ,max = 15
                        ,value = 3
                        ,step = 1
            ),                        
            
            tags$div(id="predictViz"
                     ,style = "display:none; background-color: darkorchid; padding: 5px"
                     ,tags$code("Settings for your own predictions")
            ),
            
            numericInput("petalW"
                         ,htmlOutput("titlenumericInput01")
                         ,value = round(mean(iris$Petal.Width),0)
                         ,min = 0
                         ,max = round(max(iris$Petal.Width)*1.5,0)
                         ,step = 0.1
            ),
            
            numericInput("petalL"
                         ,htmlOutput("titlenumericInput02")
                         ,value = round(mean(iris$Petal.Length),0)
                         ,min = 0
                         ,max = round(max(iris$Petal.Length)*1.5,0)
                         ,step = 0.1
            ),            

            numericInput("sepalW"
                         ,htmlOutput("titlenumericInput03")
                         ,value = round(mean(iris$Sepal.Width),0)
                         ,min = 0
                         ,max = round(max(iris$Sepal.Width)*1.5,0)
                         ,step = 0.1
            ),        
        
            numericInput("sepalL"
                         ,htmlOutput("titlenumericInput04")
                         ,value = round(mean(iris$Sepal.Length),0)
                         ,min = 0
                         ,max = round(max(iris$Sepal.Length)*1.5,0)
                         ,step = 0.1
            ),
            
            tags$script(HTML(strJavaScript))
            
        ),

        # Show results
        mainPanel(
            tabsetPanel(type="tabs",
                tabPanel(htmlOutput("titletabPanel01"),
                         br(),
                         tableOutput("headiris")
                ),
                tabPanel(htmlOutput("titletabPanel02"),
                         br(),
                         htmlOutput("graphcode"),
                         plotOutput(outputId = "distPlot")
                ),
                tabPanel(htmlOutput("titletabPanel03"),
                         br(),
                         htmlOutput("distModel")
                ),
                tabPanel(htmlOutput("titletabPanel04"),
                         br(),
                         htmlOutput("distModel1")
                )
            )
        )
    )
  )
)


# Load necessary packages
library(shiny)
library(psych)
library(scatterplot3d)
library(ggplot2)
library(gridExtra)
library(grid)
library(caret)
library(kernlab)
library(randomForest)
library(e1071)


# Define server
shinyServer( function(input, output) {

    # Functions for graphs
    ########################################################################################

    graphType <- reactive({
        
        if(input$graphtype == "[Correlation] with 'plot'"){
            graphexp <- "plot(irisS[,-5], col=irisS$Species)"
        }else if(input$graphtype == "[Correlation] with 'psych'")
        {
            graphexp <- "pairs.panels(irisS[,-5], method = 'pearson', hist.col = 'darkorchid', density = TRUE, ellipses = TRUE)"
        }else if(input$graphtype == "[Correlation] with 'colored pairs'")
        {
            graphexp <- "pairs(irisS[,-5], pch = 19,  cex = 0.8, col = c('red','green','blue')[irisS$Species], lower.panel=NULL)"
        }else if(input$graphtype == "[Correlation] with 'scatterplot3d'")
        {
            graphexp <- "scatterplot3d(prcomp(irisS[, -5])$x[, 1], prcomp(irisS[, -5])$x[, 2], prcomp(irisS[, -5])$x[, 3],xlab='Sepal.length', ylab='Sepal.width', zlab='Petal.length', pch = 16, color=c('red','green','blue')[as.numeric(irisS$Species)]) \n legend('bottomright', legend = levels(irisS$Species), col =  c('red','green','blue'), pch = 16)"
        
        }else if(input$graphtype == "[Density Plot] with 'ggplot2'")
        {
            graphexp <- paste0("grid.arrange(ggplot(irisS, aes(x=Petal.Length, colour=Species, fill=Species)) + ",
                       "geom_density(alpha=.3) + ",
                       "geom_vline(aes(xintercept=mean(Petal.Length),  colour=Species),linetype='dashed',color='red', size=1) + ",
                       "xlab('Petal Length (cm)') + ",  
                       "ylab('Density') + ",
                       "theme(legend.position='none'), ",
                       "ggplot(irisS, aes(x=Petal.Width, colour=Species, fill=Species)) + ",
                       "geom_density(alpha=.3) + ",
                       "geom_vline(aes(xintercept=mean(Petal.Width),  colour=Species),linetype='dashed',color='red', size=1) + ",
                       "xlab('Petal Width (cm)') + ",  
                       "ylab('Density') + ",
                       "theme(legend.position='none'), ",
                       "ggplot(irisS, aes(x=Sepal.Width, colour=Species, fill=Species)) + ",
                       "geom_density(alpha=.3) + ",
                       "geom_vline(aes(xintercept=mean(Sepal.Width),  colour=Species), linetype='dashed',color='red', size=1) + ",
                       "xlab('Sepal Width (cm)') + ",  
                       "ylab('Density') + ",
                       "theme(legend.position='none'), ",
                       "ggplot(irisS, aes(x=Sepal.Length, colour=Species, fill=Species)) + ", 
                       "geom_density(alpha=.3) + ",
                       "geom_vline(aes(xintercept=mean(Sepal.Length),  colour=Species),linetype='dashed', color='red', size=1) + ",
                       "xlab('Sepal Length (cm)') + ",  
                       "ylab('Density'),  ",
                       "nrow = 2, ",
                       "top = textGrob('Iris Density Plot', gp=gpar(fontsize=15)))") 
        }else{
            graphexp <- paste0("grid.arrange(ggplot(irisS, aes(Species, Sepal.Length, fill=Species)) + ",
                    "geom_boxplot() + ",
                    "scale_y_continuous('Sepal Length (cm)', breaks= seq(0,30, by=.5)) + ",
                    "theme(legend.position='none') + ggtitle(''), ",
                    "ggplot(irisS, aes(Species, Sepal.Width, fill=Species)) + ",
                    "geom_boxplot() + ",
                    "scale_y_continuous('Sepal Width (cm)', breaks= seq(0,30, by=.5)) + ",
                    "theme(legend.position='none') + ggtitle(''), ",
                    "ggplot(irisS, aes(Species, Petal.Length, fill=Species)) + ", 
                    "geom_boxplot() + ",
                    "scale_y_continuous('Petal Length (cm)', breaks= seq(0,30, by=.5)) + ",
                    "theme(legend.position='none') + ggtitle(''), ",
            		"ggplot(irisS, aes(Species, Petal.Width, fill=Species)) + ", 
                    "geom_boxplot() + ",
                    "scale_y_continuous('Petal Width (cm)', breaks= seq(0,30, by=.5)) + ",
                    "labs(title = 'Iris Box Plot', x = 'Species') + ggtitle(''), ",
            		"nrow = 2, ",
                    "top = textGrob('Iris Box Plot', gp=gpar(fontsize=15)))")            
        }
    })
    

    # Functions for Models and Predictions
    ########################################################################################

    modelTypeOut <- reactive({


        # Adjust dataset to sampling size
        irisS <- iris[sample(nrow(iris),input$sidl),]
        
        strsampleIris <- paste0("### Adjust 'iris' to sampling size<br>> irisS <- iris[sample(nrow(iris),",input$sidl,"),]")
        
        # Create training and validation datasets
        trainIndex <- createDataPartition(irisS$Species, p=input$sdsize, list=FALSE)
        # select 20% of the data for validation
        validset <- irisS[-trainIndex,]
        # use the remaining 80% of data to training and testing the models
        trainset <- irisS[trainIndex,]
        strtrain = paste0("### Create training and validation datasets<br>> trainIndex <- createDataPartition(irisS$Species, p=",input$sdsize,", list=FALSE)")
        
        # Create cross-validation controls
        control <- trainControl(method = "repeatedcv", number = input$scvfold,  repeats = input$scvrepeat, verbose = FALSE)
        strcontrol <- paste0("### Create cross-validation controls<br>> control <- trainControl(method='repeatedcv', number=",input$scvfold,", repeats=",input$scvrepeat,", verbose = FALSE)")

        dfMyPrediction <- data.frame(Sepal.Length = input$sepalL, Sepal.Width = input$sepalW , Petal.Length = input$petalL,  Petal.Width = input$petalW)
        strPredictors <- paste0("Petal.Width = <b>",input$petalW,"</b><br>Petal.Length = <b>",input$petalL,"</b><br>Sepal.Width = <b>",input$sepalW,"</b><br>Sepal.Length = <b>",input$sepalL,"</b>")
        
        metric <- "Accuracy"
        
        # Apply selected model
        if(input$modeltype == "Linear Discriminant Analysis (LDA)")            
        {
            strPackages <- paste0("### Load necessary packages<br>> library(caret)")
            modelfunc <- paste0("<b>Linear Discriminant Analysis (LDA)</b><br><b><br>R Code:</b><br>",strPackages,"<br>",strsampleIris,"<br>",strtrain,"<br>",strcontrol,"<br>","> model.lda <- train(Species~., data=trainset, method='lda', metric='",metric,"', trControl=control)")
            model.lda <- train(Species~., data=trainset, method='lda', metric=metric, trControl=control)
            strAccuracyT <- max(model.lda$results$Accuracy)
            predictfunc <- paste0("> predictions <- predict(model.lda, validset)<br>> predictedSpecie <- predict(model.lda, dfMyPrediction)")
            predictions <- predict(model.lda, validset)
            strAccuracyV <- confusionMatrix(predictions, validset$Species)$overall[1]
            predictedSpecie <- predict(model.lda, dfMyPrediction)
            
        }else if(input$modeltype == "Classification and Regression Trees (CART)")
        {
            strPackages <- paste0("### Load necessary packages<br>> library(caret)")
            modelfunc <- paste0("<b>Classification and Regression Trees (CART)</b><br><b><br>R Code:</b><br>",strPackages,"<br>",strsampleIris,"<br>",strtrain,"<br>",strcontrol,"<br>","> model.cart <- train(Species~., data=trainset, method='part', metric='",metric,"', trControl=control)")
            model.cart <- train(Species~., data=trainset, method="rpart", metric=metric, trControl=control)
            strAccuracyT <- max(model.cart$results$Accuracy)
            predictfunc <- paste0("> predictions <- predict(model.cart, validset)<br>> predictedSpecie <- predict(model.cart, dfMyPrediction)")
            predictions <- predict(model.cart, validset)
            strAccuracyV <- confusionMatrix(predictions, validset$Species)$overall[1]
            predictedSpecie <- predict(model.cart, dfMyPrediction)
            
        }else if(input$modeltype == "k-Nearest Neighbors (kNN)")
        {
            strPackages <- paste0("### Load necessary packages<br>> library(caret)")
            modelfunc <- paste0("<b>k-Nearest Neighbors (kNN)</b><br><b><br>R Code:</b><br>",strPackages,"<br>",strsampleIris,"<br>",strtrain,"<br>",strcontrol,"<br>","> model.knn <- train(Species~., data=trainset, method='knn', metric='",metric,"', trControl=control)")            
            model.knn <- train(Species~., data=trainset, method="knn", metric=metric, trControl=control)
            strAccuracyT <- max(model.knn$results$Accuracy)
            predictfunc <- paste0("> predictions <- predict(model.knn, validset)<br>> predictedSpecie <- predict(model.knn, dfMyPrediction)")
            predictions <- predict(model.knn, validset)
            strAccuracyV <- confusionMatrix(predictions, validset$Species)$overall[1]
            predictedSpecie <- predict(model.knn, dfMyPrediction)
            
        }else if(input$modeltype == "Support Vector Machines (SVM) with a linear kernel")
        {
            strPackages <- paste0("### Load necessary packages<br>> library(caret)<br>> library(kernlab)")
            modelfunc <- paste0("<b>Support Vector Machines (SVM) with a linear kernel</b><br><b><br>R Code:</b><br>",strPackages,"<br>",strsampleIris,"<br>",strtrain,"<br>",strcontrol,"<br>","> model.svm <- train(Species~., data=trainset, method='svmRadial', metric='",metric,"', trControl=control)")            
            model.svm <- train(Species~., data=trainset, method="svmRadial", metric=metric, trControl=control)
            strAccuracyT <- max(model.svm$results$Accuracy)
            predictfunc <- paste0("> predictions <- predict(model.svm, validset)<br>> predictedSpecie <- predict(model.svm, dfMyPrediction)")
            predictions <- predict(model.svm, validset)
            strAccuracyV <- confusionMatrix(predictions, validset$Species)$overall[1]
            predictedSpecie <- predict(model.svm, dfMyPrediction)
            
        }else
        {
            strPackages <- paste0("### Load necessary packages<br>> library(caret)<br>> library(randomForest)")
            modelfunc <- paste0("<b>Random Forest (RF)</b><br><b><br>R Code:</b><br>",strPackages,"<br>",strsampleIris,"<br>",strtrain,"<br>",strcontrol,"<br>","> model.rf <- train(Species~., data=trainset, method='rf', metric='",metric,"', trControl=control)")            
            model.rf <- train(Species~., data=trainset, method="rf", metric=metric, trControl=control)
            strAccuracyT <- max(model.rf$results$Accuracy)
            predictfunc <- paste0("> predictions <- predict(model.rf, validset)<br>> predictedSpecie <- predict(model.rf, dfMyPrediction)")
            predictions <- predict(model.rf, validset)
            strAccuracyV <- confusionMatrix(predictions, validset$Species)$overall[1]
            predictedSpecie <- predict(model.rf, dfMyPrediction)
            
        }
        
        # Mount html output
        strHtml01 <- paste0("<div id='modelOut1' style='display:block';><hr style='background-color:blue; height:5px; border:0;'><font color='blue'><big><big><b>Selected Model</b></big></big><br><br>",modelfunc,"<br><br><b>Summary:</b><br><b>DataPartition:&nbsp;</b>",input$sdsize*100,"&nbsp;%&nbsp;&nbsp;&nbsp;<b>Used Package:&nbsp;</b>Caret&nbsp;&nbsp;&nbsp;<b>Cross validation Folds:&nbsp;</b>",input$scvfold,"&nbsp;&nbsp;&nbsp;<b>Repetitions:&nbsp;</b>",input$scvrepeat,"<b>&nbsp;&nbsp;&nbsp;Metric:&nbsp;</b>",metric,"<br><br><br><big><big><b>Processing Result:</b><br>Best Obtained Acurracy in Training Set:&nbsp;<b><blink>",round(strAccuracyT*100,2),"&nbsp;%</blink><br></b>Obtained Accuracy in Validation Set:&nbsp;<b><blink>",round(strAccuracyV*100,2),"&nbsp;%</blink></b></font></big></big></div>")
        strHtml02 <- paste0("@<div id='predictOut1' style='display:block';><hr style='background-color:darkorchid; height:5px; border:0;'><font color='darkorchid'><big><big><b>Your Specie Prediction</b></big></big><br><br><b>Selected Model</b>:",input$modeltype,"<br><b><br>R Code:</b><br>",predictfunc,"<br><br><b>Summary:</b><br><b>DataPartition:&nbsp;</b>",input$sdsize*100,"&nbsp;%&nbsp;&nbsp;&nbsp;<b>Used Package:&nbsp;</b>Caret&nbsp;&nbsp;&nbsp;<b>Cross validation Folds:&nbsp;</b>",input$scvfold,"&nbsp;&nbsp;&nbsp;<b>Repetitions:&nbsp;</b>",input$scvrepeat,"<b>&nbsp;&nbsp;&nbsp;Metric:&nbsp;</b>",metric,"<br><br><br><big><big><b>My Predictors </big></b>(dfMyPrediction)<b><big>:&nbsp;</b></big></big><br>",strPredictors,"<br><br><br><big><big><b>Predicted Specie:&nbsp;</b><br><blink>",predictedSpecie,"</blink</font></big></big</div>")
        finalHtmlOutput <- paste0(strHtml01,strHtml02)
    })		
    

    # Formating texts for titles, tabPanels, Slider, Inputs, etc..
    ########################################################################################
    
    output$title <- renderText("Playing With Iris Dataset and Machine Learning Algorithms<small><small><br>[created by Marc Etienne Montrigaud, 15 Jul 2019]</small></small><br><br><small><font color='red'>Explore Dataset (1)</font> </small>/<small> <font color='green'>Explore Vizualizations (2)</font> </small>/<small> <font color='blue'>Explore Machine Learning Models (3)</font> </small>/<small> <font color='darkorchid'>Specie Predictions from selected Model (4)</font><br>&nbsp;</small>")    
    
    output$titletabPanel01 <- renderText("<div id='dataVizu' OnClick='showHr(this)'><font color='red'>1 - Explore Data Set</font></div>")
    output$titletabPanel02 <- renderText("<div id='graphVizu' OnClick='showHr(this)'><font color='green'>2 - Explore Vizualizations</font></div>")
    output$titletabPanel03 <- renderText("<div id='modelVizu' OnClick='showHr(this)'><font color='blue'>3 - Explore Models</font></div>")
    output$titletabPanel04 <- renderText("<div id='predictVizu' OnClick='showHr(this)'><font color='darkorchid'>4 - Try Your Own Specie Predictions</font></div>")
    
    output$titleslider01 <- renderText("<font color='red'>Rows sample lenght (random) for exploratory view:</font>")
    output$titleslider02 <- renderText("<font color='blue'>Define training set partition:</font>")
    output$titleslider03 <- renderText("<font color='blue'>Define number of cross-validation folders:</font>")
    output$titleslider04 <- renderText("<font color='blue'>Define number of cross-validation repetitions:</font>")
    
    output$titleselectInput01 <- renderText("<font color='green'>Choose graph type for data vizualization:</font>")
    output$titleselectInput02 <- renderText("<font color='blue'>Choose machine learning model:</font>")
    
    output$titlenumericInput01 <- renderText("<font color='darkorchid'>Petal Width:</font>")
    output$titlenumericInput02 <- renderText("<font color='darkorchid'>Petal Length:</font>")
    output$titlenumericInput03 <- renderText("<font color='darkorchid'>Sepal Width:</font>")
    output$titlenumericInput04 <- renderText("<font color='darkorchid'>Sepal Length:</font>")
    
    
    # Outputs for table, graphs, graph code, models and predictions
    ########################################################################################
    output$graphcode <- renderText(paste("<b>Graph Code:</b><br>",graphType()))

    output$headiris <- renderTable(iris[sample(nrow(iris),input$sidl),])
    output$distPlot <- renderPlot({
        irisS <- iris[sample(nrow(iris),input$sidl),]
        pca1 <- prcomp(irisS[, -5])
        eval(parse(text = graphType()))
    })            
    output$distModel <- renderText( gsub("@","",substr(modelTypeOut(),0,gregexpr('@',modelTypeOut()))) ) 
    output$distModel1 <- renderText( gsub("@","",substr(modelTypeOut(),gregexpr('@',modelTypeOut()),nchar(modelTypeOut()))) )
})

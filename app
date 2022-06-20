# load libraries and read files ####
library(shiny)
library(shinyjs)
library(gbm)
library(caret)
library(dplyr)
library(ggplot2)
library(scales)
#getwd()
#setwd('Z:/kviars/Using R')
#setwd("~/Documents/Purdue/2022 Summer MGMT 590 UR4A/Final Project")
cleanLego <- read.csv('LegoData.csv')

# set up base for each prediction ####
rating_data <- cleanLego[, -c(1, 2, 3, 5, 6, 7, 10, 17)]
price_data <- cleanLego[, -c(1, 3, 5, 7,  10, 16, 17)]

# rating model ####
df <- rating_data
current_data_valid_values <- df[!is.na(df$star_rating),]
inTrain <- createDataPartition(y = current_data_valid_values$star_rating,
                               p = .7,
                               list = F)
train <- current_data_valid_values[inTrain, ]
test <- current_data_valid_values[-inTrain, ]

# design how I want to design the run
ctrl <- trainControl(method = 'cv',
                     number = 3,
                     classProbs = F,
                     summaryFunction = defaultSummary,
                     allowParallel = T)
gradient_model <- train(star_rating ~ .,
                        data = train,
                        method = 'gbm',
                        trControl = ctrl,
                        metric = 'Rsquared',
                        na.action = na.exclude)

combinedRatingData <- data.frame(current_data_valid_values$star_rating, predict(gradient_model, newdata=current_data_valid_values))
names(combinedRatingData) <- c("actual", "predicted")
combinedRatingData$more_than <- abs((combinedRatingData$actual-combinedRatingData$predicted)/combinedRatingData$actual)>0.25

# price model ####
d <- price_data
d <- d[!is.na(d$star_rating),]
d$y <- d$list_price
y <- d$list_price
d$list_price <- NULL
d <- data.frame(d$y, d[1:ncol(d)-1])
dummies <- dummyVars(d.y ~ review_difficulty + theme_name + country, data = d)            # create dummies for Xs
ex <- data.frame(predict(dummies, newdata = d))  # actually creates the dummies
names(ex) <- gsub("\\.", "", names(ex))          #" removes dots from col names
d$review_difficulty <- NULL
d$theme_name <- NULL
d$country <- NULL
d <- cbind(d, ex)                              # combine target var with Xs
names(d)[1] <- "y"                               # name target var 'y'
columns <- names(d)
rm(dummies, ex)                                  # clean environment

preProcValues <- preProcess(d[,2:ncol(d)], method = c("range"))
d <- predict(preProcValues, d)
rm(preProcValues)
inTrain <- createDataPartition(y = d$y,   # outcome variable
                               p = .70,   # % of training data you want
                               list = F)
# create your partitions
train <- d[inTrain,]  # training data set
test <- d[-inTrain,]  # test data set

ctrl <- trainControl(method="cv",     # cross-validation set approach to use
                     number=5,        # k number of times to do k-fold
                     classProbs = F,  # if you want probabilities
                     #summaryFunction = twoClassSummary, # for classification
                     summaryFunction = defaultSummary,  # for regression
                     allowParallel=T)
price_gradient_model <- train(y ~ .,
                        data = train,
                        method = 'gbm',
                        trControl = ctrl,
                        metric = 'Rsquared')

combinedData <- data.frame(d$y, predict(price_gradient_model, newdata=d))
names(combinedData) <- c("actual", "predicted")
combinedData$more_than <- abs((combinedData$actual-combinedData$predicted)/combinedData$actual)>0.25

# shiny app ####
# ui ####
ui <- fluidPage(
  useShinyjs(),
  theme = bslib::bs_theme(bootswatch = "united"),
  titlePanel('Brick by Brick: Leveraging Lego Analytics for Rating and Pricing Decisions'), 
  sidebarLayout(
      sidebarPanel(
          actionButton(inputId='go', label='Ready to Explore/Predict!'),                    
          numericInput('pieces', 'Number of Pieces', 100),
          selectInput('levels', 'Number of Difficulty Levels', c(1:6), 1),
          selectInput('theme', 'Theme', sort(unique(cleanLego$theme_name))), 
          selectInput('country', 'Target Country', sort(unique(cleanLego$country)), 'US'), 
          numericInput('minage', 'Minimum Age', 6),
          numericInput('maxage', 'Maximum Age', 12),
          radioButtons('difficulty', 'Difficulty Level', selected = 'Very Easy', 
                       choices=c("Very Easy", "Easy", "Average", 
                                 "Challenging", "Very Challenging")),
          h4('Set Options (T/F)', id="label"),
          checkboxInput('registered', 'Registered Product'), 
          checkboxInput('trademark', 'Trademarked Product'), 
          checkboxInput('specialized', 'Specialized Set')
      ),
  
      mainPanel( #####
         tabsetPanel(type = 'tab',
                     id='tabset',
                     tabPanel('Explore Pricing',
                              textOutput('note'),
                              plotOutput('explore_pricing', height=750)
    
                     ),
                     tabPanel('Explore Ratings',
                              textOutput('note2'),
                              uiOutput('note3'),
                              dataTableOutput('explore_rating'),
                              splitLayout(cellWidths = c("50%", "50%"), 
                                          plotOutput("rating_plot1"), plotOutput("rating_plot2")),
                              plotOutput('rating_plot3'),
                              uiOutput('note4'),
                              tags$style("p {color: blue; style: bold}"),
                              dataTableOutput('corr')
                     ),
                     tabPanel('Explore Difficulty',
                              textOutput('note6'),
                              plotOutput('guess'),
                              plotOutput('actuals')
                     ),
                     tabPanel('Model Performance', 
                              textOutput('note7'),
                              plotOutput('ratingPerformance'),
                              plotOutput('pricePerformance')
                     ),
                     tabPanel('Predictions', 
                              uiOutput('note5'),
                              dataTableOutput('summary'),
                              textOutput('rating'),
                              tags$head(tags$style("#rating{color: red;
                                 font-size: 40px;
                                 font-style: italic;
                                 }"
                              )),
                              textOutput('price'),
                              tags$head(tags$style("#price{color: red;
                                 font-size: 40px;
                                 font-style: italic;
                                 }"
                              )),
                              textOutput('percentile'),
                              plotOutput('comparison_plot')
                     )
       )
     )
  )
)

  

# server ####
server <- function(input, output) {
  library(caret)
  library(wesanderson)
  #Hides non relevant inputs
  observeEvent(input[["tabset"]], {
    if(input[["tabset"]] != "Explore Difficulty") {
      hideElement(selector ="#levels")
      
    } else {
      showElement(selector="#levels")
    }
  })
  observeEvent(input[["tabset"]], {
    if(input[["tabset"]] != "Model Performance") {
      showElement(selector ="#go")
      
    } else {
      hideElement(selector="#go")
    }
  })  
  observeEvent(input[["tabset"]], {
    if(input[["tabset"]] != "Predictions") {
      hideElement(selector ="#pieces")
      hideElement(selector ="#minage")
      hideElement(selector ="#maxage")
      hideElement(selector ="#registered")
      hideElement(selector ="#specialized")
      hideElement(selector ="#trademark")
      hideElement(selector ="#label")
      
    } else {
      showElement(selector="#pieces")
      showElement(selector="#minage")
      showElement(selector="#maxage")
      showElement(selector="#registered")
      showElement(selector="#specialized")
      showElement(selector="#trademark")
      showElement(selector="#label")
      
    }
  })
  observeEvent(input[["tabset"]], {
    if(input[["tabset"]] == "Predictions" | input[["tabset"]] == "Explore Pricing") {
      showElement(selector="#theme")
    } else {
      hideElement(selector ="#theme")
    }
  })
  observeEvent(input[["tabset"]], {
    if(input[["tabset"]] == 'Explore Difficulty' | input[["tabset"]] == "Model Performance") {
      hideElement(selector ="#country")
    } else {
      showElement(selector="#country")
    }
  })
  observeEvent(input[["tabset"]], {
    if(input[["tabset"]] == 'Explore Ratings' | input[["tabset"]] == "Predictions") {
      showElement(selector ="#difficulty")
    } else {
      hideElement(selector="#difficulty")
    }
  })
  
  
  # wait until users hit go  ####
  # tab 1 exploring price ####
  output$note <- renderText(print('Select country and theme name to learn more about the pricing'))
  # subset the data based on inputs
  explor_data <- eventReactive(input$go, {
    cleanLego[cleanLego$theme_name == input$theme & 
              #  cleanLego$review_difficulty == input$difficulty & 
                cleanLego$country == input$country, ]
  })
  # find higher priced set name
  output$explore_pricing <- renderPlot({
    df <- explor_data()
    df$price_per_brick <- df$list_price/df$piece_count
    ggplot(df, aes(x=price_per_brick, y=reorder(set_name, +price_per_brick), fill=review_difficulty)) + 
      geom_bar(stat="Summary", fun="mean") +
      labs(y='', x='Price per Brick', main="Price per Brick by Set Name", fill="Difficulty Level") +
      scale_x_continuous(labels=scales::dollar_format())
    
  })
 
  # tab 2 exploring ratings ####
  output$note2 <- renderText(print('Select country and difficulty level to learn more about the rating.'))
  output$note3 <- renderUI(HTML(markdown::renderMarkdown(text = "***\nThere are 3 types of ratings in our dataset. Let's investigate if there is any significant difference between records with valid and missing ratings.\n- play_star_rating = Play Experience\n- star_rating = Overall rating\n- val_star_rating = Value for Money")))
  # subset the data based on inputs
  explor_data2 <- eventReactive(input$go, {
    cleanLego[cleanLego$review_difficulty == input$difficulty & 
                cleanLego$country == input$country, ]
  })
  # compare between missing or valid
  output$explore_rating <- renderDataTable(
    explor_data2() %>%
      mutate(play_experience_rating = ifelse(is.na(play_star_rating), 'missing', 'valid'),
             value_for_money_rating = ifelse(is.na(val_star_rating), 'missing', 'valid')) %>%
    select(play_experience_rating, value_for_money_rating, list_price, 
           num_reviews, piece_count, difficulty) %>%
    group_by(play_experience_rating, value_for_money_rating) %>%
    summarise(avg_price = mean(list_price),
              avg_num_reviews = mean(num_reviews),
              avg_piece_count = mean(piece_count),
              avg_difficulty = mean(difficulty, na.rm = TRUE),
              count = n()) %>%
    data.frame(), 
    options=list(info=FALSE, lengthChange = FALSE, filter=F, paging=F, searching=F)
    )
  # explore those with valid values
  validRating_data <- eventReactive(input$go, {
    validRating <- explor_data2()
    validRating[!(is.na(validRating$play_star_rating) | is.na(validRating$star_rating) | is.na(validRating$val_star_rating)), ]
  })
  
  # prove all 3 types of ratings are positively related
  output$rating_plot1 <- renderPlot({
    ggplot(validRating_data(), aes(play_star_rating, star_rating, color = theme_name)) +
      geom_point() +
      ggtitle('Play Experience vs. Overall Rating') + 
      theme(legend.position="bottom")
  })
  output$rating_plot2 <- renderPlot({
    ggplot(validRating_data(), aes(play_star_rating, val_star_rating, color = theme_name)) +
      geom_point() +
      ggtitle('Play Experience vs. Value for Money Rating') +
      theme(legend.position="bottom")
  })
  output$rating_plot3 <- renderPlot({
    ggplot(validRating_data(), aes(star_rating, val_star_rating, color = theme_name)) +
      geom_point() +
      ggtitle('Value for Money vs. Overall Rating') +
      theme(legend.position="bottom")
  })
  # tab 3 exploring difficulty ####
  
  output$note6 <- renderText(print('Guess how many difficulty levels in the lego world. We hope that you will observe something interesting trend here. And how close do you think it reflects the truth?'))
  # run kmeans model based on inputs
  kmean_data <- eventReactive(input$go, {
    cleanLego[!(is.na(cleanLego$play_star_rating) | is.na(cleanLego$star_rating) | is.na(cleanLego$val_star_rating)), ]
  })
  clusters <- eventReactive(input$go,{
    # remove non-numerical columns
    df <- kmean_data()[, -c(1,6,7,9,11,16)]
    kmeans(scale(df), input$levels)
  })
  # the guessing difficulty levels
  output$guess <- renderPlot({
    df <- kmean_data()
    df$cluster <- clusters()$cluster
    df$cluster <- as.factor(df$cluster)
    ggplot(df, aes(x=num_reviews, y=piece_count, color=cluster)) + 
        geom_point() + 
        scale_y_log10() +
        ggtitle('Your Experiment') +
        scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))
  })
  # the actual difficulty levels
  output$actuals <- renderPlot({
      ggplot(cleanLego, aes(x=num_reviews, y=piece_count, color=review_difficulty)) + 
        geom_point() + 
        scale_y_log10() +
        ggtitle('Actual Difficulty Levels')
  })
  # make conclusion
  output$note4 <- renderUI(HTML(markdown::renderMarkdown(text = "***\nBased on the exploratory analysis among all three types of ratings and below correlation matrix on the entire data set, we decided to use star_rating in our pricing prediction model.")))
  output$corr <- renderDataTable(
  #  col_names <- 
  #  corr <- 
    cbind(variables = c('play_star_rating', 'star_rating', 'val_star_rating'), round(cor(cleanLego[, c('play_star_rating', 'star_rating', 'val_star_rating')], use = 'complete.obs'),2)) , options=list(info=FALSE, lengthChange = FALSE, filter=F, paging=F, searching=F)
  )
  #model prediction tab
  output$note7 <- renderText('Below shows the actual vs predicted values for the models that predict the star rating and product price')
  
  output$ratingPerformance <- renderPlot({
    ggplot(combinedRatingData, aes(x=actual, y=predicted, color=more_than)) + geom_point() + xlim(0, 5) + ylim(0, 5) + geom_abline(slope=1, intercept=0) + labs(title="Performance of Rating Model", x="Actual Rating", y="Predicted Rating", color="More than 25% Error")
  })
  output$pricePerformance <- renderPlot({
    ggplot(combinedData, aes(x=actual, y=predicted, color=more_than)) + geom_point() + geom_abline(slope=1, intercept=0) + labs(title="Performance of Pricing Model", x="Actual Price", y="Predicted Price", color="More than 25% Error")
  })  
  
  # tab 4 predictions ####
  output$note5 <- renderUI(HTML(markdown::renderMarkdown(text = "Now it's your time to build your own Lego product!\n- Take a moment to think about the features that you would like to add to your product. \n- When you are ready, make choices on every item on the left hand side.\n- We will tell you the predicted rating and predicted pricing based on your plans.")))
  # repeat users inputs
  return_selectData <- eventReactive(input$go, {
    sum <- data.frame(matrix(0, ncol=9, nrow=2))
    sum[1, ] <- names(rating_data)[-2]
    sum[2, ] <- c(input$pieces, input$theme, input$country, input$registered,
                  input$trademark, input$minage, input$maxage, input$difficulty, input$specialized)
    sum2 <- data.frame(t(sum))
    names(sum2) <- c('Variables', "Your Product's Value")
    sum2
  })
  output$summary <- renderDataTable(return_selectData(), options=list(info=FALSE, lengthChange = FALSE, filter=F, paging=F, searching=F))
  # get users inputs for predicting rating ####
  predRating <- eventReactive(input$go, {
    # create empty data frame to load inputs
    pre <- data.frame(matrix(0, ncol=10, nrow=1))
    names(pre) <- names(rating_data)
    # assign inputs to data frame
    diff_value  <- if (input$difficulty == "Blank") {0}
                   else if (input$difficulty == "Very Easy") {1}
                   else if (input$difficulty == "Easy") {2}
                   else if (input$difficulty == "Average") {3}
                   else if (input$difficulty == "Challenging") {4}
                   else {5}
    pre[1, c(1,7:9)]  <- c(input$pieces, input$minage, input$maxage, diff_value)
    pre[1, c(3,4)]    <- c(input$theme, input$country)
    pre[1, c(5,6,10)] <- c(input$registered, input$trademark, input$specialized)
    # convert data types
    pre[, c(3,4)]    <- lapply(pre[1, c(3,4)], as.factor)
    pre[, c(5,6,10)] <- lapply(pre[, c(5,6,10)], as.logical)
    # use gradient model to predict ratings based on inputs
    round(predict(gradient_model, newdata=pre), 2)
  })

  
  # return the predicted ratings
  output$rating <- reactive({paste0("Predicted Star Rating: ", predRating())})

  # get users inputs for predicting rating ####
  predPrice <- eventReactive(input$go, {
    # create empty data frame to load inputs
    prePrice <- data.frame(matrix(0, ncol=ncol(d), nrow=1))
    names(prePrice) <- names(d)

    # assign inputs to data frame
    theme <-  gsub("[^[:alnum:]]","",input$theme)
    prePrice[1, paste0("theme_name", theme)] <- 1
    prePrice[1, paste0("review_difficulty", gsub("[^[:alnum:]]","",input$difficulty))] <- 1
    prePrice[1, paste0("country", input$country)] <- 1

    pieceCt <- cleanLego$piece_count
    prePrice[1, 'piece_count'] <-  (input$pieces-min(pieceCt))/(max(pieceCt)-min(pieceCt))
    minAge <- cleanLego$min_age
    prePrice[1, "min_age"] <- (input$minage-min(minAge))/(max(minAge)-min(minAge))
    maxAge <- cleanLego$max_age
    prePrice[1, "max_age"] <- (input$maxage-min(maxAge))/(max(maxAge)-min(maxAge))

    logic_columns <- c("registered", "trademark", "specialized")
    prePrice[1, logic_columns] <- c(input$registered, input$trademark, input$specialized)

    # convert data types
    prePrice[, logic_columns] <- lapply(prePrice[, logic_columns], as.logical)
    # replace missing star_rating with the predicted rating
    prePrice$star_rating <- predRating()
    # use gradient model to predict price based on inputs and predicted rating
    predict(price_gradient_model, newdata=prePrice)
  })

  # return the predicted ratings
  output$price <- reactive({ paste0("Predicted Price: $", round(predPrice(),2))})

  # percentile info ####
  output$percentile <- reactive ({
    count <- nrow(cleanLego[!is.na(cleanLego$star_rating),])
    countBelow <- nrow(cleanLego[cleanLego$star_rating <= predRating(),])
    a <- paste0("This set is predicted to be higher rated than ",
                round(countBelow/count*100,2), "% of all LEGO sets")
  })

  # plot ####
  output$comparison_plot <- renderPlot({
    col_names <- c("list_price", "star_rating", "piece_count")
    data <- cleanLego[cleanLego$theme_name==input$theme & cleanLego$country==input$country,
                      col_names]
    data$type <- "Original"
    data <- rbind(data, c(as.numeric(predPrice()), as.numeric(predRating()),
                          as.numeric(input$pieces), "Predicted"))
    data[, col_names] <- lapply(data[, col_names], as.numeric)
    ggplot(data, aes(x=piece_count, y=list_price, color=type)) +
        geom_point(aes(size=type)) +
      labs(x="Number of Pieces", y="List Price",
           title=paste0("Comparison for ", input$theme, " sets in ", input$country))
  })

}

# shinyApp ####
shinyApp(ui = ui, server = server)

# deploy ####
# library(rsconnect)
#deployApp('~/Documents/Purdue/2022 Summer MGMT 590 UR4A/Final Project/apps')
#deployApp('Z:/kviars/Using R/Final Project')

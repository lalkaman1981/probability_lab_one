library(dplyr)
library(tidytext)
library(ggplot2)


text_filtering <- function(file) {
  
  data <- read.csv(file, stringsAsFactors = FALSE)
  rows <- nrow(data)
  filtered_messages <- vector("character", length = rows)
  bin_vec <- vector("numeric", length = rows)
  
  remove_words <- readLines("stop_words.txt")
  
  for (i in 1:rows) {
    category <- data$Category[i]
    message <- data$Message[i]
    
    line <- gsub("\\bspam\\b|\\bham\\b|[.,@$%&*!?:()#;]", "", message)
    words <- strsplit(line, "\\s+")[[1]]
    
    filtered_words <- words[!tolower(words) %in% tolower(remove_words)]
    
    result_line <- paste(filtered_words, collapse = " ")
    filtered_messages[i] <- result_line
    
    if (category == "ham") {
      bin_vec[i] <- 1
    } else {
      bin_vec[i] <- 0
    }
  }
  
  return(list(lines = filtered_messages, binary_vec = bin_vec))
}


naiveBayes <- setRefClass("naiveBayes",
                          
                          fields = list(
                            class_probabilities = "list",
                            word_frequencies = "list",
                            vocab = "character",
                            lens = "list"
                          ),
                          
                          methods = list(
                            
                            initialize = function() {
                              class_probabilities <<- list(ham = 0, spam = 0)
                              word_frequencies <<- list(ham = list(), spam = list(), all = list())
                              vocab <<- character()
                              lens <<- list(len = 0, spam = 0, ham = 0)
                            },
                            
                            fit = function(X, y) {
                              .self$lens[["len"]] <- length(y) + .self$lens[["len"]]
                              len <- .self$lens[["len"]]
                              multiplayer <- 1 / len
                              
                              classes <- list(ham = 1, spam = 0)
                              
                              for (class in names(classes)) {
                                val <- classes[[class]]
                                class_count <- sum(y == val)
                                .self$lens[[class]] <- class_count + .self$lens[[class]]
                                
                                .self$class_probabilities[[class]] <- (class_count + .self$class_probabilities[[class]]) / len
                                
                                class_messages <- X[y == val]
                                
                                for (message in class_messages) {
                                  words <- unlist(strsplit(message, "\\s+"))
                                  for (word in words) {
                                    if (!(word %in% .self$vocab)) {
                                      .self$vocab <- c(.self$vocab, word)
                                    }
                                    if (!is.null(.self$word_frequencies[[class]][[word]])) {
                                      .self$word_frequencies[[class]][[word]] <- .self$word_frequencies[[class]][[word]] + multiplayer
                                      .self$word_frequencies[["all"]][[word]] <- .self$word_frequencies[["all"]][[word]] + multiplayer
                                    } else {
                                      .self$word_frequencies[[class]][[word]] <- multiplayer
                                      .self$word_frequencies[["all"]][[word]] <- multiplayer
                                    }
                                  }
                                }
                              }
                            },
                            
                            predict = function(message) {
                              words <- unlist(strsplit(message, "\\s+"))
                              product_lst <- list(spam = .self$class_probabilities[["spam"]], 
                                                  ham = .self$class_probabilities[["ham"]])
                              
                              for (class in names(product_lst)) {
                                for (word in words) {
                                  if (!is.null(.self$word_frequencies[[class]][[word]])) {
                                    product_lst[[class]] <- product_lst[[class]] * .self$word_frequencies[[class]][[word]]
                                  } else {
                                    product_lst[[class]] <- product_lst[[class]] * (1 / (.self$lens[[class]] + 1))
                                  }
                                }
                              }
                              
                              if (product_lst$spam > product_lst$ham) {
                                return(0)  # spam
                              }
                              return(1)  # ham
                            },
                            
                            score = function(X_test, y_test) {
                              test_number <- length(y_test)
                              counter <- 0
                              for (test in 1:test_number) {
                                result <- .self$predict(X_test[test])
                                if (result == y_test[test]) {
                                  counter <- counter + 1
                                }
                              }
                              return(counter / test_number)
                            }
                          ))

visualize_classification_results <- function(y_true, y_pred) {
  true_positive <- sum((y_pred == 1) & (y_true == 1))
  false_positive <- sum((y_pred == 1) & (y_true == 0))
  false_negative <- sum((y_pred == 0) & (y_true == 1))  # Spam classified as ham
  true_negative <- sum((y_pred == 0) & (y_true == 0))
  
  results <- data.frame(
    Result = c("True Positive", "False Positive", "False Negative", "True Negative"),
    Count = c(true_positive, false_positive, false_negative, true_negative)
  )
  
  ggplot(results, aes(x = Result, y = Count, fill = Result)) +
    geom_bar(stat = "identity") +
    labs(title = "Histogram of Classification Results", x = "Result Type", y = "Count") +
    theme_minimal() +
    scale_fill_manual(values = c("True Positive" = "blue",
                                 "False Positive" = "red",
                                 "False Negative" = "orange",
                                 "True Negative" = "green"))
}

top_words <- function(filtered_text) {
  data <- data.frame(
    Message = unlist(filtered_text$lines),
    Category = ifelse(filtered_text$binary_vec == 1, "ham", "spam"),
    stringsAsFactors = FALSE
  )
  
  data_words <- data %>%
    unnest_tokens(word, Message) %>%
    anti_join(data.frame(word = readLines("stop_words.txt"), stringsAsFactors = FALSE), by = "word")
  
  word_counts <- data_words %>%
    count(Category, word, sort = TRUE) %>%
    ungroup()
  
  top_words <- word_counts %>%
    group_by(Category) %>%
    top_n(10, n) %>%
    arrange(Category, desc(n))
  
  return(top_words)
}

visualize_top_words <- function(top_words_data) {
  ggplot(top_words_data, aes(x = reorder(word, n), y = n, fill = Category)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    coord_flip() + 
    labs(title = "Top 20 Words in Spam and Ham",
         x = "Words",
         y = "Frequency",
         fill = "Category") +
    theme_minimal() +
    scale_fill_manual(values = c("spam" = "red", "ham" = "green"))

}

main <- function() {
  model = naiveBayes()
  
  filtered_text <- text_filtering("./4-spam/train.csv")
  model$fit(filtered_text$lines, filtered_text$binary_vec)
  
  filtered_text1 <- text_filtering("./4-spam/test.csv")
  
  res <- model$score(filtered_text1$lines, filtered_text$binary_vec)
  print(res)
  
  y_true <- filtered_text1$binary_vec
  y_pred <- sapply(filtered_text1$lines, model$predict)
  
  top_words_result <- top_words(filtered_text)
  
  results_df <- data.frame(True = y_true, Predicted = y_pred)
  print(results_df)
  
  #visualize_top_words(top_words_result)
  visualize_classification_results(y_true, y_pred)
}

main()



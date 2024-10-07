library(dplyr)
library(tidytext)

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
                              mult_lst <- list(spam = 1/.self$lens[["spam"]], ham = 1/.self$lens[["ham"]])
                              mult <- 1/.self$lens[["len"]]
                              product_lst <- list(spam = 1, ham = 1)
                              
                              for (class in names(product_lst)) {
                                for (word in words) {
                                  if (!is.null(.self$word_frequencies[[class]][[word]])) {
                                    product_lst[[class]] <- product_lst[[class]] * .self$word_frequencies[[class]][[word]] *
                                      .self$class_probabilities[[class]] / .self$word_frequencies[["all"]][[word]]
                                  } else {
                                    product_lst[[class]] <- product_lst[[class]] * mult * .self$class_probabilities[[class]]/
                                      mult_lst[[class]]
                                  }
                                }
                              }
                              
                              if (product_lst$spam > product_lst$ham) {
                                return(0)
                              }
                              return(1)
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

main <- function() {
  model = naiveBayes()
  filtered_text <- text_filtering("./4-spam/train.csv")
  model$fit(filtered_text$lines, filtered_text$binary_vec)
  
  filtered_text1 <- text_filtering("./4-spam/test.csv")
  
  res <- model$score(filtered_text1$lines, filtered_text1$binary_vec)
  print(res)
}

main()



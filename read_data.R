library(stringr)
library(ggplot2)
library(dplyr)

remove_spam_lines <- function(lines, text_to_remove) {

  pattern <- paste0("^", text_to_remove)
  
  cleaned_lines <- lines[!grepl(pattern, lines)]
  
  cleaned_lines <- cleaned_lines[-1]
  
  result <- paste(cleaned_lines, collapse = " ")

  return(result)
}

text_filtering <- function(file, text_to_remove) {

  text <- readLines(file)
  
  text_without_removed <- remove_spam_lines(text, text_to_remove)
  
  text_no_punctuation <- gsub("\\bword1\\b|\\bword2\\b|[.,@$%&*!?:()#;]", "", text_without_removed)

  text_no_punctuation <- tolower(text_no_punctuation)

  words <- unlist(strsplit(text_no_punctuation, "\\s+"))

  remove_words <- readLines("stop_words.txt")
  
  filtered_words <- words[!tolower(words) %in% tolower(remove_words)]
  
  return(filtered_words)
}

numb_of_word <- function(file, word) {
  text <- readLines(file)
  
  numb <- str_count(text, paste0("\\b", word, "\\b"))
  
  return(sum(numb))
}

binary_bag_of_words <- function(filtered_words) {

  word_table <- table(filtered_words)

  binary_bow <- ifelse(word_table > 0, 0, 0)

  bow_df <- data.frame(Word = names(word_table), Presence = as.numeric(binary_bow))
  
  return(bow_df)
}

bag_of_words <- function(filtered_words) {
  
  word_table <- table(filtered_words)
  
  word_freq_df <- as.data.frame(word_table)

  colnames(word_freq_df) <- c("word", "frequency")
  
  return(word_freq_df)
  
}

plot_data <- function(bag_of_words) {
  top_100_words <- bag_of_words %>%
    arrange(desc(frequency)) %>%
    slice(1:20)
  
  ggplot(top_100_words, aes(x = reorder(word, frequency), y = frequency)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    coord_flip() +
    labs(title = "Top 20 Most Frequent", x = "Words", y = "Frequency") +
    theme_minimal() 
}


main <- function() {

  spam_words <- text_filtering("4-spam/train.csv", "ham")
  
  ham_words <- text_filtering("4-spam/train.csv", "spam")

  bag_of_spam <- bag_of_words(spam_words)
  
  bag_of_ham <- bag_of_words(ham_words)

  spam_number <- numb_of_word("4-spam/train.csv", "spam")

  ham_number <- numb_of_word("4-spam/train.csv", "ham")

  p_ham <- ham_number/(spam_number + ham_number)

  p_spam <- spam_number/(spam_number + ham_number)

  pdf("output_graphs.pdf")
  
  plot_data(bag_of_spam)

  plot_data(bag_of_ham)
  
  dev.off()
}

main()


library(dplyr)
library(tidyverse)
library(stringr)

# load the data and get all lines into a list (not good for big files but ok here)
con <- file("Day6_data.txt", open="r")
data <- readLines(con)

# first step is to get the lines corresponding to a unique person on a single line
# the procedure is basically the same as the day 4 challenge, so I am reusing much of that code
person <- 1
i <- 1
data_cleansed <- list() # list to hold each unique person
new_line <- "" # start new person with an ID numbers
for(line in data){ # get the next line of data
  if (nchar(line)>0){ # if it is not a blank line
    new_line <- paste(new_line, line) # append it to the current person
  }
  else{ # if it is a blank line, it is the end of a group's answers
    complete_line <- str_replace_all(new_line, " ", "") # compress the spaces
    sorted_line <- paste(sort(unlist(strsplit(complete_line, ""))), collapse = "") # sort the letters in order
    data_cleansed[[person]] <- gsub('([[:alpha:]])\\1+', '\\1', sorted_line) # removes all duplicate letters
    person <- person + 1                # increment the count
    new_line <- "" # start a new group
  }
  if (i == length(data)){ # if it is the last line of the data
    complete_line <- str_replace_all(new_line, " ", "")
    sorted_line <- paste(sort(unlist(strsplit(complete_line, ""))), collapse = "")
    data_cleansed[[person]] <- gsub('([[:alpha:]])\\1+', '\\1', sorted_line) # write out the last group's info
  }
  i <- i+1
}

# Unlist the items to form a single series of the answer strings
unlisted <- unlist(data_cleansed)
# Put the answers in a data frame so we can easily manipulate
df <- as.data.frame(unlisted, stringsAsFactors = FALSE)
df_full <- df %>% mutate(total_qs = nchar(unlisted))

# Now the answer to part 1 is just the sum of the total_qs column
sum(df_full$total_qs)

data_raw <- list() # list to hold each unique person
person <- 1
i <-1
new_line <- "" # start new person with an ID numbers
for(line in data){ # get the next line of data
  if (nchar(line)>0){ # if it is not a blank line
    new_line <- paste(new_line, line) # append it to the current person
  }
  else{ # if it is a blank line, it is the end of a group's answers
    data_raw[[person]] <- new_line # write out this line
    person <- person + 1                # increment the count
    new_line <- "" # start a new person
  }
  if (i == length(data)){ # if it is the last line of the data
    data_raw[[person]] <- new_line # write out the last group's info
  }
  i <- i+1
}

# unlist the new data
data_raw <- unlist(data_raw)

# write a function to take the string, count the blanks, then determine which letters
# have a count equal to the blanks
count_shared_answers <- function(group_answers){
  num_blanks <- str_count(group_answers, " ")    # each blank represents 1 person, so count of blanks is total people in group
  out_list <- ""                                 # blank string to store answers
  for (i in 1:nchar(group_answers)){             # for each character in the string
    char = substr(group_answers, i, i)           # get the character
    char_count <- str_count(group_answers, char) # count the number of occurences of the character
    if (char_count==num_blanks){                 # if the count is the same as the number of spaces
      out_list <- paste0(out_list, char)         # write the character out, since everyone answered it
    }
  }
  # Now we can follow the same procedure to remove all blanks and duplicate letters
  complete_line <- str_replace_all(out_list, " ", "") # compress the spaces
  sorted_line <- paste(sort(unlist(strsplit(complete_line, ""))), collapse = "")
  sorted_line <- gsub('([[:alpha:]])\\1+', '\\1', sorted_line)
  sorted_line
}

# Now we can use lapply to apply the function to all items in our data_raw list
shared_list <- unlist(lapply(data_raw, count_shared_answers))

# Make a data frame of the information as before
df_shared <- as.data.frame(shared_list, stringsAsFactors = FALSE)
df_shared$answers <- data_raw
df_shared <- df_shared %>% mutate(total_qs = nchar(shared_list))

# The answer to part 2 is again just the sum of the total_qs column
sum(df_shared$total_qs)

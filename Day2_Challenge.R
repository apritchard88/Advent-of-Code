library(tidyverse)
library(readr)

# first read data in, set stringsAsFactors = FALSE as we need them to be recognised as characters for stringr functions
data <- read.delim("Day2_data.txt", header=FALSE, sep=" ", stringsAsFactors = FALSE)
# make the column names more understandable
colnames(data) <- c("value", "letter", "password")
# now manipulate the data into a better format for the problem
data_summary <- data %>% mutate(val_start = as.integer(str_split_fixed(value, "-",2)[,1]),        # split the range column into a start and end position
                        val_end = as.integer(str_split_fixed(value, "-",2)[,2]),
                        find_letter = substr(letter,1,1),                                         # get rid of the colon 
                        occurences = as.integer(str_count(password, find_letter)),                # count occurences of the letter in the password
                        match = ifelse(occurences>(val_start) & occurences <(val_end), 1, 0)) %>% # flag where the number of occurences falls in the range required
                 select(val_start, val_end, find_letter, password, occurences, match)

# separate the matches into a new data frame so we can check it makes sense
password_matches <- data_summary %>% filter(occurences<=val_end & occurences>=val_start)

# after some corrections (needed to use as.integer above) the matches are correct
# count matching rows to get the answer
nrow(password_matches)

## Part 2
# make a new data frame with some manipulation specific to part 2
data_part_2 <- data_summary %>% mutate(pos1 = substring(password, val_start, val_start), # find the character in the val_start position of the password
                                       pos2 = substring(password, val_end, val_end),     # find the character in the val_end position of the password
                                       pos1_match = ifelse(pos1 == find_letter, 1, 0),   # check if the val_start character matches the required
                                       pos2_match = ifelse(pos2 == find_letter, 1, 0),   # check if the val_end character matches the required
                                       matches = pos1_match+pos2_match) %>%              # how many of the character positions match? (0, 1, or 2)
                                select(val_start, val_end, find_letter, password, pos1, pos2, pos1_match, pos2_match, matches)

# data frame of only rows where 1 of the 2 positions is correct (as per password requirement)
position_matches <- data_part_2 %>% filter(matches == 1) 
nrow(position_matches) # total rows is the number of correct passwords

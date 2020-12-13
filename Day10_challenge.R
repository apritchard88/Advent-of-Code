library(dplyr)
library(tidyverse)
library(igraph)

# Read in the data
con <- file("Day10_data.txt", open="r")
data <- readLines(con)

# change to integers and and nodes for the start (0) and end (max + 3)
data <- as.integer(data)
data <- append(data, max(data)+3)
data <- append(data, 0)

# put the data into a data frame and sort it low to high
df <- data.frame(sort(data))

# the following line calculates the difference from one item to the next
diff_vals <- tail(df, -1) - head(df, -1)

# make a table of the differences, find the totals for 1 and 3
tab <- table(diff_vals)
ones <- tab[names(tab)==1]
threes <- tab[names(tab)==3]

# answer to part 1 is ones*threes
ones*threes


#### Part 2
# We need to find all possible paths from 0 to 186
# I plan to use igraph to make a network of the adapters and then find all possible paths
possible_rows <- nrow(df)*3 # how many possible links could there be
links <- data.frame(matrix(NA, nrow = possible_rows, ncol = 2)) # data frame of NAs
colnames(links) <- c("start", "end")
colnames(df) <- c("val")

# loop through the list of different adapters in order
# find the 3 valid values for the next step (ie +1, +2, +3) and see if they are in the list
# for each step, create a row with the link shown
row <- 1
for (i in df$val){
  step_1 <- i+1
  step_2 <- i+2
  step_3 <- i+3
  if (step_1 %in% df$val){
    links$start[row] <- i
    links$end[row] <- step_1
    row <- row+1
  }
  if (step_2 %in% df$val){
    links$start[row] <- i
    links$end[row] <- step_2
    row <- row+1
  }
  if (step_3 %in% df$val){
    links$start[row] <- i
    links$end[row] <- step_3
    row <- row+1
  }
  
}
# get rid of any unused rows
links <- links %>% filter(!is.na(end))

# now links contains all possible links from one adapter to the next
# we can use igraph to create a network from 0 to 186
g <- graph_from_data_frame(links, directed = TRUE)

# Now count all the possible routes from the start to the end
#paths_to_end <- length(all_simple_paths(g, from = "0", to = "186", mode = "out"))

# Too many nodes to calculate all paths, it kills the memory
# Instead split the path into sections, cutting at places where there is only one 
# possible path, and multiply the total paths through each section

# Probably should have automated this, but paths were short enough to just find places
# to cut by eye
# Cutting into 6 sections to make sure the memory holds
path_to_31 <- links %>% filter(end<=31)
path_to_63 <- links %>% filter(end>31 & end<=63)
path_to_96 <- links %>% filter(end>63 & end<=96)
path_to_118 <- links %>% filter(end>96 & end<=118)
path_to_138 <- links %>% filter(end>118 & end<=138)
path_to_end <- links %>% filter(end>138)

g31 <- graph_from_data_frame(path_to_31, directed = TRUE)
g63 <- graph_from_data_frame(path_to_63, directed = TRUE)
g96 <- graph_from_data_frame(path_to_96, directed = TRUE)
g118 <- graph_from_data_frame(path_to_118, directed = TRUE)
g138 <- graph_from_data_frame(path_to_138, directed = TRUE)
gend <- graph_from_data_frame(path_to_end, directed = TRUE)

paths_to_31 <- as.double(length(all_simple_paths(g31, from = "0", to = "31", mode = "out")))
paths_to_63 <- as.double(length(all_simple_paths(g63, from = "31", to = "63", mode = "out")))
paths_to_96 <- as.double(length(all_simple_paths(g96, from = "63", to = "96", mode = "out")))
paths_to_118 <- as.double(length(all_simple_paths(g118, from = "96", to = "118", mode = "out")))
paths_to_138 <- as.double(length(all_simple_paths(g138, from = "118", to = "138", mode = "out")))
paths_to_186 <- as.double(length(all_simple_paths(gend, from = "138", to = "186", mode = "out")))

# Found I need to increase the displayed digits in order to get the full answer
options(digits=20)

# total possible paths is all the section totals multiplied together
paths_total <- paths_to_31*paths_to_63*paths_to_96*paths_to_118*paths_to_138*paths_to_186
paths_total
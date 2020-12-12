library(dplyr)
library(tidyverse)

# Read in the data
data <- read.delim('Day8_data.txt', sep=' ', header=FALSE)

# function to loop through instructions
findAcc <- function(data){
  data$V3 <- 0       # Set a new column full of 0
  rows <- nrow(data) # find how many rows in data
  row <- 1           # start row for loop
  while(row != rows){# while loop to keep going until we reach the end
    if (data$V3[row]!=0){  # check if the V3 entry is not 0 (ie did we visit here before)
      acc <- "FAIL"        # failure if we revisit a row (comment out this line for part 1)
      break                # break the while loop
    }
    instr <- data$V1[row]  # what is the instruction for this row?
    if (instr=="acc"){
      acc <- acc+as.integer(data$V2[row]) # if it is acc, add to the acc number
      data$V3[row] <- acc                 # change the V3 value for the row
      row <- row+1
    }
    else if (instr=="nop"){ # if it is nop
      data$V3[row] <- acc   # change the V3 for the row
      row <- row+1
    }
    else if (instr=="jmp"){ 
      data$V3[row] <- acc   # change the V3 for the row
      row <- row+as.integer(data$V2[row]) # jump to row as instructed
    }
  }
  return_val <- as.character(acc) # return the current value of acc
  return_val
}

# Now need to loop through, change each jmp to nop in turn and see if it passes

# first count how many jumps in data to start
tab <- table(data$V1)
total_jmp <- tab[names(tab)=="jmp"] 
jmp_num <- 1
row_num <- 1

while (jmp_num < total_jmp){
  instr <- data$V1[row_num]
  if (instr=="jmp"){           # if the instruction is jmp, change it to nop and see what happens
    data$V1[row_num] <- "nop"
    value <- findAcc(data)
    data$V1[row_num] <- "jmp"  # change it back to jmp for the next attempt
    if (value!="FAIL"){        # if we didn't fail, print the answer and exit the loop
      print(value)
      break
    }
    jmp_num <- jmp_num+1
  }
  row_num <- row_num+1
}
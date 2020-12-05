library(dplyr)
library(tidyverse)
library(stringr)

# load the data and get all lines into a list (not good for big files but ok here)
con <- file("Day4_data.txt", open="r")
data <- readLines(con)

# first step is to get the lines corresponding to a unique person on a single line
person <- 1
i <- 1
data_cleansed <- list() # list to hold each unique person
new_line <- paste0("person:",person) # start new person with an ID numbers
for(line in data){ # get the next line of data
  if (nchar(line)>0){ # if it is not a blank line
    new_line <- paste(new_line, line) # append it to the current person
  }
  else{ # if it is a blank line, it is the end of a person's info
    #print(new_line)
    data_cleansed[[person]] <- new_line # write the info to the list
    person <- person + 1                # increment the person ID
    new_line <- paste0("person:",person)# start a new person
  }
  if (i == length(data)){ # if it is the last line of the data
    #print(new_line)
    data_cleansed[[person]] <- new_line # write out the last person's info
  }
  i <- i+1
}

# Now data_cleansed is a list of all unique people from the input data
# Next step is to extract the items 

# empty data frame to append the people to
data_sorted <- data.frame(person=character(0), # data frame structure that I will rbind with
                          byr=character(0),
                          iyr=character(0),
                          eyr=character(0),
                          hgt=character(0),
                          hcl=character(0),
                          ecl=character(0),
                          pid=character(0),
                          cid=character(0))

parse_line <- function(line){
  data_row <- data.frame(person=NA, # data frame with 1 line of NA
                         byr=NA,    # values that exist will be replaced, NAs will help to establish missing data later
                         iyr=NA,
                         eyr=NA,
                         hgt=NA,
                         hcl=NA,
                         ecl=NA,
                         pid=NA,
                         cid=NA)
  line_pairs <- str_split(line, " ")
  for (i in line_pairs[[1]]){
    key = str_split(i, ":")[[1]][1] # split the key:value into respective parts
    val = str_split(i, ":")[[1]][2]
    if (key=="person"){             # read the various values to their correct keys in the data frame
      data_row$person <- val
    }
    else if (key=="byr"){
      data_row$byr <- val
    }
    else if (key=="iyr"){
      data_row$iyr <- val
    }
    else if (key=="eyr"){
      data_row$eyr <- val
    }
    else if (key=="hgt"){
      data_row$hgt <- val
    }
    else if (key=="hcl"){
      data_row$hcl <- val
    }
    else if (key=="ecl"){
      data_row$ecl <- val
    }
    else if (key=="pid"){
      data_row$pid <- val
    }
    else if (key=="cid"){
      data_row$cid <- val
    }
  } 
  data_row
}

### Now we can loop through all lines, parse them, and join the result to the main data frame
for (line in data_cleansed){                  # loop through all lines
  new_line <- parse_line(line)                # use the parse_line function on each line
  data_sorted <- rbind(data_sorted, new_line) # join the parsed line to the data frame
}

# Get rid of cid column which we do not care about, drop all rows with any NA
data_no_cid <- data_sorted %>% select(-cid) %>% drop_na()

nrow(data_no_cid) # answer to part 1

### Part 2 now requires us to validate the entries according to a set of rules
# The obvious solution is to add filters for all columns
# Alternatively we could write a second function with validation built-in

# We need a function to validate heights, as the rules are fairly complex
check_valid_height <- function(height){
  if (grepl("[cm]$",height)){ # regex looking for 'cm' at end of entry
    height <- as.integer(str_split(height, "cm")[[1]][1]) # get the height part
    if (height >= 150 & height <= 193){
      check <- TRUE # if the height matches our requirement in cm
    }
    else {
      check <- FALSE
    }
  }
  else if (grepl("[in]$",height)){ # same idea as above but for the inches case
    height <- as.integer(str_split(height, "in")[[1]][1])
    if (height >= 59 & height <= 76){
      check <- TRUE
    }
    else {
      check <- FALSE
    }
  }
  else{
    check <- FALSE
  }
}

### Rewrite the function from before but with built-in validation checks
parse_line_validate <- function(line){
  data_row <- data.frame(person=NA,
                         byr=NA,
                         iyr=NA,
                         eyr=NA,
                         hgt=NA,
                         hcl=NA,
                         ecl=NA,
                         pid=NA,
                         cid=NA)
  line_pairs <- str_split(line, " ")
  for (i in line_pairs[[1]]){
    key = str_split(i, ":")[[1]][1]
    val = str_split(i, ":")[[1]][2]
    if (key=="person"){
      data_row$person <- val
    }
    else if (key=="byr"){
      if (as.integer(val)>=1920 & as.integer(val)<=2002){ # birth year between 1920 and 2002
        data_row$byr <- val
      }
    }
    else if (key=="iyr"){
      if (as.integer(val)>=2010 & as.integer(val)<=2020){ # issue year between 2010 and 2020
        data_row$iyr <- val
      }
    }
    else if (key=="eyr"){
      if (as.integer(val)>=2020 & as.integer(val)<=2030){ # expiry year between 2020 and 2030
        data_row$eyr <- val
      }
    }
    else if (key=="hgt"){
      if (check_valid_height(val)){ # rules were complicated, so used a separate function to validate heights
        data_row$hgt <- val
      }
    }
    else if (key=="hcl"){
      if (grepl("^#[0-9a-f]{6}$",val)){ # regex for # followed by 6 numbers or letters a-f
        data_row$hcl <- val
      }
    }
    else if (key=="ecl"){
      if (val %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth")){ # check colour is in valid list
        data_row$ecl <- val
      }
    }
    else if (key=="pid"){
      if (nchar(val)==9 & grepl("^[0-9]+$",val)){ # regex for 9 consecutive numbers
        data_row$pid <- val
      }
    }
    else if (key=="cid"){ # no selection criteria needed
      data_row$cid <- val
    }
  } 
  data_row
}

# new blank data frame to store the results for part 2
data_sorted_validated <- data.frame(person=character(0),
                                    byr=character(0),
                                    iyr=character(0),
                                    eyr=character(0),
                                    hgt=character(0),
                                    hcl=character(0),
                                    ecl=character(0),
                                    pid=character(0),
                                    cid=character(0))

# loop through and find valid lines, parse them, read them out
for (line in data_cleansed){
  new_line <- parse_line_validate(line)
  data_sorted_validated <- rbind(data_sorted_validated, new_line)
}

# Get rid of cid column and drop rows with any NA
data_no_invalid <- data_sorted_validated %>% select(-cid) %>% drop_na()

nrow(data_no_invalid) # answer to part 2
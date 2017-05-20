shapes <- c("SquAre", "CirCle", "TriaNgle")
shapes

paste0("Square", "Circle", "Triangle")
paste0(shapes)
paste0(c("Square", "Circle", "Triangle"))

paste("My favorite shape is a", shapes)
paste(shapes, collapse = " ")

nchar("Supercalifragilisticexpialidocious")

tolower(shapes)
toupper(shapes)
str_to_title(shapes)

#################################################

# grepl() which stands for  "grep logical"

grepl("a.b", c("aaa", "aab", "abb", "acadb"))

# There ’s a dataset that comes with R called state.name
# which is a vector of Strings, one for
#  each state in the United States of America.
head(state.name)


# You can also specify exact numbers of expressions
# using curly brackets{ } .
#  "a{5}" specifies “ a exactly five times ”
#  "a{2,5}" specifies “ a between 2 and 5 times ”
#  "a{2,}" specifies “ a at least 2 times ”

# Does "Mississippi" contain between 2 adjacent "ss" ?
grepl("(ss){2}", "Mississippi")

# Does "Mississippi" contain the pattern of an "i" followed by 
# 2 of any character, with that pattern repeated three times adjacently?
grepl("(i.{2}){3}", "Mississippi")


# words("\\w") Words specify any letter, digit, or a underscore
#   ("\\W") not words
# digits("\\d") Digits specify the digits 0 through 9
#   ("\\D") not digits
# whitespace characters("\\s") Whitespace specifies line breaks, tabs, or spaces
#   ("\\S") not whitespace characters

grepl("\\d", "abcdefghijklmnopqrstuvwxyz")
grepl("\\D", "abcdefghijklmnopqrstuvwxyz")


# "\n" this regex for a new line
#  "\t" is the regex for a tab

grepl("\\s", "\n\t   ")


# You can also specify specific character sets using straight brackets[]
# For example a character set of just the vowels would look like:"[aeiou]"
# You can find the complement to a specific character by
# putting a carrot ^ after the first bracket.
# "[^aeiou]" matches all characters except the lowercase vowels.
# You can also specify ranges of characters using
# a hyphen - inside of the brackets.
# "[a-m]" matches all of the lowercase characters between a and m
# , while  "[5-8]" matches any digit between 5 and 8 inclusive.

grepl("[a-m]", "ABC")
grepl("[Aa]", "ABC")
grepl("[a-mA-M]", "ABC")

# Putting two backslashes before a punctuation mark that is
#  also a metacharacter indicates that you are looking for
#  the symbol and not the metacharacter meaning

grepl("\\+", "tragedy + time = humor")
grepl("\\.", "http://www.jhsph.edu/")

#  "^"  matching the beginning of a string
#  "$"  matching the end of a string

grepl("^a", c("bab", "aab"))
grepl("b$", c("bab", "aab"))
grepl("^[ab]+$", c("bab", "aab", "abc"))

#  "|"  matching the regex on the left or the right side of 
grepl("a|b", c("abc", "bcd", "cde"))
grepl("North|South", c("South Dakota", "North Carolina", "West Virginia"))


start_end_vowel <- "^[AEIOU]{1}.+[aeiou]{1}$"
vowel_state_lgl <- grepl(start_end_vowel, state.name)
head(vowel_state_lgl)

state.name[vowel_state_lgl]

# Metacharacter
# Meaning
# .
# Any Character
# \w
# A Word
# \W
# Not a Word
# \d
# A Digit
# \D
# Not a Digit
# \s
# Whitespace
# \S
# Not Whitespace
# [xyz]
# A Set of Characters
# [ ^ xyz]
# Negation of Set
# [a - z]
# A Range of Characters
# ^
# Beginning of String
# $
# End of String
# \n
# Newline
# +
# One or More of Previous
# *
# Zero or More of Previous
# ?
# Zero or One of Previous
# |
# Either the Previous or the Following
# { 5 }
# Exactly 5 of Previous
# { 2, 5 }
# Between 2 and 5 or Previous
# { 2, }
# More than 2 of Previous

##############################################################
sub("[Ii]", "1", c("Hawaii", "Illinois", "Kentucky"))
gsub("[Ii]", "1", c("Hawaii", "Illinois", "Kentucky"))

two_s <- state.name[grep("ss", state.name)]
two_s
strsplit(two_s, "ss")

##############################################################

head(state.name)
head(state.abb)

state_tbl <- paste(state.name, state.area, state.abb)
head(state_tbl)

find.package("tidyverse")
find.package("stringr")
.libPaths()

# install.packages("tidyverse", lib = "C:/Users/USER/Documents/R/win-library/3.3")

library(tidyverse)
library(stringr)

str_extract(state_tbl, "[0-9]+")
str_order(state.name)

str_pad("Thai", width = 8, side = "left", pad = "-")
str_pad("Thai", width = 8, side = "right", pad = "-")
str_pad("Thai", width = 8, side = "both", pad = "-")

##############################################################

# The Role of Physical Memory
find.package("pryr")
install.packages("pryr", lib = "C:/Users/USER/Documents/R/win-library/3.3")

library(pryr)
mem_used()

ls()
object.size(shapes)
object_size(shapes)

find.package("magrittr")
library(magrittr)

sapply(ls(), function(x) object_size(get(x))) %>% sort %>% tail(5)
rm(state_tbl)
mem_change(rm(shapes))

object.size(integer(0))    ## 40 bytes
object.size(integer(1000)) ## 40 bytes per integer
object.size(numeric(1000)) ## 80 bytes per numeric

str(.Machine)
gc() ## manual garbage collection ... to return memory

##############################################################

find.package("swirl")
install.packages("swirl", lib = "C:/Users/USER/Documents/R/win-library/3.3")

library(swirl)

install_course("The R Programming Environment")

swirl()

##############################################################

swirl::install_course("Advanced R Programming")
swirl::install_course("Regression Models")
swirl::install_course("Getting and Cleaning Data")
swirl::install_course("Exploratory Data Analysis")
swirl::install_course("Data Science and R")
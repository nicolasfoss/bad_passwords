
# Importing the tidyverse library
library(tidyverse)

# Loading in datasets/users.csv 
users <- read_csv("datasets/users.csv")

# Counting how many users we've got
users %>% select(user_name) %>% n_distinct()

# Taking a look at the 12 first users
head(users, n = 12)

library(testthat) 
library(IRkernel.testthat)
run_tests({
    test_that("Read in data correctly.", {
        expect_is(users, "tbl_df", 
            info = 'You should use read_csv (with an underscore) to read "datasets/users.csv" into users')
    })
    
    test_that("Read in data correctly.", {
        correct_users <- read_csv('datasets/users.csv')
        expect_equivalent(users, correct_users, 
            info = 'users should contain the data in "datasets/users.csv"')
    })
})

# Calculating the lengths of users' passwords
users$length <- users$password %>% str_length()

# Flagging the users with too short passwords
users$too_short <- if_else(users$length < 8, TRUE, FALSE)

# Counting the number of users with too short passwords
users %>% count(too_short)

# Taking a look at the 12 first rows
head(users, n = 12)

run_tests({
    test_that("The correct number of users are flagged", {
    sum(str_length(users$password) < 8)

    expect_equal(sum(str_length(users$password) < 8), sum(users$too_short), 
        info = "users$too_short should be a TRUE/FALSE column where all rows with passwords < 8 are TRUE.")
    })
})

# Reading in the top 10000 passwords
common_passwords <- read_lines("datasets/10_million_password_list_top_10000.txt")

# Taking a look at the top 100
head(common_passwords, n = 100)

run_tests({
    correct_common_passwords <- read_lines("datasets/10_million_password_list_top_10000.txt")
    test_that("the data read in is correct", {
    expect_equal(correct_common_passwords, common_passwords, 
        info = "datasets/10_million_password_list_top_10000.txt should be read in using read_lines and put into common_passwords.")
    })

})

# Flagging the users with passwords that are common passwords
users$common_password <- users$password %in% common_passwords

# Counting the number of users using common passwords
users %>% count(users$common_password)

# Taking a look at the 12 first rows
head(users, n = 12)

run_tests({
    test_that("the number of flagged passwords is correct", {
    expect_equal(sum(users$password %in% common_passwords), sum(users$common_password), 
        info = "users$common_password should be TRUE for each row with a password that is also in common_passwords.")
    })
})

# Reading in a list of the 10000 most common words
words <- read_lines("datasets/google-10000-english.txt")

# Flagging the users with passwords that are common words
users$common_word <- str_to_lower(users$password) %in% words

# Counting the number of users using common words as passwords
users %>% count(common_word)

# Taking a look at the 12 first rows
head(words, n = 12)

run_tests({
    correct_words <- read_lines("datasets/google-10000-english.txt") 
    test_that("google-10000-english.txt is read in correctly", {
        expect_equal(correct_words, words, 
            info = "datasets/google-10000-english.txt should be read in using read_lines and put into words.")
    })
    
    test_that("the number of flagged passwords is correct", {
        users$common_word <- str_to_lower(users$password) %in% words
        expect_equal(sum(users$common_word), sum(str_to_lower(users$password) %in% correct_words), 
            info = "users$common_word should be TRUE for each row with a password that is also in words.")
    })

})

# Extracting first and last names into their own columns
users$first_name <- str_extract(users$user_name, "^\\w+[^.](?=.)")
users$last_name <- str_extract(users$user_name, "(?<=.)\\w+[^.]$")

# Flagging the users with passwords that matches their names
users$uses_name <- if_else(users$password == users$first_name, TRUE,
                          if_else(users$password == users$last_name, TRUE, FALSE)
                          )

# Counting the number of users using names as passwords
users %>% count(uses_name)

# Taking a look at the 12 first rows
head(users, n = 12)

run_tests({
    correct_first_name <- str_extract(users$user_name, "^\\w+")
    correct_last_name <- str_extract(users$user_name, "\\w+$")

    # Flagging the users with passwords that matches their names
    correct_uses_name <- str_to_lower(users$password) == correct_first_name |
                         str_to_lower(users$password) == correct_last_name
    test_that("the number of flagged passwords is correct", {
        expect_equal(sum(correct_uses_name), sum( users$uses_name), 
            info = "users$uses_name should be TRUE for each row with a password which is also the first or last name.")
    })

})

# Splitting the passwords into vectors of single characters
split_passwords <- str_split(users$password, "")

# Picking out the max number of repeat characters for each password
users$max_repeats <- sapply(split_passwords, function(split_password) {
    rle_password <- rle(split_password)
    max(rle_password$lengths)
})


# Flagging the passwords with >= 4 repeats
users$too_many_repeats <- if_else(users$max_repeats >= 4, TRUE, FALSE)

# Taking a look at the users with too many repeats
users %>% filter(too_many_repeats == TRUE)

run_tests({
    correct_max_repeats <- sapply(users$password, function(password) {
        split_password <- str_split(password, "")[[1]]
        rle_password <- rle(split_password)
        max(rle_password$lengths)
    })

    test_that("the number of flagged passwords is correct", {
        expect_equal(sum(users$too_many_repeats), sum( users$max_repeats >= 4), 
            info = "users$too_many_repeats should be TRUE for each row with a password with 4 or more repeats.")
    })

})

# Flagging all passwords that are bad
users$bad_password <- if_else(users$too_short == TRUE | 
                              users$common_password == TRUE | 
                              users$common_word == TRUE | 
                              users$uses_name == TRUE | 
                              users$too_many_repeats == TRUE, TRUE, FALSE
                             )

# Counting the number of bad passwords
users %>% count(bad_password)

# Looking at the first 100 bad passwords
head(users %>% filter(bad_password == TRUE), n = 100)

run_tests({
    correct_bad_password <- users$too_short | users$common_word |
                      users$common_password | users$uses_name |
                      users$too_many_repeats
    test_that("all the bad passwords are flagged", {
    expect_equal(sum(correct_bad_password), sum(users$bad_password), 
        info = "All rows with passwords that should be flagged as bad should have users$bad_password set to TRUE.")
    })

})

# Enter a password that passes the NIST requirements
# PLEASE DO NOT USE AN EXISTING PASSWORD HERE
new_password <- "Randy_24_28_Starshine"

run_tests({
    temp_common_passwords <- read_lines("datasets/10_million_password_list_top_10000.txt")
    temp_words <- read_lines("datasets/google-10000-english.txt")

    is_bad <- str_length(new_password) < 8 |
        new_password %in% temp_common_passwords |
        str_to_lower(new_password) %in% temp_words |
        max(rle(str_split(new_password, "")[[1]])$lengths) >= 4

    test_that("", {
    expect_false(is_bad, 
        info = "This password does not fulfill the NIST requirements.")
    })

})

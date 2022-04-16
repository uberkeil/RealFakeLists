library(rvest)
library(tidyverse)
library(digest)

#Building list architecture upon which all outputs will be populated.
big_list <- list(url_list = list(),
                 html_list = list(),
                 table_list = list(),
                 temp_list = list(), 
                 email = tibble())

#Websites which we will be scraping for common first names, surnames, and email domains.
big_list$url_list <- list("https://www.ssa.gov/oact/babynames/decades/century.html", #ranked (commonality) list of male and female names
                          "https://forebears.io/new-zealand/surnames", #list of common nz surnames
                          "https://gist.github.com/ammarshah/f5c2624d767f91a7cbdc4e54db8dd0bf") #git db of email domain names

#Returning xml_document from each url
big_list$html_list <- map(big_list$url_list, read_html)

#A simple for loop to pull the table info from each url saving it back 
#as an item on our list.
for (i in seq_along(big_list$html_list)){
  big_list$table_list[[i]] <-
    big_list$html_list[[i]] %>%
    html_node("table") %>%
    html_table()
}

#Randomized first names n = 200 (M & F)
big_list$temp_list[[1]] <- #aka first names
  big_list$table_list[[1]] %>%
  filter(X1 %in% 1:100) %>% #pulling rows with ranked names only
  select(2,4) %>% #selecting columns with names only
  rename(F = 2, M = 1) %>% #Quick column name change to id m/f names
  pivot_longer(cols = c("M", "F"), names_to = "gender", values_to = "name") %>% #collapsing both names into a single column
  slice(sample(200, replace = TRUE)) %>% #Selecting 200 random names, allowing for multiple occances of a single name.
  select(name) %>% 
  mutate_all(tolower) %>%
  mutate(name = sub("$", ".", name), name) #adding a "." to the end of each name

#...Essentially repeating the process for surnames.
big_list$temp_list[[2]] <- #aka surnames
  big_list$table_list[[2]] %>%
  rename_with(tolower) %>%
  filter(rank %in% 1:999) %>%
  select(2) %>%
  slice(sample(nrow(big_list$temp_list[[1]]), replace = TRUE)) %>%
  mutate_all(tolower)

#...And again for email domain names. 
big_list$temp_list[[3]] <- #aka email domain.
  big_list$table_list[[3]] %>%
  rename(email = 2) %>%
  select(2) %>%
  slice(sample(nrow(big_list$temp_list[[1]]), replace = TRUE)) %>%
  mutate(email = sub("^", "@", email), email) #adding an "@" to the beginning of the domain name.

set.seed(5000)

big_list$email <- #final product, simulated list of email addresses
  big_list$temp_list[1:3] %>% #Benefit of saving previous outputs to a list, they can easily be called for the bind_col() function.
  bind_cols() %>%
  unite(email, everything(), sep="") %>% #uniting name, surname, and email df's into a single list of simulated email addresses. 
  mutate_all(funs(str_replace_all(., "'", ""))) %>% #Removing ' from all email addresses for greater realism*
  rowwise() %>%
  mutate(anonymail = digest(email, algo = "sha1"))

# Simulate Dataset


#[*] It seems there are quite a few things I could do in the interest of creating a real fake email list. namely: 
#     - Mix of different email name types (e.g. first_lastname@email.com, f.lastname@email.com, flastname_111@email.com, radicalnickname@email.com, etc)
#     - More diverse selection of first and last names.
#     - Domain shortlist (some of the domains on this list seem a bit fabricated). 
#     - Why stop at emails? Why not full name, age, address, and occupation?
#
# Will definitely attempt to address these considerations in a future version.  But for now, this does more or less what I need it to do :) 

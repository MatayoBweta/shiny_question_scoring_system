install.packages("duckdb")

install.packages("arrow")
install.packages("thematic")
install.packages("pivottabler")
install.packages("devtools")
devtools::install_github("stefanwilhelm/ShinyRatingInput")
devtools::install_github("MatayoBweta/activityinfo-R")
install.packages("formattable")
install.packages("bs4Dash")

library(bs4Dash)
library(thematic)
bs4DashGallery()

library(activityinfo)
library(janitor)
library(tidyverse)

activityInfoLogin(Sys.getenv('ACTIVITY_INFO_UN'),
                  Sys.getenv('ACTIVITY_INFO_TOKEN'),
                  savePassword = FALSE)

d <-  queryTable(
  Sys.getenv('QUESTIONS_FORM_ID'),
  "id" = "_id",
  "Question" = "question",
  "Description" = "question_description",
  "Active" = "active",
  truncate.strings = FALSE
)%>% janitor::clean_names() %>% mutate(question_ = question) %>% remove_rownames() %>% column_to_rownames(var = "question_") %>%  filter(active == "Yes")

glimpse(d)
d

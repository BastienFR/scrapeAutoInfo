library(rvest)
library(magrittr)
library(dplyr)

url <- "https://www.auto123.com/en/new-cars/search/nissan/frontier/"


webpage <- read_html(url)


text <- html_nodes(webpage, ".grid") %>% html_text
models <- strsplit(text[4], "20..") %>% lapply(strsplit, "Frontier ") %>% extract2(1)
models[1] <- NULL

years <- strsplit(text[4], "Frontier")[[1]] %>% .[grep(pattern="20..", .)] %>% stringr::str_sub(-4)

names(models) <- years
###

title_html <- html_nodes(webpage, ".grid__item")
text <- html_text(title_html)
html_nodes(title_html, ".grid__item")
html_attr(title_html, ".grid__item")


models_url <- html_nodes(webpage, ".grid__item") %>%
  html_nodes(".border_white") %>%
  html_nodes("a") %>%
  html_attr("href")

## Technical details:

tech <- read_html(paste0("https://www.auto123.com", models_url[1]))

dimension <- tech %>% html_nodes(xpath='//*[@id="dimensions"]/div[2]') %>% html_text() %>%
  gsub(pattern = " ", "", .) %>%
  strsplit("\n") %>%
  extract2(1) %>%
  head(-1) %>%
  matrix(nrow=12, byrow = T) %>%
  as.data.frame(stringsAsFactors=F) %>%
  select(-1,-3) %>%
  rename(measure = V2, value = V4) %>%
  mutate(unit = gsub("^[0-9]*", "", value),
         value = as.numeric(gsub("[a-zA-Z]*$", "", value)))

wheels <- tech %>% html_nodes(xpath='//*[@id="suspension"]/div[2]') %>% html_text() %>%
  gsub(pattern = " ", "", .) %>%
  strsplit("\n") %>%
  extract2(1) %>%
  head(-1) %>%
  extract(.!="") %>%
  matrix(ncol=2, byrow = T) %>%
  as.data.frame(stringsAsFactors=F) %>%
  rename(measure = V1, value = V2)

gaz <- tech %>% html_nodes(".col") %>% html_nodes(".fuel_type") %>% html_children() %>% html_text %>%
  as.data.frame() %>%
  setNames("v1") %>%
  mutate(value = as.numeric(unlist(stringi::stri_extract_all_regex(v1, "[[:digit:]]+\\.[[:digit:]]+"))),
         measure = stringi::stri_extract_all_regex(v1, "(?<=\\().*?(?=\\))"),
         unit="l/100km") %>%
  mutate(measure = paste("Gaz", measure)) %>% 
  select(-v1)

bind_rows(dimension, gaz)



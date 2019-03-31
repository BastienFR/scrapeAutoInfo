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

models_url <- html_nodes(webpage, ".grid__item") %>%
  html_nodes(".border_white") %>%
  html_nodes("a") %>%
  html_attr("href")

## Technical details:

extract_info_model <- function(mod_url){
  
  tech <- read_html(paste0("https://www.auto123.com", mod_url))
  
  dimension <- tech %>% html_nodes(xpath='//*[@id="dimensions"]/div[2]') %>% html_text() %>%
    gsub(pattern = " ", "", .) %>%
    strsplit("\n") %>%
    extract2(1) %>%
    extract(.!="") %>%
    head(-1) %>%
    matrix(nrow=length(.)/2, byrow = T) %>%
    as.data.frame(stringsAsFactors=F) %>%
    rename(measure = V1, value = V2) %>%
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
  
  
  num_data <- wheels %>% 
    filter(grepl('Wheel',measure)) %>%
    mutate(unit="inch",
           value=as.numeric(substr(value,1,2))) %>% 
    bind_rows(dimension) %>% 
    bind_rows(gaz)
  
  cat_data <- wheels
  
  list(num_data, cat_data)
}

qwqw <- extract_info_model(models_url[3])

qwqw[[1]] %>% 
  mutate(measure = paste0(measure, "_",unit)) %>% 
  mutate(model="temp") %>% 
  select(-unit) %>% 
  reshape2::dcast(model ~ measure)

###  pro-x4 problem

/html/body/div[4]/div[2]/div[1]/section[3]/div/div[2]/div[4]/select
tech %>% html_nodes(xpath='/html/body/div[4]/div[2]/div[1]/section[3]/div/div[2]/div[4]/select') %>% html_text()

tech %>% html_nodes("div.techspec_menu:nth-child(4) > select:nth-child(2)") %>% html_text() %>% 
  gsub(pattern = " ", "", .) 


html body div.main_content.grid.grid--full div.grid__item.content_section div.techspecs section.section.group div.col.span_12_of_12.gallery_section div.col.span_4_of_12 div.techspec_menu select.redirect_dropdown.input_select

submodel <- "/en/new-cars/technical-specs/nissan/frontier/2013/4wd-crew-cab/sv/"



fill_missing_models <- function(allmodels){

 

  rescanned_list <- lapply(allmodels, find_missing_models)
  
  rescanned_list %>% unlist() %>% unique %>% sort(decreasing = T)
  
}


fill_missing_models(allmodels = models_url)


brand_model <- c(nissan="frontier", ford="ranger")

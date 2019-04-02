###########
######
###
##
#    All functions for scraping car models info from www.123auto.com
##
###
######
###########



####  Find the missing models from initial list  
## Sometime, models are not showed on the main page and you have to check deeper in the
## tree to fin the url

find_missing_models <- function(submodel){  
  read_html(paste0("https://www.auto123.com", submodel)) %>% 
    html_nodes(".techspec_menu") %>%
    extract2(3) %>% 
    html_nodes(".redirect_dropdown") %>%
    html_children() %>% 
    html_attr("value")
}


#### List all model from search page

list_from_search <- function(webpage){
  models_url <- html_nodes(webpage, ".grid__item") %>%
    html_nodes(".border_white") %>%
    html_nodes("a") %>%
    html_attr("href")
  
  models_url
}

####  Find all models
# brand_model  = named character, the name being the brand and the model that actual
#                value (e.g. c(ford="focus", nissan="frontier")), all lower case.
# years        = numerical vector of the years wanted

find_all_urls <- function(brand_model, years){

  webpages <- brand_model %>% 
    sprintf("https://www.auto123.com/en/new-cars/search/%s/%s/", names(.), .) %>% 
    lapply(read_html)
  
  principal_models <- lapply(webpages, list_from_search)
  
  # filter by years
  principal_models <- sapply(principal_models, grep, pattern=paste(years, collapse = "|"), value = T)
  
  # in some case, some models are missing, we'll find them by diging deeper in the trees
  
  all_models <- lapply(principal_models, function(xx) sapply(xx, find_missing_models))
  
  all_models <- lapply(all_models, unlist) %>% 
    lapply(unname)
  
  return(all_models)
}


#### Extract the model info, one url at the the time


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
    mutate(value = gsub(",", "", value),
           unit = gsub("^[0-9]*", "", value),
           value = as.numeric(unlist(regmatches(value, gregexpr("^[0-9]+",value)))) )

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
    filter(grepl("13|14|15|16|17|18|19|20|21|22",value)) %>% 
    mutate(unit="inch",
           value=as.numeric(gsub(".*?([0-9]+).*", "", value))) %>% 
    bind_rows(dimension) %>% 
    bind_rows(gaz)

  cat_data <- wheels
  
  list(num_data, cat_data)
}


#### model specs as one line data.frame

spec_as_one_liner <- function(model_specs, model_url){
  

  numer <- model_specs[[1]] %>% 
    mutate(measure = paste0(measure, "_",unit)) %>% 
    mutate(temp_id="temp") %>% 
    select(-unit) %>% 
    filter(!is.na(value)) %>% 
    mutate(measure = make.unique(measure)) %>% 
    reshape2::dcast(temp_id ~ measure) %>% 
    select(-temp_id)
  
  categ <- model_specs[[2]] %>% 
    mutate(temp_id="temp") %>% 
    mutate(measure = make.unique(measure)) %>% 
    reshape2::dcast(temp_id ~ measure) %>% 
    select(-temp_id)
  
  meta <- strsplit(model_url, "/") %>%
    extract2(1) %>% 
    extract(-c(1:4)) %>% 
    t %>% 
    data.frame() %>% 
    setNames(c("brand", "model", "year", "submodel", "trim")) %>% 
    mutate_all(funs(as.character(.)))
  
  res <- data.frame(meta, numer, categ)
  
  return(res)
}


#### Extract all models info, and make the data.frame

extract_info_models <- function(all_urls){

  all_specs <- lapply(all_urls,extract_info_model)
  
  res <- mapply(spec_as_one_liner, all_specs, all_urls, SIMPLIFY = F) %>% 
    do.call(bind_rows, .)
 
  return(res)
}



### crap for testing
# for(i in 1:length(all_urls)){
#   print(i)
#   extract_info_model(mod_url = all_urls[i])
#   #print(warnings())
   #spec_as_one_liner(model_specs = all_specs[[i]], model_url = all_urls[i])
   
# }
# i=43


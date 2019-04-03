
###  Scrapping to help me chose my pickup:




## first load libraries:
library(rvest)
library(magrittr)
library(dplyr)




## Select the models you want scrapped:
interest <- c(nissan="frontier", toyota="tacoma", ford="ranger", ford="f-150", honda="ridgeline", chevrolet="colorado", gmc="canyon")

## I'll do model in a loop  in case some fails:
urls <- vector(mode = "list", length = length(interest)) %>% "names<-"(interest)
specs <- vector(mode = "list", length = length(interest)) %>% "names<-"(interest)
start.time <- Sys.time()
for(model in 1:length(interest)) {
  try(urls[[model]] <- find_all_urls(brand_model=interest[model], years = 2005:2019))
  try(specs[[model]] <- lapply(urls[[model]], extract_info_models))
}
Sys.time() - start.time

for(i in 1:length(urls[[model]])) extract_info_models(all_urls =  urls[[model]][[1]])

final_specs[,1:19]

model=7
model=4


str(specs,2)

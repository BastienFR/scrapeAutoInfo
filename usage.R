########
####
##     Usage of function to scrap auto info
####
########


## first load libraries:
library(rvest)
library(magrittr)
library(dplyr)


## Now select the models you want scrapped:
interest <- c(nissan="frontier", toyota="tacoma")

## Now use function `find_all_urls` to find the url associated to all models years
all_models_urls <- find_all_urls(brand_model=interest, years = 2005:2019)


## Now we can extract the technical information
ls_specs <- lapply(all_models_urls, extract_info_models)

## The previous result is a list, lets make it a data frame
final_specs <- do.call(bind_rows, ls_specs)



final_specs[,1:19]

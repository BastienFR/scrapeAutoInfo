
###  Scrapping to help me chose my pickup:




## first load libraries:
library(rvest)
library(magrittr)
library(dplyr)
library(reshape2)
library(ggplot2)




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

## need to fix the canyon
str(specs,2)

save(urls, specs, file="res_pickup_1.rdata")
load("res_pickup_1.rdata")

final_specs <- do.call(bind_rows, specs) %>% 
  mutate(propulsion = stringr::str_extract(submodel, "2wd|4wd|4x2|4x4"),
         propulsion = ifelse(propulsion=="4x4", "4wd", propulsion),
         propulsion = ifelse(propulsion=="4x2", "2wd", propulsion))



## gaz

gaz <- final_specs %>% 
  select(brand, model, year, submodel, trim, propulsion, contains("Gaz")) %>% 
  rename(Automatic_City = Gaz.Automatic.City_l.100km,
         Automatic_Highway = Gaz.Automatic.Highway_l.100km,
         Manual_City = Gaz.Manual.City_l.100km,
         Manual_Highway = Gaz.Manual.Highway_l.100km) %>% 
  #reshape(varying = 6:9, sep = "_", direction = 'long')
  melt(id.vars=c("brand", "model", "year", "submodel", "trim")) %>% 
  #mutate(transmission = strsplit(variable,"_"))
  tidyr::separate(variable, c("transmission", "route"), "_") %>% 
  mutate(year = as.numeric(year))



ggplot(gaz, aes(x=model, y=value, color=route)) + 
  geom_boxplot() 

ggplot(gaz, aes(x=model, y=value, color=transmission)) + 
  geom_boxplot() 


## dimension
dimen <- final_specs %>% 
  select(brand, model, year, submodel, trim, propulsion, Width_mm, Length_mm, CurbWeight_kg) 


ggplot(dimen, aes(x=Width_mm, y=CurbWeight_kg, color=model)) +
  geom_point()

ggplot(dimen, aes(x=Width_mm, y=Length_mm, color=model)) +
  geom_point()

dimen %>% 
  filter(model=="frontier") %>% 
  filter(propulsion=="4wd") %>% 
ggplot(aes(x=year, y=CurbWeight_kg, color=trim)) +
  geom_point()

## towing

tow <- final_specs %>% 
  select(brand, model, year, submodel, trim, propulsion, MaxTrailerWeight_kg) 

ggplot(tow, aes(x=model, y=MaxTrailerWeight_kg)) + 
  geom_boxplot() 

library(tidyverse)
library(testthat)
library(aocodeR)

#Code by Ephafrus 



#get session from chrome,  save to working Directory  
input <- aoc_get_input(day= 11, cookie_path = paste0(rprojroot::find_rstudio_root_file(),"session_cookie.txt")) 

hex_dist <- function(input, max_out = F) {
  steps <- input %>% strsplit(., ",") %>% unlist
  max_o <- 0
  loc <- c(0,0)
  for(move in steps){
    loc <- loc + switch(move,
                        "n" = c(1,0),
                        "ne" = c(0.5, 0.5),
                        "e" = c(0, 1),
                        "se" = c(-0.5, 0.5),
                        "s" = c(-1, 0),
                        "sw" = c(-0.5, -0.5),
                        "w" = c(0, -1),
                        "nw" = c(0.5, -0.5))
    max_o <- max(max_o, loc %>% abs %>% sum)
  }
  if(max_out){max_o}else{
    loc %>% abs %>% sum
  }
}  

#trial 608 


input %>% hex_dist 


input %>% hex_dist(max_out = T)

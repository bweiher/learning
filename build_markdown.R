
library(tidyverse)
library(glue)


list.files("links") %>% 
  str_subset("\\.URL$") -> urls


# read.csv(urls[1], stringsAsFactors = FALSE) %>% 
#   as_tibble()

links <- map_df(seq_along(urls), function(x){
  
  this_url <- urls[x]
  
  glue("links/{this_url}") %>% 
    read.csv(stringsAsFactors = FALSE) %>% 
    as_tibble() %>%
    setNames("col") %>% 
    filter(
      str_detect(col, "URL")
    ) %>% 
    mutate(
      col = str_replace_all(col, "^URL=", ""),
      x =  x
    ) 
  
  
})


data <- tibble(
  urls = urls
) %>% 
  mutate(
    x = row_number()
  ) %>% 
  inner_join(links, by = "x")




data %>% 
  mutate(
    urls = str_replace_all(urls, "\\.URL$", ""), #,
    output =  glue("* [{urls}]({col})")
  ) %>% 
  pull(output) -> markdown_links


c("## Learning \n",
  as.character(markdown_links)
) -> lines


writeLines(lines, "readme.MD")


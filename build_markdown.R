library(tidyverse)
library(glue)

urls <- paste0("links/", list.files("links") %>% 
  str_subset("\\.URL$"))


get_url <- function(url){
  read.csv(url, stringsAsFactors = FALSE) %>% 
  as_tibble() %>%
  setNames("col") %>% 
  filter(
    str_detect(col, "URL")
  ) %>% 
    mutate(col  = str_replace_all(col, "URL=", "")) %>% 
    pull(col)
}

get_url_safely <- safely(get_url, otherwise = NA_character_)

df <- map_df(urls, function(x){
    
    print(x)
  
    fs::file_info(x) %>% 
    as_tibble() %>% 
    mutate(type=as.character(type),
           path=as.character(path),
           permissions=as.character(permissions),
           size=as.character(size),
           url =  get_url_safely(x)$result
    )  

}) %>% 
  arrange(desc(modification_time))


markdown <- df %>%
  filter(!is.na(url)) %>% 
  transmute(name = str_replace_all(path, "^links/", ""), url) %>% 
  mutate(x = glue("* [{name}]({url})")) %>%
  pull(x)


c("## Learning \n",
  as.character(markdown)
) -> lines


writeLines(lines, "readme.MD")


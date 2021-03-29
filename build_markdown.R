library(tidyverse)
library(glue)

# ---  functions ----

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



# --- script --- 

dirs <- list.files('links')
dlist <- list()

md <- map(seq_along(dirs), function(g){

  
  dir <- dirs[g]
  path <- glue("links/{dir}/")
  urls <- list.files(path) %>% str_subset("\\.URL$")
  urls <- paste0(path,urls)
  
  
  links_df <- map_df(urls, function(x){
      fs::file_info(x) %>% 
      as_tibble() %>% 
      mutate(type=as.character(type),
             path=as.character(path),
             permissions=as.character(permissions),
             size=as.character(size),
             url =  get_url_safely(x)$result
      )  
    
  })
  

    links <- links_df %>% 
    arrange(desc(modification_time)) %>% 
    filter(!is.na(url)) %>% 
    select(path, url) %>% 
    transmute(
      name = str_replace_all(path, glue("^links/{dir}/"), ""), 
      url) %>% 
    mutate(
      name = str_remove_all(name, "\\.URL"),
      x = glue("* [{name}]({url})") %>% 
        str_remove_all('\\.URL')
      ) %>% 
    select(x) %>% 
    pull()
  
  
  c(
    "\n",
    paste0("### ", str_to_title(dir)),
    links
  )
  
})  
  

  
  
c("## Learning \n",
  unlist(md) 
) -> lines


writeLines(lines, "readme.MD")


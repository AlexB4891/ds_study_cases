library(tidyverse)
library(rvest)


# Vectors to search

newspapers <- c(
  "elcomercio",
  "eluniverso",
  "elmercurio",
  "larepublica"
)

subjects <- c(
  "covid",
  "campeonato",
  "espectaculo",
  "europa",
  "trump"
)

# List of searches:

search_list <- purrr::cross(list(newspapers,subjects)) %>% 
  map(~.x %>% reduce(c) %>% 
        str_c(.,collapse = "+") %>% 
        str_c("ecuador+",.,"+",Sys.Date() %>% format("%d %B %Y")) %>% 
        str_replace_all(" ","+"))


# Obtain all the news 

# Custom URL for 24 hours ago search in google:

cst_url <- "http://www.google.com/search?q="

# Create all the searches:


get_text <- function(url){
  
  page <- read_html(url)
  # 
  html_nodes(page,"a") %>% 
    invoke_map(
      list(text =function(x) html_text(x),
           url = function(x) html_attr(x,"href")
           ),
      x = .
    ) %>% 
    as_tibble %>% 
    filter(str_detect(url,".url.*" )) 
    
}

# Create a safely function to run over the list:

get_text_s <- safely(get_text)

# Extract the news URL:

news_list <- search_list %>% 
  map(~{
    url <- str_c(cst_url,.x)
    
    get_text_s(url) 
    
  })

# Add the topics to the tibble:

news_df <- news_list %>% 
  map("result") %>% 
  map2(search_list,~.x %>% mutate(query = .y)) %>% 
  reduce(bind_rows)  

# Some modifications over the URLS:

news <- newspapers %>% 
  map(~{
    
    expres <- str_c(".http*.*ww.*",.x,".*")
    
    news_df %>% 
        filter(str_detect(url,expres)) %>%
      mutate(class = .x,
             url = str_replace_all(url,".ur.{4}",""))
    
    }) 


# Each news papers has it's own transformation:

f_list <- list(
  function(x) x %>% 
    mutate(url= str_extract(url,".*.html")) %>% 
    filter(complete.cases(.)),
  function(x) x %>% 
    mutate(url= str_replace_all(url,"&.*","")) %>% 
    filter(str_detect(url,"noticias")),
  function(x) x %>% 
    mutate(url= str_replace_all(url,"/&.*",""),
           url = str_replace(url,"elmerom","elmercurio"),
           url = str_replace(url,"\\.ec","\\.com\\.ec")) %>% 
    filter(str_detect(url,"2020/05")),
  function(x) x %>% 
    mutate(url= str_replace_all(url,"/&.*","")) %>% 
    filter(str_detect(url,"2020/05"))
  
)

# Apply the functions:

final_list <-
  invoke_map(.x = news %>% map(list),
           .f = f_list) %>% 
  reduce(bind_rows)

# Method to scrap each newspaper as a class:

scrapp_ec_news <- function(x){
  UseMethod("scrapp_ec_news")
}



# El comercio: check

scrapp_ec_news.elcomercio <- function(x){
  y <- x %>% 
    unclass %>% 
    read_html
  
  list(
    content = y %>% 
      html_node(".paragraphs") %>% 
      html_nodes("p") %>% 
      html_text %>% 
      str_c(collapse = " "),
    title = y %>% 
      html_nodes(".title") %>% 
      html_node("h1") %>% 
      html_text %>% 
      na.omit %>% 
      str_c("",.)
  )
  
  
}



# El universo: check

scrapp_ec_news.eluniverso <- function(x){
  y <-  x %>% 
    unclass %>% 
    read_html 
  
  list(
    content = y %>% 
      html_node(".field-name-body") %>% 
      html_nodes("p") %>% 
      html_text %>% 
      str_c(collapse = " "),
    title = y %>% 
      html_node(".title-node") %>% 
      # html_nodes("p") %>% 
      html_text
  )
}


# El mercurio: check

scrapp_ec_news.elmercurio <- function(x){
  y <-  x %>% 
    unclass %>% 
    read_html
  
  list(content =  y %>% 
         html_node(".td-post-content") %>% 
         html_nodes("p") %>% 
         html_text %>% 
         str_c(collapse = " "),
       title = y %>% 
         html_node(".entry-title") %>% 
         html_text
       
  )
}



# La republica: check


scrapp_ec_news.larepublica <- function(x){
  y <- x %>% 
    unclass %>% 
    read_html 
  
 list(content =  y %>% 
    html_node(".mh-content") %>% 
    html_nodes("p") %>% 
    html_text %>% 
    str_c(collapse = " "),
    title = y %>% 
      html_node(".entry-title") %>% 
      html_text
   )
}

# Get all news:

complete <- final_list %>% 
  split(.$url) %>%  

  map(~{
    
    object <- structure(.x$url %>% unique  ,class = .x$class %>% unique)
    
    corpus <- safely(scrapp_ec_news)(object)
    
    list(.x[1,],
         corpus)
    
  })
  
# Final edition:

complete <- complete %>% transpose


corpus <- complete[[2]] %>% 
  map("result") %>% 
  map(as_tibble) 

meta <- complete[[1]]

# Join all the information in one tibble:

fl_tabl <- map2(corpus,meta,~{
  
  if(nrow(.y) != 0 & nrow(.x) != 0){
  
    bind_cols(.x,.y)
  
  }else{
      .x
    }
  }) %>% 
  reduce(bind_rows) %>% 
  filter(complete.cases(.),
         content != "") %>% 
  rowid_to_column


# Writing:

path <- "news_categorization/texts/"

if(!dir.exists("news_categorization/texts")){
 
   dir.create("news_categorization/texts")
  
  n_ant <-  0
  
} else{
 
   n_ant <- list.files(path) %>% 
    str_extract_all("[0-9]*") %>% 
    reduce(c) %>% 
    unique %>% 
    as.numeric %>% 
    max(.,na.rm = T)
}

# Separate all the information scrapped:



fl_tabl %>% 
  mutate(rowid = rowid + n_ant) %>% 
  split(.$rowid) %>% 
  iwalk(~{
    write_lines(.x$content,str_c(path,"article_",.y,".txt"))
    
    write_lines(.x$title,str_c(path,"title_",.y,".txt"))
    
    write_lines(.x$query,str_c(path,"topics_",.y,".txt"))
  })

# Conditional index of all news:

if(!file.exists("news_categorization/index.rds")){
  fl_tabl %>% 
    write_rds("news_categorization/index.rds")
}else{
  ant_tbl <- read_rds("news_categorization/index.rds")
  
  bind_rows(fl_tabl,ant_tbl) %>% 
    write_rds("news_categorization/index.rds")
}


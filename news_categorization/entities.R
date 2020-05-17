library(tidytext)
library(tm)
library(igraph)
library(kernlab)
library(kknn)
library(tidyverse)
library(Spectrum)

# Scrap text news
# I used `rvest` package check, in the repo for the script.
# source("scrap_script.R")

# Create a table with news info

path <- "news_categorization/texts/"

news_df <- list(text = "article.*txt",
     title = "title.*txt",
     topics = "topics.*txt") %>% 
  map(~list.files(path,full.names = T) %>% 
        str_subset(.x) %>% 
        map(~read_lines(.x) %>% str_c(collapse = " ")) %>% 
        unlist  
        ) %>% 
  as_tibble %>% 
  filter(!duplicated(title))

# Tokenize text:

token_text <- news_df %>% 
  rowid_to_column %>% 
  unnest_tokens(word,text) %>% 
  count(title,word,rowid) %>% 
  group_by(title) %>% 
  mutate(total = sum(n)) 

# Create a custim dictionary of stop words:

custom_stop_words <- bind_rows(stop_words,
                               tibble(word = tm::stopwords("spanish"),
                                          lexicon = "custom"),
                               tibble(word = tm::stopwords("spanish") %>% str_to_title,
                                          lexicon = "custom_2"))

# Extract sequence of words that starts with capital letters,
# those will be our entities:

entities_extract <- function(tabla,variable){
tabla %>% 
    rowid_to_column %>% 
  split(.$rowid) %>% 
  imap(~{
    .x %>% 
      select({{variable}}) %>% 
      unnest_tokens(word, {{variable}},to_lower = FALSE) %>%
      anti_join(custom_stop_words,sort = FALSE) %>%
      pull(word) %>%
      str_c(collapse = " ") %>%
      # chartr("áéíóúñ","aeioun",.) %>%
      str_extract_all("(?:\\s+[A-Z][\\w']*)*") %>% 
      unlist %>% 
      tibble(word = .) %>% 
      anti_join(custom_stop_words,sort = FALSE) %>% 
      mutate(id = .y)
    
  }) %>% 
    reduce(bind_rows)
}


# Entity extraction:

entities <- entities_extract(news_df,text) %>% 
  unique %>% 
  filter(word != "",
         nchar(word) != 1)

# Check for unique elements:

entities %>% 
  unnest_tokens(word,word) %>% 
  split(.$id) %>% 
  map_int(nrow)

entities <- entities %>% 
  unnest_tokens(word,word) %>% 
  split(.$id) %>% 
  map(~unique(.x)) %>% 
  reduce(bind_rows)

# The corpus of each news will be the entities in it:

comp_ent <- inner_join(token_text %>%
             mutate(rowid = as.character(rowid)),
           entities,by = c("word" = "word","rowid" = "id"))


# Carculate the term frequency table:

ft_mat <- comp_ent %>% 
  group_by(title,rowid,word,total) %>% 
  summarise(n = sum(n)) %>%
  ungroup %>% 
  anti_join(custom_stop_words) %>% 
  filter(!str_detect(word,"[:digit:]")) %>% 
  bind_tf_idf(word,title,n)

# Spread the title as columns, and the words as rows:

ft_mat <- ft_mat %>% 
  select(title,word,tf_idf) %>% 
  spread(title,tf_idf)

# Spectrum package needs a data.frame:

ft_mat <- ft_mat %>%
  ungroup %>% 
  group_by(word) %>% 
  summarise_if(is.numeric,sum,na.rm = T) %>% 
  as.data.frame

# Some adittional format:

rownames(ft_mat) <- ft_mat$word

ft_mat <- ft_mat %>% select(-word)

# Apply the Spectral clustering:
# Note that if the NN argument increases, each node connects 
# with more and more points giving less clusters if the content
# of the news is too similar:

res <- Spectrum(ft_mat,NN = 2,maxk = 10)

# Similarity plot:

sym_mat <- res[[4]] 

colnames(sym_mat) <- colnames(sym_mat) %>% str_sub(1,20)

rownames(sym_mat) <- rownames(sym_mat) %>% str_sub(1,20)
  
sym_mat %>%  graph_from_adjacency_matrix %>% plot

# Check the result of the clustering, using the topics used previously:

validation <- tibble(title = names(ft_mat),
       cluster = res[[1]]) %>% 
  inner_join(news_df) 



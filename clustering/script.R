
library(seqinr)
library(tidyverse)
library(broom)

#' Separate a vector given a length
#'
#' @param vector result from readinf a FASTA file with `read.fasta`
#' @param by length vector
#'
#' @return List with vector of lenth `by`
sep_vector <- function(vector,by){
  
  seq(by + 1,length(vector),by = by) %>% 
    map(~{
      
      vector[(.x-by):(.x-1)] 
      
    }) 
  
}



#' Take a list of vectors and fragment them in even smaller pieces
#' 
#' Separate the vectors inside a list, collapse the fragments, and count the
#' frequency of the resulting character 
#'
#' @param length of the smaller fraction
#' @param ... argumentos for `sep_vector` function
#'
#' @return Table of frequency for each element of the list and the fragments
subs_freq <- function(length, ...){
  sep_vector(...) %>% 
    # map(length)
    map(~sep_vector(.x,length) %>% 
          map(str_c,collapse = "") %>% 
          unlist %>% 
          table %>% 
          enframe %>% 
          spread(name,value)
    ) %>% 
    reduce(bind_rows)
}

# PCA

nested_pca <- string_separated %>% 
  map(~.x %>% 
        mutate_all(~replace_na(.,0)) %>% 
        nest() %>%
        mutate(pca = map(data,~prcomp(.x,scale. = TRUE,center = TRUE)))
      
  )

# Information of PCA:

performance <- nested_pca %>% 
  map(~{
    .x$pca %>% 
      map(~tidy(.x,"pcs")) %>% 
      as.data.frame
  })


# Plot of information of PCA:

map2(1:4,performance,~.y %>% mutate(length = str_c("N = ",.x))) %>% 
  reduce(bind_rows) %>% 
  gather("var","value",-PC,-length) %>% 
  ggplot(aes(x = PC,y = value,fill = var)) +
  geom_bar(stat = "identity",position = "dodge") + 
  facet_grid(.~length,scales = "free")

# Get the components:

components <- nested_pca %>% 
  map(~.x %>% 
        
        mutate(pca_aug = map2(pca,data,~augment(.x,.y))) %>% 
        unnest(pca_aug) %>% 
        select(matches("PC"))
        )

# Plot the first two components for each length:

map2(1:4,components,~.y %>% mutate(length = str_c("N = ",.x)) %>% select(length,.fittedPC1, .fittedPC2)) %>% 
  reduce(bind_rows) %>% 
  ggplot(aes(x =.fittedPC1,y = .fittedPC2, color = length )) +
  geom_point(size = 0.2)+
  facet_wrap(length~.,nrow = 2,ncol = 2,scales = "free")

# Clustering and plotting:

cluster <- components[[3]] %>% 
  select(2,3) %>%
  nest(data = everything()) %>% 
  mutate(kmeans = map(data,~kmeans(.x,7)),
         points = map2(data,kmeans,~augment(.y,.x)) # En augment va primero kmeans, leugo data
         ) %>% 
  unnest(points) 

cluster  %>% 
  ggplot(aes(x = .fittedPC1, y =.fittedPC2, color = factor(.cluster))) +
  geom_point()
  
  

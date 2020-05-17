
faces[[1]] %>% 
  image_quantize(colorspace = "rgb") %>% 
  image_data



faces[[1]] %>% 
  image_data("rgb") %>% 
  apply(2:3, paste, collapse= "") %>% 
  apply(1:2,function(x)paste0("#",x) %>% col2rgb(x))
# %>% 
#   col2rgb("#ffffff")

# face_2 <- 
  # 
all <- faces %>% 
  map(~.x %>% 
        image_convert(type = 'grayscale') %>% 
        image_raster( frame = 1, tidy = TRUE) %>% 
        spread(y,col) %>% 
        dplyr::select(-x) %>% 
        map_df(~col2rgb(.x)[1,]) 
        
  ) 


all <- all %>% 
  map(~{
    
    wide <- .x %>% 
        rowid_to_column %>% 
        split(.$rowid) %>% 
        map(~.x %>% dplyr::select(-rowid))%>%
        reduce(cbind)
    
    
    names(wide) <- str_c("var_",1:90000)
    
    return(wide )
    
    
    })

all_3 <- all %>% 
  reduce(bind_rows) %>% 
  dim
# %>% 
  # reduce(bind_rows)
  


all %>%
  slice(1:3) %>% 
  nest()
mutate(pca = map(data,~prcomp(.x,scale. = TRUE,center = TRUE)))
# 
# faces[[1]] 
#   mutate(col = str_sub(col,1,7),
#          col2 = map(col,~.x %>% col2rgb() %>% t() %>% as_tibble))  %>%
#   as_tibble %>% 
#   unnest(col2)
# # )
#   
# face_2 %>% 
#   group_by(red,green,blue) %>% tally %>% View
# 
# pixels <- face_2 %>% 
#   imap(~.x %>% 
#         dplyr::select(x,y,red) %>% 
#          mutate(photo_id = .y)
#         ) %>% 
#   reduce(bind_rows) %>% 
#   dplyr::select(red,photo_id) %>% 
#   mutate(pix = rep(1:90000,14)) %>% 
#   spread(key = pix,value = red)
#   
# 
# center <- pixels %>% 
#   dplyr::select(-photo_id) %>% 
#   as.matrix %>% 
#   scale(center = T,scale = T)
# 
# sigma <- center%*%t(center) / (nrow(center)-1)
# 
# eig <- eigen(sigma)
# 
# 
# eigenvalues  <- eig$values
# eigenvectors <- eig$vectors
# 
# 
# prop.var <- eigenvalues / sum(eigenvalues)
# cum.var  <- cumsum(eigenvalues) / sum(eigenvalues)
# thres    <- min(which(cum.var > .95))
# 
# 
# scaling    <- diag(eigenvalues[1:thres]^(-1/2)) / (sqrt(nrow(center)-1))
# 
# eigenfaces <- t(center[1,])%*%eigenvectors[1,1:thres]%*%scaling[1,]
# 
# eigenface <- array(t(eigenfaces[,6]), c(300,300))
# 
# a <- pixels %>% slice(1) %>% dplyr::select(-photo_id) %>% as.matrix 
# 
# 
# b = as.array(t(a),c(300,300))
# 
# imageShow(b)
# 
# 
# data.new <- data.frame(labels = c(1,14),
#                        x = t(t(eigenfaces)%*%t(center)))
# 
# projection <- data.frame(labels = c(1,14),
#                          x = t(t(eigenfaces)%*%t(scale(center[,c(2:ncol(center))], 
#                                                        center = mean.face, 
#                                                        scale = std.face))))
# 
# dev.off()

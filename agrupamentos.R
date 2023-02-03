########## AGRUPAMENTO ###############
library(ggplot2)
library(tidyverse)
####################################
df.agrup.completo <- df3
df.agrup <- df3[,c('depressao','ansiedade','estresse')]
dist.df <- df.agrup |> dist()
df.agrup[,c('depressao','ansiedade','estresse')]  |> plot()

######### Teste quantidade de grupos no k-means ##################
#install.packages('factoextra')
factoextra :: fviz_nbclust(df.agrup ,
                           kmeans ,
                             method = "wss") +
  geom_vline( xintercept = 5, linetype = 2) +
  geom_vline( xintercept = 3, linetype = 2) +
  labs(x = "Numero de Grupos",
          y = "Variancia Total")
set.seed (666)
k_medias1 <- kmeans(df.agrup ,
                     centers = 5)
k_medias2 <- kmeans(df.agrup,
                    centers = 3)
####### k medoides #################

factoextra::fviz_nbclust(df.agrup,
                         cluster::pam,
                         method ='wss') +
  geom_vline(xintercept = 3,linetype = 2) +
  geom_vline(xintercept = 4, linetype = 2) +
  geom_vline(xintercept = 5,linetype = 2) +
  labs(x = "Numero de Grupos",
       y = "Variancia Total Intragrupo",
       title = "PAM")
factoextra :: fviz_nbclust(df.agrup ,
                            cluster ::clara ,
                            method = "wss") +
 geom_vline( xintercept = 5, linetype = 2) +
  geom_vline( xintercept = 3, linetype = 2) +
   labs(x = "Numero de Grupos",
    y = "Variancia Total Intragrupo",
      title = "CLARA")
####################################
pam1<- cluster :: pam(df.agrup ,
                       k = 3)
pam2<- cluster :: pam(df.agrup ,
                      k = 4)
pam3<- cluster :: pam(df.agrup ,
                      k = 5)
clara1 <- cluster :: clara(df.agrup ,
                              k = 3,
                              samples = 10)

clara2 <- cluster :: clara(df.agrup ,
                           k = 5,
                           samples = 10)
###############################################
################### HIERARQUICOS ###############
library(ggdendro)
grup.dist <- dist(df.agrup)
agl_ward <- hclust(grup.dist,method = "ward.D2")

 plot(cut(as.dendrogram (agl_ward), h = 20)$upper ,
        main = "Ward - cortado em H = 20")

 agl_ward_res <- cutree(agl_ward , k = 3:5)
################################################
# agl_single <-
#    hclust(grup.dist , method = "single")
#
#  plot(cut(as.dendrogram (agl_single), h = 4)$upper ,
#          main = "Vizinho mais P r x i m o - cortado em H = 4",
#          xlab = "")
#   agl_single_res <- cutree(agl_single , k = 3:8)
 #############################################
agl_complete <-
 hclust(grup.dist , method = "complete")

plot(cut(as.dendrogram (agl_complete), h = 10)$upper ,
main = "Vizinho mais Distante - cortado em H = 10")
agl_complete_res <- cutree(agl_complete , k = 2:5)

 ##############################################
 ##############################################
 df.agrup.completo <- k_medias1 |>
   broom::augment(df.agrup.completo) |>
   rename_at(vars(starts_with('.')), funs(paste0('kmedias5')))
 df.agrup.completo <- k_medias2 |>
   broom::augment(df.agrup.completo) |>
   rename_at(vars(starts_with('.')), funs(paste0('kmedias3')))
 df.agrup.completo <- pam1 |>
   broom::augment(df.agrup.completo) |>
   rename_at(vars(starts_with('.')), funs(paste0('pam3')))
 df.agrup.completo <- pam2 |>
   broom::augment(df.agrup.completo) |>
   rename_at(vars(starts_with('.')), funs(paste0('pam4')))
 df.agrup.completo <- pam3 |>
   broom::augment(df.agrup.completo) |>
   rename_at(vars(starts_with('.')), funs(paste0('pam5')))
 df.agrup.completo$clara3 <- clara1$clustering
 df.agrup.completo$clara5 <- clara2$clustering
 df.agrup.completo$kmedias5 <- k_medias1$cluster
############################################
######## selecao melhor metodo #################
df.agrup.completo |> head()
df.agrup.completo |> colnames()

#davies bouldin ##########

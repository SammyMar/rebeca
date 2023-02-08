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

################################################
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
df.agrup
dados_diss <- dist(df.agrup,method = 'euclidian')
# PAM 3
pam_index_3 <- data.frame(
metodo = "pam3",
 db_index_cent = clusterSim::index.DB(df.agrup , pam1$clustering )$DB ,
  db_index_med = clusterSim::index.DB(
    df.agrup ,
    pam1$clustering ,
     d = dados_diss ,
     centrotypes = "medoids"
     )$DB ,
   dunn_index = clValid::dunn(distance = dados_diss , pam1$clustering ),
   silh_index = clusterSim::index.S(dados_diss , pam1$clustering ),
   ch_index_cent = clusterSim::index.G1(df.agrup , pam1$clustering ),
   ch_index_med = clusterSim::index.G1(df.agrup , pam1$clustering , d = dados_diss ,
                                        centrotypes = "medoids")
   )
# PAM 4
pam_index_4 <- data.frame(
  metodo = "pam4",
  db_index_cent = clusterSim::index.DB(df.agrup , pam1$clustering )$DB ,
  db_index_med = clusterSim::index.DB(
    df.agrup ,
    pam1$clustering ,
    d = dados_diss ,
    centrotypes = "medoids"
  )$DB ,
  dunn_index = clValid::dunn(distance = dados_diss , pam1$clustering ),
  silh_index = clusterSim::index.S(dados_diss , pam2$clustering ),
  ch_index_cent = clusterSim::index.G1(df.agrup , pam2$clustering ),
  ch_index_med = clusterSim::index.G1(df.agrup , pam2$clustering , d = dados_diss ,
                                      centrotypes = "medoids")
)
# PAM 5
pam_index_5 <- data.frame(
  metodo = "pam5",
  db_index_cent = clusterSim::index.DB(df.agrup , pam1$clustering )$DB ,
  db_index_med = clusterSim::index.DB(
    df.agrup ,
    pam1$clustering ,
    d = dados_diss ,
    centrotypes = "medoids"
  )$DB ,
  dunn_index = clValid::dunn(distance = dados_diss , pam1$clustering ),
  silh_index = clusterSim::index.S(dados_diss , pam3$clustering ),
  ch_index_cent = clusterSim::index.G1(df.agrup , pam3$clustering ),
  ch_index_med = clusterSim::index.G1(df.agrup , pam3$clustering , d = dados_diss ,
                                      centrotypes = "medoids")
)
# CLARA 3
clara_3_index <- data.frame(
   metodo = "clara3",
  db_index_cent = clusterSim::index.DB(df.agrup , clara1$clustering )$DB ,
  db_index_med = clusterSim::index.DB(
    df.agrup ,
    clara1$clustering ,
     d = dados_diss ,
     centrotypes = "medoids"
    )$DB ,
  dunn_index = clValid::dunn(distance = dados_diss , clara1$clustering ),
   silh_index = clusterSim::index.S(dados_diss , clara1$clustering ),
  ch_index_cent = clusterSim::index.G1(df.agrup , clara1$clustering ),
  ch_index_med = clusterSim::index.G1(df.agrup , clara1$clustering , d =
                                             dados_diss , centrotypes = "medoids") )


# CLARA 5
clara_5_index <- data.frame(
  metodo = "clara5",
  db_index_cent = clusterSim::index.DB(df.agrup , clara2$clustering )$DB ,
  db_index_med = clusterSim::index.DB(
    df.agrup ,
    clara2$clustering ,
    d = dados_diss ,
    centrotypes = "medoids"
  )$DB ,
  dunn_index = clValid::dunn(distance = dados_diss , clara2$clustering ),
  silh_index = clusterSim::index.S(dados_diss , clara2$clustering ),
  ch_index_cent = clusterSim::index.G1(df.agrup , clara2$clustering ),
  ch_index_med = clusterSim::index.G1(df.agrup , clara2$clustering , d =
                                        dados_diss , centrotypes = "medoids") )
# KMEDIAS 5
k_medias5_index <- data.frame(
  metodo = "k_medias5",
  db_index_cent = clusterSim::index.DB(df.agrup , k_medias1$cluster)$DB ,
  db_index_med = clusterSim::index.DB(
    df.agrup ,
   k_medias1$cluster ,
   d = dados_diss ,
     centrotypes = "medoids"
     )$DB ,
   dunn_index = clValid::dunn(distance = dados_diss , k_medias1$cluster),
   silh_index = clusterSim::index.S(dados_diss , k_medias1$cluster),
   ch_index_cent = clusterSim::index.G1(df.agrup , k_medias1$cluster),
   ch_index_med = clusterSim::index.G1(df.agrup , k_medias1$cluster , d =
                                             dados_diss , centrotypes = "medoids"))

# KMEDIAS 3
k_medias3_index <- data.frame(
  metodo = "k_medias3",
  db_index_cent = clusterSim::index.DB(df.agrup , k_medias2$cluster)$DB ,
  db_index_med = clusterSim::index.DB(
    df.agrup ,
    k_medias2$cluster ,
    d = dados_diss ,
    centrotypes = "medoids"
  )$DB ,
  dunn_index = clValid::dunn(distance = dados_diss , k_medias2$cluster),
  silh_index = clusterSim::index.S(dados_diss , k_medias2$cluster),
  ch_index_cent = clusterSim::index.G1(df.agrup , k_medias2$cluster),
  ch_index_med = clusterSim::index.G1(df.agrup , k_medias2$cluster , d =
                                        dados_diss , centrotypes = "medoids"))

tabela1 <- rbind(pam_index_3,pam_index_4,
                 pam_index_5,clara_3_index,clara_5_index,
                 k_medias3_index,k_medias5_index)

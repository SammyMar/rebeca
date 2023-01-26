########## AGRUPAMENTO ###############
library(ggplot2)
library(tidyverse)
####################################
df.agrup.completo <- df3
df.agrup <- df3[,c('depressao','ansiedade','estresse')]
dist.df <- df.agrup |> dist()
df.agrup[,c('depressao','ansiedade','estresse')]  |> plot()
######### hierarquicos ################################

h.fit.WD <- hclust(dist.df,method ='ward.D')
h.fit.WD2 <- hclust(dist.df,method ='ward.D2')
h.fit.S <- hclust(dist.df,method ='single')
h.fit.CO <- hclust(dist.df,method ='complete')
h.fit.A <- hclust(dist.df,method ='average')
h.fit.MC <- hclust(dist.df,method ='mcquitty')
h.fit.ME <- hclust(dist.df,method ='median')
h.fit.CE <- hclust(dist.df,method ='centroid')
plot(cut(as.dendrogram(h.fit.WD), h = 20)$upper ,
      main = "Ward - cortado em H = 20")
agl_ward_res <- cutree(h.fit.WD , k = 3:5)

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

df.agrup.completo |>
  group_by(kmedias5) |>
  summarise_at(vars('depressao','ansiedade','estresse'),mean)
df.agrup.completo |>
  filter(kmedias5 == 1) |> ggplot()+
  aes(quantidade) +
  geom_bar()
par(mfrow = c(3,2))

df.agrup.completo |> ggplot()+
    aes(quantidade) +
    geom_bar() + facet_wrap(~k_medias5)
df.agrup.completo |> esquisse::esquisser()


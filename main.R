########## bibliotecas ##############

library(readxl)
library(dplyr)
library(esquisse)
library(ggplot2)
library(devtools)

install_github('brsantos/baquantreg')
########## importacao ###############

REBECA <- read_excel("dados/Correção DASS-21 (arquivo das salas) - REBECA.xlsx")
AIRYS <- read_excel("dados/Correção DASS-21 AIRYS E BIA.xlsx")

# CORRECAO REBECA
REBECA |> colnames()
df1 <- REBECA |>
  select(-starts_with("..."))
df1$DEPRESSÃO <- NULL
df1 <- df1 |>
  mutate(
    DNIVEL = case_when(
      (Depressão %in% 0:9) ~ 'Normal',
      (Depressão %in% 10:12) ~ 'Leve',
      (Depressão %in% 13:20) ~ 'Moderada',
      (Depressão %in% 21:27) ~ 'Severa',
      (Depressão %in% 28:42) ~ 'Extremo Severo'
    ),
    ANIVEL = case_when(
      (Ansiedade %in% 0:6) ~ 'Normal',
      (Ansiedade %in% 7:9) ~ 'Leve',
      (Ansiedade %in% 10:14) ~ 'Moderada',
      (Ansiedade %in% 15:19) ~ 'Severa',
      (Ansiedade %in% 20:42) ~ 'Extremo Severo'
    ),
    ENIVEL = case_when(
      (Estresse %in% 0:10) ~ 'Normal',
      (Estresse %in% 11:18) ~ 'Leve',
      (Estresse %in% 19:26) ~ 'Moderada',
      (Estresse %in% 27:34) ~ 'Severa',
      (Estresse %in% 35:42) ~ 'Extremo Severo'
  )
)

# CORECAO AIRYS

AIRYS |> colnames()
df2 <- AIRYS |>
  select(-starts_with("..."))
df2 <- df2 |>
  mutate(
    DNIVEL = case_when(
      (Depressão %in% 0:9) ~ 'Normal',
      (Depressão %in% 10:12) ~ 'Leve',
      (Depressão %in% 13:20) ~ 'Moderada',
      (Depressão %in% 21:27) ~ 'Severa',
      (Depressão %in% 28:42) ~ 'Extremo Severo'
    ),
    ANIVEL = case_when(
      (Ansiedade %in% 0:6) ~ 'Normal',
      (Ansiedade %in% 7:9) ~ 'Leve',
      (Ansiedade %in% 10:14) ~ 'Moderada',
      (Ansiedade %in% 15:19) ~ 'Severa',
      (Ansiedade %in% 20:42) ~ 'Extremo Severo'
    ),
    ENIVEL = case_when(
      (Estresse %in% 0:10) ~ 'Normal',
      (Estresse %in% 11:18) ~ 'Leve',
      (Estresse %in% 19:26) ~ 'Moderada',
      (Estresse %in% 27:34) ~ 'Severa',
      (Estresse %in% 35:42) ~ 'Extremo Severo'
    )
)

### EXPLORATORIA ######################

#algumas alteracoes
df3 <- rbind(df1,df2)
df3 <- df3[-1,]
df3$Período <- df3$Período |>
  as.factor()
df3$Cor <- df3$Cor |>
  stringr::str_to_upper() |>
  gsub(pattern = '.{1}$', replacement = 'O')

df3 <- data.frame(lapply(df3, function(v) {
  if (is.character(v)) return(toupper(v))
  else return(v)
})) |> as_tibble()

df3$Quantidade <-  df3$Quantidade |>
  gsub(pattern = 'MODERADAMENTE',replacement ='MODERADO')

df3 <- df3 |>
  filter(!if_all(everything(), ~  is.na(.)))
df <- data.frame(Name=c('John Smith', 'John Smith', 'Jeff Smith'),
                 State=c('MI','WI','WI'), stringsAsFactors=F)
df3 <- within(df3, {
  Frequência[is.na(Frequência) == T & Alcool.drogas == 'NÃO'] <- 'NUNCA'
  Quantidade[is.na(Quantidade) == T & Frequência == 'NUNCA'] <- 'NENHUM'
  Cargo.de.Ocupação[Trabalho == 'NÃO'] <- 'DESEMPREGADO'
  })
df3$NA.count <-rowSums(is.na(df3))
df3 <- cbind(ID = 1:nrow(df3),df3) |> as_tibble()

#DETECTAR IMPLAUSIBILIDADES E INCONSISTENCIAS


implau <- df3 |>
  filter(Idade < 18 | Idade > 50)
implau <- rbind(df3 |>
  filter(Alcool.drogas == 'NÃO') |>
  filter(Frequência != 'NUNCA'), implau)
incom <- df3 |>
  filter(is.na(Depressão) == T | is.na(Ansiedade) == T| is.na(Estresse) == T)

df3 |> dplyr::filter(is.na(Cor)) |> ggplot() + aes(Gênero) + geom_bar()
df3 |> ggplot(aes(Turno)) + geom_bar()
df3 |> skimr::skim()
  df3 <- janitor::clean_names(df3)
##################### RESOLUCAO NA'S ##############################
df3 <- df3 |>
  filter(!is.na(df3$Depressão))
df3 |>
  filter(is.na(mudanca_p_vix))



# pkgbuild::check_build_tools(debug = TRUE)
# install.packages('Rcpp')


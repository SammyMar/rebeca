########## bibliotecas ##############

library(readxl)
library(dplyr)
library(esquisse)
library(ggplot2)

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
df3[,c('1° Curso',"Deficiência",'Trabalho',"Mudança p/ vix",)] <- df3$`1° Curso` |>
  stringr::str_to_upper()
df3 |> esquisser()
df3$`Mudança p/ vix`





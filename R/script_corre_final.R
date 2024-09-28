#########################################################################################################
dir.create(file.path("~/diretorio_r/corre", "planilhas"))
dir.create(file.path("~/diretorio_r/corre", "bancos"))

#########################################################################################################
setwd(file.path("~/corre/bancos"))#configurar diretorio
#########################################################################################################
# importando os bancos

painel <- read.csv("painel.csv",header=TRUE, sep="|", dec=".", encoding = "UTF-8", skip = 0) ##Lendo arquivo de texto separado por vírgulas (CSV) e que usa o ponto.
banco_semiliberdade <- read.csv("semiliberdade.csv",header=TRUE, sep="|", dec=".", encoding = "UTF-8", skip = 0) ##Lendo arquivo de texto separado por vírgulas (CSV) e que usa o ponto.
banco_internacao <- read.csv("internacao.csv",header=TRUE, sep="|", dec=".", encoding = "UTF-8", skip = 0) ##Lendo arquivo de texto separado por vírgulas (CSV) e que usa o ponto.

#########################################################################################################

# acertando os nomes das variáveis de todos o bancos

painel =
  painel %>%
  clean_names()


banco_semiliberdade =
  banco_semiliberdade %>%
  clean_names()

banco_internacao =
  banco_internacao %>%
  clean_names()


#################################################################################################################
#TRATAMENTO banco_semiliberdade
#################################################################################################################


# formatando campos data_da_atividade e hora_da_atividade:

banco_semiliberdade$data_da_atividade = as.Date(banco_semiliberdade$data_da_atividade)

banco_semiliberdade$hora_da_atividade = strptime(banco_semiliberdade$hora_da_atividade, format = "%H:%M")

banco_semiliberdade$hora_da_atividade <- format(banco_semiliberdade$hora_da_atividade, "%H:%M")


# Separar letras
banco_semiliberdade$texto <- gsub("[0-9]", "", banco_semiliberdade$nomes_e_id_s_dos_jovens_indicados_somente_iniciais)

# Separar números
banco_semiliberdade$numero <- gsub("[^0-9]", "", banco_semiliberdade$nomes_e_id_s_dos_jovens_indicados_somente_iniciais)


# funcão para separar
insert_separator <- function(string, n, separator) {
  gsub(paste0("(.{", n, "})"), paste0("\\1", separator), string)
}

# usando a função
banco_semiliberdade$numero1 <- insert_separator(banco_semiliberdade$numero, 5, "-")

# apagando o último caracter (-) do campo

banco_semiliberdade$id <- sub("\\-$", "", banco_semiliberdade$numero1)

# preencher campos vazios com NA
banco_semiliberdade$id[banco_semiliberdade$id == ""] <- NA
banco_semiliberdade_bkp = banco_semiliberdade

#excuindo colunas desnecessárias e salvar

banco_semiliberdade1 <- banco_semiliberdade %>% select(-texto,-numero, -numero1)

#########################################################################################################
setwd(file.path("~/corre/planilhas"))#configurar diretorio
#########################################################################################################
write.table(banco_semiliberdade1, "banco_semiliberdadeliberdade_COM_na.csv", sep = ",", row.names = FALSE, col.names = TRUE, quote = TRUE)
#write.xlsx(banco_semiliberdade, "banco_semiliberdadeliberdade_COM_na.ods", fileType = "ods")


# excluir linhas com NA coluna id
#df_limpo <- na.omit(banco_semiliberdade)

banco_semiliberdade <- banco_semiliberdade %>% filter(!is.na(id))

#separando os id que se encontram na mesma linha

banco_semiliberdade <- banco_semiliberdade %>%
  separate_rows(id, sep = "-")

#tratando coluna id
banco_semiliberdade$id = as.factor(banco_semiliberdade$id)
#################################################################################################################
#ESTE BANCO servirá para o tratamento do projeto na internação
#################################################################################################################


#selecionando colunas do banco painel

painel_tratado = painel |> select(1,3)

#tratando banco painel
painel_tratado <- painel_tratado %>%
  rename(id = id_do_adolescente)

#tratando coluna id do banco painel
painel_tratado$id = as.factor(painel_tratado$id)

#excluindo repetidos
painel_tratado  <- painel_tratado %>% distinct()


#################################################################################################################

#################################################################################################################


#juntando bancos
banco_semiliberdade_geral = left_join(banco_semiliberdade, painel_tratado, by = "id")

banco_semiliberdade_geral = distinct(banco_semiliberdade_geral, id, hora_da_atividade, data_da_atividade, atividade, .keep_all= TRUE)

#excuindo colunas desnecessárias e salvar

banco_semiliberdade_geral <- banco_semiliberdade_geral %>% select(-texto,-numero, -numero1)
write.table(banco_semiliberdade_geral, "banco_semiliberdade_geral.csv", sep = ",", row.names = FALSE, col.names = TRUE, quote = TRUE)

#################################################################################################################
#TRATAMENTO banco_semiliberdade FIM
#################################################################################################################
#################################################################################################################
#TRATAMENTO banco_internacao
#################################################################################################################


# formatando campos data_da_atividade e hora_da_atividade:

banco_internacao$data_da_atividade = as.Date(banco_internacao$data_da_atividade)

banco_internacao$hora_da_atividade = strptime(banco_internacao$hora_da_atividade, format = "%H:%M")

banco_internacao$hora_da_atividade <- format(banco_internacao$hora_da_atividade, "%H:%M")


# Separar letras
banco_internacao$texto <- gsub("[0-9]", "", banco_internacao$nomes_e_id_s_dos_jovens_indicados_somente_iniciais)

# Separar números
banco_internacao$numero <- gsub("[^0-9]", "", banco_internacao$nomes_e_id_s_dos_jovens_indicados_somente_iniciais)


# funcão para separar
insert_separator <- function(string, n, separator) {
  gsub(paste0("(.{", n, "})"), paste0("\\1", separator), string)
}

# usando a função
banco_internacao$numero1 <- insert_separator(banco_internacao$numero, 5, "-")

# apagando o último caracter (-) do campo

banco_internacao$id <- sub("\\-$", "", banco_internacao$numero1)

# preencher campos vazios com NA
banco_internacao$id[banco_internacao$id == ""] <- NA
banco_internacao_bkp = banco_internacao

#excuindo colunas desnecessárias e salvar

banco_internacao1 <- banco_internacao %>% select(-texto,-numero,-numero1)
write.table(banco_internacao1, "banco_internacao_COM_na.csv", sep = ",", row.names = FALSE, col.names = TRUE, quote = TRUE)
#write.xlsx(banco_internacao, "banco_internacao_COM_na.ods", fileType = "ods")


# excluir linhas com NA coluna id
#df_limpo <- na.omit(banco_internacao)

banco_internacao <- banco_internacao %>% filter(!is.na(id))

#separando os id que se encontram na mesma linha

banco_internacao <- banco_internacao %>%
  separate_rows(id, sep = "-")

#tratando coluna id
banco_internacao$id = as.factor(banco_internacao$id)
#################################################################################################################
#ESTE BANCO servirá para o tratamento do projeto na internação
#################################################################################################################


#selecionando colunas do banco painel

painel_tratado = painel |> select(1,3)

#tratando banco painel
painel_tratado <- painel_tratado %>%
  rename(id = id_do_adolescente)

#tratando coluna id do banco painel
painel_tratado$id = as.factor(painel_tratado$id)

#excluindo repetidos
painel_tratado  <- painel_tratado %>% distinct()


#################################################################################################################
#ESTE BANCO servirá para o tratamento do projeto na internação
#################################################################################################################


#juntando bancos
banco_internacao_geral = left_join(banco_internacao, painel_tratado, by = "id")

banco_internacao_geral = distinct(banco_internacao_geral, id, hora_da_atividade, data_da_atividade, atividade, .keep_all= TRUE)


#excuindo colunas desnecessárias e salvar

banco_internacao_geral <- banco_internacao_geral %>% select(-texto,-numero, -numero1)
write.table(banco_internacao_geral, "banco_internacao_geral.csv", sep = ",", row.names = FALSE, col.names = TRUE, quote = TRUE)

#################################################################################################################
#TRATAMENTO banco_internacao FIM
#################################################################################################################

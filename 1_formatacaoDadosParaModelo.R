# ===============================================================================================
# |                               inputs, funções, pacotes e variáveis                         |
# ===============================================================================================


# -----------------------------------------------------------------------------------------------
# |                                         variáveis                                         |
# -----------------------------------------------------------------------------------------------

# mínimo número de vezes que uma classe (nome do ator, nome da distribuidora, etc)
# deve aparecer deve aparecer para ser considerada como uma variável que pode influenciar o público
nMin_variavel  <- 8


# -----------------------------------------------------------------------------------------------
# |                                         input                                               |
# -----------------------------------------------------------------------------------------------

# carrega a tabela com os dados dos filmes
load( "~/Dropbox/trabalhos/matias/modelo_filmesNacionais/inputs/brDf.RData")


# -----------------------------------------------------------------------------------------------
# |                                       funcoes                                               |
# -----------------------------------------------------------------------------------------------

# Carrega as funções que estão na pasta 'funcoes'
sources  <- list.files("~/Dropbox/trabalhos/matias/modelo_filmesNacionais/funcoes/", 
                       pattern = "\\.R$", full.names = TRUE)
for(s in sources) source(s)



# ===============================================================================================
# |                               formata dos dados                                             |
# ===============================================================================================


# -----------------------------------------------------------------------------------------------
# |                             remove filmes (linhas)                                         |
# -----------------------------------------------------------------------------------------------

### Generos que não serão considerados no modelo (só se irá considerar filmes de ficção)
nrow(brDf)
generosSemAtores  <- c("documentario")
brDf  <- brDf[!brDf$atores %in% generosSemAtores,]
brDf  <- brDf[!grepl("Documentário", brDf$genero),]
nrow(brDf)

### Não tem informacao que serão consideradas no modelo
# Não possuem o $ captado
brDf  <- brDf[!(is.na(brDf$captado) | brDf$captado == 0),]
# Não possue avaliação de quão bom o filme é
brDf  <- brDf[!is.na(brDf$nota),]
# Não possue público
brDf  <- brDf[!is.na(brDf$publico),]

#### é relancamento, logo o público esperado não é o mesmo
brDf  <- brDf[!brDf$relancamento,]



# -----------------------------------------------------------------------------------------------
# |     transforma colunas com multiplos valores em multiplas colunas com variáveis dummy       |
# -----------------------------------------------------------------------------------------------

#### colunas que serão transformadas em dumb values
dumbColumns  <- c("diretor", "atores", "distribuidora", "genero")


#### cria uma matriz para cada coluna e coloca as matrizes em uma lista.
listDumb  <- list()
for(dc in dumbColumns){
  listDumb[[dc]]  <- makeDumb(dfColuna = brDf[,dc])
}

### histograma para o relatório
distribuicao_distribuidoras  <- colSums(listDumb[[3]]) 
hist(distribuicao_distribuidoras, breaks = 17, axes = FALSE, 
     main = "",
     ylab = "Frequência",
     xlab = "Filmes distribuídos", family = "Palatino")
axis(1, lwd = 0, lwd.ticks = 1)
axis(2, lwd = 0, lwd.ticks = 1, las = 1)


#### Filtra as variaveis de interesse para um minimo de ocorrências
####     (se por exemplo só há dois filmes com o ator X, se irá
####      remover a coluna do ator X)

# Quantos filmes de determinado ator, diretor, distribuidora, genero existem...
nFilmes_variaveis  <- lapply(lapply(listDumb, colSums), sort)
# nomes das classes (eg nomes dos atores) que em cada coluna aparecem um minomo de vezes 
nomes_classeVariaveis  <- lapply(nFilmes_variaveis, function(x) names(x)[x >= nMin_variavel])
# Se retira o "ator animação"
nomes_classeVariaveis$atores  <- nomes_classeVariaveis$atores[!nomes_classeVariaveis$atores %in% "animacao"]
# filtra as tabelas para só conter as classes que ocorrem um mínimo número de vezes
tabelas_selectClasses  <- lapply(names(listDumb), function(x) listDumb[[x]][,nomes_classeVariaveis[[x]]])
# se alguma coluna não possui nenhuma classe com o número mino de ocorrência se remove a matriz da lista
failMin  <- sapply(tabelas_selectClasses, ncol) == 0
tabelas_selectClasses  <- tabelas_selectClasses[!failMin]
# nomeia as tabelas
names(tabelas_selectClasses)  <- names(nomes_classeVariaveis)[!failMin]
# formata o nome das colunas das tabelas
tabelas_selectClasses  <- lapply(names(tabelas_selectClasses), 
                                 function(x) {
                                   colnames(tabelas_selectClasses[[x]])  <- paste(x, colnames(tabelas_selectClasses[[x]]), sep = "__"); 
                                   tabelas_selectClasses[[x]]})
# junta todas as tabelas
tabelaDummy  <- do.call(cbind, tabelas_selectClasses)
# tira o espaco dos nomes das colunas
colnames(tabelaDummy)  <- gsub(" ", "_", colnames(tabelaDummy))
# nomeia as linhas
rownames(tabelaDummy)  <- rownames(brDf)


# -----------------------------------------------------------------------------------------------
# |             coloca notas como bom (3), médio (2) e ruim (1)                                 |
# -----------------------------------------------------------------------------------------------

# As notas estavam como fatores ordenados. O modelo (lm) iria trata-las então como uma regressão polinomial
# dessa forma ele as tratará como uma regressão linear
notasVec  <- as.numeric(brDf$nota)

# Se divide em gostou (4 e 5), indiferente (3) e não gostou (1 e 2)
notasVec[brDf$nota == 4 | brDf$nota == 5]  <- 3
notasVec[brDf$nota == 3]  <- 2
notasVec[brDf$nota == 2 | brDf$nota == 1 ]  <- 1

# insere as notas formatadas no data frame
brDf$nota  <- notasVec


# -----------------------------------------------------------------------------------------------
# |             publico do filme anterior como log                                              |
# -----------------------------------------------------------------------------------------------

brDf$publico_filme_anterior_log  <- log(brDf$publico_filme_anterior)
# botar zero em -Inf não irá fazer diferença pq os filmes com -Inf são os que tinham filmee  
# anterior. Dado que só se considera o público do filme anterior em sequência isso é indiferente
# par ao modelo
brDf$publico_filme_anterior_log[is.infinite(brDf$publico_filme_anterior_log)]  <- 0



# -----------------------------------------------------------------------------------------------
# |                                 remove colunas                                              |
# -----------------------------------------------------------------------------------------------


# colunas que foram representadas como variáveis dummy
brDf  <- brDf[,!colnames(brDf) %in% dumbColumns]

# colunas que dizem as fontes das informações
brDf  <- brDf[,!grepl("fonte", colnames(brDf))]

# colunas descritivas
colunasDescritivas  <- c("titulo_filme_anterior", "nome_programaTv", "relancamento")
colunasDescritivasIndex  <- which(colnames(brDf) %in% colunasDescritivas)
brDf  <- brDf[,-colunasDescritivasIndex]

# colunas que não serão usadas no modelo
colunasNaoUsadasNoModelo  <- c("titulo", "dataLancamento", "maximo_numero_de_salas", 
                               "bilheteria", "classificacao_indicativa")
for( i in colunasNaoUsadasNoModelo){
  brDf[,i]  <- NULL
}



# -----------------------------------------------------------------------------------------------
# |                                 salva o df do modelo                                        |
# -----------------------------------------------------------------------------------------------

# data frame com todas as colunas que serão usadas no modelo
df_modelo  <- cbind(brDf, tabelaDummy)


##########################3 TESTE
colunasDistribuidoras  <- colnames(df_modelo)[grepl("^distribuidora__", colnames(df_modelo))]
mtxDistribuidora  <- df_modelo[,colunasDistribuidoras]
df_modelo$distribuidora__pequenas  <- !apply(mtxDistribuidora, 1, any)




save(df_modelo, file = "~/Dropbox/trabalhos/matias/modelo_filmesNacionais/inputs/df_modelo.RData")


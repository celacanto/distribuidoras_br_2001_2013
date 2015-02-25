# ===============================================================================================
# |                               inputs, funções, pacotes e variáveis                         |
# ===============================================================================================


# -----------------------------------------------------------------------------------------------
# |                                         pacotes                                             |
# -----------------------------------------------------------------------------------------------

# para usar a função stepAIC
library(MASS)

# para fazer o output das tabelas lm
library(stargazer)

# -----------------------------------------------------------------------------------------------
# |                                         input                                               |
# -----------------------------------------------------------------------------------------------

# carrega a tabela com os dados dos filmes
load( "~/Dropbox/trabalhos/matias/modelo_filmesNacionais/inputs/df_modelo.RData")


# -----------------------------------------------------------------------------------------------
# |                                       funcoes                                               |
# -----------------------------------------------------------------------------------------------

# Carrega as funções que estão na pasta 'funcoes'
sources  <- list.files("~/Dropbox/trabalhos/matias/modelo_filmesNacionais/funcoes/", 
                       pattern = "\\.R$", full.names = TRUE)
for(s in sources) source(s)

# ===============================================================================================
# |                               Variáveis para o stepwise                                    |
# ===============================================================================================

bic  <- TRUE
direcao  <- "forward"

# ===============================================================================================
# |                               modelo                                                        |
# ===============================================================================================

k <- ifelse(test = bic, yes = log(nrow(df_modelo)), no = 2)

# -----------------------------------------------------------------------------------------------
# |                         modelo sem as distribuidoras                                        |
# -----------------------------------------------------------------------------------------------

modelo_noDummy  <- lm(log(publico)
                   ~
                   + (publico_filme_anterior_log : temFilmeAnterior)  #  o publico do  filme anterior só entrará no modelo se houver filme anterior
                   + (pontos_ibobeSP_Ano_do_lancamento : filmeBaseadoSerieTV) # o ibope do filme só entrará nomodelo se o filme for baseado em uma série de tv                      
                   + log(captado) # o qto se captou para o filme
                   + nota  # o quão bom é o filme
                   ,
                   data = df_modelo)

  
# Se completa o modelo com variáveis  múltiplas* dummy (atores, diretores, genero) com excessão das distribuidoras (generos, atores e diretores)
# * múltiplas pq um filme pode ter mais de um.
# Essas variáveis podem ou não serem importante para o modelo (stepAIC que dirá) 
colunasVariaviesDummy  <- colnames(df_modelo)[grepl("^.*__", colnames(df_modelo))]
colunasVariaviesDummyNoDist  <- colunasVariaviesDummy[!grepl("^distri", colunasVariaviesDummy)]
formula_adicionandoVariaveisDummy  <- as.formula(paste0("~.+", paste(colunasVariaviesDummyNoDist, collapse = "+")))

# modelo com todas as variáveis possíveies
modelo_maxSemDistribuidoras  <- update(modelo_noDummy, formula = formula_adicionandoVariaveisDummy)



# acha o melhor modelo (elimina variáveis que não ajudam a prever)
modeloSemDistribuidoras <- stepAIC(modelo_noDummy, 
                                   direction = direcao, 
                                   trace = FALSE,
                                   scope = list(upper = modelo_maxSemDistribuidoras,
                                                lower = modelo_noDummy), 
                                   k = k)




# -----------------------------------------------------------------------------------------------
# |                         modelo com as distribuidoras                                        |
# -----------------------------------------------------------------------------------------------


colunasDasDistribuidoras  <- colnames(df_modelo)[grepl("^distribuidora", colnames(df_modelo))]
formula_adicionandodistribuidoras  <- as.formula(paste0("~.+", paste(colunasDasDistribuidoras, collapse = "+")))


##### Com as pequenas 
modeloMaxComDistribuidoras  <- update(modeloSemDistribuidoras, formula = formula_adicionandodistribuidoras)

# (modelo somente com as distribuidoras que ajudam a prever o público)
modelo <- stepAIC(modeloSemDistribuidoras, 
                  direction = direcao,
                  trace = FALSE,
                  scope = list(upper = modeloMaxComDistribuidoras,
                               lower = modeloSemDistribuidoras),
                  k = k)

#### Sem as pequenas
modeloMaxSemPequenas  <- update(modeloMaxComDistribuidoras, formula. = "~.-distribuidora__pequenas")
modelo2 <- stepAIC(modeloSemDistribuidoras, 
                   direction = direcao, 
                  trace = FALSE,
                  scope = list(upper = modeloMaxSemPequenas,
                               lower = modeloSemDistribuidoras),
                  k = k)



# ===============================================================================================
#                               Output da tabela dos modelos                                    |
# ===============================================================================================


stargazer(modelo_noDummy, modeloSemDistribuidoras, modelo2,  modelo, title="Regression Results", align=TRUE)


# -----------------------------------------------------------------------------------------------
# |                         salva o modelo                                                      |
# -----------------------------------------------------------------------------------------------

save(modelo, file = "~/Dropbox/trabalhos/matias/modelo_filmesNacionais/inputs/modelo.RData")

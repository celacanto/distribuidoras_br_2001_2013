# ===============================================================================================
# |                                         input, funcoes                                      |
# ===============================================================================================


primoVplot  <- function(modelo_comDist, df_modelo, dist, axis = TRUE, legend.y = "",
                        y.data = -7.5, y.dataSpace = 0.5){
  
  removeDistribuidora  <- as.formula(paste("~.-", dist))
  modelo_semDist  <- update(modelo, removeDistribuidora)
  
  residuos_semDist  <- modelo_semDist$residuals
  residuos_dist  <- residuos_semDist[df_modelo[,dist]]
  list_allDist  <-list(residuos_semDist, residuos_dist)
  
  
  
  distNome  <- gsub("^.*__", "", dist)
  distNome  <- gsub("_", " ", distNome)
  distNome  <- .simpleCap(distNome)
  names(list_allDist)  <- c("", "")
  
  myViolinsPlot(x = list_allDist, yLim = c(-6, 6), n.Yticks = 7, axis = axis, 
                legend.y = legend.y, 
                legend.y.cex = 0.8,
                legendLine.y = 4.3)
  
  data  <- summary(modelo_comDist)$coefficient[paste0(dist, "TRUE"),] 
  
  dataR  <- round(data, 3)
  dataR[dataR == 0]  <- "<0.001"
  names(dataR)  <- c("Coef.", "Erro Pad.", "t", names(dataR)[4])
  xData  <- seq(0.5, 2, length.out = 4)
  
  n  <- sum(df_modelo[,dist])
  if(distNome == "Riofilme"){
    distNome  <- "RioFilme"
  }
  if(distNome == "Pequenas"){
    distNome  <- "DO"
  }
  plotTitle  <- paste0(distNome, " (n=", n, ")")
  mtext(text = plotTitle, side = 3, family = "Palatino", cex = 1, adj = 0.5, at = 1.5)

  text(names(dataR), y = y.data, x = xData, xpd = TRUE, family = "Palatino", cex = 1)  
  text(dataR, y = y.data - y.dataSpace, x = xData, xpd = TRUE, family = "Palatino")
}  





# -----------------------------------------------------------------------------------------------
# |                                         input                                               |
# -----------------------------------------------------------------------------------------------


# carrega o modelo
load( "~/Dropbox/trabalhos/matias/modelo_filmesNacionais/inputs/modelo.RData")
# carrega o df que o modelo se refere
load( "~/Dropbox/trabalhos/matias/modelo_filmesNacionais/inputs/df_modelo.RData")


# -----------------------------------------------------------------------------------------------
# |                                       funcoes                                               |
# -----------------------------------------------------------------------------------------------

# Carrega as funções que estão na pasta 'funcoes'
sources  <- list.files("~/Dropbox/trabalhos/matias/modelo_filmesNacionais/funcoes/", 
                       pattern = "\\.R$", full.names = TRUE)
for(s in sources) source(s)




# ===============================================================================================
# |                               grafico                                                      |
# ===============================================================================================

# Para cada distribuidora se plota a distribuição dos resíduos do modelo sem ela.
# Do lado esquerdo estará  a distribuição de todos os filmes e do lado direito 
# dos filmes da distribuidora


termos  <- names(attr(terms(modelo), "dataClasses"))
distribuidora  <- termos[grepl("^distribuidora__", termos)]


ncolunas  <- 3
nlinhas  <- 1
par(mfrow = c(nlinhas, ncolunas))

totalSlots  <- nlinhas * ncolunas

coluna  <- 1
for(d in distribuidora){
  
  if(coluna == 1){
    axis  <- TRUE
    legend.y <-  "Resíduos"
  } else {
    axis  <- FALSE
    legend.y  <- ""
  }
  
  if(coluna == ncolunas) {
    coluna  <- 1
  } else {
    coluna   <- coluna  + 1
  }
  
  primoVplot(modelo_comDist = modelo,    
             df_modelo = df_modelo, 
             dist = d, 
             axis = axis, 
             legend.y = legend.y,
             y.data = -6.5,
             y.dataSpace = 0.3)  
}

slotsFaltando  <- totalSlots - length(distribuidora)
if(slotsFaltando){
  for(i in 1:slotsFaltando){
   plot(1:10, type = "n", axes = FALSE, xlab = "", ylab = "") 
  }
}





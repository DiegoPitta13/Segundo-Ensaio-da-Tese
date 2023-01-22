library(pdftools)
library(tcltk)
library(tm)
library(tcltk) # TCL/TK para abrir o bd

#=================================================================
# Diret?rios
getwd()
setwd("C:/Users/marcu/Desktop/Arquivos/Pós-Graduação/Doutorado/Tese/Artigo2")

pastaIn <- "C:/Users/marcu/Desktop/Arquivos/Pós-Graduação/Doutorado/Tese/Artigo2/PDFs"
pastaOut <- "C:/Users/marcu/Desktop/Arquivos/Pós-Graduação/Doutorado/Tese/Artigo2/TXT"
lista.arquivos <- list.files(pastaIn)
lista.arquivos


#=========================================================================
#                    Processo de Limpeza 
#=========================================================================
#=================================================================
# PDF para TXT


for (i in 1:length(lista.arquivos)){
  print(i)
  Bbrasiling_1t19.pdf <- lista.arquivos[i]
  arquivo <- file.path(pastaIn, Bbrasiling_1t19.pdf)
  texto <- readPDF(control = list(text = "-layout"))(elem = list(uri = arquivo),
                                                     language = "pt", id = "id1")
  
  texto <- as.character(texto)
  
  if (length(texto) > 1){
    writeLines(texto, file.path(pastaOut, paste0(substr(Bbrasiling_1t19.pdf , 1, (nchar(Bbrasiling_1t19.pdf)-3)), "txt")))
  }
}


#=========================================================================
#                    Processo de Limpeza 
#=========================================================================

#===== CORPUS ========================================
corpus <- VCorpus(DirSource("C:/Users/marcu/Desktop/Arquivos/P?s-Gradua??o/Doutorado/Tese/Artigo2/Relat?rios/Relatorios_TXT/SANTANDER"), readerControl = list(language = "pt")) 
#=====================================================================
corpus <- tm_map(corpus, content_transformer(function(x) gsub(x, pattern = "P&D", replacement = "pd")))
corpus <- tm_map(corpus, stripWhitespace) # remo??o de espa?os
corpus <- tm_map(corpus, removeWords, stopwords("portuguese"))
corpus <- tm_map(corpus, removePunctuation) 
corpus <- tm_map(corpus, removePunctuation, ucp = TRUE) 
corpus <- tm_map(corpus, removeNumbers) # remove n?meros 
corpus <- tm_map(corpus, content_transformer(function(x) gsub(x, pattern = "-", replacement = ""))) # removendo travess?o 
corpus <- tm_map(corpus, removeWords, c("wwwbndesgovbrra")) # removendo termos espec?ficos

corpus <- tm_map(corpus, content_transformer(function(x) gsub(x, pattern = "?", replacement = "c")))


#===================================================================
#  Extra??o de Sentimentos
#==================================================================

#===================================================================
#                  Leitura 
#===================================================================

#========== LISTA DE PALAVRAS POSITIVAS =======================================

local <- tclvalue(tkgetOpenFile(title="Abrir Banco de Dados"))
lista_positiva <- read.table(file = local, header=FALSE, dec=",")
lista_positiva = as.vector(lista_positiva$V1,mode = "any")

#========== LISTA DE PALAVRAS NEGATIVAS =======================================

local <- tclvalue(tkgetOpenFile(title="Abrir Banco de Dados"))
lista_negativa <- read.table(file = local, header=FALSE, dec=",")
lista_negativa = as.vector(lista_negativa$V1,mode = "any")

#===============================================================================
# Matriz com as palavras chave dos Dicion?rios
#===============================================================================


positiva <- DocumentTermMatrix(corpus, list(dictionary = lista_positiva)) 
negativa <- DocumentTermMatrix(corpus, list(dictionary = lista_negativa)) 


# ===================================================================
# Estima??o dos Indicadores: POSITIVA, NEGATIVA 
# ===================================================================


ind_negativa <- as.matrix(apply(negativa, MARGIN=1,FUN=sum))
ind_positiva <- as.matrix(apply(positiva, MARGIN=1,FUN=sum))
port <- cbind(ind_positiva, ind_negativa)  
colnames(port)[1:2] <- c("Positiva", "Negativa")
port

# Gera??o do Sentimento 

# Sentimento Geral 
sent_geral = (ind_positiva - ind_negativa)/(ind_positiva + ind_negativa) 
sent_geral

plot(sent_geral)
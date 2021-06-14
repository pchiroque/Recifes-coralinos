### Laboratório 14/06/2021####

# O documento foi compilado por Pamela. 
#As dúvidas em relação ao uso do pacote R podem ser discutidas com a monitora, Larissa Maracajá.

require(dplyr)
require(ggplot2)
require(tidyr)

# Leitura dos dados#

urlfile<-'https://raw.githubusercontent.com/pchiroque/Recifes-coralinos/master/GNov010319.csv'

dado <- as.data.frame(read.csv(url(urlfile),header = TRUE,dec = ".",sep = ","))

#urlfile<-'https://raw.githubusercontent.com/pchiroque/Recifes-coralinos/master/Hematita.csv'
#dsin <-read.csv(url(urlfile))


#Estruturados da matriz de dados

dado%>%str

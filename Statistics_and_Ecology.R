#######################################################################
#
# INTRODUÇÃO À ECOLOGIA DOS RECIFES CORALINEOS
# 15 DE JULHO DE 2019 - 26 DE JULHO DE 2019
#
# Estatística Descritiva 27/07/2019 (10:00-12:00 hrs)
#
#######################################################################
#
# R code by:
#
# Pamela Massiel Chiroque Solano
# SAGE - Rede Abrolhos 
#
#######################################################################

summary(iris)
summary(iris[iris$Species=="setosa",])
summary(iris[iris$Species=="versicolor",])
summary(iris[iris$Species=="virginica",])

attach(iris)
boxplot(Sepal.Length~Species, main="Sepal Lengths of Irises", ylab="centimeters",col=c("pink","lightblue","lightgreen"))

plot(Sepal.Length, Sepal.Width,Petal.Length, pch=c(21, 22, 23)[as.numeric(Species)],
     col=c("red", "blue", "green")[as.numeric(Species)])

pairs(iris[1:4], main = "Iris dataset", pch = 21, bg = c("red", "green3", "blue")[unclass(iris$Species)], lower.panel=NULL, 
      #labels=c("SL","SW","PL","PW"), 
      font.labels=2, cex.labels=1.5) 

require(ggplot2)
require(dplyr)
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) + geom_point()

library(GGally)

ggpairs(iris[,-5])+ theme_bw()


library(GGally)
p <- ggpairs(iris, aes(color = Species))+ theme_bw()
# Change color manually.
# Loop through each plot changing relevant scales
for(i in 1:p$nrow) {
  for(j in 1:p$ncol){
    p[i,j] <- p[i,j] + 
      scale_fill_manual(values=c("#00AFBB", "#E7B800", "#FC4E07")) +
      scale_color_manual(values=c("#00AFBB", "#E7B800", "#FC4E07"))  
  }
}
p

  iris %>% select(-Species) %>% # tira a coluna da especie
  scale() %>%                 # normaliza os dados, mean zero and unit variance 
  prcomp() ->                 # faz PCA
  pca                         # 

# Mostrar resultados do PCA
pca

# Os principais resultados do PCA são desvio padrão e a matrix de rotação.
# Vamos plotar os componentes principais PC2 vs. PC1. 
# os dados rotacionados estão disponíveis no pca$x:
  
  head(pca$x)

# Repare que até agora nada é relacionado com as especies (foram tiradas no início da análise)
# Por quê?. Devemos incorporar a informação das especies?


# Adicionando informação das especies nos resultados do PCA
pca_data <- data.frame(pca$x, Species=iris$Species)
head(pca_data)

# Graficando
ggplot(pca_data, aes(x=PC1, y=PC2, color=Species)) + geom_point()

# Se utilizamso os primeiros dois componentes PC2 vs PC1, 
# Vemos que versicolor e virginica encontram-se afastadas

# Vejamos as rotações (perto pouca e longe muita: contribuição)

  pca$rotation

# Estes resultados indicam as contribuições de cada variável sobre cada componente principal
# Exemplo: Sepal.Width contribui pouco para PC1 mas contribui muito no PC2.
# Para melhorar o visual das contribuicoes vejamos o gráfico das rotações com as zetas.

    
# capturamos as saidas das rotações em um data.frame
  rotation_data <- data.frame(pca$rotation, variable=row.names(pca$rotation))
# definir o estilo, zeta
arrow_style <- arrow(length = unit(0.05, "inches"),
                     type = "closed")
# vamos fazer o gráfico usando segmentos

ggplot(rotation_data) + 
  geom_segment(aes(xend=PC1, yend=PC2), x=0, y=0, arrow=arrow_style) + 
  geom_text(aes(x=PC1, y=PC2, label=variable), hjust=0, size=3, color='red') + 
  xlim(-1.,1.25) + 
  ylim(-1.,1.) +
  coord_fixed() # fix aspect ratio to 1:1

# A figura mostra claramente que: 
# Petal.Length, Petal.Width, e Sepal.Length todas elas contribuim sobre PC1, 
# e Sepal.Width domina o PC2.

# Para analisar o porcentagem de variancia explicada 
# vamos usar a funcao prcomp() que fornece os desvios padrões: pca$sdev. 
# Para isso é necessario fazer uma conta:


percent <- 100*pca$sdev^2/sum(pca$sdev^2)
percent

# O primeiro componente explica 73% da variância
# o segundo explica 23%, o terceiro explica 4% e o último 0.5%. 

#Um gráfico de barras pode ser feito
perc_data <- data.frame(percent=percent, PC=1:length(percent))
ggplot(perc_data, aes(x=PC, y=percent)) + 
  geom_bar(stat="identity") + 
  geom_text(aes(label=round(percent, 2)), size=4, vjust=-.5) + 
  ylim(0, 80)

library("FactoMineR")
library("factoextra")
var <- get_pca_var(pca)

library("corrplot")
corrplot(var$cos2, is.corr=FALSE)


# Color by cos2 values: quality on the factor map
fviz_pca_var(pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)

head(var$contrib, 4)

library("corrplot")
corrplot(var$contrib, is.corr=FALSE)  


# Contributions of variables to PC1
fviz_contrib(pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(pca, choice = "var", axes = 2, top = 10)

fviz_pca_var(pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

fviz_pca_ind(pca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)

fviz_pca_ind(pca, pointsize = "cos2", 
             pointshape = 21, fill = "#E7B800",
             repel = TRUE # Avoid text overlapping (slow if many points)
)

ind.p <- fviz_pca_ind(pca, geom = "point", col.ind = iris$Species)
ggpubr::ggpar(pca,
              title = "Principal Component Analysis",
              subtitle = "Iris data set",
              caption = "Source: factoextra",
              xlab = "PC1", ylab = "PC2",
              legend.title = "Species", legend.position = "top",
              ggtheme = theme_gray(), palette = "jco"
)

fviz_pca_biplot(pca, 
                col.ind = iris$Species, palette = "jco", 
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Species") 

fviz_pca_biplot(pca, 
                # Fill individuals by groups
                geom.ind = "point",
                pointshape = 21,
                pointsize = 2.5,
                fill.ind = iris$Species,
                col.ind = "black",
                # Color variable by groups
                col.var = factor(c("sepal", "sepal", "petal", "petal")),
                
                legend.title = list(fill = "Species", color = "Clusters"),
                repel = TRUE        # Avoid label overplotting
)+
  ggpubr::fill_palette("jco")+      # Indiviual fill color
  ggpubr::color_palette("npg")      # Variable colors


fviz_pca_biplot(pca, 
                # Individuals
                geom.ind = "point",
                fill.ind = iris$Species, col.ind = "black",
                pointshape = 21, pointsize = 2,
                palette = "jco",
                addEllipses = TRUE,
                # Variables
                alpha.var ="contrib", col.var = "contrib",
                gradient.cols = "RdYlBu",
                
                legend.title = list(fill = "Species", color = "Contrib",
                                    alpha = "Contrib")
)

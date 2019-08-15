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

# Muitas vezes na pesquisa ecologica estamos interessados não apenas na comparação de descritores univariados 
# das comunidades como a diversidade, mas também em como a espécie constituinte ou a composição muda de uma 
# comunidade para outra.
# uma tecnica comum para este objetivo é o escalamento multidimensional não métrico, cujo objetivo é reduzir 
# a informação de múltiples dimensões (exemplo: comunidades, locais, etc.) dentro de um conjunto de menor dimensão,
# para logo interpretar e vizualizar.
# Diferente a outras técnicas de redução (ordenação) esta técnica se baseia sobre as distâncias 
# (principalmente ecuclineanas), o NMDS usa a ordem de classificação o que a torna extremamente flexível  
# para se adaptar a uma variedade de tipo de dados.

# O que é NMDS
# Imagine que consideramos um único eixo para representar a abundância de uma especie. 
# Ao longo deste eixo nós podemos desenhar as comunidados nas quais as espécies apparecem, baseada sobre a
# abundância dentro de cada comunidade.


plot(0:10,0:10,type="n",axes=F,xlab="Abundance of Species 1",ylab="") 
axis(1)
points(5,0); text(5.5,0.5,labels="community A")
points(3,0); text(3.2,0.5,labels="community B")
points(0,0); text(0.8,0.5,labels="community C")


# agora consideremos um segundo eixo de abundância representando outra especie. Nós podemos desenhar 
# a comunidade ao longo dos dois eixos, representando a porcentagem de participação na comunidade por
# cada especie. (Species 1 and Species 2).

plot(0:10,0:10,type="n",xlab="Abundance of Species 1",
     ylab="Abundance of Species 2")
points(5,5); text(5,4.5,labels="community A")
points(3,3); text(3,3.5,labels="community B")
points(0,5); text(0.8,5.5,labels="community C")


# Agora consderemos um terceiro eixo de abundânca representando outra especie.


# install.packages("scatterplot3d")
library(scatterplot3d)
d=scatterplot3d(0:10,0:10,0:10,type="n",xlab="Abundance of Species 1",
                ylab="Abundance of Species 2",zlab="Abundance of Species 3"); d
d$points3d(5,5,0); text(d$xyz.convert(5,5,0.5),labels="community A")
d$points3d(3,3,3); text(d$xyz.convert(3,3,3.5),labels="community B")
d$points3d(0,5,5); text(d$xyz.convert(0,5,5.5),labels="community C")

# Vamos imaginar mais especies, cada esdpecie é uma dimensão.

#NMDS#

# O objetivo do NMDS é representar a posição inicial das comunidades em
# um espaço multidimensional com a maior precisão possível usando um número reducido de 
# dimensões que podem ser fácilmente plotados e visualizados 

# Como funciona?

# O MDS não usa as medidas absolutas das especies na comunidades, mas ao invés de isso
# eles usam a ordem de classificação.
# Mas o uso destas classificações omite alguns problemas associado com as distâncias 
# absolutas (sensíveis a transformação), estas caracteristicas fornecem à tecnica NMDS 
# maior flexibilidade para aceptar uma variedade de tipos de dados. Repare que o fato
# de ter como objetivo ordenar classificações torna ao NMDS uma tecnica não métrica.


#install.packages("vegan")
library(vegan)
set.seed(2)
community_matrix=matrix(
  sample(1:100,300,replace=T),nrow=10,
  dimnames=list(paste("community",1:10,sep=""),paste("sp",1:30,sep="")))

example_NMDS=metaMDS(community_matrix,k=2,trymax=100)

stressplot(example_NMDS)



plot(example_NMDS)


ordiplot(example_NMDS,type="n")
orditorp(example_NMDS,display="species",col="red",air=0.01)
orditorp(example_NMDS,display="sites",cex=1.25,air=0.01)


treat=c(rep("Treatment1",5),rep("Treatment2",5))
ordiplot(example_NMDS,type="n")
ordihull(example_NMDS,groups=treat,draw="polygon",col="grey90",label=F)
orditorp(example_NMDS,display="species",col="red",air=0.01)
orditorp(example_NMDS,display="sites",col=c(rep("green",5),rep("blue",5)),
         air=0.01,cex=1.25)

# First, create a vector of color values corresponding of the 
# same length as the vector of treatment values
colors=c(rep("red",5),rep("blue",5))
ordiplot(example_NMDS,type="n")
#Plot convex hulls with colors baesd on treatment
for(i in unique(treat)) {
  ordihull(example_NMDS$point[grep(i,treat),],draw="polygon",
           groups=treat[treat==i],col=colors[grep(i,treat)],label=F) } 
orditorp(example_NMDS,display="species",col="red",air=0.01)
orditorp(example_NMDS,display="sites",col=c(rep("green",5),
                                            rep("blue",5)),air=0.01,cex=1.25)

  elevation=runif(10,0.5,1.5)
# Use the function ordisurf to plot contour lines
ordisurf(example_NMDS,elevation,main="",col="forestgreen")
# Finally, display species on plot
orditorp(example_NMDS,display="species",col="grey30",air=0.1,
         cex=1)
#######################################################################
#
# INTRODUÇÃO À ECOLOGIA DOS RECIFES CORALINEOS
# 15 DE JULHO DE 2019 - 26 DE JULHO DE 2019
#
# Inferência Bayesiana 25/07/2019 (10:00-17:00 hrs)
#
#######################################################################
#
# R code by:
#
# Pamela Massiel Chiroque Solano
# SAGE - Rede Abrolhos 
#
#######################################################################

### Frio Quente ####
#install.packages("plotly")
library(plotly)
# volcano is a numeric matrix that ships with R
p <- plot_ly(z = ~volcano) %>% add_surface(
  contours = list(
    z = list(
      show=TRUE,
      usecolormap=TRUE,
      highlightcolor="#ff0000",
      project=list(z=TRUE)
    )
  )
) %>%
  layout(
    scene = list(
      camera=list(
        eye = list(x=1.87, y=0.88, z=-0.64)
      )
    )
  )

p


kd <- with(MASS::geyser, MASS::kde2d(duration, waiting, n = 50))
p <- plot_ly(x = kd$x, y = kd$y, z = kd$z) %>% add_surface()


##### Peixes nos locais ####

Florest_Campo <- as.data.frame(cbind(1:10, c(9,6,4,6,7,10,12,9,12,10),
c(rep(1,6),rep(0,4))))


colnames(Florest_Campo) <- c("id","NFor","Hab")

Florest_Campo$Hab<- as.factor(Florest_Campo$Hab)
plot(1:10,Florest_Campo$NFor,col=Florest_Campo$Hab,pch=20,cex=3)

library(ggpubr)

Geom.Circle <- function(CC=CC,Vary=Vary,Varx=Varx,VarG=VarG){
  # Add group mean points and stars
  ggscatter(CC, x = Varx, y = Vary,
            color = VarG, palette = VarG,
            shape = VarG, ellipse = TRUE, 
            mean.point = TRUE, star.plot = TRUE,
            ggtheme = theme_minimal())
} ### Groups.Circle


Geom.Circle(CC=Florest_Campo,Vary="NFor",Varx="id",VarG="Hab")
summary(aov(Florest_Campo$NFor~Florest_Campo$Hab))

#p-value
pf(8.78,1,8,lower.tail = FALSE)

require(dplyr)
Florest_Campo %>% group_by(Hab) %>% summarize(meaN= mean(NFor),sD= sd(NFor))

# Hab    meaN    sD
# 0      10.8  1.5 
# 1       7    2.19


Dentro_resid<- sum((c(9,6,4,6,7,10) - 7)^2)+
sum((c(12,9,12,10)-10.8)^2)

SumTotal <- sum((Florest_Campo$NFor - mean(Florest_Campo$NFor))^2)

EntreGrupos <- SumTotal - Dentro_resid

#Dentro_resid/8
#[1] 3.845
# SumTotal - Dentro_resid/(Dentro_resid/8)
# 56.5
# (SumTotal - Dentro_resid)/(Dentro_resid/8)
#[1] 8.775033

qf(0.95,1,8)
qf(0.05,1,8,lower.tail = FALSE)


Florest_Campo %>% ggplot(mapping = aes(x = id, y = NFor)) +
  geom_density(y = NFor)

data_frame(x = seq(0, +15, by = 0.01),
           y = df(x,1,8)) %>%
  ggplot(mapping = aes(x = x, y = y)) +
  geom_line()+  ggtitle("Distribuição F teórica") +
  ylab("Densidade de Probabilidade")+
  labs(x=quote("Razão F"~  ( "SQMEntreGrupos"/"SQMDentroGrupos")))+
  geom_vline(xintercept = qf(0.95,1,8),colour="red")+
annotate(geom="text", x=qf(0.95,1,8)+1.1, y=0.15, label="Valor crítico(tabela): F = 5.32",
         color="red")  +
annotate(geom="text", x=qf(0.95,1,8)+3, y=0.025, label="Pr(dados das formigas|hipótese nula)=0.018",
         color="blue")  +
  geom_ribbon(data=subset(data_frame(x = seq(1, +15, by = 0.01),
                                      y = df(x,1,8)),x> qf(0.95,1,8) & x<+15),aes(ymax=y),ymin=-1,
              fill="blue",colour="blue",alpha=0.5)+
  scale_y_continuous(limits=c(0, .4))


library(rstanarm)


post <- stan_lm(NFor ~ Hab, data = Florest_Campo,
                prior = R2(location = 0.2,what = "mean"), 
                chains = 2)




fig0 <- plot(post,"areas",prob=0.95, prob_outer=1,pars = c("(Intercept)","Hab1") ) + geom_vline(xintercept = 0)

fig0

simple <- stan_glm(NFor ~ Hab, data = Florest_Campo, family = gaussian(), 
                   prior = cauchy(), 
                   prior_intercept = cauchy(),
                   chains = 2)#, cores = CORES, seed = SEED)

fig1 <- plot(simple,"areas",prob=0.98, prob_outer=1,pars = c("(Intercept)","Hab1") ) + geom_vline(xintercept = 0)

fig1

glm_1 <- lm(NFor ~ Hab, 
            data = Florest_Campo)


summary(glm_1)


cad <- as.array(simple)

cad.df <- as.data.frame(cad)
cad.df <- cad.df[,c(1,4)]
colnames(cad.df) <- c("LocalB","EfLocalA")

cad.df$LocalA <- cad.df$EfLocalA+cad.df$LocalB

plot(cad.df$LocalA,ty="l")
plot(cad.df$LocalB,ty="l")
 
y.fit <- mean(cad.df$`chain:1.(Intercept)`)+mean(cad.df$`chain:2.Hab1`)*Florest_Campo$Hab

plot(y.fit-Florest_Campo$NFor,ty="l")

Florest_Campo






plot(density(cad.df$LocalB),xlim=c(0,16),col="Blue")
lines(density(cad.df$LocalA),col="red")
legend("topleft",col=c("red","blue"),legend=c("Local A","Local B"))

quantile((cad.df$LocalA),c(0.025,0.975))


##### Peixe fêmeas e maturas BINOMIAL ####

DD_Bin <- data.frame(T = c(12.5,22.5,32.5,62.5))
DD_Bin$Total <- c(3,5,4,5)
DD_Bin$Maduras <- c(0,1,3,5)
DD_Bin$NMaduras <- DD_Bin$Total-DD_Bin$Maduras
DD_Bin$Med <- DD_Bin$T-mean(DD_Bin$T)

plot(DD_Bin$Med,DD_Bin$Maduras/DD_Bin$Total, col=4,pch=20,cex=3,ylab="Proporção de Maturidade",xlab = "Comprimento(cm)")

glm_1 <- glm(cbind(Maduras,NMaduras) ~ Med,
             data = DD_Bin, 
             family = binomial(link = "logit"))
round(coef(summary(glm_1)), 3)


mdB1 <- stan_glm(cbind(Maduras,NMaduras) ~ Med,
                 data = DD_Bin,
                 family = binomial(link = "logit"), 
                 prior = student_t(df = 7), 
                 prior_intercept = student_t(df = 7),
                 chains = 1)
summary(mdB1)
fig1 <- plot(mdB1,"areas",prob=0.95, prob_outer=1,pars = c("(Intercept)","Med")) + geom_vline(xintercept = 0)
fig1

fig1 <- plot(mdB1,"areas",prob=0.95, prob_outer=1,pars = c("Med")) + geom_vline(xintercept = 0)
fig1


library(bayesplot)
mcmc_pairs(as.array(mdB1),pars = c("(Intercept)","Med"))
cad <- as.array(mdB1)

cad.df <- as.data.frame(cad)
colnames(cad.df) <- c("Int","slope")


 cad.df%>%ggplot(aes(y=Int,x=1:dim(cad.df)[1]) )+geom_line(color="purple")+
 labs(x="iterações")
 cad.df%>%ggplot(aes(y=slope,x=1:dim(cad.df)[1]) )+geom_line(color="lightblue")+
   labs(x="iterações")
 
Pred_LT_50 <- -cad[,1,1]/cad[,1,2] + 32.5

ggplot(data.frame(x=Pred_LT_50),aes(x=x))+geom_density()+
  labs(x="LT-50",y="P(LT-50|Dados)")+
ggtitle("Distribuição posterior para LT-50, com dados de 17 fêmeas de peixe Galo")

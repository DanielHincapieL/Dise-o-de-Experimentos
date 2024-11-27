ZZZ <- ZZZ
attach(ZZZ)

library(ggplot2) 
# con una significancia del 0,1

ZZZ$Almacenamiento <- as.factor(ZZZ$Almacenamiento)
ZZZ$Archivo<- as.factor(ZZZ$Archivo)
ZZZ$RAM <- as.factor(ZZZ$RAM)


#Boxplot comparativo de almacenamiento y tiempo
ggplot(ZZZ, aes(x = Almacenamiento, y = Tiempo)) + 
  stat_boxplot(geom = "errorbar",
               width = 0.25) +
  geom_boxplot(fill = "dodgerblue1",
               colour = "black",
               alpha = 0.5,
               outlier.colour = "tomato2")

#Boxplot comparativo de archivo y tiempo

ggplot(ZZZ, aes(x = Archivo, y = Tiempo)) + 
  stat_boxplot(geom = "errorbar",
               width = 0.25) +
  geom_boxplot(fill = "dodgerblue1",
               colour = "black",
               alpha = 0.5,
               outlier.colour = "tomato2")

#Boxplot comparativo de RAM y tiempo

ggplot(ZZZ, aes(x = RAM, y = Tiempo)) + 
  stat_boxplot(geom = "errorbar",
               width = 0.25) +
  geom_boxplot(fill = "dodgerblue1",
               colour = "black",
               alpha = 0.5,
               outlier.colour = "tomato2")

require(gplots)

par(mfrow= c(1,3))

#medias de Tiempo vs Almacenamiento
plotmeans(Tiempo~Almacenamiento,data= ZZZ)
#"" vs Tiempo de Archivo
plotmeans(Tiempo~Archivo,data= ZZZ)
#"" vs Tiempo de RAM
plotmeans(Tiempo~RAM,data= ZZZ)




### graficos para ver interaccion entre las variables

par(mfrow= c(1,2))

interaction.plot(Almacenamiento, Archivo, Tiempo, 
                 xlab="Almacenamiento", ylab="Tiempo")

interaction.plot(Archivo, Almacenamiento, Tiempo, 
                 xlab="Archivo", ylab="Tiempo")

par(mfrow= c(1,2))

interaction.plot(Archivo, RAM, Tiempo, 
                 xlab="Archivo", ylab="Tiempo")

interaction.plot(RAM, Archivo, Tiempo, 
                 xlab="RAM", ylab="Tiempo")

par(mfrow= c(1,2))

interaction.plot(Almacenamiento, RAM, Tiempo, 
                 xlab="Almacenamiento", ylab="Tiempo")

interaction.plot(RAM, Almacenamiento, Tiempo, 
                 xlab="RAM", ylab="Tiempo")

#Grafica de diseño

require(graphics)
formula <- Tiempo ~ Almacenamiento + Archivo + RAM
plot.design(formula, data=ZZZ, col= "coral", xlab="Efectos", ylab="Tiempo")



#Ahora a ANALISIS DE VARIANZA


anova <- aov(Tiempo ~ Almacenamiento * Archivo * RAM, data = ZZZ)
anova
summary(anova)
# La RAM aparentemente no es significativa

SST= 611.9256 +  90.5818 + 2.5607 + 94.8765 + 2.5950 + 0.4712 + 1.5518 + 27.4343
SST


Tratamiento= 27.4343/SST
Tratamiento

ERROR= 1- Tratamiento
ERROR


modelo1<- aov(Tiempo ~ Almacenamiento * Archivo * RAM , data = ZZZ)
summary(modelo1)

modelo2<- aov(Tiempo ~ Almacenamiento * Archivo + Almacenamiento * RAM 
              + Archivo * RAM, data = ZZZ)
summary(modelo2)

modelo3<- aov(Tiempo ~ Almacenamiento * Archivo + RAM , data = ZZZ)
summary(modelo3)
modelolm1<- lm(Tiempo ~ Almacenamiento * Archivo + RAM , data = ZZZ)
summary(modelolm1)



modelo4<- aov(Tiempo ~ Almacenamiento * Archivo , data = ZZZ)
summary(modelo4)
modelo4

modelolm<- lm(Tiempo ~ Almacenamiento * Archivo , data = ZZZ)
summary(modelolm)

SST= 611.9256 + 90.5818 + 94.8765 + 34.6130
SST


Tratamiento= (611.9256 + 90.5818 + 94.8765)/SST
Tratamiento

ERROR= 1- Tratamiento
ERROR






#NORMALIDAD

residuales <- rstandard(modelo4)
library(tseries)
library(nortest)
library(ggplot2)
library(car)

shapiro.test(residuales)
ad.test(residuales)
jarque.bera.test(residuales)

qqPlot(residuales, xlab = 'Cuantiles de distribucion normal',
       ylab = 'Cuantiles de residuales', pch = 16, col = "dodgerblue1",
       col.lines = "red")
# si esta entre banda de confianza, tiene normalidad 

#NO ES normal

hist(residuales)
#Con el histograma podemos explicar que tiene una distrubución casi simetrica
#Entonces que a pesar de que no nos dio el supuesto de normalidad vamos a continuar
#Con esto, ya que es un analisis exploratorio.


#HOMOCEDASTICIDAD

library(lmtest)
bptest(modelo4)

ZZZ$valores_ajustados <- modelo4$fitted.values

# Crear el gráfico
ggplot(ZZZ, aes(x = valores_ajustados, y = residuales)) +
  geom_point(color = "dodgerblue") +      # Puntos de los residuos
  geom_hline(yintercept = c(-3.5,3.5), linetype = "dashed", color = "red") + # Línea en 0
  labs(x = "Valores Ajustados", y = "Residuos Estandarizados") +
  ggtitle("Gráfico de Residuos vs Valores Ajustados") +
  theme_minimal()

#  ES HOMOCEDASTICO

#INDEPENDENCIA

library(lmtest)
#bgtest(modelo4)
dwtest(modelo4)

ggplot(data = data.frame(index = seq_along(residuals(modelo4)), residuals = residuals(modelo4)), 
       aes(x = index, y = residuals)) +
  geom_point(color = "dodgerblue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Índice de observación", y = "Residuos", 
       title = "Gráfico de dispersión de residuos vs índice de observación") +
  theme_minimal()



'##################################################################################'

library(agricolae)

par(mfrow= c(1,2))
LSD.test(modelo4,"Almacenamiento", console = T,group = F)
plot(LSD.test(modelo4, "Almacenamiento"))

LSD.test(modelo4,"Archivo", console = T,group = F)
plot(LSD.test(modelo4, "Archivo"))


##Metodo Tukey
par(mfrow=c(1,2))
HSD.test(modelo4,"Almacenamiento",group=FALSE, console= TRUE)
plot(TukeyHSD(modelo4, "Almacenamiento"))

HSD.test(modelo4,"Archivo",group=FALSE, console= TRUE)
plot(TukeyHSD(modelo4, "Archivo"))


##Grafico de superficie 

# Modelo de regresión y superficie de respuesta
modelolm <- lm(Tiempo ~ Almacenamiento * Archivo, data=ZZZ1)
summary(modelolm)




# Sea a=Tiempo y b=Medio 
# ygorro = 29.62 + 4.96*a - 0.6250*b - 1.96*a*b

par(mfrow=c(1,2))
# Superficie de respuesta 
x     <-  seq(-1, 1, 0.1) # x=Tiempo
y     <-  seq(-1, 1, 0.1) # y=Medio
model <-  function (x, y){4.8332 - 2.2563*x + 1.0656*y - 1.3692*x*y}
z     <-  outer(x, y ,model)

# Superficie de respuesta : persp
persp(x,y,z, phi=30, theta=130, xlab="Almacenamiento", ylab="Archivo", 
      zlab="Tiempo", col = "lightblue", 
      expand=0.9, ticktype = "detailed")

# gr?fico de contornos : contour
contour(x,y,z,nlevels=30)

# Grafico con plotly
library(plotly)
fig <- plot_ly(x=x, y=y, z=z)
fig <- fig %>% add_surface()
fig

# Grafico con plotly
library(plotly)
fig <- plot_ly(x=x, y=y, z=z) %>% add_surface(
  contours = list(
    z = list(
      show=TRUE,
      usecolormap=TRUE,
      highlightcolor="#ff0000",
      project=list(z=TRUE)
    )
  )
)
fig <- fig %>% layout(
  scene = list(
    camera=list(
      eye = list(x=1.87, y=0.88, z=-0.64)
    )
  )
)

fig




####Punto 7####

library(pwr)

a <- 27
D <- 4.24
sd <- sd(ZZZ$Tiempo)
n <- 3

#Cargar librería que nos permite generar la curva de manera más precisa

library(pwr)

f <- sqrt((D^2)/(2*a*sd^2))
f

pwr.anova.test(f=0.1789176, k=5, power= 0.90, sig.level= 0.1)


#Ver potencia
#Fact ByC
a <-3
b <-3
D <-4
sd <-sd(ZZZ$Tiempo)

require(pwr2)

f.a <- sqrt((a*D^2)/(2*b*sd^2))

f.b <- sqrt((b*D^2)/(2*a*sd^2))

P <- pwr.2way(a=3, b=3, alpha = 0.05, size.A = 6, 
               size.B = 6, f.A = f.a, f.B = f.b)

P

#Ver numero de replicas

#f.a <- sqrt((a*D^2)/(2*b*sigma^2))
#f.b <- sqrt((b*D^2)/(2*a*sigma^2))

n <- ss.2way(a=3, b=3, alpha = 0.05, beta = 0.1, f.A = f.a, 
              f.B = f.b, B=100)
n


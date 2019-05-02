#########################################################
#### Ley de Okun
#########################################################
####Librerias: ----
library(vars)
library(dplyr)
library(readxl)
library(tidyverse)
library(stargazer)
library(stats)
library(ggthemes)
library(aTSA)
library(xtable)
options(scipen = 1000000) #Prevenir Notacion Cientifica
Sys.setlocale("LC_ALL", "es_ES.UTF-8") #Permitir Acentos

####Base de datos: ----
VAR_LEYOKUN <- read_excel("Documents/CEEY/VAR_LEYOKUN.xlsx")
View(VAR_LEYOKUN)

#Uso tasas desestacionalizadas
crecdes<- VAR_LEYOKUN %>% 
  select(`Tasa de desocupaci??n trimestral`,`Variaci??n anual`) 
View(crecdes)

#Cambiamos nombre de las variables del data frame 
colnames(crecdes) <- c("Desempleo","Crecimiento")
View(crecdes)

#Antes de correr el modelo es necesario observar si las series utilizadas son estacionarias o no:

####Test de estacionariedad: ----
#Test de estacionariedad para tasa de desocupacion trimestral. Cuanto mas negativo es, mas fuerte es el rechazo de la hipotesis nula de que existe una raiz unitaria para un cierto nivel de confianza.1 La hipotesis nula es que la serie no es estacionaria, es decir, quiero rechazar la hipotesis nula. La hip??tesis alternativa es que la serie es estacionaria. 
#Si p es mayor a .05, entonces, no puedo rechazar la hipotesis nula y asi mi serie no es estacionaria.
#ADF:Performs the Augmented Dickey-Fuller test for the null hypothesis of a unit root of a univarate time series x (equivalently, x is a non-stationary time series).

#Prueba parala serie de desempleo
testacionario<- stationary.test(crecdes$Desempleo, method = c("adf", "pp", "kpss"), nlag = NULL, type = c("Z_rho", "Z_tau"), lag.short = TRUE, output = TRUE)

xtable(testacionario$type1)
xtable(testacionario$type2)
xtable(testacionario$type3)


#Prueba para la serie de crecimiento
testacionario1<- stationary.test(crecdes$Crecimiento, method = c("adf", "pp", "kpss"), nlag = NULL, type = c("Z_rho", "Z_tau"), lag.short = TRUE, output = TRUE)

xtable(testacionario1$type1)
xtable(testacionario1$type2)
xtable(testacionario1$type3)

#Como veo que mi serie no es estacionaria, entomces procedo a hacer una primera diferencia a la serie. 


####Primera deferencia: ----

#Primera diferencia de serie de desempleo
desempleodif<- diff(crecdes$Desempleo,lag = 1,differences = 1)

#Primera diferencia de serie de crecimiento
crecimientodif<- diff(crecdes$Crecimiento,lag = 1,differences = 1)

#Nuevos test de estacionariedad:
#Si p es mayor a .05, entonces, no puedo rechazar la hipotesis nula y asi mi serie no es estacionaria. As??, si el p-value es menor a .05 entonces rechazo la hip??teis nula y mi serie es estacionaria. 

#Serie desempleo con primera diferencia
testacionario2<- stationary.test(desempleodif, method = c("adf", "pp", "kpss"), nlag = NULL, type = c("Z_rho", "Z_tau"), lag.short = TRUE, output = TRUE)

xtable(testacionario2$type1)
xtable(testacionario2$type2)
xtable(testacionario2$type3)

#Serie crecimiento con primera diferencia
testacionario3<- stationary.test(crecimientodif, method = c("adf", "pp", "kpss"), nlag = NULL, type = c("Z_rho", "Z_tau"), lag.short = TRUE, output = TRUE)

xtable(testacionario3$type1)
xtable(testacionario3$type2)
xtable(testacionario3$type3)

#Claramente puedo pbservar que una vez hecha la primera diferencia mis series son estacionarias!!! Ahora armamos una data frame con mis dos series diferenciadas:

desempleodif
crecimientodif

desempleodif_name <- "difDesempleo"
crecimientodif_name <- "difCrecimiento"

df <- data.frame(desempleodif,crecimientodif)
names(df) <- c(desempleodif_name, crecimientodif_name)
print(df)



####Modelo VAR: ----
#Corremos el modelo Var 


l_okun<- VAR(df,p=1,type = "both",season = NULL,exogen = NULL,lag.max=7, ic=c("AIC"))

#var results
l_okun
View(l_okun)
stargazer(l_okun$varresult)


####Funciones Impulso-Respuesta: ----
imp_resp<- irf(l_okun, impulse = NULL, response = NULL, n.ahead = 10,ortho = TRUE, cumulative = FALSE, boot = TRUE, ci = 0.95,runs = 100, seed = NULL)

imp_resp#impulse responses.
View(imp_resp)
stargazer(imp_resp$irf)


resokun<- summary(l_okun)
View(resokun)

xtable(resokun$varresult$difDesempleo)
xtable(resokun$varresult$difCrecimiento)

xtable(imp_resp$irf$difDesempleo)
impcreci<- xtable(imp_resp$irf$difCrecimiento)
View(impcreci)

periods <- c(1,2,3,4,5,6,7,8,9,10,11)

plot(periods,impcreci$difDesempleo)
plot(periods,impcreci$difCrecimiento)


#as.character(periods)

#FIR Crecimiento-Desempleo
impcreci%>%
  ggplot(aes(periods, impcreci$difDesempleo)) + geom_point(na.rm = TRUE) +
  xlab("Periodos") + ylab("Tasa de desempleo") +
  labs(title = "FIR (Crecimiento-Desempleo)",
       subtitle = element_blank(),
       x = "Periodos",
       y = "Tasa de desempleo",
       colour = element_blank(),
       size = element_blank(),
       caption = "Fuente: Elaboracion propia con datos del INEGI.") +
  scale_x_continuous(breaks=1:11)+
  theme(axis.text.x = element_text(angle = 0))

#FIR Desempleo-Crecimiento
impcreci%>%
  ggplot(aes(periods, impcreci$difCrecimiento)) + geom_point(na.rm = TRUE) +
  xlab("Periodos") + ylab("Tasa de crecimiento") +
  labs(title = "FIR (Desempleo-Crecimiento)",
       subtitle = element_blank(),
       x = "Periodos",
       y = "Tasa de crecimiento",
       colour = element_blank(),
       size = element_blank(),
       caption = "Fuente: Elaboracion propia con datos del INEGI.") +
  scale_x_continuous(breaks=1:11)+
  theme(axis.text.x = element_text(angle = 0))

###Example with c.i. ----
impcreci%>%
  ggplot(aes(periods, impcreci$Variaci??n.anual)) + geom_line(na.rm = TRUE) +
  geom_smooth(mapping = aes(periods, impcreci$Variaci??n.anual), alpha = 0.3) + 
  xlab("Periodos") + ylab("Tasa de crecimiento") +
  labs(title = "FIR (Desempleo-Crecimiento)",
       subtitle = element_blank(),
       x = "Periodos",
       y = "Tasa de crecimiento",
       colour = element_blank(),
       size = element_blank(),
       caption = "Fuente: Elaboracion propia con datos del INEGI.") +
  scale_x_continuous(breaks=1:11)+
  theme(axis.text.x = element_text(angle = 0))



#### Causalidad de Granger

gtest1<- grangertest(`Variaci??n anual` ~`Tasa de desocupaci??n trimestral` , order = 7, data = crecdes)
gtest2<- grangertest(`Tasa de desocupaci??n trimestral` ~`Variaci??n anual` , order = 7, data = crecdes)


stargazer(gtest1)
stargazer(gtest2)
periods <- c(1:70)
table(VAR_LEYOKUN$`Variaci??n anual`)

plot(VAR_LEYOKUN$`Variaci??n anual`,type="l")

VAR_LEYOKUN%>%
  ggplot(aes(periods, VAR_LEYOKUN$Variaci??n.anual)) + geom_line(na.rm = TRUE) +
  xlab("Periodos") + ylab("Tasa de crecimiento") +
  labs(title = ,
       subtitle = element_blank(),
       x = "Periodos",
       y = "Tasa de crecimiento",
       colour = element_blank(),
       size = element_blank(),
       caption = "Fuente: Elaboracion propia con datos del INEGI.") +
  theme(axis.text.x = element_text(angle = 0))

#Veamos con el modelo VAR- ERROR
#stationary.test(l_okun, method = c("adf", "pp", "kpss"), nlag = NULL, #type = c("Z_rho", "Z_tau"), lag.short = 7, output = TRUE)


####Example with bars ----
#impcreci %>% 
#  ggplot() + 
#  geom_bar(mapping = aes(x = periods, y = impcreci$Variaci??n.anual),
#           stat = "identity") +
#  xlab("Periodo") + ylab("Tasa de crecimiento") +
#  labs(title = "FIR (Desempleo-Crecimiento)",
#       subtitle = element_blank(),
#       x = "Periodo",
#       y = "Tasa de crecimiento",
#       colour = element_blank(),
#       size = element_blank(),
#       caption = "Fuente: Elaboracion propia con datos del INEGI.") +
#  theme(axis.text.x = element_text(angle = 0))



## Versi??n 6
#cemabe_df %>%
#  filter(P166 < 4000, P167 < 4000) %>%
#  ggplot() +
#  geom_point(mapping = aes(x = P167, y = P166), alpha = 0.3) +
#  geom_smooth(mapping = aes(x = P167, y = P166))    # A??adir geom





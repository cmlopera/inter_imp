# Carga la librería para trabajo en paralelo
#if(!require(snowfall)){
#  install.packages('snowfall')
#  require(snowfall)
#}

# Carga librería survival
if(!require(survival)){
  install.packages('survival')
  require(survival)
}

# Carga librería Icens
source("https://bioconductor.org/biocLite.R")
biocLite()
biocLite("Icens")

# Carga librería interval
if(!require(interval)){
  install.packages('interval')
  require(interval)
}

# inicia el procesamiento en 8 procesadores en paralelo
# sfInit(parallel=TRUE, cpus=8, type="SOCK")

# carga las funciones para la simulación
# sfS
source("FUN.R")

######### inicio de parámetros de simulación #######
# tamaño de muestra
n<-100
# Simulaciones
s<-500
# Visitas
v<-6
# tiempo entre visitas
len<-0.25
# asistencia a visitas
asist<-F
# nivel de significancia
alfa<-0.05
# vecinos cercanos
vc<-10
######### fin de parámetros de simulación    #######

# generación de datasets para s simulaciones de n individuos con 6 visitas posibles
# set.seed(1234)
datos<-lapply(1:s,gendata,n,v,len,asist)
med<-median(do.call(mapply, c(cbind, datos))[,4])
estFO<-as.data.frame(matrix(unlist(lapply(datos,estim,med)),byrow=T,ncol=9));names(estFO)<-c("FO_ES","FO_SE","FO_CR","PO_ES","PO_SE","PO_CR","NPMLEI_ES","NPMLEI_SE","NPMLEI_CR");head(estFO)
colMeans(estFO)

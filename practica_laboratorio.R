library(openxlsx)
datos <- read.xlsx(file.choose(), sheet="aeusp")
#Sexo
Sexo=datos[ ,3]
Sexo
y=factor(Sexo,levels=c(1,2),labels=c("Masculino","Femenino"))
head(y) # muestra el panorama de los datos
fi=table(y)
hi = prop.table(fi)
pi= prop.table(fi)*100
tabla = cbind(fi,hi,pi)
tabla
# Grafico de barras
# Frecuencia Absoluta
barplot(fi)
bp=barplot(fi,main = "aspectos socioeconómicos y culturales de comunidades \nde bajos ingresos en la región de Butantã, São Paulo",col=c("gray","orange"), names=c("Masculino","Femenino"),ylim=c(0,max(fi)*1.1))
text(x=bp, y=fi, labels=fi, pos=3)
# Frecuencia relativa Percentual
porcentaje<-c(43.8961,56.1039)
etiqueta<-c(paste(porcentaje,"%",sep=""))
colores<-c("gray","orange")
pie(porcentaje,labels=etiqueta,cloCkwise=TRUE,col=c("gray","orange"), main = "Diagrama circular aspectos socioeconómicos y culturales de \ncomunidades de bajos ingresos en la región de Butantã, São Paulo")
legend("bottomright",c("Masculino", "Femenino"), cex=0.6,fill=colores)

#Nro_habitantes
Resid=datos[ ,8]
Resid
residentes <- c(1,2,3,4,5,6,7,8,9,10)
frec_abs <- table(residentes)
frec_rel <- prop.table(frec_abs)
frec_porc <- prop.table(frec_abs) * 100
# Crear un data.frame con los resultados
resultados <- data.frame(Art_defectuosos = names(frec_abs),
                         fi = as.numeric(frec_abs),
                         hi = as.numeric(frec_rel),
                         pi = as.numeric(frec_porc))
# Imprimir la tabla de resultados
print(resultados)
# Ahora hallamos la misma tabla pero con sus frecuencias acumuladas
# Datos
residentes <- c(1,2,3,4,5,6,7,8,9,10)
# Crear tabla de frecuencias absolutas
tabla_frecuencias <- as.data.frame(table(residentes))
colnames(tabla_frecuencias) <- c("nro_residentes", "fi")
# Calcular frecuencia relativa
tabla_frecuencias$hi <- tabla_frecuencias$fi / sum(tabla_frecuencias$fi)
# Calcular frecuencia porcentual
tabla_frecuencias$pi <- tabla_frecuencias$hi * 100
# Calcular frecuencias acumuladas
tabla_frecuencias$Fi <- cumsum(tabla_frecuencias$fi)
tabla_frecuencias$Hi<- cumsum(tabla_frecuencias$hi)
tabla_frecuencias$Pi <- cumsum(tabla_frecuencias$pi)
# Mostrar tabla final
print(tabla_frecuencias)
#GRAFICOS
# Frecuencia absoluta: Grafico de barras
barplot(pi)
bp=barplot(table(residentes),main="Número de residentes",xlab = "Número de residencias", ylab="Número de personas",col=c("green","pink","red","blue","black","yellow","purple","brown","white","gray"), border = "black",ylim=c(0,max(frec_abs)*1.1))
text(x=bp, y=frec_abs, labels=frec_abs, pos=3)


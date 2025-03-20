x <- c(1,1,4,3,3,3,2,2,4,2,2,1,4,2,3,2,3,4,2,3) # data

y=factor(x,levels=c(1,2,3,4),labels=c("Sin estudios","Estudios primarios", "Es
tudios secundarios", "Estudios superiores"))
head(y) # muestra el panorama de los datos
fi=table(y)
hi = prop.table(fi)
pi= prop.table(fi)*100
tabla = cbind(fi,hi,pi)
tabla
# Grafico de barras
# Frecuencia Absoluta
barplot(fi)
bp=barplot(fi,main = "Nivel de Estudio de los trabajadores",col=c("green","pink","red","blue"), names=c("Sin estudios","Estudios primarios", "Estudios secundarios", "Estudios superiores"),ylim=c(0,max(fi)*1.1))
text(x=bp, y=fi, labels=fi, pos=3)
# Frecuencia relativa Percentual
porcentaje<-c(15,35,30,20)
etiqueta<-c(paste(porcentaje,"%",sep=""))
colores<-c("green","pink","red","blue")
pie(porcentaje,labels=etiqueta,cloCkwise=TRUE,col=c("green","pink","red","blue"), main = "Diagrama circular del Nivel de Estudio de los trabajadores")
legend("bottomright",c("Sin estudios", "Estudios primarios", "Estudios Secundarios", "Estudios superiores"), cex=0.6,fill=colores)
#puede moverse de ubicación la leyenda
legend("topright",c("Sin estudios", "Estudios primarios", "Estudios Secundarios", "Estudios superiores"), cex=0.6,fill=colores)

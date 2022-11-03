
"  •	Desarrolla un algoritmo de mechas
Para cada activo, cada día, compra a precio de apertura y vende cuando ocurra el primero de los siguientes eventos: 
  El activo sube 3 céntimos (stop profit)
El activo cae 10 céntimos (stop loss)
Si no ocurre ninguno de los anteriores, vende a precio de cierre

Ojo, habrá días positivos y negativos a la vez, en estos casos, supón que toca primero el stop loss

El capital que invertimos en cada activo, cada día, debe ser 30.000 €
La comisión de compra será de 0.0003 * capital. Lo mismo para la venta.
Comprueba que tienes al menos 30 datos para hacer los cálculos (si no es así descarta el activo)

Entregable: Código que genere un dataframe con la siguiente estructura (para todos los activos):

"
#rm(list = ls())

install.packages('shiny')
library(shiny)

setwd("C:/Users/david/OneDrive/Escritorio/Enunciado de la práctica de R")
ibex<-read.table("Ibex_data.csv ",header=T, sep=",",stringsAsFactors = F,dec=".")

benefdia<-0  #beneficio diario
benefacum<-0 #beneficio acumulado por empresa
positivos<-0 #número de dias positivos por empresa
negativos<-0 #número de días negativos por empresa
nºdatos<-0   #número de datos totales
n<-0         #numero de datos empresa
horqsup<-0   
horqinf<-0

variables<- c("Bº medio por operación","Beneficio acumulado","% días positivos","%días negativos", "Horquilla superior media","Horquilla inferior media", "Número de operaciones")
result<-data.frame(variables=variables)  #dataframe resultado
graficos<-list()  #lista para gráficos
indexgraf<-1      #índice para crear gráficos
dataframe<-FALSE  #indica si se ha creado para ese activo una columna
empresas<-list()  #lista con nombres de empresas
for (i in   1:length(ibex[,1]) ){
  
  empresa1=ibex$X[i]
  if(i<length(ibex$X)){
    empresa2=ibex$X[i+1]
  }
  else{
    empresa2="ultima"
  }
  
  nºacc=trunc(30000*0.9997/ibex$open[i])
  nºaccdec=30000*0.9997/ibex$open[i]
  resto=(nºaccdec-nºacc)*ibex$open[i]
  horqsup=horqsup+(ibex$high[i]-ibex$open[i])
  horqinf=horqinf+(ibex$open[i]-ibex$low[i])
  
  if(ibex$open[i]-ibex$low[i]>=0.1){
  
    benefdia=nºacc*(ibex$open[i]-0.1)-30000*0.0003-30000+resto
    benefacum=benefacum+benefdia
  
    
  }
  
  else if(ibex$high[i]-ibex$open[i]>=0.03){
    
    benefdia=nºacc*(ibex$open[i]+0.03)-30000*0.0003-30000+resto
    benefacum=benefacum+benefdia
    
  }
  
  else{
    
    benefdia=nºacc*(ibex$close[i])-30000*0.0003-30000+resto
    benefacum=benefacum+benefdia
  }
  
  if(benefdia>0){
    positivos=positivos+1
    }
    
  else{
    negativos=negativos+1
    }
  
  
  nºdatos=nºdatos+1
  n=n+1
  
  if(!dataframe){
    dataframe=TRUE
    df<-data.frame(empresa1=benefacum)
    colnames(df)[1]=empresa1 
    graficos[[indexgraf]]=df
    
  }
  else{
    
    graficos[[indexgraf]]<-rbind(graficos[[indexgraf]],benefacum)
    
  }
  

  if(empresa1!=empresa2){
    
    #Comprobar si tiene al menos 30 datos
    if(n>=30){
      dataframe=FALSE
      result<-data.frame(result,empresa1=c(benefacum/n,benefacum,positivos/n,negativos/n,horqsup/n,horqinf/n,n))
      colnames(result)[length(result)]=empresa1 
      indexgraf=indexgraf+1
      empresas[length(empresas)+1]=empresa1
    }
    positivos=0
    negativos=0
    benefacum<-0
    n=0
    horqinf=0
    horqsup=0
  }

}    

result

for(i in 1:length(empresas)){
  y<-graficos[[i]]
  x=length(y)
  y<-y[,1]
  x=length(y)
  plot(x=c(1:x),y,type="l",main=empresas[i])
  
}

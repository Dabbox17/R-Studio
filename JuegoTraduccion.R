"El Excel Palabras.xlsx contiene palabras en inglés y su traducción
el objetivo del ejercicio es hacer un “juego” en donde se elija al azar una palabra en inglés y se dé al usuario 3 posibles traducciones en español (y el juego contrario).
"
install.packages("readxl")
library(readxl)

setwd("D:/Users/Usuario/Downloads")
getwd()
datos<-read_excel("Palabras.xlsx")
datos<-rbind(datos,c("abroad","en el extranjero"))
colnames(datos)=c("ingles","español")

rondas<-10
rondas<- as.integer(readline(prompt="Introduzca numero de rondas: "))
aciertos<-0
errores<-0
correcto<-FALSE

while(!correcto){
  juego <- readline(prompt="Introduzca inglés si quiere traducir palabras al español o español si quiere traducir palabras al inglés: ")
  if(juego=="español" || juego=="inglés"){
    correcto=TRUE
  }
}


if(juego=="español"){
  while(rondas>0){ 
    rondas=rondas-1
    indsol=sample(1:dim(datos)[1],1)
    indice2=sample(1:dim(datos)[1],1)
    indice3=sample(1:dim(datos)[1],1)
    lista<- c(indsol,indice2,indice3)
    lista<-sample(lista)
    pregunta<-paste("Indica la opción correcta ( escribe '1','2' o '3') con la traducción de la siguiente palabra: ",datos[indsol,2])
    print(pregunta)
    respuesta<-readline(prompt=paste("Opcion 1:",datos[lista[1],1],"Opcion 2:",datos[lista[2],1],"Opcion 3:",datos[lista[3],1],"-->"))
    
    if (datos[lista[as.integer(respuesta)],1]==datos[indsol,1]){
      aciertos=aciertos+1
      
    }else{
      errores=errores+1
      
    }
  }
}else if(juego=="inglés"){
  while(rondas>0){   
      rondas=rondas-1
      indsol=sample(1:dim(datos)[1],1)
      indice2=sample(1:dim(datos)[1],1)
      indice3=sample(1:dim(datos)[1],1)
      lista<- c(indsol,indice2,indice3)
      lista<-sample(lista)
      pregunta<-paste("Indica la opción correcta ( escribe '1','2' o '3') con la traducción de la siguiente palabra: ",datos[indsol,1])
      print(pregunta)
      respuesta<-readline(prompt=paste("Opcion1:",datos[lista[1],2],"Opcion 2:",datos[lista[2],2],"Opcion 3:",datos[lista[3],2],"-->"))
  
      if (datos[lista[as.integer(respuesta)],2]==datos[indsol,2]){
        aciertos=aciertos+1
        
      
      }else{
        errores=errores+1
        
      }
  }
}

print("Tus resultados")
print(paste("Número de aciertos: ",aciertos))
print(paste("Número de errores: ",errores))
print(paste("Porcentaje de aciertos",aciertos/(errores+aciertos)*100,"%"))


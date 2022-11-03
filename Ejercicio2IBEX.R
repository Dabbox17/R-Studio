

"•	Partiendo del algoritmo de mechas anterior, añade el parámetro price_departure.
Para cada activo, cada día, si el price_departure es >= 0.75, compra a precio de apertura y vende cuando ocurra el primer de los siguientes eventos:
El activo sube 3 céntimos (stop profit)
El activo cae 10 céntimos (stop loss)
Si no ocurre ninguno de los anteriores, vende a precio de cierre

Ojo, habrá días positivos y negativos a la vez, en estos casos, supón que toca primero el stop loss

El capital que invertimos en cada activo, cada día, debe ser 30.000 €
La comisión de cada compra y venta será de 0.0003 * capital
Homogeneiza los datos Ibex_data y price_departure (utiliza solo las fechas que existan en ambos DF)
Comprueba que tienes al menos 30 datos para hacer los cálculos, antes de aplicar el filtro del price_departure (si no es así descarta el activo)

Entregable: Código que genere el mismo dataframe que en el ejercicio anterior y los mismos gráficos.

"


ibex <-
  read.table(
    "Ibex_data.csv ",
    header = T,
    sep = ",",
    stringsAsFactors = F,
    dec = "."
  )

lista <- ibex$X
price_departure <-
  read.table(
    "price_departures.csv ",
    header = T,
    sep = ",",
    stringsAsFactors = F,
    dec = "."
  )

benefdia <- 0  #beneficio diario
benefacum <- 0 #beneficio acumulado por empresa
positivos <- 0 #número de dias positivos por empresa
negativos <- 0 #número de días negativos por empresa
nºdatos <- 0   #número de datos totales
n <- 0         #numero de datos con price_departure>=0.75
num <- 0       #numero de datos empresa
horqsup <- 0
horqinf <- 0

variables <-
  c(
    "Bº medio por operación",
    "Beneficio acumulado",
    "% días positivos",
    "%días negativos",
    "Horquilla superior media",
    "Horquilla inferior media",
    "Número de operaciones"
  )
result <- data.frame(variables = variables)

graficos <- list()  #lista para gráficos
indexgraf <- 1      #índice para crear gráficos
dataframe <-
  FALSE  #indica si se ha creado para ese activo una columna
empresas <- list()  #lista con nombres de empresas


for (i in   1:length(ibex[, 1])) {
  empresa1 = ibex$X[i]
  if (i < length(ibex$X)) {
    empresa2 = ibex$X[i + 1]
  }
  else{
    empresa2 = "ultima"
  }
  
  nºacc = 30000 * 0.9997 / ibex$open[i]
  horqsup = horqsup + (ibex$high[i] - ibex$open[i])
  horqinf = horqinf + (ibex$open[i] - ibex$low[i])
  
  indprice <- match(ibex$X.1[i], price_departure$X)
  if (!is.na(price_departure[indprice, empresa1])) {
    num = num + 1
  }
  
  if (!is.na(price_departure[indprice, empresa1]) &&
      price_departure[indprice, empresa1] >= 0.75) {
    n = n + 1
    
    if (ibex$open[i] - ibex$low[i] >= 0.1) {
      benefdia = nºacc * (ibex$open[i] - 0.1) * 0.9997 - 30000
      benefacum = benefacum + benefdia
      
      
    }
    
    else if (ibex$high[i] - ibex$open[i] >= 0.03) {
      benefdia = nºacc * (ibex$open[i] + 0.03) * 0.9997 - 30000
      benefacum = benefacum + benefdia
      
    }
    
    else{
      benefdia = nºacc * (ibex$close[i]) * 0.9997 - 30000
      benefacum = benefacum + benefdia
    }
    
    if (benefdia > 0) {
      positivos = positivos + 1
    }
    
    else{
      negativos = negativos + 1
    }
    
    
    nºdatos = nºdatos + 1
    
    
    if (!dataframe) {
      dataframe = TRUE
      df <- data.frame(empresa1 = benefacum)
      colnames(df)[1] = empresa1
      graficos[[indexgraf]] = df
      
    }
    else{
      graficos[[indexgraf]] <- rbind(graficos[[indexgraf]], benefacum)
      
    }
    
  }
  
  if (empresa1 != empresa2) {
    #Comprobar si tiene al menos 30 datos
    
    if (num >= 30) {
      dataframe = FALSE
      result <-
        data.frame(
          result,
          empresa1 = c(
            benefacum / n,
            benefacum,
            positivos / n,
            negativos / n,
            horqsup / n,
            horqinf / n,
            n
          )
        )
      colnames(result)[length(result)] = empresa1
      #print(result)
      indexgraf = indexgraf + 1
      empresas[length(empresas) + 1] = empresa1
    }
    positivos = 0
    negativos = 0
    benefacum <- 0
    n = 0
    horqinf = 0
    horqsup = 0
  }
  
  
  
}

result

for (i in 1:length(empresas)) {
  y <- graficos[[i]]
  x = length(y)
  y <- y[, 1]
  x = length(y)
  plot(x = c(1:x),
       y,
       type = "l",
       main = empresas[i])
  
}


# ENTREGA PRACTICA 3 ------------------------------------------------------

setwd("~/PracticasLabo/Entrega_P3/Info_estaciones") #Setea tu propio directorio 
                                                     #de trabajo donde tengas 
                                                      #los archivos con la info

### 1 ###

rm(list=ls())

#Leo los archivos (Los archivos para descargar estan en la carpeta 
#"Info_estaciones" del directorio Entrega_P3 de Github)
datos.azul = read.table("AZUL.txt")
datos.aeroparque = read.table("AEROPARQUE.txt")
datos.catamarca = read.table("CATAMARCA.txt")
datos.chilecito = read.table("CHILECITO.txt")
datos.iguazu = read.table("IGUAZU.txt")
datos.mendoza = read.table("MENDOZA.txt")
info = read.table("estaciones.txt")


#Agrego los datos de Mendoza a mano
info.mendoza = c("MENDOZA",-32.5,-68.5,1070) #Los busque en Google
info[6,] = info.mendoza


#Convierto las temperaturas a grados centigrados y los datos faltantes a NA
datos.azul[,3:4] = (datos.azul[,3:4]-32)*(5/9)
datos.azul[datos.azul==9999.9] = NA

datos.aeroparque[,3:4] = (datos.aeroparque[,3:4]-32)*(5/9)
datos.aeroparque[datos.aeroparque==9999.9] = NA

datos.catamarca[,3:4] = (datos.catamarca[,3:4]-32)*(5/9)
datos.catamarca[datos.catamarca==9999.9] = NA

datos.chilecito[,3:4] = (datos.chilecito[,3:4]-32)*(5/9)
datos.chilecito[datos.chilecito==9999.9] = NA

datos.iguazu[,3:4] = (datos.iguazu[,3:4]-32)*(5/9)
datos.iguazu[datos.iguazu==9999.9] = NA

datos.mendoza[,3:4] = (datos.mendoza[,3:4]-32)*(5/9)
datos.mendoza[datos.mendoza==9999.9] = NA


#Hago una lista para cada estacion
lista.azul = list("Nombre"="AZUL", "Lat"=info[1,2], "Lon"=info[1,3],
                  "Altura"=info[1,4], "Cod"=datos.azul[[1]][1],
                  "Dia"=datos.azul[,2], "Temp"=datos.azul[,3],
                  "Temp_ro"=datos.azul[,4], "Presion"=datos.azul[,5])
lista.aeroparque = list("Nombre"="AEROPARQUE", "Lat"=info[2,2], "Lon"=info[2,3],
                        "Altura"=info[2,4], "Cod"=datos.aeroparque[[1]][1],
                        "Dia"=datos.aeroparque[,2], "Temp"=datos.aeroparque[,3],
                        "Temp_ro"=datos.aeroparque[,4], "Presion"=datos.aeroparque[,5])
lista.catamarca = list("Nombre"="CATAMARCA", "Lat"=info[3,2], "Lon"=info[3,3],
                       "Altura"=info[3,4], "Cod"=datos.catamarca[[1]][1],
                       "Dia"=datos.catamarca[,2], "Temp"=datos.catamarca[,3],
                       "Temp_ro"=datos.catamarca[,4], "Presion"=datos.catamarca[,5])
lista.chilecito = list("Nombre"="CHILECITO", "Lat"=info[4,2], "Lon"=info[4,3],
                       "Altura"=info[4,4], "Cod"=datos.chilecito[[1]][1],
                       "Dia"=datos.chilecito[,2], "Temp"=datos.chilecito[,3],
                       "Temp_ro"=datos.chilecito[,4], "Presion"=datos.chilecito[,5])
lista.iguazu = list("Nombre"="IGUAZU", "Lat"=info[5,2], "Lon"=info[5,3],
                    "Altura"=info[5,4], "Cod"=datos.iguazu[[1]][1],
                    "Dia"=datos.iguazu[,2], "Temp"=datos.iguazu[,3],
                    "Temp_ro"=datos.iguazu[,4], "Presion"=datos.iguazu[,5])
lista.mendoza = list("Nombre"="MENDOZA", "Lat"=info[6,2], "Lon"=info[6,3],
                     "Altura"=info[6,4], "Cod"=datos.mendoza[[1]][1],
                     "Dia"=datos.mendoza[,2], "Temp"=datos.mendoza[,3],
                     "Temp_ro"=datos.mendoza[,4], "Presion"=datos.mendoza[,5])


#Armo el array de listas
estaciones = list(lista.azul, lista.aeroparque, lista.catamarca, lista.chilecito,
                  lista.iguazu, lista.mendoza)


### 2 ###

#I
#Funcion que da el resumen de varias series de datos
resumen = function(x) { #Arme un array, en vez de un data.frame, me parecio 
                         #mas comodo
 informacion = array(list(),dim=c(length(x),16))
 colnames(informacion) = c("Estacion","Cantidad de datos","Temperatura media",
                            "Temperatura de rocio media","Presion media",
                            "Desvio de la temperatura", "Desvio de la 
                            temperatura de rocio","Desvio de la presion",
                            "Temperatura maxima","Temperatura de rocio maxima",
                            "Presion maxima","Temperatura minima",
                            "Temperatura de rocio minima","Presion minima",
                            "Inicio","Final")
 for (i in 1:length(x)) {
    estacion = x[[i]]
    nombre = estacion$Nombre
    cantdatos = length(estacion$Dia)
    mediatemp = round(mean(estacion$Temp, na.rm = T),2)
    mediarocio = round(mean(estacion$Temp_ro, na.rm = T),2)
    mediapresion = round(mean(estacion$Presion, na.rm = T),2)
    dstemp = round(sd(estacion$Temp, na.rm = T),2)
    dsrocio = round(sd(estacion$Temp_ro, na.rm = T),2)
    dspresion = round(sd(estacion$Presion, na.rm = T),2)
    maxtemp = round(max(estacion$Temp, na.rm = T),2)
    maxrocio = round(max(estacion$Temp_ro, na.rm = T),2)
    maxpresion = round(max(estacion$Presion, na.rm = T),2)
    mintemp = round(min(estacion$Temp, na.rm = T),2)
    minrocio = round(min(estacion$Temp_ro, na.rm = T),2)
    minpresion = round(min(estacion$Presion, na.rm = T),2)
    inicio = estacion$Dia[1]
    final = estacion$Dia[length(estacion$Dia)]
    informacion[i,] = c(nombre, cantdatos, mediatemp, mediarocio,
                        mediapresion, dstemp, dsrocio, dspresion, maxtemp,
                        maxrocio, maxpresion, mintemp, minrocio, minpresion,
                        inicio, final)
  }
  return(informacion)
}

resumen.estaciones = resumen(estaciones) #Salta un Warning porque Chilecito 
                                          #tiene todos datos faltantes en la 
                                           #presion


#II
localizacion_de_estaciones = function(x,latmin,latmax,lonmin,lonmax) {
  for (i in 1:length(x)) {
    estacion = x[[i]]
    if (as.numeric(estacion$Lat)>=latmin & as.numeric(estacion$Lat)<=latmax & 
        as.numeric(estacion$Lon)>=lonmin & as.numeric(estacion$Lon)<=lonmax) {
      print(paste("La estacion",estacion$Nombre,"se encuentra en esta region"))
    } else {
      print(paste("La estacion",estacion$Nombre,"no se encuentra en esta region"))
    }
  }
}

localizacion_de_estaciones(estaciones,-70,-20,-70,-20)
localizacion_de_estaciones(estaciones,30,60,30,60)


#III
guardar.archivo = function(x,nombre_archivo) {
  save(x, file = nombre_archivo)
}

guardar.archivo(estaciones,"Datos_Estaciones.Rdata")






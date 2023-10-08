
# ENTREGA PRACTICA 3 ------------------------------------------------------

setwd("~/PracticasLabo/Entrega_P3") #Setea tu propio directorio de trabajo 
                                     #donde tengas los archivos con la info

### 1 ###

rm(list=ls())

#Leo los archivos
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
lista.azul = list("AZUL", info[1,2], info[1,3], info[1,4], datos.azul[[1]][1],
                  datos.azul[,2],datos.azul[,3], datos.azul[,4], datos.azul[,5])
lista.aeroparque = list("AEROPARQUE", info[2,2], info[2,3], info[2,4],
                        datos.aeroparque[[1]][1], datos.aeroparque[,2], 
                        datos.aeroparque[,3], datos.aeroparque[,4], 
                        datos.aeroparque[,5])
lista.catamarca = list("CATAMARCA", info[3,2], info[3,3], info[3,4],
                      datos.catamarca[[1]][1], datos.catamarca[,2],
                      datos.catamarca[,3], datos.catamarca[,4],
                      datos.catamarca[,5])
lista.chilecito = list("CHILECITO", info[4,2], info[4,3], info[4,4],
                       datos.chilecito[[1]][1], datos.chilecito[,2], 
                       datos.chilecito[,3], datos.chilecito[,4],
                       datos.chilecito[,5])
lista.iguazu = list("IGUAZU", info[5,2], info[5,3], info[5,4],
                    datos.iguazu[[1]][1], datos.iguazu[,2], datos.iguazu[,3],
                    datos.iguazu[,4], datos.iguazu[,5])
lista.mendoza = list("MENDOZA", info[6,2], info[6,3], info[6,4],
                     datos.mendoza[[1]][1], datos.mendoza[,2],
                     datos.mendoza[,3], datos.mendoza[,4], datos.mendoza[,5])


#Armo el array de listas
estaciones = array(list(),dim=c(6,9))
colnames(estaciones) = c("Nombre", "Latitud", "Longitud", "Altura", "Cod.id",
                         "Dia","Temp", "Temp.rocio", "Presion")
estaciones[1,] = lista.azul
estaciones[2,] = lista.aeroparque
estaciones[3,] = lista.catamarca
estaciones[4,] = lista.chilecito
estaciones[5,] = lista.iguazu
estaciones[6,] = lista.mendoza



### 2 ###

#I
#Funcion que da el resumen de varias series de datos
resumen = function(x) { #Arme un array, en vez de un data.frame, me parecio 
                         #mas comodo
  informacion = array(list(),dim=c(nrow(x),16)) 
  colnames(informacion) = c("Estacion","Cantidad de datos","Temperatura media",
                            "Temperatura de rocio media","Presion media",
                            "Desvio de la temperatura", "Desvio de la 
                            temperatura de rocio","Desvio de la presion",
                            "Temperatura maxima","Temperatura de rocio maxima",
                            "Presion maxima","Temperatura minima",
                            "Temperatura de rocio minima","Presion minima",
                            "Inicio","Final") 
 for (i in 1:nrow(x)) {
    estacion = x[[i,"Nombre"]]
    cantdatos = length(x[[i,"Dia"]])
    mediatemp = mean(x[[i,"Temp"]], na.rm = T)
    mediarocio = mean(x[[i,"Temp.rocio"]], na.rm = T)
    mediapresion = mean(x[[i,"Presion"]], na.rm = T)
    dstemp = sd(x[[i,"Temp"]], na.rm = T)
    dsrocio = sd(x[[i,"Temp.rocio"]], na.rm = T)
    dspresion = sd(x[[i,"Presion"]], na.rm = T)
    maxtemp = max(x[[i,"Temp"]], na.rm = T)
    maxrocio = max(x[[i,"Temp.rocio"]], na.rm = T)
    maxpresion = max(x[[i,"Presion"]], na.rm = T)
    mintemp = min(x[[i,"Temp"]], na.rm = T)
    minrocio = min(x[[i,"Temp.rocio"]], na.rm = T)
    minpresion = min(x[[i,"Presion"]], na.rm = T)
    inicio = x[[i,"Dia"]][1]
    final = x[[i,"Dia"]][length(estaciones[[i,"Dia"]])]
    informacion[i,] = c(estacion, cantdatos, mediatemp, mediarocio,
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
localizacion_de_estaciones = function(latmin,latmax,lonmin,lonmax) {
  if (-36.8>=latmin & -36.8<=latmax & -59.9>=lonmin & -59.9<=lonmax) {
    print("La estacion de Azul se encuentra en esta region")
    if (-34.6>=latmin & -34.6<=latmax & -58.4>=lonmin & -58.4<=lonmax) {
      print("La estacion de Aeroparque se encuentra en esta region")
    } 
    if (-28.6>=latmin & -28.6<=latmax & -65.8>=lonmin & -65.8<=lonmax) {
      print("La estacion de Catamarca se encuentra en esta region")
    } 
    if (-29.2>=latmin & -29.2<=latmax & -67.4>=lonmin & -67.4<=lonmax) {
      print("La estacion de Chilecito se encuentra en esta region")
    }
    if (-25.7>=latmin & -25.7<=latmax & -54.5>=lonmin & -54.5<=lonmax) {
      print("La estacion de Iguazu se encuentra en esta region")
    } 
    if (-32.5>=latmin & -32.5<=latmax & -68.5>=lonmin & -68.5<=lonmax) {
      print("La estacion de Mendoza se encuentra en esta region")
    }
  } else {print("No hay estaciones cercanas")}
}

localizacion_de_estaciones(-70,-20,-70,-20)
localizacion_de_estaciones(30,60,30,60)


#III
guardar.archivo = function(x,nombre_archivo) {
  save(x, file = nombre_archivo)
}

guardar.archivo(estaciones,"Datos_Estaciones.Rdata")



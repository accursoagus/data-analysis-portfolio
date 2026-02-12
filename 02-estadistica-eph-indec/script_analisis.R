install.packages("eph")
install.packages("ggplot2")
install.packages("qcc")
install.packages("dplyr")
install.packages("DescTools")
library(eph)
library(dplyr)
library(ggplot2)
library(qcc)
library(DescTools)

#armamos el dataframe
microdata<-get_microdata(year=2024,trimester=1,type="individual")

#no hay filas duplicadas
duplicadas <- microdata %>% duplicated()
filas_duplicadas <- microdata[duplicadas,]

#no hay filas completamente nulas
filas_completamente_na <- apply(microdata, 1, function(x) all(is.na(x)))
any(filas_completamente_na)

#discriminamos a las personas que no contestaron y que no corresponden
#el -9 simboliza no sabe/no contesta en la columna de ingresos totales, al igual que el 3 en ch13,
#y el 0 y 99 también simbolizan no contestado o no corresponde
responde = microdata[microdata$P47T != -9 & microdata$CH12 != 0 &  microdata$CH12 != 99 & microdata$CH13 != 3,]

#pasamos a tomar en cuenta a personas mayores de 18 y menores de 65
microdata_mayor = responde[responde$CH06 >= 18,]
microdata_trabaja = microdata_mayor[microdata_mayor$CH06 <= 65,]


#revisamos de nuevo si hay filas duplicadas o completamente nulas
duplicadas2 <- microdata_trabaja %>% duplicated()
filas_duplicadas2 <- microdata_trabaja[duplicadas2,]
sum(duplicadas2)

filas_completamente_na2 <- apply(microdata_trabaja, 1, function(x) all(is.na(x)))
any(filas_completamente_na2)
sum(filas_completamente_na2)


#podemos observar que no hay filas duplicadas, sino 69 que están completamente vacías, 
#elegimos eliminarlas ya que no representan una proporción significativa de la muestra
nrow(microdata_trabaja)
microdata_trabaja <- microdata_trabaja[rowSums(is.na(microdata_trabaja)) < ncol(microdata_trabaja),]
nrow(microdata_trabaja)


#vemos el numero total de registros, y comparamos con los que nos resultan útiles por estar en edad laboral
nro_total <- nrow(microdata)
nro_filtrado <- nrow(microdata_trabaja)

# Calcular la cantida de datos  que no pasaron el filtro
nro_no_filtrado <- nro_total - nro_filtrado

# Vector con valores
valores <- c(Filtrado = nro_filtrado, NoFiltrado = nro_no_filtrado)

#creamos y descargamos el gráfico de torta que representa los datos filtrados.
png("grafico_torta_fltrados.png", width = 1200, height = 900)

torta_filtrados <- pie(valores,
                       labels = paste(names(valores), round(valores / sum(valores) * 100, 1), "%"),
                       cex = 1.5,  #tamaño de las labels
                       main = "Distribución: Filtrados vs No Filtrados",
                       col = c("lightblue", "lightgray"),
                       cex.main = 2.5)  #tamaño del título

dev.off()         #gráfico eliminado en la 2da parte del tp


#pasamos a crear una tabla de frecuencias para cada nivel educativo alcanzado

#Con "table" contamos la cantidad de veces que aparece cada nivel educativo
frecuencias <- table(microdata_trabaja$CH12)

#Calculamos con prop.table a que porcentaje del total corresponde la frecuencia de cada nivel educativo, y lo pasamos a porcentaje.
porcentajes <- round(prop.table(frecuencias) * 100, 2)

#Creamos una tabla con los datos obtenidos. Muestra la cantidad de personas con el nivel alcanzado y el porcentaje que representa.
#Se usa "as.vector" para que los valores aparezcan como números y no como tablas distitas.
tabla_nivel_educativo <- data.frame(
  Nivel_Educativo = names(frecuencias),
  Frecuencia = as.vector(frecuencias),
  Porcentaje = as.vector(porcentajes)
)

#creamos el gráfico de barras
png("grafico_barra_nvl_edu.png", width = 1200, height = 900)

grafico_nivel_educativo = barplot(tabla_nivel_educativo$Frecuencia,
        names.arg = tabla_nivel_educativo$Nivel_Educativo,
        col = "skyblue",
        las = 2,  # gira etiquetas en eje y para que se vean
        main = "Distribución por Nivel Educativo",
        ylab = "Cantidad de personas",
        ylim = c(0, max(tabla_nivel_educativo$Frecuencia) + 10),
        cex.main = 2,
        cex.lab = 1.4,
        cex.names = 2)

# Agregamos leyenda explicativa
legend("topright", 
       legend = c(
         "1 = Jardín/preescolar", 
         "2 = Primario", 
         "3 = EGB", 
         "4 = Secundario", 
         "5 = Polimodal", 
         "6 = Terciario", 
         "7 = Universitario", 
         "8 = Posgrado universitario", 
         "9 = Educación especial (discapacitado)"
       ),
       bty = "n",
       cex = 2)

dev.off()

#vamos a repetir el procedimiento pero para quienes finalizaron el nivel educativo que alcanzaron,
#así vemos qué nivel tiene más proporción de 'abandono'.

#los seleccionamos en niv_completo
niv_completo = microdata_trabaja[microdata_trabaja$CH13 == 1,]

frecuencias2 <- table(niv_completo$CH12)
porcentajes2 <- round(prop.table(frecuencias2) * 100, 2)

tabla_nivel_educativo2 <- data.frame(
  Nivel_Educativo = names(frecuencias2),
  Frecuencia = as.vector(frecuencias2),
  Porcentaje = as.vector(porcentajes2)
)

#creamos el gráfico de barras
png("grafico_barra_nvl_edu_com.png", width = 1200, height = 900)

grafico_nivel_educativo = barplot(tabla_nivel_educativo2$Frecuencia,
                                  names.arg = tabla_nivel_educativo2$Nivel_Educativo,
                                  col = "skyblue",
                                  las = 2,  # gira etiquetas en eje y para que se vean
                                  main = "Distribución por Nivel Educativo completo",
                                  ylab = "Cantidad de personas",
                                  ylim = c(0, max(tabla_nivel_educativo2$Frecuencia) + 1000),
                                  cex.main = 2,
                                  cex.lab = 1.4,
                                  cex.names = 2)
  # Agregamos leyenda explicativa
  legend("topright", 
         legend = c(
           "1 = Jardín/preescolar", 
           "2 = Primario", 
           "3 = EGB", 
           "4 = Secundario", 
           "5 = Polimodal", 
           "6 = Terciario", 
           "7 = Universitario", 
           "8 = Posgrado universitario", 
           "9 = Educación especial (discapacitado)"
         ),
         bty = "n",
         cex = 2)
  


dev.off()


#pasamos a otra de las variables, relacionando el nivel educativo alcanzado con el ingreso promedio.

png("grafico_boxplot_sal_prom.png", width = 1200, height = 900)

boxplot(P47T ~ CH12, data = microdata_trabaja,
        outline = TRUE, #no se tienen en cuenta valores atípicos porque impiden la visualizacion clara 
        main = "Ingreso total por nivel educativo",
        xlab = "Nivel educativo (CH12)",
        ylab = "Ingreso total (P47T)",
        col = "lightblue",
        cex.lab = 1.3, #tamaño de la etiqueta del eje y
        cex.main = 3,
        cex.names = 2.5,
        cex.axis = 1.5)

# Agregamos leyenda explicativa
legend("topleft", 
       legend = c(
         "1 = Jardín/preescolar", 
         "2 = Primario", 
         "3 = EGB", 
         "4 = Secundario", 
         "5 = Polimodal", 
         "6 = Terciario", 
         "7 = Universitario", 
         "8 = Posgrado universitario", 
         "9 = Educación especial (discapacitado)"
       ),
       bty = "n",
       cex = 2)
dev.off()

#Calculamos el máximo de los sueldos
max(microdata_trabaja$P47T)

#ahora queremos ver el monto promedio de los ingresos por subsidios por nivel educativo

#calculamos el promedio del monto de subsidio por nivel

promedio_salario_por_nivel <- aggregate(P47T ~ CH12, data = microdata_trabaja, FUN = mean, na.rm = TRUE)

promedio_subsidio <- tapply(microdata_trabaja$V5_M, microdata_trabaja$CH12, mean)

png("grafico_barra_sub_nivel.png", width = 1200, height = 900)

barras_subsidio_por_nivel = barplot(promedio_subsidio,
        names.arg = promedio_salario_por_nivel$CH12,
        col = "salmon",
        cex.names = 2,
        main = "Promedio del monto de subsidio por nivel educativo",
        ylab = "Subsidio promedio (V5_M)",
        cex.lab = 1.5, #tamaño de la etiqueta del eje y
        cex.main = 2.5,
        cex.axis = 1)
# Agregamos leyenda explicativa
legend("topright", 
       legend = c(
         "1 = Jardín/preescolar", 
         "2 = Primario", 
         "3 = EGB", 
         "4 = Secundario", 
         "5 = Polimodal", 
         "6 = Terciario", 
         "7 = Universitario", 
         "8 = Posgrado universitario", 
         "9 = Educación especial (discapacitado)"
       ),
       bty = "n",
       cex = 2)

dev.off()


#cambia cuando vemos cuantos usuarios recibe un subsidio segun el nivel

#contamos cuantos usuarios reciben subsidios
subs_por_nivel <- sort(tapply(microdata_trabaja$V5_M > 0, microdata_trabaja$CH12, sum), decreasing = TRUE)

png("grafico_par_sub_cant.png", width = 1200, height = 900)

pareto.chart(subs_por_nivel,
             names = (names(subs_por_nivel)),
             main = "",
             ylab = "Frecuencia absoluta",
             xlab = "Nivel educativo",
             ylab2 = "Porcentaje acumulado",
             cex.main = 1,
             cex.lab = 1.5,
             cex.names = 1.5,
             cex.axis = 1.5,
             )
title(main = "Subsidio según nivel educativo", cex.main = 1.8) #cambio el tam del titulo afuera ya que adentro no se podía
# Agregamos leyenda explicativa
legend("right", 
       legend = c(
         "1 = Jardín/preescolar", 
         "2 = Primario", 
         "3 = EGB", 
         "4 = Secundario", 
         "5 = Polimodal", 
         "6 = Terciario", 
         "7 = Universitario", 
         "8 = Posgrado universitario", 
         "9 = Educación especial"
       ),
       bty = "n",
       cex = 2,
       inset = c(0.05, 0))


dev.off()










#Parte 2
#muestra cada una de las categorias
niveles_educativos <- sort(unique(microdata_trabaja$CH12))

#un bucle para que de cada categoría nos devuelva los datos necesarios
for (nivel in niveles_educativos) {
  subgrupo <- subset(microdata_trabaja, CH12 == nivel)
  ingreso <- subgrupo$P47T
  
  if (length(ingreso) > 10) {  # para evitar grupos muy pequeños
    resultado <- MeanCI(ingreso, conf.level = 0.95)
    cat("\nNivel educativo:", nivel, "\n")
    cat("Media:", round(resultado[1], 2), "\n")
    cat("IC 95%: (", round(resultado[2], 2), ";", round(resultado[3], 2), ")\n")
    cat("N:", length(ingreso), "\n")
  }
}


#Creamos una tabla con los valores de las categorías con las que podemos hacer un análisis descriptivo
tabla <- data.frame(
  Nivel_educativo = c("Primario", "Secundario", "Terciario", "Universitario"),
  Media = c(191391, 229981.4, 323477, 339513.1),
  IC_inf = c(184799.3, 224384.1, 312214.7, 321955),
  IC_sup = c(197982.8, 235578.7, 334739.2, 357071.2)
)

#Graficamos la tabla

png("grafico_IC_exhaustivo.png", width = 800, height = 550)

ggplot(tabla, aes(x = Nivel_educativo, y = Media)) +
  geom_point(size = 4, color = "steelblue") +  
  geom_errorbar(aes(ymin = IC_inf, ymax = IC_sup), width = 0.3, size = 1) +  
  theme_minimal(base_size = 14) +  
  labs(title = "Ingresos medios por nivel educativo",
       y = "Ingreso promedio ($)",
       x = "Nivel educativo") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 13), 
    axis.text.y = element_text(size = 13),                         
    plot.title = element_text(face = "bold", size = 16)            
  )

dev.off()


#vamos a hacer inferencia sobre la proporcion de personas que 
#tiene ingresos por subsidios, según el nivel educativo

niveles <- c()
proporciones <- c()
IC_inf <- c()
IC_sup <- c()
tamaños <- c()  

for (nivel in niveles_educativos) {
  subgrupo <- subset(microdata_trabaja, CH12 == nivel)
  subsidios <- subgrupo$V5_M
  subsidios <- subsidios[!is.na(subsidios)]
  
  n <- length(subsidios)
  k <- sum(subsidios > 0)
  
  if (n > 10 && k > 0) {
    resultado <- BinomCI(k, n, conf.level = 0.95)
    
    niveles <- c(niveles, nivel) #solo van los niveles que pasaron el filtro
    proporciones <- c(proporciones, resultado[1])
    IC_inf <- c(IC_inf, resultado[2])
    IC_sup <- c(IC_sup, resultado[3])
    tamaños <- c(tamaños, n)
  } #si bien en el IC realizado para la media se cargaron los datos a mano,
}   #en este caso lo 'automatizamos' para no cometer errores

#Paso los códigos a nombres
nombres_niveles <- c(
  "Jardín/preescolar", "Primario", "EGB", "Secundario", "Polimodal",
  "Terciario", "Universitario", "Posgrado universitario", "Educación Especial"
)

tabla_subsidios <- data.frame(
  Nivel_educativo = nombres_niveles[niveles],
  Proporcion = round(proporciones, 3),
  IC_inf = round(IC_inf, 3),
  IC_sup = round(IC_sup, 3),
  tamaño = tamaños
)

#para que salgan los niveles ordenadamente en el gráfico
orden_niveles <- c(
  "Jardín/preescolar", "Primario", "EGB", "Secundario", "Polimodal",
  "Terciario", "Universitario", "Posgrado universitario", "Educación Especial"
)

tabla_subsidios$Nivel_educativo <- factor(tabla_subsidios$Nivel_educativo, levels = orden_niveles)

png("grafico_IC_subsidios.png", width = 800, height = 550)

ggplot(tabla_subsidios, aes(x = Nivel_educativo, y = Proporcion)) +
  geom_point(size = 4, color = "darkgreen") +
  geom_errorbar(aes(ymin = IC_inf, ymax = IC_sup), width = 0.3, size = 1) +
  theme_minimal(base_size = 14) +
  labs(title = "Proporción de personas que reciben subsidios por nivel educativo",
       y = "Proporción",
       x = "Nivel educativo") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 13),
    axis.text.y = element_text(size = 13),
    plot.title = element_text(face = "bold", size = 16)
  )

dev.off()


















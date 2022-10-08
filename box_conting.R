# Titulo: Metodo de box-conting para dimensiones Fractales
# Autor: Critian Camilo Paz Delgado

# Descripción del codigo: Desarrollar un algoritmo para el calculo de dimensiones 
#                         fractales para objetos unidimensionales. 

set.seed(10211)
m = runif(1000)
Muestra = data.frame(x=m[1:(1000-2)],
                     y=m[2:(1000-1)],
                     z=m[3:(1000)])

plot(m[1:999],m[2:1000])

# Mediante paquete de R ( solo para 3 dimensiones o más)
VoxR::box_counting(Muestra,store_fit = T)

# Box_conting para R1

Nr = 0 # Cantidad de cajas necesarias
r = 1/(2^Nr) # Tamaño de las cajas

# tamaño mínimo de las cajas (entre más pequeñas, mas precias y largo el algoritmo)
rmin = 0.001 

# Data de resultados ( no incluye las transformaciones)
resultados = data.frame(res=c(0),n = c(0))

while (r>=rmin) {
  limites = seq(0,1,by=r) # Limites de los intervalos
  contin = 0 # Cantidad de intervalos que contiene puntos
  for (i in 1:(2^Nr)) {
    # Se comprueba si el intervalo i contiene puntos
    if (length(m[m>=limites[i]&m<=limites[i+1]])!=0) contin = contin +1 
  }
  # Guardado de los resultados
  resultados = rbind(resultados,data.frame(res = r,n=contin))
  Nr = Nr + 1
  r = 1/(2^Nr)
}

resultados = resultados[-1,]

# diagrama de dispersión 
plot(log(1/resultados$res),log(resultados$n))

# Calculo de dimensión fractal a partir del modelo lineal. 
dimencion = lm(log(n)~log(1/res),data=resultados)$coefficients[2];dimencion

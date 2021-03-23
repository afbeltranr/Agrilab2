sln.1.A <- c(10,470,30)
sln.2.A <- c(20,500,6)
sln.2.B <- c(250,250)

sln.1.Exp <- 250*sln.1.A/500
sln.2.Exp <- 200*sln.2.A/500
sln.2.B.Exp <- (100 * sln.2.B) /500

ingredientes.sln.1 <- c('molibdato', 'agua', 'HCl')
ingredientes.sln.2 <- c('Ac.Oxalico', 'agua', 'Sulfato')
ingredientes.sln.2.B <- c('Ac.Sulfurico', 'agua')


sln.1 <- data.frame(Ingredientes =ingredientes.sln.1, cantidad = sln.1.Exp)
sln.1

sln.2.A <- data.frame(Ingredientes =ingredientes.sln.2, cantidad = sln.2.Exp)
sln.2.A


sln.2.B <-  data.frame(Ingredientes =ingredientes.sln.2.B, cantidad = sln.2.B.Exp)
sln.2.B 
 print(c(sln.1, sln.2.A, sln.2.B))
 
 
 ### ---- Preparación curva de calibración
 
 # Primero se halla la concentración tentativa de los extractos, al llevarlos a una masa final de 25g.
 
# Material de referencia 
 
M.MRC <- (0.1*20.9*28.09)/(1000*60.8) # Masa en g de Si en la muestra de 0.1g
C.MRC <- (M.MRC/0.025)*1000 # concentración en mg/kg 
C.MRC

# Muestra de tierrad e diatomea 0402

M.402 <- (0.1*74.2*28.09)/(100*60.8) # Masa en g de Si en la muestra de 0.1g
C.402 <- (M.402/0.025)*1000 # concentración en mg/kg 
C.402

# Se prepara una solución intermedia de 100 mg/kg a partir de una solución estándar de Si 1000 mg/L, 0.998 mg Si / kg

m.100 <- (100*0.1*1002)/(1000*1000)
m.100 # alícuota en gramos necesaria para preparar la solución de partida.


# La cantidad necesaria de la solución stock se calcula a partir de la suma de las alícuotas de todos los puntos de la curva de calibración. El espectrofotometro hach tiene un volumen de celda de 10 mL :

# Se crea un vector con las concentraciones 

curva <- c(0,1,2,5,10,20,50,75,100)
str(curva)



# a partir de las concentraciones finales, y la concentracion de partida de 100 mg / kg se calculan las masas necesarias para cada nivel:

alicuotas <- vector('numeric', 9)
for(i in 1:9){
  alicuotas[i] <- 30*(curva[i]/239.07)# 30g de nivel, a partir de 239.07 mg / kg
}
alicuotas
write.csv(round(alicuotas,4), 'alicuotas.csv', dec= ',', sep = '\t')
sum(alicuotas)


# La concentracion esperada del material de referecia certificado entra en la mitad de la curva, pero el de la muestra no. es posible diluir la muestra 50 veces:

Concentracion.diluncion50.402 <- 0.1*(1371/1000)*(1/0.005) # tomar 0.1 g y llevarlo a 5g 
Concentracion.diluncion50.402
C.MRC

#---------------------------- Concentraciones reales---------------

C.real.curva <- round(c(0,	1.280575788,	2.13755923,	5.424153552,	10.00936614,	19.84188555,	49.35550284,	74.04978593,	101.2714394),3)
C.real.curva <- round(C.real.curva,3)

Alicuota.10.ug <- vector('numeric', 9)

for(i in 1:9) {
  
  Alicuota.10.ug[i] <- 10/C.real.curva[i]
  
  
}
Alicuota.10.ug <- round(Alicuota.10.ug,3)


#---------------- calculo para la alícuota de la muestra y el MRC -------------------

C.estimada.muestras <- vector('numeric', 14)
C.estimada.muestras[c(2,8)] <- 19.3 # Material de referencia D2
C.estimada.muestras[c(3,4,5,7,9,11,12,13,14)] <- 27.42 # Muestra 402 D50
C.estimada.muestras

Alicuota.10.ug.muestras <- vector('numeric', 14)

for (i in 1:14) { 
  
  Alicuota.10.ug.muestras[i] <- 10/C.estimada.muestras[i]
  
  }
Alicuota.10.ug.muestras <- round(Alicuota.10.ug.muestras,4)
Alicuota.10.ug.muestras 
# Los calculos anteriores están hechos para que siempre haya la misma cantidad aproximada de Si (10ug). esto no es válido para la curva de calibración pero si puede tenerse en cuenta para las muestras.



## Debido al aumento de muestras es necesario preparar nuevas soluciones indicador, con un volumen total de 2500mL

sln.1.A.final <- sln.1*1.2
names(sln.1.A.final) <- names(sln.1)
sln.1.A.final
sln.1


###----------------------------Curva de calibración y cuantificación


C.curva <- c(0, 1,2,5,10,20,50,75,100)
Abs.curva <- c(0.000,0.013,0.026,0.075, mean(0.179,0.123,0.140), 0.269, mean(0.623,0.628,0.625),mean(1.107,1.112,1.105),mean(1.215,1.213,1.219))
Abs.curva

curva <- data.frame(Si.mg.L=C.curva, Absorbancia.u.a.=Abs.curva)
curva
modelo <- lm( Absorbancia.u.a. ~ Si.mg.L , data = curva)
summary(modelo)

library(ggplot2)

g <- ggplot(curva, aes(Si.mg.L, Absorbancia.u.a.))
g + geom_point() + geom_smooth(method ="lm")

###----Resultados de las muestras---------------


nombres <- c("blanco procedimiento 1",
             "MRC Dil 2-1",
             "402-D50-1",
             "402-D50-2",
             "402-D50-3",
             "blanco procedimiento 2",
             "402-D50-3",
             "MRC Dil 2-2",
             "402-D50-4",
             "blanco procedimiento 3",
             "402-D50-5",
             "402-D50-6",
             "402-D50-7",
             "402-D50-8",
             "Si Soluble D10",
             "Si Soluble D20",
             "MRC-1",
             "MRC-2")

absorbancia <- c(0.048,0.237,0.124,0.120,0.062,0.048,0.106,0.271,0.159,0.046,0.117,0.179,0.134,0.132,0.847,0.472,0.432,0.447)

muestras <- data.frame(muestra = nombres, absorbancia= absorbancia)
muestras


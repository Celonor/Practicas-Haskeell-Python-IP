import math

# Ejercicio 1

def imprimir_hola_mundo () -> None :
    print ("¡Hola mundo!")

def imprimir_un_verso () -> None :
    a = "Si te lo meto, no me llame \nQue esto no es pa' que me ame', ey \nSi tu novio no te mama el culo \nPa' eso que no mame"
    print (a)

def raizDe2 () -> float :
    return round( math.sqrt (2), 4)

def factorial_de_dos () -> int : 
     return 2

def perimetro () -> float :
     res: float = 2 * math.pi * 1
     return res

# EJERCICIO 2

def imprimir_saludo (nombre:str) -> None :
    print ("Hola "+ nombre)

def raiz_cuadrada_de (numero: float) -> float :
     res: float = math.sqrt (numero)
     return res

def fahrenheit_a_celsius (t:float) -> float :
     res:float = ((t-32)*5)/9
     return res

def imprimir_dos_veces (estribillo:str) -> None:
     res:str = estribillo * 2
     print (res)

def es_multiplo_de (n:int, m:int) -> bool :
     return n % m == 0

def es_par (numero:int) -> bool :
     return es_multiplo_de (numero, 2)

def cantidad_de_pizzas (comensales:int,min_cant_de_porciones:int) -> int :
     cantidad:float = (comensales * min_cant_de_porciones)/8
     return math.ceil (cantidad)

#EJERCICIO 3

# Ejercicio 3.1

def alguno_es_0 (numero1:float, numero2:float) -> bool:
     return (numero1 == 0) or (numero2 == 0)

# Ejercicio 3.2

def ambos_son_0 (numero1:float, numero2:float) -> bool:
     return (numero1 == 0) and (numero2 == 0)

# Ejercicio 3.3

def es_nombre_largo (nombre:str) -> bool:
     return (3<=len(nombre)<=8)

# Ejercicio 3.4

def es_bisiesto (año:int) -> bool:
     return (año % 400 == 0) or ((año % 4 == 0) and (año % 100 != 0))
 
# EJERCICIO 4

def peso_pino (altura:float) -> float:
     alturaCm:float = altura*100
     hasta3cm:float = min(alturaCm,300)
     masde3cm:float = max(0,alturaCm-300)
     return (hasta3cm*3) + (masde3cm*2)

def es_peso_util (peso:float) -> bool:
     return (400<=peso<=1000)

def sirve_pino (altura:float) -> bool:
     return es_peso_util(peso_pino(altura))

# EJERCICIO 5

def doble_si_es_par(numero:int) -> int:
     if numero % 2 == 0:
          return numero*2
     else:
          return numero

def devolver_valor_si_es_par_si_no_el_que_sigue (numero:int) -> int:
     if numero % 2 == 0:
          return numero
     else:
          return numero+1

def doble_si_es_multiplo3_el_triple_si_es_multiplo9 (numero:int) -> int:
     if numero % 9 == 0:
          return numero*3
     elif numero % 3 == 0:
          return numero*2
     else:
          return numero

def lindo_nombre(nombre:str) -> str:
     if len(nombre) >= 5:
          return "Tu nombre tiene muchas letras!"
     else:
          return "Tu nombre tiene menos de 5 caracteres"

def elRango (numero:int) -> str:
     if numero < 5:
          return "Menor a 5"
     elif 10<=numero<=20:
          return "Entre 10 y 20"
     elif numero > 20:
          return "Mayor a 20"

def trabajar_o_vacaciones (sexo:str, edad:int) -> str:
     if sexo == "M":
          if edad >= 65 or edad < 18:
               return "Andá de vacaciones"
          else:
               return "Te toca trabajar"
     else:
          if edad >=60 or edad < 18:
               return "Andá de vacaciones"
          else:
               return "Te toca trabajar"
               

## EJERCICIO 6

## 6.1
def cuenta_del_1_al_10 () -> None:
     contador:int = 1
     while contador <= 10:
          print (contador)
          contador= contador+1

## 6.2

def pares_entre_10_y_40 () -> None:
     contador:int = 10
     while contador <= 40:
          print (contador)
          contador = contador+2

## 6.3

def eco_10_veces () -> None:
     repeticiones:int = 10
     while repeticiones > 0:
          print ("eco")
          repeticiones = repeticiones - 1

## 6.4

def cuenta_regresiva (num:int) -> None:
     while num > 0:
          print (num)
          num = num-1
     print ("Despegue")

## 6.5

def viaje_en_el_tiempo (año_partida:int, año_llegada:int) -> None:
     while año_partida > año_llegada:
          año_partida = año_partida - 1
          print ("Viajo un año al pasado, estamos en el año:", año_partida)

## 6.6

def conocer_a_Aristoteles (año_partida:int) -> None:
     año_actual:int = año_partida - 20
     while (año_actual) > -384:
          if año_actual > 0:
               print (año_actual)
          else:
               print ((año_actual*-1),"a.C.")
          año_actual = año_actual - 20

## EJERCICIO 7

## 7.1

def del_1_al_10 () -> None :
     for cuenta in range(10) :
          print (cuenta+1)

## 7.2

def del_10_al_40 () -> None :
     for cuenta in range(10,41,2):
          print (cuenta)

## 7.3

def eco_10_veces2 () -> None :
     for contador in range(10):
          print ("eco")

## 7.4

def cuenta_regresiva2 (num:int) -> None :
     for contador in range(num,0,-1):
          print (contador)
     print ("Despegue")

## 7.5

def vitacora_tiempo (año_partida:int, año_llegada:int) -> None :
     for año in range (año_partida-1,año_llegada-1,-1):
          print ("Viajó un año al pasado, estamos en el año:",año)

## 7.6

def vitacora_tiempo2 (año_partida:int) -> None :
     for año in range(año_partida,-384,-20):
          if año > 0:
               print (año)
          else:
               print (año*-1,"a.C.")



##if __name__ == "__main__":
        ###vitacora_tiempo2(2021)
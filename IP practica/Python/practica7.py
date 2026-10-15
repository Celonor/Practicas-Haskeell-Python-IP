## EJERCICIO 1

## 1.1

def pertenece (s:list[int], e:int) -> bool :
    indice:int=0
    while len(s) > indice:
        if s[indice] == e:
            return True
        else:
            indice += 1
    return False

# 1.2

def divide_a_todos (s:list[int], e:int) -> bool :
    for elem in s:
        if elem % e != 0:
            return False
    return True

## 1.3

def suma_total (s:list[int]) -> int :
    acumulado:int = 0
    for elem in s:
        acumulado += elem
    return acumulado

## 1.4

def maximo (s:list[int]) -> int :
    valor:int = s[0]
    for elem in s:
        if elem > valor:
            valor = elem
    return valor

## 1.5

def minimo (s:list[int]) -> int :
    valor:int = s[0]
    for elem in s:
        if elem < valor:
            valor = elem
    return valor

## 1.6

def ordenados (s:list[int]) -> bool :
    for indice in range(len(s)-1) :
        if s[indice] >= s[indice+1]:
            return False
    return True

## 1.7

def pos_maximo (s:list[int]) -> int :
    if s == []:
        return -1
    indice_max:int = 0
    valor:int = s[0]
    for indice in range(len(s)):
        if s[indice] > valor:
            valor = s[indice]
            indice_max = indice
    return indice_max

## 1.8

def pos_minimo (s:list[int]) -> int : 
    if s == []:
        return -1
    indice_min:int = 0
    valor:int = s[0]
    for indice in range(len(s)):
        if s[indice] <= valor:
            valor = s[indice]
            indice_min = indice
    return indice_min

## 1.9

def long_mayorASiete (s:list[str]) -> bool :
    for elem in s:
        if len(elem) > 7:
            return True
    return False

## 1.10

def es_palindroma (s:str) -> bool : 
    if len(s) > 1:
        valor:int = len(s) - 1
        for elem in range(len(s)//2):
            if s[elem] != s[valor]:
                return False
            else:
                valor -= 1
    return True

## 1.11

def iguales_consecutivos (s:list[int]) -> bool :
    if len(s) > 2:
        for indice in range(len(s)-2):
            if s[indice] == s[indice+1] == s[indice+2]:
                return True
    return False

## 1.12

def vocales_distintas (s:str) -> bool :
    vocales:list[str] = ["a","e","i","o","u"]
    contador:int = 0
    for letra in s:
        if letra in vocales:
            vocales.remove(letra)
            contador += 1
        if contador >= 3:
            return True
    return False

## 1.13

def pos_secuencia_ordenada_mas_larga (s:list[int]) -> int :
    indice_mayor:int = 0
    indice_actual:int = 0
    cantidad:int = 0
    contador:int = 0
    for indice in range(len(s)-1) :
        if s[indice] <= s[indice+1]:
            contador += 1
            if contador > cantidad :
                cantidad = contador
                indice_mayor = indice_actual
        else:
            contador = 0
            indice_actual = indice + 1
    return indice_mayor

## 1.14

def separar_por_digito (s:int) -> list[int] :
    lista:list[int] = []
    while s >= 10 :
        lista.append(s%10)
        s = (s-(s%10)) / 10
    lista.append(s)
    return lista

def cantidad_digitos_impares (s:list[int]) -> int :
    contador:int = 0
    for num in s:
        for digito in separar_por_digito(num):
            if digito % 2 != 0 :
                contador += 1
    return contador

## EJERCICIO 2

## 2.1

def CerosEnPosicionesPares (s:list[int]) -> None:
    for indice in range(len(s)):
        if s[indice] % 2 == 0:
            s[indice] = 0
    print (s)

## 2.2

def CerosEnPosicionesPares2 (s:list[int]) -> list[int]:
    nueva_lista:list[int] = []
    for elem in s:
        if elem % 2 == 0:
            nueva_lista.append(0)
        else:
            nueva_lista.append(elem)
    return nueva_lista

## 2.3

def sin_vocales (s:str) -> str :
    no_vocales:str = ""
    vocales:list[str] = ["a","e","i","o","u"]
    s_min:str = s.lower()
    for letra in s_min :
        if letra not in vocales:
            no_vocales += letra
    return no_vocales

## 2.4

def reemplaza_vocales (s:str) -> str :
    sin_vocales:str = ""
    vocales:list[str] = ["a","e","i","o","u"]
    s_min:str = s.lower()
    for letra in s_min:
        if letra in vocales:
            sin_vocales += "_"
        else:
            sin_vocales += letra
    return sin_vocales

## 2.5

def da_vuelta_str (s:str) -> str :
    reverso:str = ""
    for indice in range(len(s)) :
        reverso += s[len(s)-indice-1]
    return reverso

## 2.6

def eliminar_repetidos (s:str) -> str :
    limpio:str = ""
    for letra in s:
        if letra not in limpio:
            limpio += letra
    return limpio

## EJERCICIO 3

def promedio (notas:list[int]) -> float :
    nota:int = 0
    for num in notas:
        nota+=num
    return (nota/len(notas))

def mayores_a_cuatro (notas:list[int]) -> bool :
    for num in notas:
        if num < 4:
            return False
    return True

def resultadoMateria (notas:list[int]) -> int :
    if mayores_a_cuatro(notas):
        if promedio(notas) >= 7 :
            return 1
        else:
            return 2
    else:
        return 3

## EJERCICIO 4

def saldoActual (movimientos:list[tuple[str,int]]) -> int :
    saldoFinal:int = 0
    for tupla in movimientos :
        if tupla[0] == "I":
            saldoFinal += tupla[1]
        else:
            saldoFinal -= tupla[1]
    return saldoFinal


## EJERCICIO 5

## 5.1

def pertenece_a_cada_uno_version_1 (s:list[list[int]], e:int, res:list[bool]) -> None :
    for fila in s:
        res.append(pertenece(fila,e))

## 5.3

def pertenece_a_cada_uno_version_3 (s:list[list[int]], e:int) -> list[bool] :
    res:list[bool] = []
    for fila in s:
        res.append(pertenece(fila,e))
    return res

## EJERCICIO 6

## 6.1

def es_matriz (s:list[list[int]]) -> bool :
    if s == [] or s[0] == []:
        return False
    for lista in s:
        if len(lista) != len(s[0]):
            return False
    return True

## 6.2

def filas_ordenadas (m:list[list[int]], res:list[bool]) -> None :
    res.clear()
    for indice in range(len(m)) :
        res.append(ordenados(m[indice]))
    
## 6.3

def columna (m:list[list[int]], c:int) -> list[int] :
    lista_columnas:list[int] = []
    for lista in m :
        lista_columnas.append(lista[c])
    return lista_columnas

## 6.4

def columnas_ordenadas (m:list[list[int]]) -> list[bool] :
    lista:list[bool] = []
    for indice in range(len(m[0])) :
        lista.append(ordenados(columna(m,indice)))
    return lista
    
        




if __name__ == "__main__":
    print(len(s[1]))
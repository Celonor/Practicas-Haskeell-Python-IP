import Test.HUnit
import Sistema_de_stock

-- Ejercicio 1

mercaderiaLarga :: [String]
mercaderiaLarga = 
  [ "arroz", "leche", "fideos", "arroz", "pan", "leche"
  , "yerba", "cafe", "pan", "arroz", "galletitas", "yerba"
  , "leche", "fideos", "arroz", "te", "pan", "cafe"
  ]

testStock = runTestTT testgenerarStock

testgenerarStock = test [
    "Caso lista Vacia" ~: generarStock []                    ~?= [],
    "Caso 1 elemento"  ~: generarStock ["pan"]               ~?= [("pan",1)],
    "Caso Distintos"   ~: generarStock ["pan","carne","sal"] ~?= [("pan",1),("carne",1),("sal",1)],
    "Varios repetidos" ~: generarStock mercaderiaLarga       ~?= [ ("arroz", 4), ("leche", 3), ("fideos", 2), ("pan", 3), ("yerba", 2),("cafe", 2),("galletitas", 1),("te", 1)]
 ]
import Test.HUnit

testSumaTotal = test [
    "Caso matriz vacia" ~: 0 ~=? sumaTotal [],
    "Caso 1 elemento"   ~: 5 ~=? sumaTotal [[5]],
    "Caso fila simple" ~: 10 ~=? sumaTotal [[1,2,3,4]]
    ]


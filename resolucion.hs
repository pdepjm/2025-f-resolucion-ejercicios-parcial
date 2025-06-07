-- Samurai

--Codigo ya existente
type Habilidad = Number
data Guerrero = UnGuerrero {
  nombre :: String,
  habilidad :: Habilidad,
  caracteristicas :: [String]
}

habilidoso :: Habilidad -> Guerrero -> Guerrero
habilidoso minimo guerrero = habilidad guerrero >= minimo
honorable:: Guerrero -> Bool
honorable guerrero = not elem "deshonra" (caracteristicas guerrero ) 

-- 1. Arreglar los errores que encuentre en el código anterior. (Errores que hacen que el código no funcione, hay al menos uno)

-- El tipo de la funcion "habilidoso" es incorrecto, deberia devolver un Bool.
-- En la funcion "honorable" es necesario poner entre parentesis el parametro del not.

--Funciones arregladas:

habilidoso' :: Habilidad -> Guerrero -> Bool
habilidoso' minimo guerrero = habilidad guerrero >= minimo

honorable' :: Guerrero -> Bool
honorable' guerrero = not (elem "deshonra" (caracteristicas guerrero))



-- 2. Completar la codificación de una función que obtenga los nombres de los candidatos a reclutar, dado un maestro y una lista de guerreros.

type Maestro = Guerrero -> Bool

reclutas :: Maestro -> [Guerrero] -> [String]
reclutas maestro guerreros = map nombre (filter maestro guerreros)

-- 3. Mostrar dos ejemplos de consulta de la función anterior, uno para cada maestro (asumir que ya hay una lista de guerreros de ejemplo)

-- Asumo que la lista ya existente de guerreros se llama "guerrerosDeEjemplo"

-- Consulta 1: "reclutas honorable guerrerosDeEjemplo" devuelve los reclutas que son honorables
-- Consulta 2: "reclutas (habilidoso 7) guerrerosDeEjemplo" devuelve los reclutas con habilidad mayor a 7. 

-- 4. Reescribir de una manera alternativa la función honorable utilizando adecuadamente la función . (composición).  ( y arreglando los errores si los hubiera detectado en el item 1)


honorable'' :: Guerrero -> Bool
honorable''  = not . elem "deshonra" . caracteristicas








 -- Videojuegos

 -- Codigo ya existente:

data Videojuego = UnVideojuego {
    titulo :: String,
    desarrolladora :: String,
    generos :: [String],
    lanzamiento :: Number -- El enunciado decia "Int", pero es lo mismo.
}
esDesarrolladoPor dev = (==dev).desarrolladora
perteneceAGenero genero videojuego = elem genero (generos videojuego)
esReciente videojuego = lanzamiento videojuego > 2015


-- 1. Modelar a los jugadores de ejemplo (Juan, María y Pedro).

type Jugador = Videojuego -> Bool

juan :: Jugador
juan juego = esDesarrolladoPor "Nintendo" juego || perteneceAGenero "Plataformas" juego

maria :: Jugador
maria = esReciente

pedro :: Jugador
pedro juego = esDesarrolladoPor "Rockstar Games" juego || perteneceAGenero "Mundo Abierto" juego

-- 2. Hacer la función preferidosDelComite que resuelva el requerimiento principal. Usar aplicación parcial y expresiones lambda de forma conveniente, indicando por qué se elige una sobre otra en cada caso.

preferidosDelComite :: [Jugador] -> [Videojuego] -> [Videojuego]
preferidosDelComite comite juegos = filter (esPreferidoPor comite) juegos -- Aca uso aplicacion parcial en vez de expresion lambda porque permite una solucion mas simple

esPreferidoPor :: [Jugador] -> Videojuego -> Bool
esPreferidoPor comite juego = all (\criterioDeJugador -> criterioDeJugador juego) comite -- Aca uso expresion lambda porque esta solución puede ser mas facil de entender que aplicación parcial.

-- Alternativa con aplicacion parcial:
esPreferidoPor' :: [Jugador] -> Videojuego -> Bool
esPreferidoPor' comite juego = all ($ juego) comite -- Esta alterantiva puede no ser facil de entender, dependiendo de la familiaridad con haskell de quien lo lea.


-- 3. Justificar la utilidad del concepto de orden superior en la solución planteada

-- Orden superior permite poder pasar un criterio (funcion) por parametro, lo cual permite filtrar los juegos segun la preferencia del comite, y saber si a todos los miembros del comite prefieren un cierto juego. 






-- Bebidas

-- Codigo ya existente

{-
--Devuelve a la persona luego de haberse tomado la bebida
tomar:: Bebida -> Persona -> Persona  

--Obtiene la nueva bebida resultante de mezclar las dos bebidas dadas
mezclar:: Bebida -> Bebida -> Bebida

-- 1. Completar la definición de una función que permita obtener cómo queda un conjunto de personas luego de que cada una se tome un "cóctel" producto de mezclar varias bebidas. 

-- con fold
mezclarTodas :: [Bebida] -> Bebida
mezclarTodas = foldr1 mezclar  

-- con recursividad
mezclarTodas' :: [Bebida] -> Bebida
mezclarTodas' [x] = x
mezclarTodas' (x:xs) = mezclar x (mezclarTodas' xs)

seTomanUnCoctelConEstasBebidas :: [Bebida] -> [Persona] -> [Persona]
seTomanUnCoctelConEstasBebidas bebidas personas = map (tomar (mezclarTodas bebidas)) personas


-- 2. Indicar qué concepto/s se están utilizando en la solución anterior:

-- [Si] Orden Superior | [No] Listas infinitas | [Si] Aplicación Parcial | [No] Efecto



-}



-- Medicamentos

--Codigo ya existente

-- Para cada medicamento:
amoxicilina = cura "infección"
bicarbonato = cura "picazón"
sugestion _ = []
cura sintoma = filter (/= sintoma)

-- Para cada enfermedad / conjunto de síntomas:
malMovimiento = ["dolor agudo", "hinchazón"]
varicela = repeat "picazón"
mejorMedicamentoPara sintomas = head . filter (idealPara sintomas)
idealPara sintomas medicamento = medicamento sintomas == []


-- 1. Definir el tipo Medicamento en base al modelo dado, y explicitar el tipo de la función mejorMedicamentoPara.

type Sintoma = String

type Medicamento = [Sintoma] -> [Sintoma]

mejorMedicamentoPara :: [Sintoma] -> [Medicamento] -> Medicamento

-- 2. Explicar qué beneficio aporta el uso de orden superior en la definición de  mejorMedicamentoPara.

-- Orden superior permite tanto poder pasar una lista de medicamentos (funciones) por parametro a la funcion mejorMedicamentoPara, como tambien poder filtrarlos segun una cierta condicion (funcion).


-- 3.a) Definir ibuprofeno, para que pueda usarse como medicamento, que cure tanto el "dolor agudo" como la "hinchazón" si es de más de 500 miligramos, y de lo contrario cure el "dolor moderado". 


-- Dos alternativas:
ibuprofeno :: Number -> Medicamento
ibuprofeno dosis sintomas
  | dosis > 500 = cura "hinchazón" (cura "dolor agudo" sintomas)
  | otherwise = cura "dolor moderado" sintomas

ibuprofeno' :: Number -> Medicamento
ibuprofeno' dosis 
  | dosis > 500 = cura "hinchazón" . cura "dolor agudo"
  | otherwise = cura "dolor moderado" 


-- 3.b) Armar una lista de medicamentos que incluya amoxicilina e ibuprofeno de 400 miligramos. En caso de que se esté aprovechando algún concepto importante para llevarlo a cabo, mencionarlo.


algunosMedicamentos :: [Medicamento]
algunosMedicamentos = [amoxicilina, ibuprofeno 400]

-- Aca se está usando aplicacion parcial para poder tener un ibuprofeno de 400 miligramos.


-- 4. ¿Qué sucederá al evaluar las siguientes consultas? Justificar conceptualmente. En caso de errores o comportamientos inesperados, indicar cuáles son y dónde ocurren.

-- Consulta: mejorMedicamentoPara malMovimiento (repeat bicarbonato)
-- Respuesta: 
-- Sirve primero entender que hacen las funciones: mejorMedicamentoPara devuelve el primer medicamento que sea ideal para los sintomas apenas lo encuentre (lazy evaluation). 
-- Un medicamento es ideal para los sintomas si los cura a todos.
-- Por lo tanto, en este caso se va a quedar colgado para siempre, porque ningun elemento de la lista infinita de bicarbonatos cura ninguno de los sintomas del malMovimiento. 
-- El bicarbonato cura "picazón", pero malMovimiento son los sintomas "dolor agudo" e "hinchazón".


-- Consulta: mejorMedicamentoPara varicela [sugestion, bicarbonato, amoxicilina]

-- Respuesta: En este caso hay una lista infinita de sintomas, la varicela, pero el primer medicamento de la lista, sugestion, siempre cura todos los sintomas, por lo que
-- la funcion mejorMedicamentoPara, al encontrar un medicamento que es ideal para esta lista de sintomas, devuelve este medicamento y termina la ejecucion.

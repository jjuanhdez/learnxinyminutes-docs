---
language: HQ9+
filename: hq9+.html
contributors:
    - ["Alexey Nazaroff", "https://github.com/rogaven"]
translators:
    - ["J. Juan Hernandez", "http://jjuanhdez.es"]
lang: es-es
---

HQ9+ es un lenguaje de programación esotérico creado por Cliff Biffle. 
Tiene solo cuatro comandos y no es Turing-completo.

```
Solo hay 4 comandos, representados por los siguientes caracteres.
H: imprimir "Hello, world!"
Q: imprimir el código fuente del programa (un Quine)
9: imprime la letra de la canción "99 Bottles of Beer"
+: incrementar en uno el acumulador (no se puede acceder al valor del
 acumulador)
Cualquier otro caracter es ignorado.

De acuerdo. Escribamos algún programa::
  HQ

Resultado:
  Hello world!
  HQ

HQ9+ es muy simple, pero permite hacer algunas cosas que son muy difíciles en 
otros lenguajes. Por ejemplo, aquí hay un programa que crea tres copias de sí
mismo en la pantalla:
  QQQ

Esto produce:
  QQQ
  QQQ
  QQQ
```

Y eso es todo. Hay muchos intérpretes para HQ9+. 
A continuación puedes encontrar uno de ellos:

+ [Uno de los intérpretes online](https://almnet.de/esolang/hq9plus.php)
+ [Sitio web oficial de HQ9+](http://cliffle.com/esoterica/hq9plus.html)

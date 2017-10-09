------
language: LOLCODE
filename: learnLOLCODE-es.lol
contributors:
    - ["abactel", "https://github.com/abactel"]
translators:
    - ["José Juan Hernández García", "http://jjuanhdez.es"]
lang: es-es
---

LOLCODE es un lenguaje de programación esotérico diseñado para asemejarse al
 habla de los [lolcats](https://upload.wikimedia.org/wikipedia/commons/a/ab/Lolcat_in_folder.jpg?1493656347257).
Las palabras clave del lenguaje son abreviaturas bien comprimidas del lenguaje utilizado por las imágenes de los Lolcat.


```
BTW Este es un comentario en línea
BTW El código comienza con 'HAI <versión del lenguaje>' y termina con 'KTHXBYE'

HAI 1.3
CAN HAS STDIO? BTW Importación de las cabeceras estándar

OBTW
    ==========================================================================
    ================================ LO BASICO ===============================
    ==========================================================================
TLDR

BTW Muestra el texo:
VISIBLE "¡Hola, mundo!"

BTW Declarando variables:
I HAS A MESSAGE ITZ "CATZ ARE GOOD"
VISIBLE MESSAGE

OBTW
	(Esto es un bloque de código.) Las variables se escriben dinámicamente, por lo que no es necesario declarar su tipo. El tipo de una variable coincide con su contenido. Estos son los tipos:
TLDR

I HAS A STRING  ITZ "DOGZ ARE GOOOD" BTW tipo YARN
I HAS A INTEGER ITZ 42               BTW tipo NUMBR
I HAS A FLOAT   ITZ 3.1415           BTW tipo NUMBAR
I HAS A BOOLEAN ITZ WIN              BTW tipo TROOF
I HAS A UNTYPED                      BTW tipo NOOB

BTW Aceptando la entrada del usuario:
I HAS A AGE
GIMMEH AGE
BTW La variable se almacena como YARN. Para convertirlo en NUMBR
AGE IS NOW A NUMBR

OBTW
    ==========================================================================
    =============================== MATEMATICAS ==============================
    ==========================================================================
TLDR

BTW LOLCODE utiliza matemática de notación polaca.

BTW Notación matemática básica:

SUM OF 21 AN 33         BTW 21 + 33
DIFF OF 90 AN 10        BTW 90 - 10
PRODUKT OF 12 AN 13     BTW 12 * 13
QUOSHUNT OF 32 AN 43    BTW 32 / 43
MOD OF 43 AN 64         BTW 43 módulo 64
BIGGR OF 23 AN 53       BTW max(23, 53)
SMALLR OF 53 AN 45      BTW min(53, 45)

BTW Notación binaria:

BOTH OF WIN AN WIN          BTW and: WIN Si x=WIN, y=WIN
EITHER OF FAIL AN WIN       BTW or: FAIL Si x=FAIL, y=FAIL
WON OF WIN AN FAIL          BTW xor: FAIL Si x=y
NOT FAIL                    BTW negación universal: WIN Si x=FAIL
ALL OF WIN AN WIN MKAY      BTW infinite arity AND
ANY OF WIN AN FAIL MKAY     BTW infinite arity OR

BTW Comparación:

BOTH SAEM "CAT" AN "DOG"             BTW WIN Si x == y
DIFFRINT 732 AN 184                  BTW WIN Si x != y
BOTH SAEM 12 AN BIGGR OF 12 AN 4     BTW x >= y
BOTH SAEM 43 AN SMALLR OF 43 AN 56   BTW x <= y
DIFFRINT 64 AN SMALLR OF 64 AN 2     BTW x > y
DIFFRINT 75 AN BIGGR OF 75 AN 643    BTW x < y

OBTW
    ==========================================================================
    ========================= ESTRUCTURAS DE CONTROL =========================
    ==========================================================================
TLDR

BTW Estructura If/Then:
I HAS A ANIMAL
GIMMEH ANIMAL
BOTH SAEM ANIMAL AN "CAT", O RLY?
    YA RLY
        VISIBLE "YOU HAV A CAT"
    MEBBE BOTH SAEM ANIMAL AN "MAUS"
        VISIBLE "NOM NOM NOM. I EATED IT."
    NO WAI
        VISIBLE "AHHH IS A WOOF WOOF"
OIC

BTW Estructura Case:
I HAS A COLOR
GIMMEH COLOR
COLOR, WTF?
    OMG "R"
        VISIBLE "RED FISH"
        GTFO
    OMG "Y"
        VISIBLE "YELLOW FISH"
        BTW Puesto que no hay 'GTFO', las siguientes declaraciones también serán probadas
    OMG "G"
    OMG "B"
        VISIBLE "FISH HAS A FLAVOR"
        GTFO
    OMGWTF
        VISIBLE "FISH IS TRANSPARENT OHNO WAT"
OIC

BTW Bucle For:
I HAS A TEMPERATURE
GIMMEH TEMPERATURE
TEMPERATURE IS NOW A NUMBR
IM IN YR LOOP UPPIN YR ITERATOR TIL BOTH SAEM ITERATOR AN TEMPERATURE
    VISIBLE ITERATOR
IM OUTTA YR LOOP

BTW Bucle While:
IM IN YR LOOP NERFIN YR ITERATOR WILE DIFFRINT ITERATOR AN -10
    VISIBLE ITERATOR
IM OUTTA YR LOOP

OBTW
    =========================================================================
    ================================ CADENAS ================================
    =========================================================================
TLDR

BTW Saltos de línea:
VISIBLE "PRIMERA LINEA :) SEGUNDA LINEA"

BTW Tabulaciones:
VISIBLE ":>LOS ESPACIOS SON SUPERIORES"

BTW Campana (como beep):
VISIBLE "NXT CUSTOMER PLS :o"

BTW Comillas:
VISIBLE "DIJO :"ME GUSTA EL PASTEL:""

BTW Dos puntos:
VISIBLE "DONDE VIVO:: CYBERSPACE"

OBTW
    =========================================================================
    =============================== FUNCIONES ===============================
    =========================================================================
TLDR

BTW Declarando una nueva función:
HOW IZ I SELECTMOVE YR MOVE BTW 'MOVE' es un argumento
    BOTH SAEM MOVE AN "ROCK", O RLY?
        YA RLY
            VISIBLE "YOU HAV A ROCK"
        NO WAI
            VISIBLE "OH NO IS A SNIP-SNIP"
    OIC
    GTFO BTW Esto devuelve NOOB
IF U SAY SO

BTW Declarando una función y devolviendo un valor:
HOW IZ I IZYELLOW
    FOUND YR "YELLOW"
IF U SAY SO

BTW Llamando a una función:
I IZ IZYELLOW MKAY

KTHXBYE
```

## Lectura adicional:

-   [compilador LCI compiler](https://github.com/justinmeza/lci)
-   [Especificaciones oficiales](https://github.com/justinmeza/lolcode-spec/blob/master/v1.2/lolcode-spec-v1.2.md)
-	[Execute LOLCODE Online](https://www.tutorialspoint.com/execute_lolcode_online.php)

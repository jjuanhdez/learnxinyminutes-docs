---
language: Fortran
contributors:
    - ["Robert Steed", "https://github.com/robochat"]
translators:
    - ["José Juan Hernández García", "http://jjuanhdez.es"]
filename: learnfortran-es.f95
lang: es-es
---

Fortran is one of the oldest computer languages. It was developed in the 1950s 
by IBM for numeric calculations (Fortran is an abbreviation of "Formula 
Translation"). Despite its age, it is still used for high-performance computing
such as weather prediction. However, the language has changed considerably over
the years, although mostly maintaining backwards compatibility; well known
versions are FORTRAN 77, Fortran 90, Fortran 95, Fortran 2003, Fortran 2008 and
Fortran 2015.

This overview will discuss the features of Fortran 95 since it is the most
widely implemented of the more recent specifications and the later versions are
largely similar (by comparison FORTRAN 77 is a very different language).

```fortran

! Este es un comentario.


program example   !declara un programa llamado example.

    ! El código solo puede existir dentro de programas, funciones, subrutinas o módulos.
    ! La sangría no es necesaria, pero es recomendable.


    ! Declarando variables
    ! ====================
    
    ! Todas las declaraciones deben venir antes de las sentencias y expresiones.
    
    implicit none    !evita la declaración dinámica de variables (¡recomendado!)
    ! Implicit no debe ser re-declarada en cada function/program/module...
    
    ! IMPORTANTE - Fortran es insensible a insensible a las mayúsculas y minúsculas.
    real z
    REAL Z2

    real :: v,x    ! ADVERTENIA: los valores iniciales por defecto dependen del compilador!    
    real :: a = 3, b=2E12, c = 0.01
    integer :: i, j, k=1, m
    real, parameter :: PI = 3.1415926535897931    !declara una constante.
    logical :: y = .TRUE. , n = .FALSE.    !tipo booleano.
    complex :: w = (0,1)    !sqrt(-1)
    character (len=3) :: month    !cadena de 3 caracteres.
    
    real :: array(6)     !declara un array (matriz) de 6 reales.
    real, dimension(4) :: arrayb    !otra forma de declarar un array.
    integer :: arrayc(-10:10)   !un array con un índice personalizado.
    real :: array2d(3,2)    !array multidimensional.
    
    ! Los separadores '::' no siempre son necesarios, pero se recomiendan.

    ! También existen muchos otros atributos variables:
    real, pointer :: p    !declara un puntero.

    integer, parameter :: LP = selected_real_kind(20)
    real (kind = LP) :: d    !variable de precisión larga.

    ! WARNING: initialising variables during declaration causes problems
    ! in functions since this automatically implies the 'save' attribute
    ! whereby values are saved between function calls. In general, separate
    ! declaration and initialisation code except for constants!
	! ADVERTENCIA: la inicialización de las variables durante la
	! declaración causa problemas en las funciones, ya que esto implica
	! automáticamente el atributo 'guardar', por el cual los valores se
	! guardan entre las llamadas a funciones. En general, ¡separar la 
	! declaración y el código de inicialización excepto con las constantes!
    
    
    ! Cadenas
    ! =======

    character :: a_char = 'i'
    character (len = 6) :: a_str = "qwerty"
    character (len = 30) :: str_b
    character (len = *), parameter :: a_long_str = "This is a long string."
    !puede tener conteo automático de longitud usando (len=*) pero solo para constantes.
    
    str_b = a_str // " keyboard"    !concatenamos cadenas usando el operador //.


    ! Asignación y aritmética
    ! =======================

    Z = 1    !asignar a la variable z declarada antes (insensible a mayúsculas/minúsculas).
    j = 10 + 2 - 3
    a = 11.54  /  (2.3 * 3.1)
    b = 2**3    !exponenciación


    ! Control de flujo y operadores
    ! =============================

    ! Declaración en un única línea
    if (z == a) b = 4  !la condición siempre va entre paréntesis.

    if (z /= a) then !z no es igual a a
    ! Otros símbolos de comparación son < > <= >= == /=
      b = 4
    else if (z .GT. a) then !z es mayor que a
    ! Losequivalentes de texto de los operadores símbolo son .LT. .GT. .LE. .GE. .EQ. .NE.  
      b = 6
    else if (z < a) then !'then' debe estar en esta línea.
      b = 5 !el bloque de ejecución debe estar en una nueva línea.
    else
      b = 10
    end if !el final de declaración necesita el 'if' (o puede usar 'endif')


    if (.NOT. (x < c .AND. v >= a .OR. z == z)) then !operadores booleanos.
      inner: if (.TRUE.) then    !puede llamar if-construct.
        b = 1
      endif inner    !entonces debe nombrar la instrucción endif.
    endif


    i = 20
    select case (i)
      case (0)      !caso i == 0
        j=0
      case (1:10)   !casos i de 1 a 10 inclusive.
        j=1
      case (11:)    !todos los casos donde i>=11
        j=2
      case default
        j=3
    end select


    month = 'jan'
	! La condición puede ser de tipo entero, lógico o de caracteres.
    ! Las construcciones seleccionadas también pueden ser nombradas.
    monthly: select case (month)
      case ("jan")
         j = 0
      case default
         j = -1
    end select monthly


    do i=2,10,2   !bucles de 2 a 10 (inclusive) en incrementos de 2.
      innerloop: do j=1,3    !los bucles también se pueden nombrar.
        exit      !salir del bucle.
      end do innerloop
    cycle         !saltar a la siguiente iteración del bucle.
    enddo

    
    ! La declaración Goto existe pero se desaconseja su uso.
    goto 10    
    stop 1    !detiene el código inmediatamente 
	          !(devolviendo el código de condición especificado)
10  j = 201   !Esta línea está etiquetada como línea 10
    
    
    ! Matrices (Arrays)
    ! =================
    array = (/1,2,3,4,5,6/)
    array = [1,2,3,4,5,6]    !usando la notación de Fortran 2003
    arrayb = [10.2,3e3,0.41,4e-5]
    array2d =  reshape([1.0,2.0,3.0,4.0,5.0,6.0], [3,2])
    
    ! La indexación de matrices en Fortran comienza desde 1, por defecto.
    ! (pero se puede definir de forma diferente para matrices específicas).
    v = array(1)    !toma el primer elemento del array.
    v = array2d(2,2)
    
    print *, array(3:5) !imprime todos los elementos del 3 al 5 (inclusive)
    print *, array2d(1,:)    !imprime la primera columna del array 2d.
    
    array = array*3 + 2  !puede aplicar expresiones matemáticas a los arrays.
    array = array*array  !Las operaciones con arrays se producen de forma elemental.
    !array = array*array2d    !estas matrices no serían compatibles.
    
    ! Hay muchas funciones incorporadas que operan sobre arrays.
    c = dot_product(array,array)    !este es el producto punto.
    ! Use matmul() para matemáticas matriciales.
    c = sum(array)
    c = maxval(array)
    print *, minloc(array)
    c = size(array)
    print *, shape(array)
    m = count(array > 0)
    
	!Bucle sobre un array (podría haber usado la función Product() normalmente).
    v = 1
    do i = 1, size(array)
        v = v*array(i)
    end do
    
    ! Condicionalmente ejecutamos asignaciones de elementos.
    array = [1,2,3,4,5,6]
    where (array > 3)
        array = array + 1
    elsewhere (array == 2)
        array = 1
    elsewhere
        array = 0
    end where
    
	! Los bucles Implied-DO son una forma compacta de crear matrices.
    array = [ (i, i = 1,6) ]       !crea una matriz de [1,2,3,4,5,6]
    array = [ (i, i = 1,12,2) ]    !crea una matriz de [1,3,5,7,9,11]
    array = [ (i**2, i = 1,6) ]    !crea una matriz de  [1,4,9,16,25,36]
    array = [ (4,5, i = 1,3) ]     !crea una matriz de [4,5,4,5,4,5]
    

    ! Entrada/Salida
    ! ==============
    
    print *, b    !imprime la variable 'b' en la línea de comandos

    ! Podemos dar formato a la salida impresa.
    print "(I6)", 320       !imprime '   320'
    print "(I6.4)", 3       !imprime '  0003' 
    print "(F6.3)", 4.32    !imprime ' 4.320'
    
	! La letra indica el tipo que se espera y el número que aparece a
	! continuación indica el número de caracteres que se deben utilizar
	! para imprimir el valor.
    ! Las letras pueden ser I (entero), F (real), L (lógico),
    ! E (formato de ingeniería), A (caracteres) ...
    print "(I3)", 3200    !imprime '***' ya que el número no se ajusta.
    
    ! Podemos tener múltiples especificaciones de formato.
    print "(I5,F6.2,E6.2)", 120, 43.41, 43.41
    print "(3I5)", 10, 20, 30  !3 repeticiones de enteros (ancho de campo = 5)
    print "(2(I5,F6.2))", 120, 43.42, 340, 65.3   !agrupación repetida de formatos.

    ! También podemos leer la entrada desde el terminal.
    read *, v
    read "(2F6.2)", v, x    !lee dos números

    ! Para leer un archivo.
    open(unit=11, file="records.txt", status="old") 
	! Referenciamos el archivo mediante un 'número de unidad', un número
	! entero seleccionado en el rango 9:99. El estado puede ser {'old', 'replace', 'new'}.
    read(unit=11, fmt="(3F10.2)") a, b, c
    close(11)

    ! Para escribir un archivo.
    open(unit=12, file="records.txt", status="replace")
    write(12, "(F10.2,F10.2,F10.2)") c, b, a
    close(12)

	! Hay más funciones disponibles de las que se analizan aquí y
	! variantes alternativas debido a la compatibilidad con versiones
	! anteriores de Fortran
    
    
    ! Funciones incorporadas
    ! ======================

    ! Fortran tiene unas 200 funciones/subrutinas intrínsecas al lenguaje.
    ! Ejemplos - 
    call cpu_time(v)   !establece 'v' a un tiempo en segundos.
    k = ior(i,j)       !bitwise OR de 2 enteros.
    v = log10(x)       !logarítmo en base 10.
    i = floor(b)       !devuelve el entero más cercano menor o igual que x.
    v = aimag(w)       !parte imaginaria de un número complejo.
    

    ! Funciones y subrutinas
    ! ======================
    
	! Una subrutina ejecuta algo de código sobre algunos valores de
	! entrada y puede provocar efectos secundarios o modificar los
	! valores de entrada.
    
    call routine(a,c,v)    !llamada a la subrutina.
    
	! Una función toma una lista de parámetros de entrada y devuelve un
	! solo valor.
    ! Sin embargo, los parámetros de entrada pueden modificarse y
	! ejecutar efectos secundarios.
    
    m = func(3,2,k)  !llamada a una función.
    
    ! Las llamadas a funciones también pueden evocarse dentro de expresiones.
    Print *, func2(3,2,k) 
    
    ! Una función pura es una función que no modifica sus parámetros de
	! entrada ni causa efectos secundarios.
    m = func3(3,2,k)


contains ! Zona para definir subprogramas internos del programa.

    ! Fortran tiene un par de formas ligeramente diferentes para definir funciones.

    integer function func(a,b,c) !una función que devuelve un valor entero.
        implicit none   !también es mejor usar implicit none en las definiciones de funciones.
        integer :: a,b,c !tipo de parámetros de entrada definidos dentro de la función.
        if (a >= 2) then
            func = a + b + c !la variable de retorno predeterminada al nombre de la función.
            return !puede devolver el valor actual de la función en cualquier momento.
        endif
        func = a + c
        ! No necesita una declaración de devolución al final de la función.
    end function func


    function func2(a,b,c) result(f)    !variable de retorno declarada como 'f'.
        implicit none
        integer, intent(in) :: a,b  !puede declarar y hacer cumplir que las
                         !las variables no sean modificadas por la función.
        integer, intent(inout) :: c
        integer :: f    !function tipo de retorno declarado dentro de la función.
        integer :: cnt = 0    !GOTCHA - la inicialización implica que la 
                           !variable es guardada entre llamadas de función.
        f = a + b - c
        c = 4    !alterando el valor de una variable de entrada.
        cnt  = cnt + 1    !cuenta el número de llamadas de función.
    end function func2


    pure function func3(a,b,c)  !una función pura no puede tener efectos secundarios.
        implicit none
        integer, intent(in) :: a,b,c
        integer :: func3
        func3 = a*b*c
    end function func3


    subroutine routine(d,e,f)
        implicit none
        real, intent(inout) :: f
        real, intent(in) :: d,e
        f = 2*d + 3*e + f
    end subroutine routine


end program example   ! Fin de la definición del programa -----------------


! Las funciones y subrutinas declaradas externamente a la lista de
! programas deben declararse al programa utilizando una declaración de
! interfaz (incluso si están en el mismo archivo fuente) (ver más abajo).
! Es más fácil definirlos dentro de la sección 'contains' de un módulo o programa.


elemental real function func4(a) result(res)
! Una función elemental es una función pura que toma una variable de
! entrada escalar pero también puede usarse en un array donde se aplicará
! por separado a todos los elementos del array y devolverá un nuevo array.
    real, intent(in) :: a
    res = a**2 + 1.0
end function func4


! Módulos
! =======

! Un módulo es una forma útil de recopilar declaraciones, funciones y
! subrutinas relacionadas para su reutilización.

module fruit
    real :: apple
    real :: pear
    real :: orange
end module fruit


module fruity
    ! Las declaraciones se harán en el orden: módulos, interfaces, variables.
    ! (También se pueden declarar módulos e interfaces en programas).
    
    use fruit, only: apple, pear    ! use apple y pear del módulo fruit.
    implicit none    !viene después de las importaciones de módulos.

    private    !hace cosas que sean privadas para el módulo (por defecto es público).
    ! Declare algunas variables/functions explícitamente públicas.
    public :: apple,mycar,create_mycar
    ! Declare algunas variables/functions privadas al módulo (redundante aquí).
    private :: func4
    
    ! Interfaces
    ! ==========
	! Declarar explícitamente una function/procedure externo dentro del
	! módulo (en general, es mejor colocar las functions/procedures en la
	! sección 'contains')
    interface
        elemental real function func4(a) result(res)
            real, intent(in) :: a
        end function func4
    end interface
    
	! Las funciones overloaded se pueden definir usando interfaces nombradas.
    interface myabs
		! Puede utilizar la palabra clave 'module procedure' para
		! incluir funciones ya definidas dentro del módulo.
        module procedure real_abs, complex_abs
    end interface 
        
    ! Tipos de datos derivados
    ! ========================
	! Puede crear colecciones de datos estructurados personalizados.
    type car
        character (len=100) :: model
        real :: weight    !(kg)
        real :: dimensions(3)    !p.e., largo-ancho-alto (metros.
        character :: colour
    end type car
    
    type(car) :: mycar    ! declara una variable de un tipo personalizado.
	! Consulte la rutina create_mycar() para su uso
    
    ! Nota: No hay sentencias ejecutables en los módulos.
    
contains

    subroutine create_mycar(mycar)
		! Demostración del uso de un tipo de datos derivado
        implicit none
        type(car),intent(out) :: mycar
        
        ! Acceda a los elementos de tipo utilizando el operador '%'.
        mycar%model = "Ford Prefect"
        mycar%colour = 'r'
        mycar%weight = 1400
        mycar%dimensions(1) = 5.0  !por defecto la indexación comienza desde 1!
        mycar%dimensions(2) = 3.0
        mycar%dimensions(3) = 1.5
        
    end subroutine

    real function real_abs(x)
        real :: x
        if (x<0) then
            real_abs = -x
        else
            real_abs = x
        end if
    end function real_abs
    
    real function complex_abs(z)
        complex :: z
		! las líneas largas se pueden continuar usando el carácter de continuación '&'
        complex_abs = sqrt(real(z)**2 + &
                                         aimag(z)**2)
    end function complex_abs


end module fruity

```

### Más recursos

Para más información sobre Fortran::

+ [wikipedia](https://en.wikipedia.org/wiki/Fortran)
+ [Fortran_95_language_features](https://en.wikipedia.org/wiki/Fortran_95_language_features)
+ [fortranwiki.org](http://fortranwiki.org)
+ [www.fortran90.org/](http://www.fortran90.org)
+ [Fortran wikibook](https://en.wikibooks.org/wiki/Fortran)
+ [Fortran courses](http://www.fortranplus.co.uk/courses/)
+ [Mistakes in Fortran 90 Programs That Might Surprise You](http://www.cs.rpi.edu/~szymansk/OOF90/bugs.html)

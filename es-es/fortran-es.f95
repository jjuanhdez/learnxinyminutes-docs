
! Esto es un comentario.


program ejemplo   ! Declara un programa llamado ejemplo.

    ! El código sólo puede existir dentro de los programas, funciones,
    ! subrutinas o módulos.
    ! No es necesario utilizar el sangrado, pero es recomendable.


    ! Declaración de variables
    ! ========================
    
    ! Todas las declaraciones se hacen antes que las sentencias y expresiones.
    
    implicit none !previene la declaración dinámica de variables (recomendado!)
    ! Implicit no debe redeclararse en cada función/programa/módulo...
    
    ! IMPORTANTE - Fortran es insensible a mayúsculas y minúsculas.
    real z
    REAL Z2

    real :: v, x  ! ADVERTENCIA: los valores iniciales dependen del compilador.
    real :: a = 3, b = 2E12, c = 0.01
    integer :: i, j, k = 1, m
    real, parameter :: PI = 3.1415926535897931    !declara una constante.
    logical :: y = .TRUE. , n = .FALSE.     !tipos booleanos.
    complex :: w = (0,1)                    !sqrt(-1)
    character (len = 3) :: month            !cadena de 3 caracteres.
    
    real :: array(6)                !declara una matriz (array) de 6 reales.
    real, dimension(4) :: arrayb    !otra forma de declarar una matriz.
    integer :: arrayc(-10:10)       !una matriz con un índice personalizado.
    real :: array2d(3,2)            !una matriz multidimensional.
    
    ! Los separadores '::' no siempre son necesarios pero se recomiendan.

    ! también existen muchos otros atributos variables:
    real, pointer :: p    !declara un puntero.

    integer, parameter :: LP = selected_real_kind(20)
    real (kind = LP) :: d    !variable de precisión larga.

    ! ADVERTENCIA: la inicialización de variables durante la declaración causa
    ! problemas en las funciones, ya que esto implica automáticamente el 
    ! atributo 'save', por el que se almacenan los valores entre llamadas
    ! de función. En general, ¡separar el código de declaración y de 
    ! inicialización, excepto para las constantes!
    
    
    ! Cadenas (Strings)
    ! =================

    character :: a_char = 'i'
    character (len = 6) :: a_str = "qwerty"
    character (len = 30) :: str_b
    character (len = *), parameter :: a_long_str = "Es una cadena larga."
    !hay contador automático de longitud (len=*) pero sólo para constantes.
    
    
    str_b = a_str // " keyboard"    !concatene cadenas mediante el operador //.


    ! Asignación y aritmética
    ! =======================

    Z = 1    !asigna a la variable anteriormente Z declarada (case insensitive)
    j = 10 + 2 - 3
    a = 11.54  /  (2.3 * 3.1)
    b = 2**3    !exponenciación


    ! Estructuras de control de flujo y operadores
    ! ============================================

    ! Estructuras de línea única
    if (z == a) b = 4  !siempre necesitan paréntesis circundantes.

    if (z /= a) then !z no es igual a a
    ! Otros símbolos de comparación son < > <= >= == /=
      b = 4
    else if (z .GT. a) then !z es mayopr que a
    ! Texto equivalente a operadores de símbolo: .LT. .GT. .LE. .GE. .EQ. .NE.
      b = 6
    else if (z < a) then !'then' debe estar en esta línea.
      b = 5 !el bloque de ejecución debe estar en una nueva línea.
    else
      b = 10
    end if     !la sentencia end necesita un 'if' (o puede usar 'endif').


    if (.NOT. (x < c .AND. v >= a .OR. z == z)) then   !operadores booleanos.
      inner: if (.TRUE.) then    !puede nombrar if-construct.
        b = 1
      endif inner    !entonces debe nombrar la sentencia endif.
    endif


    i = 20
    select case (i)
      case (0)      !caso de i == 0
        j=0
      case (1:10)    !caso i es de 1 a 10 inclusive.
        j=1
      case (11:)    !todos los casos donde i >= 11
        j=2
      case default
        j=3
    end select


    month = 'enero'
    ! La condición puede ser de tipo entero, lógico o de carácter.
    ! También se pueden nombrar determinadas construcciones.
    monthly: select case (month)
      case ("enero")
         j = 0
      case default
         j = -1
    end select monthly


    do i = 2, 10, 2    !bucles de 2 a 10 (inclusive) en incrementos de 2.
      innerloop: do j = 1, 3    !los bucles también pueden ser nombrados.
        exit    !salir del bucle.
      end do innerloop
    cycle        !saltar a la siguiente iteración del bucle.
    enddo

    
    ! La sentencia Goto existe, sin embargo está fuertemente desaconsejada.
    goto 10    
    stop 1      !detiene el código inmediatamente, 
                !(devolviendo el código de condición especificado).
10  j = 201     !esta línea se etiqueta como línea 10
    
    
    ! Matrices (Arrays)
    ! =================
    array = (/1,2,3,4,5,6/)
    array = [1,2,3,4,5,6]    !usando la notación Fortran 2003.
    arrayb = [/10.2,3e3,0.41,4e-5/]
    array2d =  reshape([/1.0,2.0,3.0,4.0,5.0,6.0/], [/3,2/])
    
    ! La indexación de matrices Fortran comienza desde 1 por defecto,
    ! (pero se puede definir de forma diferente para matrices específicas).
    
    v = array(1)    !toma el primer elemento de la matriz.
    v = array2d(2,2)
    
    print *, array(3:5)    !imprime todos los elementos del 3º al 5º (inclusive)
    print *, array2d(1,:)  !imprimir la primera columna de la matriz 2d.
    
    array = array*3 + 2    !puede aplicar expresiones matemáticas a los arrays.
    array = array*array    !operaciones de arrays ocurren a nivel de elementos.
    !array = array*array2d    !estas matrices no serían compatibles.
    
    ! Hay muchas funciones incorporadas que funcionan en matrices.
    c = dot_product(array,array)    !este es el producto del punto.
    ! Utilice matmul() para matemáticas matriciales.
    c = sum(array)
    c = maxval(array)
    print *, minloc(array)
    c = size(array)
    print *, shape(array)
    m = count(array > 0)
    
    !Bucle sobre un array (podría haber usado la función Product() normalmente)
    v = 1
    do i = 1, size(array)
        v = v*array(i)
    end do
    
    ! Ejecute condicionalmente las asignaciones de elementos.
    array = [1,2,3,4,5,6]
    where (array > 3)
        array = array + 1
    elsewhere (array == 2)
        array = 1
    elsewhere
        array = 0
    end where
    
    ! Los bucles DO implícitos son una forma compacta de crear matrices.
    array = [ (i, i = 1,6) ]      !crea una matriz de [1,2,3,4,5,6]
    array = [ (i, i = 1,12,2) ]   !crea una matriz de [1,3,5,7,9,11]
    array = [ (i**2, i = 1,6) ]   !crea una matriz de [1,4,9,16,25,36]
    array = [ (4,5, i = 1,3) ]    !crea una matriz de [4,5,4,5,4,5]
    

    ! Input/Output
    ! ============
    
    print *, b    !imprime la variable' b' en la línea de comandos.

    ! Podemos formatear nuestra salida impresa.
    print "(I6)", 320        !imprime '   320'
    print "(I6.4)", 3        !imprime '  0003' 
    print "(F6.3)", 4.32    !imprime ' 4.320'
    
    ! La letra indica el tipo esperado y el número después da el número de
    ! caracteres a utilizar para imprimir el valor.
    ! Las letras pueden ser I (entero), F (real), E (formato de ingeniería), 
    ! L (lógico), A (caracteres) ...
    print "(I3)", 3200    !imprime '***' ya que el número no encaja.
    
    ! podemos tener múltiples especificaciones de formato.
    print "(I5,F6.2,E6.2)", 120, 43.41, 43.41
    print "(3I5)", 10, 20, 30   !3 repeticiones de números enteros 
                                ! (anchura del campo = 5).
    print "(2(I5,F6.2))", 120, 43.42, 340, 65.30 !agrupación repetida de formatos

    ! También podemos leer entradas desde el terminal.
    read *, v
    read "(2F6.2)", v, x    !leer dos números.

    ! Para leer un archivo.
    open(unit = 11, file = "records.txt", status = "old") 
    ! El archivo es referido por un "número de unidad", un número entero que
    ! se selecciona en el intervalo 9:99. El estado puede ser uno entre
    ! {'old','replace','new'}.
    read(unit = 11, fmt = "(3F10.2)") a, b, c
    close(11)

    ! Para escribir un archivo.
    open(unit = 12, file = "records.txt", status = "replace")
    write(12, "(F10.2,F10.2,F10.2)") c, b, a
    close(12)

    ! Hay más funciones disponibles de las descritas aquí y variantes
    ! alternativas debido a la compatibilidad con anteriores versiones Fortran.
    
    
    ! Funciones integradas
    ! ====================

    !Fortran tiene alrededor de 200 funciones/subrutinas intrínsecas al lenguaje
    ! Ejemplos -
    call cpu_time(v)    !establece en 'v' un tiempo en segundos.
    k = ior (i, j)  !bitwise OR de 2 enteros.
    v = log10(x)    !logaritmo en base 10.
    i = floor(b)    !devuelve el entero más cercano menor o igual a x.
    v = aimag(w)    !parte imaginaria de un número complejo.

    
    ! Funciones y Subrutinas
    ! ======================
    
    ! Una subrutina ejecuta algún código en algunos valores de entrada y puede
    ! causar efectos secundarios o modificar los valores de entrada.
    
    call routine(a, c, v)  ! llamada a una subrutina.
    
    ! Una función toma una lista de parámetros de entrada y devuelve un solo 
    ! valor. Sin embargo, los parámetros de entrada pueden ser modificados y 
    ! los efectos secundarios ejecutados.
    
    m = func(3, 2, k)      ! llamada a una función.
    
    ! Las llamadas de función también se pueden evocadar dentro de expresiones.
    Print *, func2(3, 2, k) 
    
    ! Una función pura es una función que no modifica sus parámetros de
    ! entrada ni causa efectos secundarios.
    m = func3(3, 2, k)


contains ! Zona para definir subprogramas internos al programa.

    !Fortran tiene un par de formas ligeramente diferentes de definir funciones

    integer function func(a, b, c)  !una función que devuelve un valor entero.
        implicit none   !es mejor utilizar implicit none 
                        !también en las definiciones de función.
        integer :: a, b, c  !tipos de parámetros de entrada definidos dentro
                            !de la función.
        if (a >= 2) then
            func = a + b + c    !la variable de retorno predeterminada es 
                                !el nombre de la función.
            return      !puede devolver el valor actual de la función en
                        !cualquier momento.
        endif
        func = a + c
        ! No necesita una declaración de return al final de una función.
    end function func


    function func2(a, b, c) result(f)  !variable de retorno declarada como 'f'.
        implicit none
        integer, intent(in) :: a, b    !puede declarar y hacer que las 
                                !variables no sean modificadas por la función.
        integer, intent(inout) :: c
        integer :: f  !tipo de retorno de función declarado dentro de la función
        integer :: cnt = 0      !GOTCHA - la inicialización implica que la
                                !variable se guarda entre llamadas de función.
        f = a + b - c
        c = 4                !modificar el valor de una variable de entrada.
        cnt  = cnt + 1       !cuenta el número de llamadas de función.
    end function func2


    pure function func3(a,b,c)  !una función pura no puede tener 
                                !efectos secundarios.
        implicit none
        integer, intent(in) :: a, b, c
        integer :: func3
        func3 = a *b * c
    end function func3


    subroutine routine(d, e, f)
        implicit none
        real, intent(inout) :: f
        real, intent(in) :: d, e
        f = 2*d + 3*e + f
    end subroutine routine


end program ejemplo   ! Fin de la definición del programa ---------------------


! Las funciones y subrutinas declaradas externamente a la lista de programas
! deben ser declaradas al programa mediante una declaración de interfaz
! (¡incluso si están en el mismo archivo fuente!) (Véase más abajo). Es más
! fácil definirlos dentro de la sección 'contains' de un módulo o programa.

elemental real function func4(a) result(res)
! Una función elemental es una función pura que toma una variable de entrada
! escalar, pero también se puede usar en una matriz donde se aplicará por
! separado a todos los elementos de una matriz y devolverá una matriz nueva.
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
    ! Las declaraciones deben estar en el orden: módulos, interfaces, variables
    ! (¡puede declarar también módulos e interfaces en programas!).
    
    use fruit, only: apple, pear    ! user apple y pear del módulo fruit
    implicit none                    ! viene después de importar el módulo.

    private   !hacer las cosas privadas para el módulo (por defecto es público)
    ! Declarar explícitamente algunas variables/funciones públicamente.
    public :: apple, mycar, create_mycar
    ! Declarar algunas variables/funciones privadas al módulo (redundante aquí)
    private :: func4
    
    ! Interfaces
    ! ==========
    ! Declarar explícitamente una función/procedimiento externo dentro del
    ! módulo (mejor en general poner funciones/procedimientos en la sección
    ! 'contains').
    interface
        elemental real function func4(a) result(res)
            real, intent(in) :: a
        end function func4
    end interface
    
    ! Funciones sobrecargadas se pueden definir mediante interfaces específicas
    interface myabs
        ! Puede utilizar la palabra clave 'module procedure' para incluir 
        ! funciones ya definidas dentro del módulo.
        module procedure real_abs, complex_abs
    end interface 


    ! Tipos de datos derivados
    ! ========================
    ! Puede crear colecciones de datos estructuradas y personalizadas.
    type car
        character (len=100) :: model
        real :: weight          !(kg)
        real :: dimensions(3)   !es decir, longitud, anchura y altura (metros)
        character :: colour
    end type car
    
    type(car) :: mycar    !declare una variable de su tipo personalizado.
    ! Consulte la rutina create_mycar() para su uso.
    
    ! Nota: No hay sentencias ejecutables en los módulos.
    
contains

    subroutine create_mycar(mycar)
        ! Demuestra el uso de un tipo de datos derivado.
        implicit none
        type(car),intent(out) :: mycar
        
        ! Acceda a los elementos del tipo mediante el operador '%'
        mycar%model = "Ford Kuga"
        mycar%colour = 'r'
        mycar%weight = 1400
        mycar%dimensions(1) = 5.0    !la indexación por defecto comienza en 1!
        mycar%dimensions(2) = 3.0
        mycar%dimensions(3) = 1.5
        
    end subroutine

    real function real_abs(x)
        real :: x
        if (x < 0) then
            real_abs = -x
        else
            real_abs = x
        end if
    end function real_abs
    
    real function complex_abs(z)
        complex :: z
        ! las líneas muy largas pueden continuar utilizando el carácter
        ! de continuación "&".
        complex_abs = sqrt(real(z)**2 + &
                                         aimag(z)**2)
    end function complex_abs


end module fruity

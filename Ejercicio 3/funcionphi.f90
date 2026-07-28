! ==============================================================================
! 1. Creamos un módulo para tu ecuación en tu mismo archivo.
! Al estar aquí, evitamos el "Stack Trampoline" que causaba el Segmentation Fault.
! ==============================================================================
module mi_funcion
  use iso_fortran_env, only: wp => real64
  implicit none
contains
  real(wp) function phi(x)
    real(wp), intent(in) :: x
    ! Agregamos los sufijos .0_wp para no mezclar enteros de 32 bits con flotantes de 64
    phi = 5.0_wp * (1.0_wp - exp(-x))
  end function phi
end module mi_funcion


! ==============================================================================
! 2. Tu Programa Principal (Llamando al módulo roots intacto)
! ==============================================================================
Program Iteracion_Punto_Fijo
  use iso_fortran_env, only: wp => real64
  use roots, only: punto_fijo
  use mi_funcion, only: phi   ! <--- Importamos tu función desde el módulo de arriba
  implicit none (type, external)
  
  real(wp) :: x0, tol, raiz
  integer  :: n, clave

  ! Datos iniciales
  x0  = 5.0_wp
  tol = 5.0e-8_wp
  n   = 100

  ! Llamo a la subrutina del módulo roots que te dio la cátedra y calculo la raíz
  call punto_fijo(phi, x0, n, tol, raiz, clave)
  
  if (clave == 0) then
     write(*,*) 'La raiz es =', raiz
     write(*,*) 'numero de iteraciones =', n
  else
     write(*,*) 'Error =', clave
  endif

end program Iteracion_Punto_Fijo

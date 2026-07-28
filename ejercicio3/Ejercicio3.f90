module funcionphi
  use iso_fortran_env, only: wp => real64
contains
  real(wp) function phi(x)
    real(wp), intent(in) :: x
    phi = 5.0_wp*(1.0_wp-exp(-x))
  end function phi
end module funcionphi

Program Iteracion_Punto_Fijo
  use iso_fortran_env, only: wp => real64
  use roots, only: punto_fijo
  use funcionphi, only: phi
  implicit none (type, external)
  real(wp) :: x0, tol, raiz
  integer  :: n, clave

  x0 = 5.0_wp    !Datos iniciales
  tol = 5.0e-8_wp
  n = 100

  call punto_fijo(phi,x0,n,tol,raiz,clave)  !llamo a la subrutina y calculo la raiz
  if (clave==0) then
     write(*,*) 'La raiz es =', raiz
     write(*,*) 'numero de iteraciones =',n
  else
     write(*,*) 'Error =',clave
  endif

  end program Iteracion_Punto_Fijo 
     

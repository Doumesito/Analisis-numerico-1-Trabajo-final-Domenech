Program minimos_cuadrados
  use iso_fortran_env, only: wp => real64
use f95_lapack, only: la_gels

implicit none

real(wp), allocatable :: A(:,:),y(:),r(:),y_obs(:),t(:)
integer i
real(wp) y_apr
allocate(y(24),A(24,2),r(24),y_obs(24),t(24))
!para llamar a LA_GELS necesito definir mi matriz de diseño A y mi vector de mediciones
!mi tabla de y_i
y=[5.0291, 6.5099, 5.3666, 4.1272, 4.2948, 6.1261, 10.0502, 9.1614, 7.5677, &
   7.2920, 10.0357, 11.0708, 13.4045, 12.8415, 11.9666, 11.0765, 11.7774, 14.5701, 17.0440, &
   17.0398, 15.9069, 15.4850, 15.5112, 17.6572]
t = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]

y_obs=y !copio el vector    

do i = 1,24                !armo la matriz de diseño
   A(i,1) = 1              
   A(i,2) =t(i)
enddo

call LA_GELS(A,y)
write(*,*) 'Coefeicientes de la recta ajustada='
write(*,*) 'ordenada:', y(1)
write(*,*) 'pendiente:', y(2)
write(*,*) ' i      y_obs        y_apr       Residuo'

!calculo los residuos

do i = 1,24
   y_apr = y(1) + y(2)*t(i)
   r(i)= y_obs(i)- y_apr
   if(i<7.0) then
   write(*, '(I2, 3(3X, F10.4))') i, y_obs(i), y_apr, r(i)
else
   write(*, '(I2, 3(3X, F10.4))')  i+1, y_obs(i), y_apr, r(i)
   endif
enddo


end program minimos_cuadrados 

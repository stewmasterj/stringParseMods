! compile: gfortran ../lineParse.f90 calc.f90 -o calc
program calc
use lineParse
implicit none
integer :: err
character(80) :: test, pi

! variable:
write(pi,*) dacos(-1.d0)

call getarg( 1 , test )
! sub var pi
test = s_sub( "$pi", trim(test), trim(pi), err )
write(6,*) evaluate( trim(test) )

end program calc

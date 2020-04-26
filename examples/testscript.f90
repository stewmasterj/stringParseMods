! compile: gfortran ../lineParse.f90 ../scriptMod.f90 testscript.f90 -o testscript
! main driver program for the scriptMod.f90
program testscript
use scriptMod
implicit none
character(80) :: fil

! provide a script file as first argument
call getarg(1,fil)

call loadScript( 10, trim(fil) )
call runScript ! just basic stuff, no custom commands

end program testscript

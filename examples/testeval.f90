! compile: gfortran ../lineParse.f90 testeval.f90 -o testeval
program testeval
use lineParse
implicit none
character(80) :: test, eval, pr
!real(4) :: eval

pr = "/home/stewmasterj/progs/e"

test = "7.0^3.0+2.0^2.0  "
write(6,*) trim(test)
eval = evaluate( test )
write(6,*) eval
call system( pr//" '"//trim(test)//"'" )

test = "2.0+7.0^3.0-1.0  "
write(6,*) trim(test)
eval = evaluate( test )
write(6,*) eval
call system( pr//" '"//trim(test)//"'" )

test = "2.0+4.0/2.0-7.0^3.0  "
write(6,*) trim(test)
eval = evaluate( test )
write(6,*) eval
call system( pr//" '"//trim(test)//"'" )

test = "(2.0+4.0/2.0)-7.0^3.0  "
write(6,*) trim(test)
eval = evaluate( test )
write(6,*) eval
call system( pr//" '"//trim(test)//"'" )

test = "4.0/2.0-(7.0^3.0+2.0)  "
write(6,*) trim(test)
eval = evaluate( test )
write(6,*) eval
call system( pr//" '"//trim(test)//"'" )

test = "2*sin(1.57)  "
write(6,*) trim(test)
eval = evaluate( test )
write(6,*) eval
call system( pr//" '"//trim(test)//"'" )

end program testeval

program testeval
use lineParse
implicit none
character(80) :: test
real(4) :: eval

test = "7.0^3.0  "
eval = evaluate( test )
write(6,*) eval

test = "2.0+7.0^3.0  "
eval = evaluate( test )
write(6,*) eval

test = "2.0+4.0/2.0-7.0^3.0  "
eval = evaluate( test )
write(6,*) eval

end program testeval

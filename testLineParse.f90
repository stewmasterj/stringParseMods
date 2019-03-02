! compile: gfortran lineParse.f90 testLineParse.f90 -o testLineParse
! Ross J. Stewart 02 June 2012
program testLineParse
use lineParse
implicit none
character :: attr
character (len=80) :: line, wrd
integer :: wcnt, i

read(5,'(A)') line
write(6,'(A,I4)') "Number of words in line: ",s_word_count(line)
write(6,'(A)') "In capitals..."
call lower2capital(line)
write(6,*) line
call capital2lower(line)
write(6,*) line
if (index(line,"!").ne.0) then
  write(6,*) "this line has a comment... removing"
  call left_of("!",line) ! remove comment
  write(6,*) line
endif
if (s_is_fortran(line)) then
 write(6,'(A)') "This line starts with fortran."
endif
wcnt=s_word_count(line)
write(6,'(A,I4)') "Now how many words in line: ",wcnt
do i=1,wcnt
   wrd=s_get_word(i,line,attr) ! attr is optional
   if (s_is_fortran(wrd,0)) then
     write(6,'(A,i3,A)') " word",i,": "//trim(wrd)//" is fortran, attr:"//attr
   elseif (s_is_number(wrd)) then
     write(6,'(A,i3,A)') " word",i,": "//trim(wrd)//" is a number, attr:"//attr
   else
     write(6,'(A,i3,A)') " word",i,": "//trim(wrd)//" is a character array, attr:"//attr
   endif
enddo

end program testLineParse

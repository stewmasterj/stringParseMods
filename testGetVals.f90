!compile: gfortran lineParse.f90  testGetVals.f90 -o testGetVals
program test
use lineParse
implicit none
integer :: wc, inte
character(80) :: line, word
logical :: bool
real(4) :: r4
real(8) :: r8

do
   ! jsut read stuff from commandline
   read(5,'(A)') line

   call left_of("#",  line )
   wc = s_word_count( line )

write(6,*) wc, line
   if (wc .gt. 1) then
      word = s_get_word(1, line)
write(6,*) word, s_get_word(2, line)
      if (word.eq."quit") then
         call exit
      elseif (word.eq."bool") then
         call s_get_val( 2, line, bool )
         write(6,*) bool
      elseif (word.eq."int") then
         call s_get_val( 2, line, inte )
         write(6,*) inte
      elseif (word.eq."real".or.word.eq."float") then
         call s_get_val( 2, line, r4 )
         write(6,*) r4
      elseif (word.eq."dp".or.word.eq."double") then
         call s_get_val( 2, line, r8 )
         write(6,*) r8
      endif
   else
      write(0,*) "Type two words like"
      write(0,*) "  type  value"
   endif


enddo
end program test

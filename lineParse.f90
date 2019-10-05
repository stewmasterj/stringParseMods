! vim:fdm=marker
module lineParse
! logical function s_is_number(s)	is string a number?
! subroutine capital2lower(s)
! subroutine lower2capital(s)
! logical function s_is_fortran ( s, flag)
! integer(4) function s_word_count ( s )
! character(80) function s_get_word( n, s, attr )
! character(80) function s_get_word_range( n1, n2, s )
! subroutine s_get_val( n, s, val, attr )
! subroutine left_of( w , s )		returns part of s left of w
! integer function s_num_count( s )
! character(80) function s_get_num( n, s, attr )
! character(80) function s_get_line( fid, ln, eo )
! real(4) function evaluate( s )

! overload the s_get_val functions for various value types
interface s_get_val
 module procedure s_get_vali, s_get_valr4, s_get_valr8, s_get_vall
end interface

contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
function s_is_number(s) !{{{
! Ross J. Stewart, 01 June 2012
implicit none
logical :: s_is_number
integer ( kind = 4 ) lens, i
character ( len = * ) s
character ( len = 19 ) :: numbers

numbers=" 0123456789.eEdD+-"//char(09)
lens = len ( s )
s_is_number =.true.

if ( lens <= 0 ) Return

do i = 1, lens
  if (index(numbers,s(i:i)).eq.0) then
     s_is_number =.false.
     Return
  endif
enddo

end function s_is_number !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine capital2lower(s) !{{{
implicit none
integer ( kind = 4 ) lens
integer ( kind = 4 ) i
character ( len = * ) s

lens = len ( s )

if ( lens <= 0 ) Return

do i = 1, lens
  if ( ichar(s(i:i)) .le. 90 .and. &
       ichar(s(i:i)) .ge. 65 ) then
     s(i:i)=char(ichar(s(i:i))+32)
  endif
enddo

end subroutine capital2lower !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine lower2capital(s) !{{{
implicit none
integer ( kind = 4 ) lens
integer ( kind = 4 ) i
character ( len = * ) s

lens = len ( s )

if ( lens <= 0 ) Return

do i = 1, lens
  if ( ichar(s(i:i)) .le. 122 .and. &
       ichar(s(i:i)) .ge. 97 ) then
     s(i:i)=char(ichar(s(i:i))-32)
  endif
enddo

end subroutine lower2capital !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
function s_is_fortran ( s, flag) !{{{
implicit none

integer :: i, lens, f
integer, optional ::  flag ! I want this to be optional
integer, dimension(2) :: se
logical :: s_is_fortran
character ( len = * ) s
character(len=15), dimension(71) :: fword

lens = len ( s )

s_is_fortran=.false.
if ( lens <= 0 ) Return
if (.not.present(flag)) then
  f=0 !since flag is optional
else
  f=flag
endif
s_is_fortran=.false.

fword(1:5)=(/"logical  ","character","integer  ","real     ","type     "/)
fword(6:12)=(/"program   ","end       ","call      ","function  ","use       ","include   ","subroutine"/)
fword(13:20)=(/"if    ","then  ","else  ","file  ","open  ","access","read  ","write "/)
fword(21:28)=(/"implicit   ","dimension  ","allocatable","return     ","goto       ","intent     ","optional   ","parameter  "/)
!fword(29:71)=(/"trim","sin","cos","tan","asin","acos","atan","index","cycle","and","or",&
!	"not","ne","eq","lt","le","gt","ge","do","iostat","select","case",&
!	"default","char","achar","in","inout","out","present","len","true","false",&
!	"none","iargc","getarg","unit","status","old","exit","stop","rewind","allocate",&
!	"for"/)

select case (f) 
 case (1); se=(/1,5/)   ! declarations
 case (2); se=(/6,12/)  ! structures
 case (3); se=(/13,20/) ! constructs
 case (4); se=(/21,28/)
 case (5); se=(/29,71/)
 case default; se=(/1,71/)
end select

do i = se(1), se(2)
  if (index(s,fword(i)).ne.0) then
    s_is_fortran=.true.
    return
  else
    s_is_fortran=.false.
  endif
enddo

end function s_is_fortran !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
function s_word_count ( s ) !{{{
!*****************************************************************************80
!! S_WORD_COUNT counts the number of "words" in a string.
!  Licensing:
!    This code is distributed under the GNU LGPL license.
!  Modified:
!    14 April 1999, 30 May 2012
!  Author:
!    John Burkardt
!  Made into a function by Ross J. Stewart
!  Parameters:
!    Input, character ( len = * ) S, the string to be examined.
!    Output, integer ( kind = 4 ) NWORD, the number of "words" in the string.
!    Words are presumed to be separated by one or more blanks.
implicit none

logical blank
integer ( kind = 4 ) i
integer ( kind = 4 ) lens
integer ( kind = 4 ) s_word_count
character ( len = * ) s
character ( len = 19 ) :: delimiters

delimiters=" ,;:()[]{}'=*%"//char(09)//char(34)
s_word_count = 0
lens = len ( s )

if ( lens <= 0 ) Return

blank = .true.

do i = 1, lens
    if ( index(delimiters,s(i:i)).ne.0 ) then
      blank = .true.
    else if ( blank ) then
      s_word_count = s_word_count + 1
      blank = .false.
    end if
enddo

end function s_word_count !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
function s_get_word( n, s, attr ) !{{{
! Ross J. Stewart, 01 June 2012
implicit none
logical :: blank, dquo, squo
integer ( kind = 4 ), intent(in) :: n
integer ( kind = 4 ) lens, i, m
integer ( kind = 4 ) s_word_count
character, optional, intent(out) :: attr
character ( len = * ), intent(in) :: s
character ( len = 80 )  s_get_word
character ( len = 18 ) :: delimiters
character ( len = 5 ) :: obrk, cbrk

obrk="([{'"//char(34)
cbrk=")]}'"//char(34)
delimiters=" ,:;=%"//char(09)//obrk//cbrk
s_word_count = 0
lens = len ( trim(s) )
s_get_word = ""

if ( lens <= 0 ) Return
if (present(attr)) attr = " "

blank = .true.
dquo = .false.
squo = .false.

do i = 1, lens
   if ( index(delimiters,s(i:i)).ne.0 ) then
      blank = .true.
      if (n.eq.s_word_count) Return
   elseif ( blank ) then
      s_word_count = s_word_count + 1
      blank = .false.
   endif
   if (.not.blank.and.n.eq.s_word_count) then
      s_get_word=trim(s_get_word)//s(i:i)
   endif

   if (present(attr).and.index(obrk,s(i:i)).ne.0 ) then
      attr=s(i:i) !set attr to the open bracket type
      if (.not.dquo.and.attr.eq.char(34)) dquo = .true.
      if (.not.squo.and.attr.eq."'") squo = .true.
   elseif (present(attr).and.index(cbrk,s(i:i)).ne.0 ) then
      m=index(cbrk,s(i:i))
      if (attr.eq.obrk(m:m)) then ! closing bracket
         attr= " " 
      if (attr.eq.char(34)) dquo = .false.
      if (attr.eq."'") squo = .false.
      end if
   endif
enddo

end function !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
function s_get_word_range( n1, n2, s ) !{{{
implicit none
character ( len = * ), intent(in) :: s
character ( len = 80 ) s_get_word_range
integer :: i, n1, n2

s_get_word_range = ""
do i = n1, n2
   s_get_word_range = trim(s_get_word_range)//" "//s_get_word(i, s)
enddo

end function !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! subroutine s_get_val( n, s, val, attr ) !Overloade d routine !{{{
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine s_get_vali( n, s, val, attr) !{{{ 
implicit none
integer, intent(in) :: n
character(*), intent(in) :: s
integer, intent(out) :: val
character, optional, intent(out) :: attr
character(80) :: tmp
integer :: err

if (present(attr)) then
   tmp = s_get_word( n, s, attr)
else
   tmp = s_get_word( n, s)
endif

! read the character into an integer value
read(tmp,*,iostat=err)  val

end subroutine s_get_vali !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine s_get_valr4( n, s, val, attr) !{{{
implicit none
integer, intent(in) :: n
character(*), intent(in) :: s
character, optional, intent(out) :: attr
character(80) :: tmp
integer :: err
real(4) :: val

if (present(attr)) then
   tmp = s_get_word( n, s, attr)
else
   tmp = s_get_word( n, s)
endif

! read the character into an integer value
read(tmp,*,iostat=err) val

end subroutine s_get_valr4 !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine s_get_valr8( n, s, val, attr) !{{{
implicit none
integer, intent(in) :: n
character(*), intent(in) :: s
character, optional, intent(out) :: attr
character(80) :: tmp
integer :: err
real(8) :: val

if (present(attr)) then
   tmp = s_get_word( n, s, attr)
else
   tmp = s_get_word( n, s)
endif

! read the character into an integer value
read(tmp,*,iostat=err)  val

end subroutine s_get_valr8 !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine s_get_vall( n, s, val, attr) !{{{
implicit none
integer, intent(in) :: n
character(*), intent(in) :: s
character, optional, intent(out) :: attr
character(80) :: tmp
integer :: err
logical :: val

if (present(attr)) then
   tmp = s_get_word( n, s, attr)
else
   tmp = s_get_word( n, s)
endif

! read the character into an integer value
read(tmp,*,iostat=err) val

end subroutine s_get_vall !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! end subroutine s_get_val !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine left_of( w , s ) !{{{
! returns the string, s, as everything left of the character or word, w
! Ross J. Stewart, 31 May 2012
implicit none
logical comment
integer ( kind = 4 ) i
integer ( kind = 4 ) lens, lenw
character ( len = * ) s
!character ( len = 80 ) stmp
character ( len = * ) w

lens = len ( s )
lenw = len ( w )

!if ( lens <= 0 .or. lenw .le. 0 .or. index(s,w).eq.0 ) Return
if ( lens <= 0 .or. lenw .le. 0 ) Return

comment = .false.

do i = 1, lens-lenw
   !if ( index(s(i:i+lenw-1),w).ne.0 .or. comment) then
   if ( s(i:i+lenw-1).eq.w .or. comment) then
        s = s(1:i-1)
        !comment=.true.
        !s(i:i)=" " ! overwrite the rest with blanks
   endif
enddo
        
end subroutine left_of !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
function s_num_count ( s ) !{{{
! Ross J. Stewart, 13 March 2016
!count numbers
implicit none

logical blank
integer ( kind = 4 ) i
integer ( kind = 4 ) lens
integer ( kind = 4 ) s_num_count
character ( len = * ) s
character ( len = 19 ) :: delimiters

delimiters="+-*^/"
s_num_count = 0
lens = len ( s )

if ( lens <= 0 ) Return

blank = .true.

do i = 1, lens
    if ( index(delimiters,s(i:i)).ne.0 ) then
      blank = .true.
    else if ( blank ) then
      s_num_count = s_num_count + 1
      blank = .false.
    end if
enddo

end function s_num_count !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
function s_get_num( n, s, attr ) !{{{
! Ross J. Stewart, 13 March 2016
! get numbers as if they're words
! here attr is the delimeter before the number
implicit none
logical :: blank
integer ( kind = 4 ), intent(in) :: n
integer ( kind = 4 ) lens, i
integer ( kind = 4 ) s_word_count
character, intent(out) :: attr
character ( len = * ), intent(in) :: s
character ( len = 80 )  s_get_num
character ( len = 18 ) :: delimiters

delimiters="+-*/^"
s_word_count = 0
lens = len ( trim(s) )
s_get_num = ""
attr = " "

if ( lens <= 0 ) Return

blank = .true.

do i = 1, lens
   if ( index(delimiters,s(i:i)).ne.0 ) then
      blank = .true.
      attr = s(i:i) ! save the delimeter caught
      if (n.eq.s_word_count) Return
   elseif ( blank ) then
      s_word_count = s_word_count + 1
      blank = .false.
   endif
   if (.not.blank.and.n.eq.s_word_count) then
      s_get_num=trim(s_get_num)//s(i:i)
   endif

enddo

end function !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
function getLine( fid, ln, eo ) !{{{
! Ross J. stewart  October 30, 2016
implicit none
integer :: err
integer, intent(out) :: eo
integer, intent(in) :: fid
integer, intent(inout) :: ln
character(80) :: getLine, line
   eo = 0
   read(fid,'(A)',iostat=err) line
   ln = ln + 1
   if (err.gt.0) then
      write(6,'(A)') line !show what they wrote
      write(0,'(A,i4,A,i4)') "getLine: read ERROR: iostat=",err," on input line: ",ln
      eo = 1 !error
   elseif (err.lt.0) then
      eo = -1 !End of script
   endif
   call left_of("!",line)
   call left_of("#",line) !input file can use either symbol for comments
   getLine = trim(line)

end function !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
function evaluate( s ) !{{{
implicit none
real(4) :: evaluate, x, y
character ( len = * ), intent(in) :: s
integer :: wc, dc, i, lens
character :: attr
character ( len = 19 ) :: delimiters
character ( len = 80 ) :: delims, cx, cy, tmp1, tmp2, tmp3
integer, dimension(30) :: deliml
logical :: first

first = .false.
delimiters="+-*^/"
wc = s_num_count( s )
dc = 0 
lens = len ( trim(s) )
tmp1 = trim(s)
delims = ""

! order and collect the delimeters
delims = ""
do i = 1, len(trim(tmp1))
   if ( index(delimiters,s(i:i)).ne.0 ) then
      dc = dc + 1
      delims = trim(delims)//s(i:i)
      deliml(dc) = i !location of delimiter
   end if
enddo

if (dc.ge.wc) first=.true.
!deal with exponents
do i = 1, dc
   if (delims(i:i).eq."^") then 
      if (first) then
         cx = s_get_num( i+1, tmp1, attr )
         cy = s_get_num( i+2, tmp1, attr )
      else
         cx = s_get_num( i,   tmp1, attr )
         cy = s_get_num( i+1, tmp1, attr )
      endif
      read(cx,*) x
      read(cy,*) y
      !must substitute the result into the main string
      write(6,*) "initial ",tmp1
      if (i.eq.1.and.i.eq.dc) then
         evaluate = x**y
         Return
      elseif (i.eq.1) then
         write(tmp2,*) x**y
         tmp3=trim(adjustl(tmp2))//tmp1(deliml(i+1):len(tmp1))
      elseif (i.eq.dc) then
         write(tmp2,*) x**y
         tmp3=tmp1(1:deliml(i-1))//trim(adjustl(tmp2))
      else
         write(tmp2,*) x**y
         tmp3=tmp1(1:deliml(i-1))//trim(adjustl(tmp2))//tmp1(deliml(i+1):len(tmp1))
      endif
      tmp1 = trim(tmp3)
      write(6,*) "exponents ",tmp1
   endif
enddo
delims = ""; dc = 0
do i = 1, len(trim(tmp1))
   if ( index(delimiters,tmp1(i:i)).ne.0 ) then
      dc = dc + 1
      delims = trim(delims)//tmp1(i:i)
      deliml(dc) = i !location of delimiter
   end if
enddo
!deal with Multiplication and division
do i = 1, dc
   if (delims(i:i).eq."*".or.delims(i:i).eq."/") then 
      if (first) then
         cx = s_get_num( i+1, tmp1, attr )
         cy = s_get_num( i+2, tmp1, attr )
      else
         cx = s_get_num( i,   tmp1, attr )
         cy = s_get_num( i+1, tmp1, attr )
      endif
      read(cx,*) x
      read(cy,*) y
      !must substitute the result into the main string
      if (i.eq.1.and.i.eq.dc) then
         if (delims(i:i).eq."*") then
            evaluate = x*y
         elseif (delims(i:i).eq."/") then
            evaluate = x/y
         endif
         Return
      elseif (i.eq.1) then
         if (delims(i:i).eq."*") then
            write(tmp2,*) x*y
         elseif (delims(i:i).eq."/") then
            write(tmp2,*) x/y
         endif
         tmp3=trim(adjustl(tmp2))//tmp1(deliml(i+1):len(tmp1))
      elseif (i.eq.dc) then
         if (delims(i:i).eq."*") then
            write(tmp2,*) x*y
         elseif (delims(i:i).eq."/") then
            write(tmp2,*) x/y
         endif
         tmp3=tmp1(1:deliml(i-1))//trim(adjustl(tmp2))
      else
         if (delims(i:i).eq."*") then
            write(tmp2,*) x*y
         elseif (delims(i:i).eq."/") then
            write(tmp2,*) x/y
         endif
         tmp3=tmp1(1:deliml(i-1))//trim(adjustl(tmp2))//tmp1(deliml(i+1):len(tmp1))
      endif
      tmp1 = trim(tmp3)
      write(6,*) "multdiv ", tmp1
   endif
enddo
delims = ""; dc = 0
do i = 1, len(trim(tmp1))
   if ( index(delimiters,tmp1(i:i)).ne.0 ) then
      dc = dc + 1
      delims = trim(delims)//tmp1(i:i)
      deliml(dc) = i !location of delimiter
   end if
enddo
!deal with Addition and Subtraction
do i = 1, dc
   if (delims(i:i).eq."+".or.delims(i:i).eq."-") then 
      if (first) then
         cx = s_get_num( i+1, tmp1, attr )
         cy = s_get_num( i+2, tmp1, attr )
      else
         cx = s_get_num( i,   tmp1, attr )
         cy = s_get_num( i+1, tmp1, attr )
      endif
      read(cx,*) x
      read(cy,*) y
      !must substitute the result into the main string
      if (i.eq.1.and.i.eq.dc) then
         if (delims(i:i).eq."+") then
            evaluate = x+y
         elseif (delims(i:i).eq."-") then
            evaluate = x-y
         endif
         Return
      elseif (i.eq.1) then
         if (delims(i:i).eq."+") then
            write(tmp2,*) x+y
         elseif (delims(i:i).eq."-") then
            write(tmp2,*) x-y
         endif
         tmp3=trim(adjustl(tmp2))//tmp1(deliml(i+1):len(tmp1))
      elseif (i.eq.dc) then
         if (delims(i:i).eq."+") then
            write(tmp2,*) x+y
         elseif (delims(i:i).eq."-") then
            write(tmp2,*) x-y
         endif
         tmp3=tmp1(1:deliml(i-1))//trim(adjustl(tmp2))
      else
         if (delims(i:i).eq."+") then
            write(tmp2,*) x+y
         elseif (delims(i:i).eq."-") then
            write(tmp2,*) x-y
         endif
         tmp3=tmp1(1:deliml(i-1))//trim(adjustl(tmp2))//tmp1(deliml(i+1):len(tmp1))
      endif
      tmp1 = trim(tmp3)
      write(6,*) "addsub ",tmp1
   endif
enddo

read(tmp1,*) evaluate

end function evaluate !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
end module lineParse 

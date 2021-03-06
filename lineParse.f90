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
! integer function s_pat_count ( w, s ) 
! integer function s_num_count( s )
! character(80) function s_get_num( n, s, attr )
! character(80) function s_get_line( fid, ln, eo )
! character(80) function s_sub( w, s, r )
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

delimiters=" ,;:{}'=%"//char(09)//char(34)
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

obrk="{'"//char(34)
cbrk="}'"//char(34)
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
function s_pat_count ( w, s ) !{{{
! returns the number of occurences of w in s
implicit none
integer ( kind = 4 ) s_pat_count
character ( len = *) :: s, w
integer :: ls, i, lw

ls = len(trim(s))
lw = len(trim(w))
s_pat_count = 0

do i = 1, ls-lw
  if (trim(w).eq.s(i:i+lw)) then
    s_pat_count = s_pat_count + 1
  endif
enddo

end function s_pat_count !}}}
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
      if (i.gt.1.and.s(i-1:i-1).ne."E".and.s(i-1:i-1).ne."e")  blank = .true.
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
      if (i.gt.1.and.s(i-1:i-1).ne."E".and.s(i-1:i-1).ne."e")  then
        blank = .true.
        attr = s(i:i) ! save the delimeter caught
        if (n.eq.s_word_count) Return
      endif
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
function s_sub( w, s, r, err ) !{{{
! substitute occurences of w in s with r
implicit none
character ( len = * ), intent(in) :: w, s, r
character(80) :: s_sub
integer :: lw, ls, wp, we
integer, optional :: err

lw = len(trim(adjustl(w)))
ls = len(trim(s))
if (lw.eq.0) then
 write(0,*) "ERROR: lineParse.f90:s_sub: word lenth is zero: "//trim(adjustl(w))
 call flush(0)
endif

! where is the word w in s?
wp = index( trim(s), trim(adjustl(w)) )
if (wp.eq.0) then
 if (present(err)) then
   err = 1
   s_sub = trim(s)
   return
 else
   write(0,*) "ERROR: lineParse.f90:s_sub: word: "//trim(adjustl(w))//" not found in string: "//trim(s)
   call flush(0)
   s_sub = trim(s)
   return
 endif
endif
we = wp +lw ! this is actually the position after the word by +1

if (wp.eq.1.and.lw.ge.ls) then ! the entire word
   s_sub = trim(adjustl(r))
elseif (wp.eq.1) then  ! at the start
   s_sub = trim(adjustl(r))//s(we:ls)
elseif (we-1.ge.ls) then
   s_sub = s(1:wp-1)//trim(adjustl(r))
else
   s_sub = s(1:wp-1)//trim(adjustl(r))//s(we:ls)
endif

end function s_sub !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
function s_get_between( w, s ) !{{{
! return string between the first two of "w" or match brackets
implicit none
character ( len = * ), intent(in) :: w, s
character(80) :: s_get_between
integer :: i, j, ls

ls = len(trim(s))
i = index(trim(s),trim(adjustl(w)))
select case ( w )
 case ("(");   j = index(s(i:ls),")")
 case ("[");   j = index(s(i:ls),"]")
 case ("{");   j = index(s(i:ls),"}")
 case default; j = index(s(i:ls),trim(adjustl(w)))
end select
s_get_between = s(i+1:i+j-2)

end function s_get_between !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
function evaluate( s ) !{{{
implicit none
!real(4) :: evaluate
character(80) :: evaluate
character ( len = * ), intent(in) :: s
integer :: wc, i, j, ls, bp, ep, np, bf
character ( len = 80 ) :: s1, s2, s3
character ( len = 19 ) :: delimiters
real(8) :: val

delimiters="+-*^/"
ls = len(trim(s))

np = 0
! outer loop for parensthenes
do i = 1, ls
 if (s(i:i).eq."(") np = np + 1   ! number of total parensthenes expected
enddo

s1 = trim(s)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
do i = 1, np ! loop over parensthenes sets
  ! find the innermost expression within ()
  ls = len(s1)
  bp = 0; ep = ls
  do j = 1, ls
   if (s1(j:j).eq."(") bp = j
   if (s1(j:j).eq.")") then
     ep = j
     exit
   endif
  enddo
  !if (bp.gt.0.and.ep.gt.bp+1) then ! there exists a parensthenes set
  !  s2 = s1(bp+1:ep-1)  !innermost expression
  !else
  !  s2 = trim(s1)
  !endif
  s2 = s1(bp+1:ep-1)  !innermost expression
  
  ! loop this for a given expression sans parensthenes
  wc = s_num_count( s2 )
  do j = 1, wc-1
   s3 = eval1( s2 )
   s2 = trim(s3)
  enddo

  ! check if parensthenes are a function
  if (bp.gt.1) then
    bf = 0
    do j = 1, 6
       if (bp-j.le.1) then 
         bf = 1; exit
       endif
       if (j.eq.1.and.index(delimiters,s1(bp-j:bp-j)).ne.0) exit ! no function
       if (index(delimiters,s1(bp-j:bp-j)).ne.0) then
         bf = bp-j+1; exit
       endif
    enddo
    if (bf.gt.0) then ! it's a function
      select case (s1(bf:bp-1)) ! what's the function name?
        case("sin"); read(s2,*) val
          write(s2,*) real(dsin(val))
        case("cos"); read(s2,*) val; write(s2,*) real(dcos(val))
        case("tan"); read(s2,*) val; write(s2,*) real(dtan(val))
        case("asin"); read(s2,*) val; write(s2,*) real(dasin(val))
        case("acos"); read(s2,*) val; write(s2,*) real(dacos(val))
        case("atan"); read(s2,*) val; write(s2,*) real(datan(val))
        case("log"); read(s2,*) val; write(s2,*) real(dlog(val))
        case("exp"); read(s2,*) val; write(s2,*) real(dexp(val))
        case default; write(0,*) "unrecognized math function: "//s1(bf:bp-1)
      end select
      bp = bf ! for substitution reasons below
    endif
  endif
  ! substitute result back into s1
  if (bp.eq.1.and.ep.eq.ls) then
     s1 = s2
  elseif (bp.eq.1) then
     s1=trim(adjustl(s2))//s1(ep+1:ls)
  elseif (ep.eq.ls) then
     s1=s1(1:bp-1)//trim(adjustl(s2))
  else
     s1=s1(1:bp-1)//trim(adjustl(s2))//s1(ep+1:ls)
  endif
  
enddo

! loop this for a given expression sans parensthenes
wc = s_num_count( s1 )
do j = 1, wc-1
 s2 = eval1( s1 )
 s1 = trim(s2)
enddo
!read(s1,*) evaluate
evaluate = trim(s1)

end function evaluate !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
function eval1( s ) !{{{
implicit none
real(4) :: x, y
integer :: wc, dc, i, lens, ival
character ( len = * ), intent(in) :: s
integer, dimension(30) :: deliml
character ( len = 19 ) :: delimiters
character ( len = 80 ) :: delims, cx, cy, tmp2, tmp3, eval1
character :: attr
logical :: first

wc = s_num_count( s )
first = .false.
delimiters="+-*^/"
dc = 0 
delims = ""
lens = len ( trim(s) )

! order and collect the delimeters
delims = ""
do i = 1, lens
   if ( index(delimiters,s(i:i)).ne.0 ) then
      if (i.gt.1.and.s(i-1:i-1).ne."E".and.s(i-1:i-1).ne."e") then
       dc = dc + 1
       delims = trim(delims)//s(i:i)
       deliml(dc) = i !location of delimiter
      endif
   end if
enddo

if (dc.ge.wc) first=.true.
!deal with exponents
do i = 1, dc
   if (delims(i:i).eq."^") then 
      if (first) then
         cx = s_get_num( i+1, s, attr )
         cy = s_get_num( i+2, s, attr )
      else
         cx = s_get_num( i,   s, attr )
         cy = s_get_num( i+1, s, attr )
      endif
      if (index(cx,".").ne.0) then; read(cx,*) x
      else;  read(cx,*) ival; x=real(ival); endif
      if (index(cy,".").ne.0) then; read(cy,*) y
      else;  read(cy,*) ival; y=real(ival); endif
      !read(cx,*) x
      !read(cy,*) y
      !must substitute the result into the main string
      if (i.eq.1.and.i.eq.dc) then
         write(tmp3,*) x**y
      elseif (i.eq.1) then
         write(tmp2,*) x**y
         tmp3=trim(adjustl(tmp2))//s(deliml(i+1):len(s))
      elseif (i.eq.dc) then
         write(tmp2,*) x**y
         tmp3=s(1:deliml(i-1))//trim(adjustl(tmp2))
      else
         write(tmp2,*) x**y
         tmp3=s(1:deliml(i-1))//trim(adjustl(tmp2))//s(deliml(i+1):len(s))
      endif
!      write(6,*) "exponents ",trim(tmp3)
      eval1 = trim(tmp3)
      return
   endif
enddo
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
do i = 1, dc
   if (delims(i:i).eq."*".or.delims(i:i).eq."/") then 
      if (first) then
         cx = s_get_num( i+1, s, attr )
         cy = s_get_num( i+2, s, attr )
      else
         cx = s_get_num( i,   s, attr )
         cy = s_get_num( i+1, s, attr )
      endif
!write(0,*) trim(cx)//" "//trim(cy)//" s:"//trim(s)
      if (index(cx,".").ne.0) then; read(cx,*) x
      else;  read(cx,*) ival; x=real(ival); endif
      if (index(cy,".").ne.0) then; read(cy,*) y
      else;  read(cy,*) ival; y=real(ival); endif
      !must substitute the result into the main string
      if (i.eq.1.and.i.eq.dc) then
         if (delims(i:i).eq."*") then
            write(tmp3,*) x*y
         elseif (delims(i:i).eq."/") then
            write(tmp3,*) x/y
         endif
      elseif (i.eq.1) then
         if (delims(i:i).eq."*") then
            write(tmp2,*) x*y
         elseif (delims(i:i).eq."/") then
            write(tmp2,*) x/y
         endif
         tmp3=trim(adjustl(tmp2))//s(deliml(i+1):len(s))
      elseif (i.eq.dc) then
         if (delims(i:i).eq."*") then
            write(tmp2,*) x*y
         elseif (delims(i:i).eq."/") then
            write(tmp2,*) x/y
         endif
         tmp3=s(1:deliml(i-1))//trim(adjustl(tmp2))
      else
         if (delims(i:i).eq."*") then
            write(tmp2,*) x*y
         elseif (delims(i:i).eq."/") then
            write(tmp2,*) x/y
         endif
         tmp3=s(1:deliml(i-1))//trim(adjustl(tmp2))//s(deliml(i+1):len(s))
      endif
!      write(6,*) "multdiv ", trim(tmp3)
      eval1 = trim(tmp3)
      return
   endif
enddo
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
do i = 1, dc
   if (delims(i:i).eq."+".or.delims(i:i).eq."-") then 
      if (first) then
         cx = s_get_num( i+1, s, attr )
         cy = s_get_num( i+2, s, attr )
      else
         cx = s_get_num( i,   s, attr )
         cy = s_get_num( i+1, s, attr )
      endif
      if (index(cx,".").ne.0) then; read(cx,*) x
      else;  read(cx,*) ival; x=real(ival); endif
      if (index(cy,".").ne.0) then; read(cy,*) y
      else;  read(cy,*) ival; y=real(ival); endif
      !read(cx,*) x
      !read(cy,*) y
      !must substitute the result into the main string
      if (i.eq.1.and.i.eq.dc) then
         if (delims(i:i).eq."+") then
            write(tmp3,*) x+y
         elseif (delims(i:i).eq."-") then
            write(tmp3,*) x-y
         endif
      elseif (i.eq.1) then
         if (delims(i:i).eq."+") then
            write(tmp2,*) x+y
         elseif (delims(i:i).eq."-") then
            write(tmp2,*) x-y
         endif
         tmp3=trim(adjustl(tmp2))//s(deliml(i+1):len(s))
      elseif (i.eq.dc) then
         if (delims(i:i).eq."+") then
            write(tmp2,*) x+y
         elseif (delims(i:i).eq."-") then
            write(tmp2,*) x-y
         endif
         tmp3=s(1:deliml(i-1))//trim(adjustl(tmp2))
      else
         if (delims(i:i).eq."+") then
            write(tmp2,*) x+y
         elseif (delims(i:i).eq."-") then
            write(tmp2,*) x-y
         endif
         tmp3=s(1:deliml(i-1))//trim(adjustl(tmp2))//s(deliml(i+1):len(s))
      endif
!      write(6,*) "addsub ",trim(tmp3)
      eval1 = trim(tmp3)
      return
   endif
enddo



end function eval1 !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
end module lineParse 

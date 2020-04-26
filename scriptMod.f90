! vim: fdm=marker
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
module scriptMod
implicit none
integer :: Nscript, Nvars, Nlabels
character(80), dimension(:), allocatable :: script
character(40), dimension(:), allocatable :: vnam ! variable name
character(40), dimension(:), allocatable :: vval ! variable value
character(40), dimension(:), allocatable :: labels  ! goto labels
integer, dimension(:), allocatable :: snum  ! script lines for labels

! call loadScript( FD, FN )
! call runScript

contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
subroutine loadScript( FD, FN ) !{{{
use lineParse
implicit none
integer, intent(in) :: FD
character(*), intent(in) :: FN
integer :: err, nn, nl, wc
character(80) :: line, word

open(FD,file=trim(FN),iostat=err)
if (err.ne.0) then
  write(0,*) "ERROR: reading script file:"//trim(FN)
  write(0,*) "ERROR: error number: ", err
  STOP
endif

nn = 0; nl = 0
do
  read(FD,'(A)',end=200) line
  call left_of( "#", line )
  wc = s_word_count(line)
  if (wc.gt.0) then
  ! if not empty line
    nn = nn + 1
    word = s_get_word(1,line)
    if (word.eq."LBL".or.word.eq."lbl") nl = nl + 1
  endif
enddo
200 continue

Nscript = nn
Nlabels = nl
!write(0,*) "read noncommented lines from script file: ", Nscript

allocate( script(Nscript), labels(nl), snum(nl) )
rewind(FD)

nn = 0; nl =0
do
  read(FD,'(A)',end=300) line
  call left_of( "#", line )
  wc = s_word_count(line)
  if (wc.gt.0) then
  ! if not empty line
    nn = nn + 1
    script(nn) = trim(adjustl(line)) ! save this line of the script
    word = s_get_word(1,line)
    if (word.eq."LBL".or.word.eq."lbl") then
     nl = nl + 1
     if (wc.ne.2) then
      write(0,*) "ERROR: sline:",nn,"bad command: "//trim(line)
      write(0,*) "ERROR: the LBL command requires an argument:"
      write(0,*) " Usage:  LBL LABEL"
      write(0,*) "   LABEL  can be an arithmetic expression or string."
      STOP
     endif 
     labels(nl) = trim(s_get_word(2,line))   ! save label name (counts duplicats so beware)
     snum(nl)   = nn            ! save script line
!     write(0,*) "LBL "//trim(labels(nl)),nn
    endif
  endif
enddo

300 continue

close(FD)

end subroutine loadScript !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
subroutine runScript
use lineParse
implicit none
integer :: i, j, wc, n
character(80) :: line, w(20)
real(4) :: rv ! real value from evaluate
logical :: debug

allocate( vnam(100), vval(100) ) ! users prolly don't need more than 100 vars
vnam(:) = ""
vval(:) = ""
Nvars = 0
debug = .false.

i = 0
do ! infinite loop cuz the script might never end!
 i = i + 1
 if (i.gt.Nscript) then ! out of bounds or end of script?
   exit
 endif
 line = script(i)
!write(0,*) "s:"//trim(line)
 ! need to substitute variables if they exist
 w(1) = subVars( line )
 line = w(1)
!write(0,*) "v:"//trim(line)
 wc = s_word_count(line)
!write(0,*) "wc:",wc
 ! evaluate the "words" for arithmetic expressions
 do j = 1, wc
   w(j) = s_get_word(j,line)
!write(0,*) j,"w:"//trim(w(j))
   if (j .eq. 1) cycle ! no expressions as first word, only commands
   !write(w(j+1),*) evaluate( w(j) )  ! this makes a number
   w(j+1) = evaluate( w(j) )  
   w(j+2) = s_sub( w(j), line, w(j+1) ) ! substitute the evaluation 
   !w(j+2) = s_sub( w(j), line, evaluate(w(j)) ) ! substitute the evaluation 
   w(j) = trim(w(j+1))  !using w(j+1) as temp variable
   line = trim(w(j+2))  !using w(j+2) as temp variable
 enddo
 if (debug) write(0,*) i,"r:"//trim(line)
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!60
 ! Let's parse the commands !
 select case (trim(w(1)))
  case ("DEBUG","debug"); debug = .true.
  case ("PRINT","print")
   write(6,'(A)') trim(line(6:80))
  case ("LBL","lbl"); ! do nothing
  case ("GOTO","goto")
   if (wc.ne.2) then
    write(0,*) "ERROR: sline:",i,"bad command: "//trim(line)
    write(0,*) "ERROR: the GOTO command requires an argument:"
    write(0,*) " Usage:  GOTO  LABEL"
    write(0,*) "   LABEL  can be an arithmetic expression or string that evaluates to an"
    write(0,*) "          existing label."
    STOP
   endif 
   do j = 1, Nlabels  ! find the label
    if (trim(w(2)).eq.trim(labels(j))) then
     i = snum(j)
!write(0,*) "jumping to sline:",i,"label:"//trim(labels(j))
     exit  ! found. change script line to line after label
    endif
   enddo
  case ("IFGO","ifgo"); 
   if (wc.ne.3) then
    write(0,*) "ERROR: sline:",i,"bad command: "//trim(line)
    write(0,*) "ERROR: the IFGO command requires two arguments:"
    write(0,*) " Usage:  IFGO  EXPR  LABEL"
    write(0,*) "   EXPR   is an arithmetic expression, if (EXPR >= 1.0) is true"
    write(0,*) "   LABEL  a label for a target GOTO"
    STOP
   endif 
   read(w(2),*) rv ! read condition
   if (rv.ge.1.0) then ! condition true
   do j = 1, Nlabels  ! find the label
    if (trim(w(3)).eq.trim(labels(j))) then
     i = snum(j)  ! found. change script line to line after label
!write(0,*) "jumping to sline:",i,"label:"//trim(labels(j))
     exit
    endif
   enddo
   endif
!!!!##########################################################################80
!!!! Add your user defined commands here !!!!
!  case ("mycommand")
!   read(w(2),*) myargument
!!!!##########################################################################80
  case default
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!40
!!!! Set variables !!!!
   if (wc.gt.1) then
    !if (trim(w(2)).eq."=") then
    if (index(trim(line),"=").gt.0) then ! there's an equal sign in here
     ! look to see if it exists, if not make it
     n = 0
     do j = 1, Nvars ! loop over existing variable names
      if (trim(w(1)).eq.trim(vnam(j))) then ! found it
       n = j
      endif
     enddo
     if (n .eq. 0) then ! wasn't found, then make it
      n = Nvars + 1
      Nvars = Nvars + 1
      vnam(n) = trim(w(1))
     endif
     vval(n) = trim(w(2))  ! set the word to the variable value
  !write(0,*) "variable:"//trim(vnam(n))//" set to:"//trim(vval(n))
     cycle
    endif
   endif
   ! if it's not an equation then...
   write(0,*) "WARNING: sline:",i,"unknown command line: "//trim(line)
 end select
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!60
enddo

end subroutine runScript
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
function subVars( s ) !{{{
use lineParse
implicit none
character(80) :: s, subVars, vv
character(40) :: delims
integer :: nv, n, i, k, endpos, startpos, ls

delims = " +-=/^$~`!@#%*(){}[];:<>,.?|\"
ls = len(trim(s))
nv = 0 ! number of variables on line
do i = 1, ls
 if (s(i:i).eq."$") then
  nv = nv + 1
 endif
enddo

subVars = s
do n = 1, nv
  ! this only makes one substitution
!write(0,*) "subVars",n, trim(subVars)
  startpos = 0
  endpos = 0
  ls = len(trim(subVars))
  do i = 1, ls
   if (subVars(i:i).eq."$") then
    startpos = i
    exit
   endif
  enddo
 
  do i = startpos+1, ls
    if (index(delims,subVars(i:i)).ne.0.or.i.eq.ls) then
      endpos = i-1
      if (i.eq.ls) then
        endpos = i
!write(0,*) "Variable at end of string", i, ls
      endif
      ! found variable position, now substitute it
      exit ! done with this variable
    endif
  enddo ! substitution of var

  k = 0
  do i = 1, Nvars
   if (subVars(startpos+1:endpos).eq.vnam(i)) then
    k = i
    exit
   endif
  enddo
  if (k.eq.0) then
    write(0,*) "ERROR: variable:"//subVars(startpos:endpos)//" not found"
  endif
  vv = s_sub( subvars(startpos:endpos), trim(subVars), vval(k) )
  subvars = vv
enddo ! n vars

end function subVars !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
end module scriptMod

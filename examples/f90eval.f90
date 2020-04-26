! compile: gfortran -fbounds-check ../lineParse.f90 f90eval.f90 -o f90eval
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1!!!!!80
! Ross J. Stewart 02 June 2012
module structDef
implicit none
type variable
  character(4) :: vtype
  integer      :: dimen, siz, nuse
  character(30):: vname, ofType
end type
type (variable),parameter :: nullvar=variable( "",0,0,0,"","")
integer, parameter :: maxvars=100 ! max number of variables per routine
type struct	! structure type for mods, progs, subs and funcs
  character(30) :: title
  character(30), dimension(10) :: uses
  integer :: nuses, nL, nI, nR, nC, nT, nCalls
  type (variable), dimension(maxvars) :: Vars
end type
type (struct), dimension(:), allocatable :: modu, prog, func, sub
integer :: nProgs, nMods, nSubs, nFuncs, nlines
end module structDef
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1!!!!!80
program f90eval
use lineParse
use structDef
implicit none
integer :: ierr, wc, i
character(80) :: line, word, fil

if (iargc().ne.1) call help
call getarg(1,fil)
open(unit=10,file=trim(fil),status="old")

nlines=0
! initialize structure counters
nProgs=0; nMods= 0; nSubs= 0; nFuncs=0

readfile0: do !main read line of file loop
  read(10,'(A)',IOSTAT=ierr) line
  ! error handling !!!!!!!!11111!!!!!!!!!!!!!!!!!!1111!!!
  if (ierr.lt.0) then
    exit readfile0 ! end of file, exit loop readfile
  elseif (ierr.gt.0) then
    write(0,*) "ERROR reading "//trim(fil)//" line: ",nlines+1
    stop ! stop the program execution
  endif
  nlines=nlines+1 !number of lines
  ! Count structures in file !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  if (index(line, "!").gt.1) then
    call left_of("!",line) ! get rid of the comment part
  endif
  wc=s_word_count ( line )
  if (wc.eq.0) cycle 		! no words
  call capital2lower(line)	! make it all lower case
  word=s_get_word( 1, line ) 	! get the first word
  countStructs0:  select case (trim(word))
    case ("module"); nMods=nMods+1
    case ("program"); nProgs=nProgs+1 
    case ("function"); nFuncs=nFuncs+1
    case ("subroutine"); nSubs=nSubs+1
  end select countStructs0
enddo readfile0

write(6,*) "Lines in "//trim(fil),nlines
nlines=0 ! reset file line number counter
rewind(10) ! set for another more thorough reading !!!!!!!!!!!!!!!!!!!!!!!!!!!80

if (nProgs.gt.1) then
  write(0,*) "warning: number of program structures counted exceeds 1,",nProgs
endif
! Allocate structure arrays
if (nMods .gt.0) allocate( modu(nMods ) )
if (nProgs.gt.0) allocate( prog(nProgs) )
if (nFuncs.gt.0) allocate( func(nFuncs) )
if (nSubs .gt.0) allocate( sub (nSubs ) )

! initialize structure counters Again !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
nProgs=0; nMods= 0; nSubs= 0; nFuncs=0

readfile1: do !main read line of file loop
  read(10,'(A)',IOSTAT=ierr) line
  ! error handling !!!!!!!!11111!!!!!!!!!!!!!!!!!!1111!!!
  if (ierr.lt.0) then
    exit readfile1 ! end of file, exit loop readfile
  elseif (ierr.gt.0) then
    write(0,*) "ERROR reading "//trim(fil)//" line: ",nlines+1
    stop ! stop the program execution
  endif
  nlines=nlines+1 !number of lines
  ! Count structures in file !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  if (index(line, "!").gt.1) then
    call left_of("!",line) ! get rid of the comment part
  endif
  wc=s_word_count ( line )
  if (wc.eq.0) cycle 		! no words
  call capital2lower(line)	! make it all lower case
  word=s_get_word( 1, line ) 	! get the first word
  countStructs1:  select case (trim(word))
    case ("module");     nMods= nMods+1
      call Define(s_get_word(2,line), "m")
    case ("program");    nProgs=nProgs+1
      call Define(s_get_word(2,line), "p") 
    case ("function");   nFuncs=nFuncs+1
      call Define(s_get_word(2,line), "f")
    case ("subroutine"); nSubs= nSubs+1
      call Define(s_get_word(2,line), "s")
  end select countStructs1
enddo readfile1

write(6,*) "structures,   nL nI nR nC nT nCalls total"
write(6,'(a12,i3)') "Modules:    ",nMods; do i=1,nMods
	  call dispstruct( modu(i) ); enddo
write(6,'(a12,i3)') "Programs:   ",nProgs; do i=1,nProgs
	  call dispstruct( prog(i) ); enddo
write(6,'(a12,i3)') "Functions:  ",nFuncs; do i=1,nFuncs
	  call dispstruct( func(i) ); enddo
write(6,'(a12,i3)') "Subroutines:",nSubs; do i=1,nSubs
	  call dispstruct( sub(i) ); enddo

end program
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1!!!!!80
subroutine Define(title, t)
use structDef
use lineParse
implicit none
integer :: ierr, wc, nV, varcount
character, intent(in) :: t
character     :: attr
character(10) :: strt
character(30), intent(in) :: title
character(80) :: line, word, word2, saveword
type (struct) :: tt
type (variable), dimension(20) :: lineVar
logical :: typeDef

tt%title= title
tt%nL=    0
tt%nI=    0
tt%nR=    0
tt%nC=    0
tt%nT=    0
tt%nCalls=0
tt%uses="" !"                              "
tt%nuses=0
tt%vars=nullvar
if(t.eq."m") strt="module"
if(t.eq."p") strt="program"
if(t.eq."f") strt="function"
if(t.eq."s") strt="subroutine"

typeDef=.false.
word2=""; saveword=""
varcount=0
nV=0

readmod: do 
  lineVar=nullvar
  read(10,'(A)',IOSTAT=ierr) line
  ! error handling !!!!!!!!11111!!!!!!!!!!!!!!!!!!1111!!!
  if (ierr.lt.0) then
    exit readmod ! end of file, exit loop readfile
  elseif (ierr.gt.0) then
    write(0,*) "ERROR reading in "//trim(strt)//": "//trim(title)//&
		", line: ",nlines+1
    stop ! stop the program execution
  endif
  nlines=nlines+1 !number of lines
  ! Count declarations
  if (index(line, "!").gt.0) then
    call left_of("!",line) ! get rid of the comment part
  endif
  wc=s_word_count ( line )
  if (wc.eq.0) cycle 		! no words
  call capital2lower(line)	! make it all lower case
  ! there shouldn't be any calls in a module, but ...
  if (index(line,"call ").ne.0) tt%nCalls=tt%nCalls+1
  word=s_get_word( 1, line ) 	! get the first word
      !write(0,*) "def:"//line
  if (typeDef) lineVar%ofType=saveword
  countDecs:  select case (trim(word))
    case ("logical");   call getVarDefs(line,lineVar,nV); tt%nL=tt%nL+nV
    case ("integer");   call getVarDefs(line,lineVar,nV); tt%nI=tt%nI+nV
    case ("real");      call getVarDefs(line,lineVar,nV); tt%nR=tt%nR+nV
    case ("character"); call getVarDefs(line,lineVar,nV); tt%nC=tt%nC+nV
    case ("type")
         word2=s_get_word( 2, line, attr )
         if (attr.ne."(") then ! type definition
            typeDef=.true.
            saveword=word2
            cycle readmod
         else  ! variable declaration
            call getVarDefs(line,lineVar,nV); tt%nT=tt%nT+nV
         endif
    case ("contains");  exit readmod! the contains contain only funcs and subs
	! future work for identifying these contained funcs and subs
	! as part of this module, it is understood only implicitly now.
    case ("end");
      if (trim(s_get_word( 2, line )).eq."type".and.typeDef) typeDef=.false.
      if (trim(s_get_word( 2, line )).eq.trim(strt)) exit readmod ! end of module
      cycle readmod
    case ("endmodule"); if(t.eq."m") exit readmod ! end of module
    case ("endprogram"); if(t.eq."p") exit readmod ! end of module
    case ("endfunction"); if(t.eq."f") exit readmod ! end of module
    case ("endsubroutine"); if(t.eq."s") exit readmod ! end of module
    case ("use"); tt%nuses=tt%nuses+1; tt%uses(tt%nuses)=trim(s_get_word( 2, line ))
    case default; call tallyVars(line,tt,varcount); cycle readmod
  end select countDecs
  if (varcount+nV.gt.maxvars) then
     write(0,*) "error: number of variables in "//trim(strt)//" "//&
		trim(title)//" exceeds maximum allocated of ",maxvars
     write(0,*) "line:",nlines,trim(line)
     call exit
  endif
  tt%vars(varcount+1:varcount+nV)=lineVar(1:nV)
  varcount=varcount+nV
!  write(6,*) "lineVar:",nV,lineVar(1:nV)
enddo readmod

if(t.eq."m") modu( nMods )=tt
if(t.eq."p") prog( nProgs)=tt
if(t.eq."f") func( nFuncs)=tt
if(t.eq."s") sub ( nSubs )=tt

end subroutine Define
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1!!!!!80
subroutine tallyVars( line, st, nv )
use structDef
use lineParse
implicit none
character :: attr
character(80), intent(in) :: line
character(80) :: word
type (struct), intent(inout) :: st
integer, intent(in) :: nv
integer :: i, j, k, tot, mnum
logical :: found

mnum=0

wc: do i=1,s_word_count(line)
   word=s_get_word(i,line,attr)
   if (s_is_fortran(word).or.s_is_number(word)) cycle wc
 vc: do j=1,nv
      if (st%vars(j)%vname.eq.trim(word)) then
         st%vars(j)%nuse=st%vars(j)%nuse+1
         found=.true.
         cycle wc
      else
         found=.false.
      endif     
   enddo vc 
 uc: do j=1,st%nuses
  mc: do k=1,nMods     
      if (st%uses(j).eq.modu(k)%title) then
         mnum=k; exit mc
      endif
   enddo mc
   if (mnum.eq.0) then
      cycle uc
      write(0,*) "module used, not found for line:"//line
      write(0,*) " used modules:",(st%uses(k),k=1,st%nuses)
      write(0,*) " modules:",modu(1)%title
   endif
   tot=(modu(mnum)%nL+modu(mnum)%nI+modu(mnum)%nR+modu(mnum)%nC+modu(mnum)%nT)
  uvc: do k=1,tot
      if (modu(mnum)%vars(k)%vname.eq.trim(word)) then
         modu(mnum)%vars(k)%nuse=modu(mnum)%vars(k)%nuse+1
         found=.true.
         cycle wc
      else
         found=.false.
      endif
    enddo uvc
   enddo uc  
   if (.not.found) then
      write(0,*) "Undefined variable: "//trim(word)
   endif
enddo wc

end subroutine tallyVars
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1!!!!!80
subroutine getVarDefs( line, var, nV )
use structDef
use lineParse
implicit none
character :: attr
character(80), intent(in) :: line
character(80) :: word, oldword, firstword
type (variable), dimension(20), intent(inout) :: var
integer, intent(out) :: nV
integer :: i, err, n

nV=0
firstword=s_get_word(1,line,attr)
wc: do i=1,s_word_count(line)
   word=s_get_word(i,line,attr)
!      write(0,*) line, nV, word
   if (trim(word).eq."type".and.i.eq.1) then
      ! this could be confusing, between type defs and declarations
      var%vtype=s_get_word(i+1,line,attr)
      oldword=word;  cycle wc
   elseif (i.eq.1) then
      var%vtype=word
      oldword=word;  cycle wc
   endif
   if (trim(oldword).eq."type".and.attr.eq."(") then
      oldword=word;  cycle wc
   endif
   if (trim(oldword).eq."dimension") then
      if (attr.ne."(") then
          var%dimen=-1
          var%siz=-1
      else
          var%dimen=var%dimen+1
          read(word,'(i4)',iostat=err) n
          if (err.ne.0) then
                var%siz=-1
          else; var%siz=n; endif
          oldword=word
          cycle wc
      endif
   endif   
   ! ignore fortran keywords and things as arguments
   if (s_is_fortran(word,4).or.attr.eq."(") then
     oldword=word; cycle wc
   endif
   if (nV.ge.30) then
      write(0,*) "ERROR number of variabes in one line exceeds 30"
      write(0,*) line, nV, word
      call exit
   endif
   nV=nV+1
   var(nV)%vname=word
   oldword=word
   if (index(line,"parameter").ne.0) exit wc ! only read one variable
enddo wc

end subroutine getVarDefs
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1!!!!!80
subroutine dispstruct( strt )
use structDef
implicit none
type (struct), intent(in) :: strt
integer :: tot, i

tot=(strt%nL+strt%nI+strt%nR+strt%nC+strt%nT)

write(6,'(3x,a10,x,7i3)') trim(strt%title), strt%nL, strt%nI, strt%nR, &
	strt%nC, strt%nT, strt%nCalls, tot

do i=1,tot  
 write(6,'(3x,a4,x,3i4,x,A)') strt%vars(i)%vtype,strt%vars(i)%dimen,strt%vars(i)%nuse,&
	strt%vars(i)%siz,trim(strt%vars(i)%vname)//char(09)//trim(strt%vars(i)%oftype)
! write(6,*) strt%vars(i)%vtype,strt%vars(i)%dimen,&
!	strt%vars(i)%siz,strt%vars(i)%vname,strt%vars(i)%oftype
enddo


end subroutine
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1!!!!!80
subroutine help
write(0,*) "Usage: f90eval [FILE.f90]"
stop ! stop the program execution
end subroutine help



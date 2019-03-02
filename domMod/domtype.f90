! vim:fdm=marker
module domType
! Domain definitions can be complicated, this module consolodates it
! Friday, June 15, 2012
! Ross J. Stewart
!
real(4), parameter :: eps_lmt = 1.0e-20
real(4), parameter :: pi = 3.141592653589739
!
type domain
    character(3)           :: ct
    logical                :: inside
    integer                :: axis
    real(4), dimension(7) :: parm
end type domain
type (domain), parameter   :: nullDom=domain("   ",.true.,0, &
                       &  (/0.0,0.0,0.0,0.0,0.0,0.0,0.0/) )

contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
subroutine readDomain( s, dom ) !{{{
use lineParse
implicit none
character(80), intent(in) :: s
character(80)             ::  word
type (domain), intent(inout) :: dom
integer :: i, wc

call left_of("!",s) ! get rid of comment
call left_of("#",s) ! get rid of comment
dom%ct=trim(s_get_word( 1, s )) !set the type of domain
wc=s_word_count( s )

if     ( dom%ct  ==  "rec" ) then
!rectangular, inside the domain, xmin xmax, ymin ymax, zmin zmax
 word=s_get_word( 2, s )
 read(word,*) dom%inside
 do i=1,6
   word=s_get_word( i+2, s )
   read(word,*) dom%parm(i)
 enddo
elseif ( dom%ct  ==  "per" ) then
!perimeter, inside the domain, xmin xmax, ymin ymax, zmin zmax, only go
! inside domain this much
 word=s_get_word( 2, s )
 read(word,*) dom%inside
 do i=1,7
   word=s_get_word( i+2, s )
   read(word,*) dom%parm(i)
 enddo
elseif ( dom%ct  ==  "sph" ) then
!sphere, inside, x y z, radius
 word=s_get_word( 2, s )
 read(word,*) dom%inside
 do i=1,4
   word=s_get_word( i+2, s )
   read(word,*) dom%parm(i)
 enddo
elseif ( dom%ct  ==  "ell" ) then
!circle, inside, axial direction i.e. 2=Y, x z, a b
 word=s_get_word( 2, s )
 read(word,*) dom%inside
 word=s_get_word( 3, s )
 read(word,*) dom%axis
 do i=1,4
   word=s_get_word( i+3, s )
   read(word,*) dom%parm(i)
 enddo
elseif ( dom%ct  ==  "tub" ) then
!tube, inside, axial direction, 2 of x y or z, outer a b, inner a b
 word=s_get_word( 2, s )
 read(word,*) dom%inside
 word=s_get_word( 3, s )
 read(word,*) dom%axis
 do i=1,6
   word=s_get_word( i+3, s )
   read(word,*) dom%parm(i)
 enddo
elseif ( dom%ct  ==  "wed" ) then
!wedge, inside, axial direction, 2 of x y or z, radius, largest Angle, angle
!between
 word=s_get_word( 2, s )
 read(word,*) dom%inside
 word=s_get_word( 3, s )
 read(word,*) dom%axis
 do i=1,5
   word=s_get_word( i+3, s )
   read(word,*) dom%parm(i)
 enddo
endif

end subroutine readDomain !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
subroutine writeDomain( dom ) !{{{
implicit none
character(30)             ::  frm
type (domain), intent(in) :: dom

if     ( dom%ct  ==  "rec" ) then
!rectangular, inside the domain, xmin xmax, ymin ymax, zmin zmax
 frm='(a1,2x,a3,1x,1L,1x,6f10.5)'
 write(6,frm) "#",dom%ct,dom%inside,dom%parm(1:6)
elseif ( dom%ct  ==  "per" ) then
!perimeter, inside the domain, xmin xmax, ymin ymax, zmin zmax, only go
! inside domain this much
 frm='(a1,2x,a3,1x,1L,1x,7f10.5)'
 write(6,frm) "#",dom%ct,dom%inside,dom%parm
elseif ( dom%ct  ==  "sph" ) then
!sphere, inside, x y z, radius
 frm='(a1,2x,a3,x,L,x,4f10.5)'
 write(6,frm) "#",dom%ct,dom%inside,dom%parm(1:4)
elseif ( dom%ct  ==  "ell" ) then
!circle, inside, axial direction i.e. 2=Y, x z, a b
 frm='(a1,2x,a3,x,L,x,i2,x,4f10.5)'
 write(6,frm) "#",dom%ct,dom%inside,dom%axis,dom%parm(1:4)
elseif ( dom%ct  ==  "tub" ) then
!tube, inside, alial direction, 2 of x y or z, outer a b, inner a b
 frm='(a1,2x,a3,x,L,x,i2,x,6f10.5)'
 write(6,frm) "#",dom%ct,dom%inside,dom%axis,dom%parm(1:6)
elseif ( dom%ct  ==  "wed" ) then
!wedge, inside, alial direction, 2 of x y or z, radius, largest Angle, angle
!between
 frm='(a1,2x,a3,x,L,x,i2,x,5f8.5)'
 write(6,frm) "#",dom%ct,dom%inside,dom%axis,dom%parm(1:5)
endif

end subroutine writeDomain !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
! function to determine if an xyz position is in the domain,
! dom [xmin xmax ymin ymax zmin zmax] (real coordinates)
! function returns boolean value
! Ross Feb 2012 V16
function inDomain(pos, dom) !{{{
implicit none
logical                            :: inDomain
integer                            :: i, n, m
logical, dimension(3)              :: good
real(4), dimension(3), intent(in)  :: pos
real(4), dimension(3)              :: dmin, dmax
integer, dimension(3)              ::  dir
type (domain), intent(in)          :: dom
real(4) :: depth, dist, slope, re, ro, dx(3,3)


DO n=1,3
   DO m=1,3
      dx(n,m) = pos(n) - dom%parm(m)
   END DO
END DO

inDomain=.false.
re = 0.0
dist = 0.0
slope = 0.0

if ( dom%ct  ==  "rec" .or. dom%ct  ==  "per" ) then
   !rectangular, inside the domain, xmin xmax, ymin ymax, zmin zmax
   good=.false.
   dmin(1)=dom%parm(1); dmax(1)=dom%parm(2)
   dmin(2)=dom%parm(3); dmax(2)=dom%parm(4)
   dmin(3)=dom%parm(5); dmax(3)=dom%parm(6)
   dir=1 ! set dimension switches for a closed domain
   
   do i=1, 3
      if ((abs(dmax(i)) + abs(dmin(i))) < eps_lmt) dir(i) = 0
   end do
   
   
   do i=1,3 !loop through each dimension
      if (dir(i) ==  1) then ! closed domain
         if (pos(i) > dmin(i) .and. pos(i) <= dmax(i)) then
            good(i)=.true.
         else
            good(i)=.false.
         endif
      elseif (dir(i) ==  0) then ! entire domain [-INF,INF]
         good(i)=.true.
      else
         write(0,*) "Something weird happend in domain, pos:",pos,&
              & "good?",good, "dir:",dir
      endif
   enddo
   
   if (good(1).and.good(2).and.good(3)) then
      inDomain=.true.
   else
      inDomain=.false.
   endif

   if ( dom%ct  ==  "per" ) then
      depth=dom%parm(7)
      do i=1,3 !loop through each dimension
         if (dir(i) ==  1) then ! closed domain
            if (pos(i) > dmin(i)+depth .and. pos(i) <= dmax(i)-depth) then
               good(i)=.true.
            else
               good(i)=.false.
            endif
         elseif (dir(i) ==  0) then ! entire domain [-INF,INF]
            good(i)=.true.
         else
            write(0,*) "Something weird happend in domain, pos:",pos,&
                 & "good?",good, "dir:",dir
         endif
      enddo
      
      if (good(1).and.good(2).and.good(3)) inDomain=.false.
   endif

   if (.not.dom%inside) inDomain=.not.inDomain ! invert result
   
elseif ( dom%ct  ==  "sph" ) then
   !sphere, inside, x y z, radius
   if ((dx(1,1)*dx(1,1) + dx(2,2)*dx(2,2) + dx(3,3)*dx(3,3)) <= &
        & dom%parm(4)*dom%parm(4)) then
      inDomain=.true.
   endif
   
   if (.not.dom%inside) inDomain=.not.inDomain ! invert result
   
elseif ( dom%ct  ==  "ell" ) then
   !ellipse, inside, axial direction i.e. 2=Y, x z, a b
   if (dom%axis == 1) then
      dist = (dx(2,1)*dx(2,1)) + (dx(3,2)*dx(3,2))
      slope = ( dx(2,1) + dx(3,2) )/ ( dx(2,1) - dx(3,2) )
      if (abs( dx(2,1) - dx(3,2) ) < eps_lmt) slope=0.0
   elseif (dom%axis == 2) then
      dist = (dx(1,1)*dx(1,1)) + (dx(3,2)*dx(3,2))
      slope = ( dx(1,1) + dx(3,2) )/ ( dx(1,1) - dx(3,2) )
      if (abs(dx(1,1) - dx(3,2)) < eps_lmt) slope=0.0
   elseif (dom%axis == 3) then
      dist = (dx(1,1)*dx(1,1) + dx(2,2)*dx(2,2))
      slope = ( dx(1,1) + dx(2,2) )/( dx(1,1) - dx(2,2) )
      if (abs(dx(1,1) - dx(2,2)) < eps_lmt) slope=0.0
   endif
   re = dom%parm(3)*dom%parm(4)*sqrt(slope**2 + 1.0)/ &
        & sqrt(dom%parm(3)**2*slope**2 + dom%parm(4)**2)
   if (dist <= re**2) inDomain=.true.
   
   if (.not.dom%inside) inDomain=.not.inDomain ! invert result
   
elseif ( dom%ct  ==  "tub" ) then
   !tube, inside, alial direction, 2 of x y or z, outer a b, inner a b
   if     (dom%axis == 1) then
      dist = (dx(2,1)*dx(2,1)) + (dx(3,2)*dx(3,2))
      slope = ( dx(2,1) + dx(3,2) )/(dx(2,1) - dx(3,2))
      if (abs(dx(2,1) - dx(3,2)) < eps_lmt) slope = 0.0
   elseif (dom%axis == 2) then
      dist = (dx(1,1)*dx(1,1)) + (dx(3,2)*dx(3,2))
      slope = ( dx(1,1) + dx(3,2) )/ ( dx(1,1) - dx(3,2) )
      if (abs(dx(1,1) - dx(3,2)) < eps_lmt) slope = 0.0
   elseif (dom%axis == 3) then
      dist = (dx(1,1)*dx(1,1)) + (dx(2,2)*dx(2,2))
      slope = ( dx(1,1) + dx(2,2) ) / ( dx(1,1) - dx(2,2) )
      if (abs( dx(1,1) - dx(2,2) ) < eps_lmt) slope = 0.0
   endif
   re = dom%parm(3)*dom%parm(4)*sqrt(slope**2 + 1.0)/ &
        sqrt(dom%parm(3)**2*slope**2 + dom%parm(4)**2)
   ro = dom%parm(5)*dom%parm(6)*sqrt(slope**2 + 1.0)/ &
        sqrt(dom%parm(5)**2*slope**2 + dom%parm(6)**2)
   if (dist <= re**2 .and. dist > ro**2) inDomain=.true.
   
   if (.not.dom%inside) inDomain=.not.inDomain ! invert result
   
elseif ( dom%ct  ==  "wed" ) then
   !wedge, inside, alial direction, 2 of x y or z, radius, largest Angle, angle
   !between
   if (dom%axis == 1) then ! using yz plane            
      dist = (dx(2,1)*dx(2,1)) + (dx(3,2)*dx(3,2))
!      if (dabs(dx(2,1) - dx(3,2)) < eps_lmt) then
!         re = 0.0_gr
!      else
         re = fullangle((/ dx(2,1), dx(3,2)/))
         ! particle angle
!      endif
   elseif (dom%axis == 2) then ! using xz plane
      dist = (dx(1,1)*dx(1,1)) + (dx(3,2)*dx(3,2))
!      if (dabs(dx(1,1) - dx(3,2)) < eps_lmt) then
!         re = 0.0_gr
!      else
         re = fullangle((/dx(1,1), dx(3,2)/))
         ! particle angle
!      endif
   elseif (dom%axis == 3) then ! using xy plane
      dist = (dx(1,1)*dx(1,1)) + (dx(2,2)*dx(2,2))
!      if (dabs(dx(1,1) - dx(2,2)) < eps_lmt) then
!         re = 0.0_gr
!      else
         re = fullangle((/dx(1,1), dx(2,2)/))
         ! particle angle
!      endif
   endif
   if (dist <= dom%parm(3)**2 .and. re > dom%parm(4)-dom%parm(5) .and. &
        re <= dom%parm(4)) inDomain=.true.
   if (.not.dom%inside) inDomain=.not.inDomain ! invert result
endif
!
end function inDomain !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
function DomainVolume( dom, box ) !{{{
implicit none
real(4)                  :: DomainVolume
integer               :: i
type (domain), intent(in) :: dom
integer :: dir(3)
real(4), dimension(3), intent(in) :: box
real(4), dimension(3) :: dmin, dmax, lbox, sbox

dmin(1)=dom%parm(1); dmax(1)=dom%parm(2)
dmin(2)=dom%parm(3); dmax(2)=dom%parm(4)
dmin(3)=dom%parm(5); dmax(3)=dom%parm(6)
dir=1 ! set dimension switches for a closed domain

do i=1, 3
   if ((abs(dmax(i)) + abs(dmin(i))) < eps_lmt) dir(i) = 0
end do

DomainVolume = 0.0
lbox = 0.0
if     ( dom%ct  ==  "rec" ) then
!rectangular, inside the domain, xmin xmax, ymin ymax, zmin zmax
 do i=1,3 !loop through each dimension
  if     (dir(i) ==  1) then ! closed domain
        lbox(i) = dmax(i) - dmin(i)
  elseif (dir(i) ==  0) then ! entire domain [-INF,INF]
        lbox(i) = box(i)
  else; write(0,*) "Error, cannot find volume of domain:"
        call writeDomain(dom)
        write(0,*) "     in model size:", box
  endif
 enddo
 DomainVolume = product(lbox)

elseif ( dom%ct  ==  "per" ) then
!perimeter, inside the domain, xmin xmax, ymin ymax, zmin zmax, only go
! inside domain this much
 do i=1,3 !loop through each dimension
  if     (dir(i) ==  1) then ! closed domain
        lbox(i) = dmax(i) - dmin(i)
        sbox(i) = (dmax(i)-dom%parm(7)) - (dmin(i)+dom%parm(7))
  elseif (dir(i) ==  0) then ! entire domain [-INF,INF]
        lbox(i) = box(i)
        sbox(i) = box(i)
  else; write(0,*) "Error, cannot find volume of domain:"
        call writeDomain(dom)
        write(0,*) "     in model size:", box
  endif
 enddo
 DomainVolume = product(lbox) - product(sbox)

elseif ( dom%ct  ==  "sph" ) then
!sphere, inside, x y z, radius
 DomainVolume = 4.0/3.0*pi*(dom%parm(4))**3

elseif ( dom%ct  ==  "ell" ) then
!circle, inside, axial direction i.e. 2=Y, x z, a b
 DomainVolume = pi*dom%parm(3)*dom%parm(4)*box(dom%axis)

elseif ( dom%ct  ==  "tub" ) then
!tube, inside, alial direction, 2 of x y or z, outer a b, inner a b
 DomainVolume = pi*(dom%parm(3)*dom%parm(4) - &
      &             dom%parm(5)*dom%parm(6))*box(dom%axis)
elseif ( dom%ct  ==  "wed" ) then
!wedge, inside, alial direction, 2 of x y or z, radius, largest Angle, angle
!between
 DomainVolume = (dom%parm(4) - dom%parm(5))*dom%parm(3)**2*box(3)/2.0
endif

if (.not.dom%inside) DomainVolume = product(box) - DomainVolume

end function DomainVolume !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
function DomainCenter( dom ) !{{{
implicit none
type (domain), intent(in) :: dom
real(4)                  :: r, a  ! for wedge
real(4), dimension(3)    :: DomainCenter

DomainCenter = 0.0
if     ( dom%ct  ==  "rec" ) then
!rectangular, inside the domain, xmin xmax, ymin ymax, zmin zmax
 DomainCenter(1) = (dom%parm(2) + dom%parm(1))/2.0
 DomainCenter(2) = (dom%parm(4) + dom%parm(3))/2.0
 DomainCenter(3) = (dom%parm(6) + dom%parm(5))/2.0
elseif ( dom%ct  ==  "per" ) then
!perimeter, inside the domain, xmin xmax, ymin ymax, zmin zmax, only go
! inside domain this much
 DomainCenter(1) = (dom%parm(2) + dom%parm(1))/2.0
 DomainCenter(2) = (dom%parm(4) + dom%parm(3))/2.0
 DomainCenter(3) = (dom%parm(6) + dom%parm(5))/2.0
elseif ( dom%ct  ==  "sph" ) then
!sphere, inside, x y z, radius
 DomainCenter = dom%parm(1:3)
elseif ( dom%ct  ==  "ell" ) then
!circle, inside, axial direction i.e. 2=Y, x z, a b
 if(dom%axis == 1)  DomainCenter(2:3) = dom%parm(1:2)
 if(dom%axis == 2) then
                    DomainCenter(1) = dom%parm(1)
                    DomainCenter(3) = dom%parm(2)
 endif
 if(dom%axis == 3)  DomainCenter(1:2) = dom%parm(1:2)
elseif ( dom%ct  ==  "tub" ) then
!tube, inside, alial direction, 2 of x y or z, outer a b, inner a b
 if(dom%axis == 1)  DomainCenter(2:3) = dom%parm(1:2)
 if(dom%axis == 2) then
                    DomainCenter(1) = dom%parm(1)
                    DomainCenter(3) = dom%parm(2)
 endif
 if(dom%axis == 3)  DomainCenter(1:2) = dom%parm(1:2)
elseif ( dom%ct  ==  "wed" ) then
!wedge, inside, alial direction, 2 of x y or z, radius, largest Angle, angle
!between
 r = 2.0/3.0*dom%parm(3)*sin(dom%parm(5))/dom%parm(5)
 a = dom%parm(4) - dom%parm(5)/2.0
 if(dom%axis == 1)  DomainCenter(2:3) = (/r*cos(a), r*sin(a)/)
 if(dom%axis == 2)  then
                    DomainCenter(1) = r*cos(a)
                    DomainCenter(3) = r*sin(a)
 endif
 if(dom%axis == 3)  DomainCenter(1:2) = (/r*cos(a), r*sin(a)/)
endif

end function DomainCenter !}}}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!80
function fullangle(v) !{{{
implicit none
real(4) :: at
real(4) :: fullangle
real(4), dimension(2), intent(in) :: v
real(4), parameter :: pi=3.1415926535

fullangle=0.0

if(abs(v(1)).lt.1.0d-10.and.abs(v(2)).lt.1.0d-10)then
 at=pi/2.0
else
 at=atan2(v(2),v(1))
endif

fullangle=at
if (at.lt.0.0) fullangle=at+2.*pi

endfunction fullangle !}}}

end module domType

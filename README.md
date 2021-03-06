# Fortran module: lineParse.f90

### Routines for easy string parsing.

-`logical function s_is_number(s)`  
	is string a number?

-`subroutine capital2lower( s )`  
	convert string to all capital letters

-`subroutine lower2capital( s )`  
	convert string to all lowercase

-`logical function s_is_fortran ( s, flag)`  
	returns .true. if string is a recognized fortran term

-`integer(4) function s_word_count ( s )`  
	returns the number of words in a string

-`character(80) function s_get_word( n, s, attr )`  
	returns the nth word in a string

-`character(80) function s_get_word_range( n1, n2, s )`  
	returns the n1th up to the n2th words in string

-`subroutine s_get_val( n, s, val, attr )`  
	returns the value of the nth "word" in string
	an overloaded function, so you must specify the returning variable in the expected type.

-`subroutine left_of( w , s )`  
	returns the part of string left of w
	useful for striping inline comments

-`integer function s_num_count( s )`  
	returns the number of numbers in string

-`character(80) function s_get_num( n, s, attr )`  
	returns the value of the nth number in string

-`character(80) function s_get_line( fid, ln, eo )`  
	reads a string, striped of inline comments from file fid

-`real(4) function evaluate( s )`  
	*TESTING*
	attempts to evaluate the string as amathematical expression

# Geometric Domain Module: domMod/domType.f90

### Geometric domain definitions can be complicated, this Fortran module consolodates it.

-`subroutine readDomain( s, dom )`  
	reads a domain format from a string, useful for input files

-`subroutine writeDomain( dom )`  
	writes the domain definition to a string that is readable by readDomain.

-`logical function inDomain( pos, dom )`  
	returns whether a given position vector is in the specified domain.

-`real(4) function DomainVolume( dom, box )`  
	returns the volume of the specified domain, `box` is required if the domain is inverted.

-`real(4), dimension(3) function DomainCenter( dom )`  
	returns the positioin vector for the center of the domain.

-`real(4) function fullangle( v )`  
	returns the full angle between the X axis and the vector `v`.

### Domain Types

The `domain` type is defined below:

```fortran
type domain
    character(3)           :: ct	!type
    logical                :: inside	!within or without boundary
    integer                :: axis	!cylinders have an axial direction (only parallel axes supported)
    real(4), dimension(7) :: parm	!parameters
end type domain
```

-`rec in xmin xmax, ymin ymax, zmin zmax`  
	rectangular domain with faces perpendicular to primary axes

-`per in xmin xmax, ymin ymax, zmin zmax, width`  
	rectangular shell or perimeter with a thickness of `width`

-`sph in x y z, radius`  
	sphere, fairly obvious.

-`ell in axis x y a b`  
	ellipse or cylinder oriented along `axis` with major and minor radii `a` and `b`

-`tub in axis x y oa ob ia ib`  
	elliptical tube with outer radii `oa` and `ob` and inner radii `ia` and `ib`

-`wed in axis x y r maxAng dAng`  
	Wedge shape with point at (x, y) and radius `r` with `maxAng` measured from X axis, the angle size of the wedge is `dAng`



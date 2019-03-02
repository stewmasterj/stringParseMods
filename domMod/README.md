# Geometric Domain Module

## Geometric domain definitions can be complicated, this Fortran module consolodates it.

### Routines

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

### Domains

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



##Fortran module: lineParse.f90

#Routines for easy string parsing.

contains

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
	an overloaded function, so you must specify the returning variable in the
	 expected type.

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


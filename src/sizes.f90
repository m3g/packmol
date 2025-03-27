!
!  Written by Leandro Martínez, 2009-2011.
!  Copyright (c) 2009-2018, Leandro Martínez, Jose Mario Martinez,
!  Ernesto G. Birgin.
!
!
! sizes.i: Define the maximum dimensions of the problems
!
!   maxrest:     Maximum number of restrictions
!   mrperatom:   Maximum number of restrictions per atom
!   maxtry:      Number of tries for building the initial point
!   nn:          Maximum number of variables
!                (at least the number of molecules*6)
!   maxkeywords: Maximum number of keywords in input file
!

module sizes

   integer :: maxrest
   integer :: mrperatom
   integer :: maxtry
   integer :: nn
   integer :: maxkeywords

   integer, parameter :: strl = 1000
   character(len=*), parameter :: str_format = "( a1000 )"

end module sizes


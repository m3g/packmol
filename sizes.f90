!  
!  Written by Leandro Martínez, 2009-2011.
!  Copyright (c) 2009-2011, Leandro Martínez, Jose Mario Martinez,
!  Ernesto G. Birgin.
!  
!
! sizes.i: Define the maximum dimensions of the problems
!
!   maxrest:     Maximum number of restrictions
!   mrperatom:   Maximum number of restrictions per atom
!   maxtry:      Number of tries for building the initial point  
!   nbp:         Maximum number of boxes for fast function evaluation (nbp**3)
!   nn:          Maximum number of variables 
!                (at least the number of molecules*6)
!   maxkeywords: Maximum number of keywords in input file
!

module sizes

  integer :: maxrest  
  integer :: mrperatom
  integer :: maxtry   
  integer :: nbp      
  integer :: nn       
  integer :: maxkeywords

end module sizes


!
!  Written by Leandro Martínez, 2009-2011.
!  Copyright (c) 2009-2018, Leandro Martínez, Jose Mario Martinez,
!  Ernesto G. Birgin.
!

!
! Arrays required by the flashsort package. Used only in heuristics, but
! defined here to be allocated dynamically
!

module flashsort

   use sizes
   implicit none
   integer, allocatable :: indflash(:) ! (ntotat)
   integer, allocatable :: lflash(:) ! (ntotat)
   integer :: mflash

end module flashsort


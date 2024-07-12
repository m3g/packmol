!
!  Written by Leandro Martínez, 2009-2011.
!  Copyright (c) 2009-2018, Leandro Martínez, Jose Mario Martinez,
!  Ernesto G. Birgin.
!
! Optimization variables passed as common go pgencan

module usegencan

   use sizes
   implicit none

   integer :: maxit, iprint1, iprint2
   integer, allocatable :: wi(:) ! (nn)
   double precision, allocatable :: l(:), u(:), g(:) ! (nn)
   double precision, allocatable :: wd(:) ! (8*nn)

end module usegencan

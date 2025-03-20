!
!  Written by Leandro Martínez, 2009-2011.
!  Copyright (c) 2009-2018, Leandro Martínez, Jose Mario Martinez,
!  Ernesto G. Birgin.
!
! Subroutines that set the indexes of a three-dimensional array
! given the undimensional counter of the vector (for an array
! with dimensions (0:ncells(1)+1,0:ncells(2)+1,0:ncells(3)+1), and
! vice-versa.
!

subroutine icell_to_ijk(icell,i,j,k)

   use compute_data, only : ncells2
   implicit none
   integer :: icell, i, j, k, iicell

   k = mod(icell,ncells2(3))
   if ( k == 0 ) k = ncells2(3)

   iicell = icell - k
   iicell = iicell / ncells2(3) + 1
   j = mod(iicell,ncells2(2))
   if ( j == 0 ) j = ncells2(2)

   iicell = iicell - j
   iicell = iicell / ncells2(2) + 1
   i = mod(iicell,ncells2(1))
   if ( i == 0 ) i = ncells2(1)

   k = k - 1
   j = j - 1
   i = i - 1

end subroutine icell_to_ijk

subroutine ijk_to_icell(i,j,k,icell)

   use compute_data, only : ncells2
   implicit none
   integer :: i, j, k, icell

   icell = i*ncells2(2)*ncells2(3) + j*ncells2(3) + k + 1

end subroutine ijk_to_icell


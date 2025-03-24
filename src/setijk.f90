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

subroutine icell_to_ijk(icell,ncells,i,j,k)

   implicit none
   integer, intent(in) :: icell, ncells(3)
   integer, intent(out) :: i, j, k
   integer :: iicell

   k = mod(icell,ncells(3))
   if ( k == 0 ) k = ncells(3)

   iicell = icell - k
   iicell = iicell / ncells(3) + 1
   j = mod(iicell,ncells(2))
   if ( j == 0 ) j = ncells(2)

   iicell = iicell - j
   iicell = iicell / ncells(2) + 1
   i = mod(iicell,ncells(1))
   if ( i == 0 ) i = ncells(1)

   k = k - 1
   j = j - 1
   i = i - 1

end subroutine icell_to_ijk

subroutine ijk_to_icell(i,j,k,ncells,icell)
   implicit none
   integer, intent(in) :: i, j, k, ncells(3)
   integer, intent(out) :: icell
   icell = i*ncells(2)*ncells(3) + j*ncells(3) + k + 1
end subroutine ijk_to_icell


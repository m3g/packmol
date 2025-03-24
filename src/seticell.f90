!
!  Written by Leandro Martínez, 2009-2011.
!  Copyright (c) 2009-2018, Leandro Martínez, Jose Mario Martinez,
!  Ernesto G. Birgin.
!
! Subroutine seticell: set cell index for given coordinates
!

subroutine seticell(x,ixcell,iycell,izcell)
   use pbc, only : v_in_box, pbc_min, pbc_length, cell_ind
   use compute_data, only : cell_length
   implicit none
   double precision, intent(in) :: x(3)
   integer, intent(out) :: ixcell, iycell, izcell
   double precision :: xt(3)
   xt = v_in_box(x, pbc_min, pbc_length)
   ixcell = int((xt(1) - pbc_min(1))/cell_length(1)) + 1
   iycell = int((xt(2) - pbc_min(2))/cell_length(2)) + 1
   izcell = int((xt(3) - pbc_min(3))/cell_length(3)) + 1
   return
end subroutine seticell


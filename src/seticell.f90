!
!  Written by Leandro Martínez, 2009-2011.
!  Copyright (c) 2009-2018, Leandro Martínez, Jose Mario Martinez,
!  Ernesto G. Birgin.
!
! Subroutine seticell: set cell index for given coordinates
!

subroutine seticell(x,y,z,ixcell,iycell,izcell)
   use pbc, only : cell_ind
   use compute_data, only : system_length, ncells, cell_length, sizemin
   implicit none
   double precision :: x, y, z, xtemp, ytemp, ztemp
   integer :: ixcell, iycell, izcell
   xtemp = modulo((x - sizemin(1)), system_length(1))
   ytemp = modulo((y - sizemin(2)), system_length(2))
   ztemp = modulo((z - sizemin(3)), system_length(3))
   ixcell = int(xtemp/cell_length(1)) + 1
   iycell = int(ytemp/cell_length(2)) + 1
   izcell = int(ztemp/cell_length(3)) + 1
   ixcell = cell_ind(ixcell, ncells(1))
   iycell = cell_ind(iycell, ncells(2))
   izcell = cell_ind(izcell, ncells(3))
   return
end subroutine seticell


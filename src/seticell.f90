!
!  Written by Leandro Martínez, 2009-2011.
!  Copyright (c) 2009-2018, Leandro Martínez, Jose Mario Martinez,
!  Ernesto G. Birgin.
!
! Subroutine seticell: set cell index for given coordinates
!

subroutine seticell(x,y,z,sizemin,cell_length,ncells,ixcell,iycell,izcell)
   use pbc
   use compute_data, only : system_length
   implicit none
   double precision :: x, y, z, sizemin(3), cell_length(3), xtemp, ytemp, ztemp
   integer :: ncells(3), ixcell, iycell, izcell
   xtemp = x - floor((x - sizemin(1))/ system_length(1)) * system_length(1)
   ytemp = y - floor((y - sizemin(2))/ system_length(2)) * system_length(2)
   ztemp = z - floor((z - sizemin(3))/ system_length(3)) * system_length(3)
   ixcell = int(xtemp/cell_length(1)) + 1
   iycell = int(ytemp/cell_length(2)) + 1
   izcell = int(ztemp/cell_length(3)) + 1
   if(xtemp <= 0) ixcell = 1
   if(ytemp <= 0) iycell = 1
   if(ztemp <= 0) izcell = 1
   if(ixcell > ncells(1)) ixcell = ncells(1)
   if(iycell > ncells(2)) iycell = ncells(2)
   if(izcell > ncells(3)) izcell = ncells(3)
   return
end subroutine seticell


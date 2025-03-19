!
!  Written by Leandro Martínez, 2009-2011.
!  Copyright (c) 2009-2018, Leandro Martínez, Jose Mario Martinez,
!  Ernesto G. Birgin.
!
! Subroutine seticell: set cell index for given coordinates
!

!elemental integer function icell(x,sizemin,system_length,cell_length,ncells)
!   double precision, intent(in) :: x, sizemin, system_length, cell_length
!   double precision :: xtemp
!   xtemp = x - floor((x - sizemin)/system_length) * system_length
!   icell = int(xtemp/cell_length) + 1
!   if (xtemp <= 0) icell = 1
!   if (icell > ncells) icell = ncells
!end function delta_vector

subroutine seticell(x,y,z,ixcell,iycell,izcell)
   use compute_data, only : system_length, ncells, cell_length, sizemin
   implicit none
   double precision :: x, y, z, xtemp, ytemp, ztemp
   integer :: ixcell, iycell, izcell
   xtemp = modulo((x - sizemin(1)) , system_length(1))
   ytemp = modulo((y - sizemin(2)) , system_length(2))
   ztemp = modulo((z - sizemin(3)) , system_length(3))
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


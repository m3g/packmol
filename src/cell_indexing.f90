module cell_indexing

implicit none
public :: setcell, icell_to_cell, index_cell

contains 
!
!  Written by Leandro Martínez, 2009-2011.
!  Copyright (c) 2009-2018, Leandro Martínez, Jose Mario Martinez,
!  Ernesto G. Birgin.
!
! Subroutine setcell: set cell indices for given coordinates
!
subroutine setcell(x,cell)
   use pbc, only : v_in_box, pbc_min, pbc_length, cell_ind
   use compute_data, only : cell_length, ncells
   double precision, intent(in) :: x(3)
   integer, intent(out) :: cell(3)
   double precision :: xt(3)
   xt = v_in_box(x, pbc_min, pbc_length)
   cell(:) = floor((xt(:) - pbc_min(:))/cell_length(:)) + 1
   ! safeguard for xt == pbc_max
   cell(:) = min(cell(:), ncells(:))
   return
end subroutine setcell

!
!  Written by Leandro Martínez, 2009-2011.
!  Copyright (c) 2009-2018, Leandro Martínez, Jose Mario Martinez,
!  Ernesto G. Birgin.
!
! Subroutines that set the indexes of a three-dimensional array
! given the undimensional counter of the vector (for an array
! with dimensions (ncells(1),ncells(2),ncells(3)), and
! vice-versa.
!
subroutine icell_to_cell(icell,ncells,cell)
   integer, intent(in) :: icell, ncells(3)
   integer, intent(out) :: cell(3)
   integer :: iicell

   cell(3) = modulo(icell,ncells(3))
   if ( cell(3) == 0 ) cell(3) = ncells(3)

   iicell = icell - cell(3)
   iicell = iicell / ncells(3) + 1
   cell(2) = modulo(iicell,ncells(2))
   if ( cell(2) == 0 ) cell(2) = ncells(2)

   iicell = iicell - cell(2)
   iicell = iicell / ncells(2) + 1
   cell(1) = modulo(iicell,ncells(1))
   if ( cell(1) == 0 ) cell(1) = ncells(1)

end subroutine icell_to_cell

integer function index_cell(cell, ncells)
   integer, intent(in) :: ncells(3), cell(3)
   index_cell = (cell(1)-1)*ncells(2)*ncells(3) + (cell(2)-1)*ncells(3) + (cell(3)-1) + 1
end

end


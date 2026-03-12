!
!  Written by Leandro Martínez, 2009-2011.
!  Copyright (c) 2009-2018, Leandro Martínez, Jose Mario Martinez,
!  Ernesto G. Birgin.
!
! Subroutine resetcells: Subroutine that resets the occupancy of
!                        linked cell cells
!

subroutine resetcells()
   use input, only: fix, ntype_with_fixed
   use cell_indexing, only: icell_to_cell, index_cell, setcell
   use compute_data, only: latomfirst, lcellfirst, lcellnext, empty_cell, ncells, &
                           ntype, natoms, ntotat, nfixedat, xcart, latomnext
   implicit none
   integer :: cell(3), icell, next_icell, iftype, icart, ifatom

   ! Incrementally reset only previously-occupied cells instead of
   ! clearing all ncells^3 entries. Walk the old occupied-cell linked
   ! list and reset each cell's data, then rebuild the fixed-atom lists.
   icell = lcellfirst
   do while ( icell > 0 )
      next_icell = lcellnext(icell)
      call icell_to_cell(icell, ncells, cell)
      latomfirst(cell(1),cell(2),cell(3)) = 0
      empty_cell(cell(1),cell(2),cell(3)) = .true.
      lcellnext(icell) = 0
      icell = next_icell
   end do
   lcellfirst = 0

   if(fix) then
      icart = ntotat - nfixedat
      do iftype = ntype + 1, ntype_with_fixed
         do ifatom = 1, natoms(iftype)
            icart = icart + 1
            call setcell(xcart(icart,:),cell)
            latomnext(icart) = latomfirst(cell(1),cell(2),cell(3))
            latomfirst(cell(1),cell(2),cell(3)) = icart

            if ( empty_cell(cell(1),cell(2),cell(3)) ) then
               empty_cell(cell(1),cell(2),cell(3)) = .false.
               icell = index_cell(cell,ncells)
               lcellnext(icell) = lcellfirst
               lcellfirst = icell
            end if
         end do
      end do
   end if

end subroutine resetcells

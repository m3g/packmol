!
!  Written by Leandro Martínez, 2009-2011.
!  Copyright (c) 2009-2018, Leandro Martínez, Jose Mario Martinez,
!  Ernesto G. Birgin.
!
! Subroutine resetcells: Subroutine that resets the occupancy of
!                        linked cell cells
!

subroutine resetcells()

   use compute_data, only : latomfirst, latomfix, &
      lcellfirst, lcellnext, hasfree, ncells2
   implicit none
   integer :: cell(3), icell

   ! Reset data for cells that contain fixed atom

   icell = lcellfirst
   do while( icell > 0 )
      call icell_to_cell(icell,ncells2,cell)
      latomfirst(cell(1),cell(2),cell(3)) = latomfix(cell(1),cell(2),cell(3))
      hasfree(cell(1),cell(2),cell(3)) = .false.
      icell = lcellnext(icell)
   end do
   lcellfirst = 0

end subroutine resetcells


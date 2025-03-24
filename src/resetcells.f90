!
!  Written by Leandro Martínez, 2009-2011.
!  Copyright (c) 2009-2018, Leandro Martínez, Jose Mario Martinez,
!  Ernesto G. Birgin.
!
! Subroutine resetcells: Subroutine that resets the occupancy of
!                        linked cell cells
!

subroutine resetcells()

   use sizes
   use compute_data, only : latomfirst, latomfix, &
      lcellfirst, lcellnext, hasfree, ncells2
   implicit none
   integer :: i, j, k, icell

   ! Reset data for cells that contain fixed atom

   icell = lcellfirst
   do while( icell > 0 )
      call icell_to_ijk(icell,ncells2,i,j,k)
      latomfirst(i,j,k) = latomfix(i,j,k)
      hasfree(i,j,k) = .false.
      icell = lcellnext(icell)
   end do
   lcellfirst = 0

end subroutine resetcells


!
!  Written by Leandro Martínez, 2009-2011.
!  Copyright (c) 2009-2018, Leandro Martínez, Jose Mario Martinez,
!  Ernesto G. Birgin.
!
!  Subroutine that swaps indexes for packing molecules one at a time
!

subroutine swaptype(n,x,itype,action)

   use sizes, only : nn
   use compute_data, only : ntype, comptype, nmols, ntotmol
   use input, only : nloop, nloop_all, nloop_type
   use swaptypemod
   use ahestetic
   implicit none
   integer ::n, itype, ilubar, ilugan, i, action
   double precision :: x(nn)

   ! Save original data

   if ( action == 0 ) then
      do i = 1, nn
         xfull(i) = x(i)
      end do
      ntemp = n
      ntottemp = ntotmol
   end if

   ! Swapping data for packing this itype

   if ( action == 1 ) then
      do i = 1, ntype
         if(i == itype) then
            comptype(i) = .true.
         else
            comptype(i) = .false.
         end if
      end do
      n = nmols(itype) * 6
      ntotmol = nmols(itype)
      nloop = nloop_type(itype)
      ilubar = 0
      do i = 1, itype - 1
         ilubar = ilubar + nmols(i) * 3
      end do
      ilubar = ilubar + 1
      ilugan = ntemp/2 + ilubar
      do i = 1, n / 2
         x(i) = xfull(ilubar)
         x(i+n/2) = xfull(ilugan)
         ilubar = ilubar + 1
         ilugan = ilugan + 1
      end do
   end if

   ! Save results for this type

   if ( action == 2 ) then
      ilubar = 0
      do i = 1, itype - 1
         ilubar = ilubar + nmols(i)*3
      end do
      ilubar = ilubar + 1
      ilugan = ntemp/2 + ilubar
      do i = 1, n/2
         xfull(ilubar) = x(i)
         xfull(ilugan) = x(i+n/2)
         ilubar = ilubar + 1
         ilugan = ilugan + 1
      end do
   end if

   ! Restore all-molecule vectors

   if ( action == 3 ) then
      n = ntemp
      ntotmol = ntottemp
      nloop = nloop_all
      do i = 1, n
         x(i) = xfull(i)
      end do
      do i = 1, ntype
         comptype(i) = .true.
      end do
   end if

end subroutine swaptype


!
!  Written by Leandro Martínez, 2009-2011.
!  Copyright (c) 2009-2018, Leandro Martínez, Jose Mario Martinez,
!  Ernesto G. Birgin.
!
! Subroutine setibox: set box index for given coordinates
!

subroutine setibox(x,y,z,sizemin,boxl,nboxes,iboxx,iboxy,iboxz)
   use pbc
   implicit none
   double precision :: x, y, z, sizemin(3), boxl(3), xtemp, ytemp, ztemp
   integer :: nboxes(3), iboxx, iboxy, iboxz

   if (using_pbc) then
      xtemp = x - nint(x / pbc_sides(1)) * pbc_sides(1)
      ytemp = y - nint(y / pbc_sides(2)) * pbc_sides(2)
      ztemp = z - nint(z / pbc_sides(3)) * pbc_sides(3)
   else
      xtemp = x - sizemin(1)
      ytemp = y - sizemin(2)
      ztemp = z - sizemin(3)
   end if
   iboxx = int(xtemp/boxl(1)) + 1
   iboxy = int(ytemp/boxl(2)) + 1
   iboxz = int(ztemp/boxl(3)) + 1
   if(xtemp <= 0) iboxx = 1
   if(ytemp <= 0) iboxy = 1
   if(ztemp <= 0) iboxz = 1
   if(iboxx >= nboxes(1)) iboxx = nboxes(1)
   if(iboxy >= nboxes(2)) iboxy = nboxes(2)
   if(iboxz >= nboxes(3)) iboxz = nboxes(3)
   return
end subroutine setibox


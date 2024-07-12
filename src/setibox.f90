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
      x = x - floor(x / pbc_box(1)) * pbc_box(1)
      y = y - floor(y / pbc_box(2)) * pbc_box(2)
      z = z - floor(z / pbc_box(3)) * pbc_box(3)
   end if

   xtemp = x - sizemin(1)
   ytemp = y - sizemin(2)
   ztemp = z - sizemin(3)
   iboxx = int(xtemp/boxl(1)) + 1
   iboxy = int(ytemp/boxl(2)) + 1
   iboxz = int(ztemp/boxl(3)) + 1
   if(xtemp.le.0) iboxx = 1
   if(ytemp.le.0) iboxy = 1
   if(ztemp.le.0) iboxz = 1
   if(iboxx.gt.nboxes(1)) iboxx = nboxes(1)
   if(iboxy.gt.nboxes(2)) iboxy = nboxes(2)
   if(iboxz.gt.nboxes(3)) iboxz = nboxes(3)

   return
end subroutine setibox


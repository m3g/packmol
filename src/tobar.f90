!
!  Written by Leandro Martínez, 2009-2011.
!  Copyright (c) 2009-2018, Leandro Martínez, Jose Mario Martinez,
!  Ernesto G. Birgin.
!
! subroutine tobar: moves molecules to their baricentres
!

subroutine tobar()

   use sizes
   use compute_data, only : coor, ntype, natoms, idfirst
   implicit none
   integer :: idatom, itype, iatom
   double precision :: xcm, ycm, zcm

   do itype = 1, ntype
      idatom = idfirst(itype) - 1
      xcm = 0.d0
      ycm = 0.d0
      zcm = 0.d0
      do iatom = 1, natoms(itype)
         idatom = idatom + 1
         xcm = xcm + coor(idatom,1)
         ycm = ycm + coor(idatom,2)
         zcm = zcm + coor(idatom,3)
      end do
      xcm = xcm / natoms(itype)
      ycm = ycm / natoms(itype)
      zcm = zcm / natoms(itype)
      idatom = idfirst(itype) - 1
      do iatom = 1, natoms(itype)
         idatom = idatom + 1
         coor(idatom,1) = coor(idatom,1) - xcm
         coor(idatom,2) = coor(idatom,2) - ycm
         coor(idatom,3) = coor(idatom,3) - zcm
      end do
   end do

   return
end subroutine tobar


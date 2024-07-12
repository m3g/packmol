!
!  Written by Leandro Martínez, 2009-2011.
!  Copyright (c) 2009-2018, Leandro Martínez, Jose Mario Martinez,
!  Ernesto G. Birgin.
!
! Subroutine cenmass
!
!            Computes the center of mass of free molecules and
!            for fixed molecules, if required.
!
subroutine cenmass()

   use sizes
   use compute_data, only : ntype, coor, idfirst, natoms
   use input, only : keyword, amass, nlines, linestrut

   implicit none
   integer :: k, iline
   integer :: itype, iatom, idatom
   double precision, allocatable :: cm(:,:), totm(:)
   logical, allocatable :: domass(:)

   ! Allocate local vectors

   allocate(cm(ntype,3),totm(ntype),domass(ntype))

   ! Setting the molecules for which the center of mass is computed

   do itype = 1, ntype
      domass(itype) = .true.
   end do

   do iline = 1, nlines
      if(keyword(iline,1).eq.'fixed') then
         do itype = 1, ntype
            if(iline.gt.linestrut(itype,1).and. &
               iline.lt.linestrut(itype,2)) then
               domass(itype) = .false.
            end if
         end do
      end if
   end do

   do iline = 1, nlines
      if(keyword(iline,1).eq.'centerofmass'.or. &
         keyword(iline,1).eq.'center') then
         do itype = 1, ntype
            if(iline.gt.linestrut(itype,1).and. &
               iline.lt.linestrut(itype,2)) then
               domass(itype) = .true.
            end if
         end do
      end if
   end do

   ! Computing the center of mass

   do itype = 1, ntype
      do k = 1, 3
         cm(itype, k) = 0.d0
      end do
   end do

   do itype = 1, ntype
      totm(itype) = 0.d0
      idatom = idfirst(itype) - 1
      do iatom = 1, natoms(itype)
         idatom = idatom + 1
         totm(itype) = totm(itype) + amass(idatom)
      end do
   end do

   do itype = 1, ntype
      idatom = idfirst(itype) - 1
      do iatom = 1, natoms(itype)
         idatom = idatom + 1
         do k = 1, 3
            cm(itype, k) = cm(itype, k)  + coor(idatom, k)*amass(idatom)
         end do
      end do
      do k = 1, 3
         cm(itype, k) = cm(itype, k) / totm(itype)
      end do
   end do

   ! Putting molecules in their center of mass

   do itype = 1, ntype
      if(domass(itype)) then
         idatom = idfirst(itype) - 1
         do iatom = 1, natoms(itype)
            idatom = idatom + 1
            do k = 1, 3
               coor(idatom, k) = coor(idatom, k) - cm(itype, k)
            end do
         end do
      end if
   end do

   deallocate(cm,totm,domass)

   return
end subroutine cenmass

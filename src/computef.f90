!
!  Written by Leandro Martínez, 2009-2011.
!  Copyright (c) 2009-2018, Leandro Martínez, Jose Mario Martinez,
!  Ernesto G. Birgin.
!
! Subroutine that computes the function value
!

subroutine computef(n,x,f)

   use sizes
   use cell_indexing, only: index_cell, icell_to_cell, setcell
   use compute_data
   use pbc
   implicit none

   integer :: n, i, j, k, icell
   integer :: ilugan, ilubar, icart, itype, imol, iatom, idatom
   integer :: cell(3)
   integer :: ip1, jp1, kp1, jm1, km1
   integer :: neigh000, neigh100, neigh010, neigh001
   integer :: neigh1m10, neigh10m1, neigh01m1, neigh011, neigh110, neigh101
   integer :: neigh1m1m1, neigh1m11, neigh11m1, neigh111

   double precision :: v1(3), v2(3), v3(3)
   double precision :: x(n)
   double precision :: f,fparc,fplus
   double precision :: xcm(3)
   double precision :: beta, gama, teta

   ! Reset function value

   f = 0.d0
   frest = 0.d0
   fdist = 0.d0

   ! Reset cells

   if(.not.init1) call resetcells()

   ! Transform baricenter and angles into cartesian coordinates
   ! Computes cartesian coordinates from vector x and coor

   ilubar = 0
   ilugan = ntotmol*3
   icart = 0

   do itype = 1, ntype
      if(.not.comptype(itype)) then
         icart = icart + nmols(itype)*natoms(itype)
         cycle
      end if

      do imol = 1, nmols(itype)

         xcm = x(ilubar+1:ilubar+3)

         ! Computing the rotation matrix

         beta = x(ilugan+1)
         gama = x(ilugan+2)
         teta = x(ilugan+3)

         call eulerrmat(beta,gama,teta,v1,v2,v3)

         ! Looping over the atoms of this molecule

         idatom = idfirst(itype) - 1
         do iatom = 1, natoms(itype)

            icart = icart + 1
            idatom = idatom + 1

            ! Computing the cartesian coordinates for this atom

            call compcart(xcart(icart,1:3),xcm,coor(idatom,1:3),v1,v2,v3)

            ! Adding to f the value relative to constraints for this atom

            call comprest(icart,fplus)
            f = f + fplus
            frest = dmax1(frest,fplus)
            if(move) frest_atom(icart) = frest_atom(icart) + fplus

            ! Putting atoms in their cells

            if(.not.init1) then
               call setcell(xcart(icart,:), cell)

               ! Atom linked list
               latomnext(icart) = latomfirst(cell(1),cell(2),cell(3))
               latomfirst(cell(1),cell(2),cell(3)) = icart

               ! cell with atoms linked list
               if ( empty_cell(cell(1),cell(2),cell(3)) ) then
                  empty_cell(cell(1),cell(2),cell(3)) = .false.
                  icell = index_cell(cell,ncells)
                  lcellnext(icell) = lcellfirst
                  lcellfirst = icell
               end if

               ibtype(icart) = itype
               ibmol(icart) = imol

            end if

         end do

         ilugan = ilugan + 3
         ilubar = ilubar + 3

      end do
   end do

   if(init1) return

   ! Minimum distance function evaluation

   icell = lcellfirst
   do while( icell > 0 )

      call icell_to_cell(icell,ncells,cell)
      i = cell(1)
      j = cell(2)
      k = cell(3)

      ip1 = cell_ind(i + 1, ncells(1))
      jp1 = cell_ind(j + 1, ncells(2))
      kp1 = cell_ind(k + 1, ncells(3))
      jm1 = cell_ind(j - 1, ncells(2))
      km1 = cell_ind(k - 1, ncells(3))

      neigh000 = latomfirst(i,j,k)
      neigh100 = latomfirst(ip1,j,k)
      neigh010 = latomfirst(i,jp1,k)
      neigh001 = latomfirst(i,j,kp1)
      neigh1m10 = latomfirst(ip1,jm1,k)
      neigh10m1 = latomfirst(ip1,j,km1)
      neigh01m1 = latomfirst(i,jp1,km1)
      neigh011 = latomfirst(i,jp1,kp1)
      neigh110 = latomfirst(ip1,jp1,k)
      neigh101 = latomfirst(ip1,j,kp1)
      neigh1m1m1 = latomfirst(ip1,jm1,km1)
      neigh1m11 = latomfirst(ip1,jm1,kp1)
      neigh11m1 = latomfirst(ip1,jp1,km1)
      neigh111 = latomfirst(ip1,jp1,kp1)

      icart = neigh000
      do while( icart > 0 )

         if(comptype(ibtype(icart))) then
            ! Interactions inside cell
            f = f + fparc(icart,latomnext(icart))
            ! Interactions of cells that share faces (6 faces - 3 forward)
            f = f + fparc(icart,neigh100) ! 4 - (1, 0, 0)
            f = f + fparc(icart,neigh010) ! 5 - (0, 1, 0)
            f = f + fparc(icart,neigh001) ! 6 - (0, 0, 1)
            ! Interactions of cells that share axes (12 edges - 6 forward)
            f = f + fparc(icart,neigh1m10) ! 4 - (1, -1, 0)
            f = f + fparc(icart,neigh10m1) ! 5 - (1, 0, -1)
            f = f + fparc(icart,neigh01m1) ! 6 - (0, 1, -1)
            f = f + fparc(icart,neigh011) ! 9 - (0, 1, 1)
            f = f + fparc(icart,neigh110) ! 10 - (1, 1, 0)
            f = f + fparc(icart,neigh101) ! 11 - (1, 0, 1)
            ! Interactions of cells that share vertices (8 vertices, 8 forward)
            f = f + fparc(icart,neigh1m1m1) ! 5 - (1, -1, -1)
            f = f + fparc(icart,neigh1m11) ! 6 - (1, -1, 1)
            f = f + fparc(icart,neigh11m1) ! 7 - (1, 1, -1)
            f = f + fparc(icart,neigh111) ! 8 - (1, 1, 1)
         end if

         icart = latomnext(icart)
      end do

      icell = lcellnext(icell)
   end do

   return
end subroutine computef

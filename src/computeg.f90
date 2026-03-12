!
!  Written by Leandro Martínez, 2009-2011.
!  Copyright (c) 2009-2018, Leandro Martínez, Jose Mario Martinez,
!  Ernesto G. Birgin.
!
! Subroutine that computes the analytical derivatives
!

subroutine computeg(n,x,g)

   use sizes
   use cell_indexing, only: index_cell, icell_to_cell, setcell, n_forward_offsets, forward_offsets
   use compute_data, only : ntotmol, ntype, ncells, nmols, natoms, idfirst, nratom, iratom, ibmol, ibtype, &
      xcart, coor, radius, gxcar, comptype, init1, latomnext, &
      latomfirst, lcellfirst, lcellnext, empty_cell
   use pbc
   implicit none

   integer :: n
   integer :: idatom, iatom, irest
   integer :: i, j, k, ilubar, ilugan, icart, itype, imol
   integer :: icell, cell(3), neigh_cell(3)
   integer :: k1, k2
   integer :: iratcount, ioffset
   integer :: neigh_first(n_forward_offsets)

   double precision :: x(n), g(n)
   double precision :: dv1beta(3), dv1gama(3), dv1teta(3),&
      dv2beta(3), dv2gama(3), dv2teta(3),&
      dv3beta(3), dv3gama(3), dv3teta(3)
   double precision :: v1(3), v2(3), v3(3)
   double precision :: xcm(3)
   double precision :: beta, gama, teta, cb, sb, cg, sg, ct, st

   ! Reset gradients
   gxcar(:,:) = 0.d0

   ! Reset cells

   if(.not.init1) then
      call resetcells()
   end if

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
         ! Compute the rotation matrix
         beta = x(ilugan + 1)
         gama = x(ilugan + 2)
         teta = x(ilugan + 3)
         call eulerrmat(beta,gama,teta,v1,v2,v3)
         idatom = idfirst(itype) - 1

         do iatom = 1, natoms(itype)
            icart = icart + 1
            idatom = idatom + 1
            call compcart(xcart(icart,1:3),xcm,coor(idatom,1:3),v1,v2,v3)

            ! Gradient relative to the wall distace
            do iratcount = 1, nratom(icart)
               irest = iratom(icart,iratcount)
               call gwalls(icart,irest)
            end do

            if(.not.init1) then
               call setcell(xcart(icart,:),cell)
               ! Atom linked list
               latomnext(icart) = latomfirst(cell(1),cell(2),cell(3))
               latomfirst(cell(1),cell(2),cell(3)) = icart

               ! cell with atoms linked list
               if ( empty_cell(cell(1),cell(2),cell(3))) then
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

   if( .not. init1 ) then

      !
      ! Gradient relative to minimum distance
      !

      icell = lcellfirst
      do while( icell > 0 )

         call icell_to_cell(icell,ncells,cell)
         i = cell(1)
         j = cell(2)
         k = cell(3)

         ! Load forward neighbor heads (self-cell + 13 forward neighbors).
         do ioffset = 1, n_forward_offsets
            neigh_cell(1) = cell_ind(i + forward_offsets(1,ioffset), ncells(1))
            neigh_cell(2) = cell_ind(j + forward_offsets(2,ioffset), ncells(2))
            neigh_cell(3) = cell_ind(k + forward_offsets(3,ioffset), ncells(3))
            neigh_first(ioffset) = latomfirst(neigh_cell(1),neigh_cell(2),neigh_cell(3))
         end do

         icart = neigh_first(1)
         do while( icart > 0 )

            if ( .not. comptype(ibtype(icart)) ) then
               icart = latomnext(icart)
               cycle
            end if

            ! Offset #1 is self-cell and uses latomnext(icart) to keep forward-only pairs.
            call gparc(icart,latomnext(icart))
            do ioffset = 2, n_forward_offsets
               call gparc(icart,neigh_first(ioffset))
            end do

            icart = latomnext(icart)
         end do

         icell = lcellnext(icell)
      end do

   end if

   ! Computing the gradient using chain rule

   g(1:n) = 0.d0
   k1 = 0
   k2 = ntotmol * 3
   icart = 0
   do itype = 1, ntype

      if(.not.comptype(itype)) then
         icart = icart + nmols(itype)*natoms(itype)
      else
         do imol = 1, nmols(itype)

            beta = x(k2 + 1)
            gama = x(k2 + 2)
            teta = x(k2 + 3)

            cb = dcos(beta)
            sb = dsin(beta)
            cg = dcos(gama)
            sg = dsin(gama)
            ct = dcos(teta)
            st = dsin(teta)

            dv1beta(1) = - cb * sg * ct - sb * cg
            dv2beta(1) = - sb * sg * ct + cb * cg
            dv3beta(1) = 0.d0

            dv1gama(1) = - sb * cg * ct - cb * sg
            dv2gama(1) =   cb * cg * ct - sb * sg
            dv3gama(1) =   cg * st

            dv1teta(1) =   sb * sg * st
            dv2teta(1) = - cb * sg * st
            dv3teta(1) =   sg * ct

            dv1beta(2) = - cb * cg * ct + sb * sg
            dv2beta(2) = - sb * cg * ct - cb * sg
            dv3beta(2) = 0.d0

            dv1gama(2) =   sb * sg * ct - cb * cg
            dv2gama(2) = - sg * cb * ct - cg * sb
            dv3gama(2) = - sg * st

            dv1teta(2) =   sb * cg * st
            dv2teta(2) = - cb * cg * st

            dv3teta(2) =   cg * ct

            dv1beta(3) =   cb * st
            dv2beta(3) =   sb * st
            dv3beta(3) = 0.d0

            dv1gama(3) = 0.d0
            dv2gama(3) = 0.d0
            dv3gama(3) = 0.d0

            dv1teta(3) =   sb * ct
            dv2teta(3) = - cb * ct
            dv3teta(3) = - st

            idatom = idfirst(itype) - 1
            do iatom = 1, natoms(itype)

               icart = icart + 1
               idatom = idatom + 1

               do k = 1, 3
                  g(k1+k) = g(k1+k) + gxcar(icart, k)
               end do

               do k = 1, 3
                  g(k2 + 1) = g(k2 + 1) &
                     + (coor(idatom,1) * dv1beta(k) &
                     + coor(idatom, 2) * dv2beta(k) &
                     + coor(idatom, 3) * dv3beta(k)) &
                     * gxcar(icart, k)

                  g(k2 + 2) = g(k2 + 2) &
                     + (coor(idatom,1) * dv1gama(k) &
                     + coor(idatom, 2) * dv2gama(k) &
                     + coor(idatom, 3) * dv3gama(k)) &
                     * gxcar(icart, k)

                  g(k2 + 3) = g(k2 + 3) &
                     + (coor(idatom,1)  * dv1teta(k) &
                     + coor(idatom, 2) * dv2teta(k) &
                     + coor(idatom, 3) * dv3teta(k)) &
                     * gxcar(icart, k)
               end do

            end do
            k2 = k2 + 3
            k1 = k1 + 3
         end do
      end if
   end do

   return

end subroutine computeg

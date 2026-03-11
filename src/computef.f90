!
!  Written by Leandro Martínez, 2009-2011.
!  Copyright (c) 2009-2018, Leandro Martínez, Jose Mario Martinez,
!  Ernesto G. Birgin.
!
! Subroutine that computes the function value
!

subroutine computef(n,x,f)

   use sizes
   use cell_indexing, only: index_cell, icell_to_cell, setcell, n_forward_offsets, forward_offsets
   use compute_data
   use pbc
   implicit none

   integer :: n, i, j, k, icell
   integer :: ilugan, ilubar, icart, itype, imol, iatom, idatom
   integer :: cell(3), neigh_cell(3)
   integer :: ioffset
   integer :: neigh_first(n_forward_offsets)

   double precision :: min_cell_dist2, max_reach, reg_reach, short_reach

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

   if(.not.init1) then
      call resetcells()
      call refresh_hot_buffers_full()
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
            call refresh_hot_buffers_atom(icart)

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

               cell_max_radius(cell(1),cell(2),cell(3)) = dmax1(cell_max_radius(cell(1),cell(2),cell(3)), radius(icart))
               if ( use_short_radius(icart) ) then
                  cell_max_short_radius(cell(1),cell(2),cell(3)) = dmax1(cell_max_short_radius(cell(1),cell(2),cell(3)), short_radius(icart))
               end if

               ! cell with atoms linked list
               if ( empty_cell(cell(1),cell(2),cell(3)) ) then
                  empty_cell(cell(1),cell(2),cell(3)) = .false.
                  icell = index_cell(cell,ncells)
                  lcellnext(icell) = lcellfirst
                  lcellfirst = icell
               end if

               ibtype(icart) = itype
               ibmol(icart) = imol
               call refresh_hot_buffers_atom(icart)

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

      ! Load current cell and forward neighbors using the shared offset ordering:
      ! (0,0,0), 3 faces, 6 edges, 4 vertices.
      do ioffset = 1, n_forward_offsets
         neigh_cell(1) = cell_ind(i + forward_offsets(1,ioffset), ncells(1))
         neigh_cell(2) = cell_ind(j + forward_offsets(2,ioffset), ncells(2))
         neigh_cell(3) = cell_ind(k + forward_offsets(3,ioffset), ncells(3))
         neigh_first(ioffset) = latomfirst(neigh_cell(1),neigh_cell(2),neigh_cell(3))
         do while ( neigh_first(ioffset) > 0 )
            if ( comptype(ibtype(neigh_first(ioffset))) ) exit
            neigh_first(ioffset) = latomnext(neigh_first(ioffset))
         end do

         if ( neigh_first(ioffset) <= 0 ) cycle

         min_cell_dist2 = cell_pair_min_dist2(cell, neigh_cell)
         reg_reach = cell_max_radius(cell(1),cell(2),cell(3)) + cell_max_radius(neigh_cell(1),neigh_cell(2),neigh_cell(3))
         short_reach = cell_max_short_radius(cell(1),cell(2),cell(3)) + cell_max_short_radius(neigh_cell(1),neigh_cell(2),neigh_cell(3))
         max_reach = dmax1(reg_reach, short_reach)
         if ( min_cell_dist2 > max_reach*max_reach ) neigh_first(ioffset) = 0
      end do

      icart = neigh_first(1)
      do while( icart > 0 )

         if ( .not. comptype(ibtype(icart)) ) then
            icart = latomnext(icart)
            cycle
         end if

         ! Offset #1 is self-cell and uses latomnext(icart) to keep forward-only pairs.
         f = f + fparc(icart,latomnext(icart))
         do ioffset = 2, n_forward_offsets
            f = f + fparc(icart,neigh_first(ioffset))
         end do

         icart = latomnext(icart)
      end do

      icell = lcellnext(icell)
   end do

   return
contains

   double precision function cell_pair_min_dist2(cell_a, cell_b)
      integer, intent(in) :: cell_a(3), cell_b(3)
      integer :: idim
      double precision :: center_a, center_b, delta, gap

      cell_pair_min_dist2 = 0.d0
      do idim = 1, 3
         center_a = pbc_min(idim) + (dble(cell_a(idim)) - 0.5d0) * cell_length(idim)
         center_b = pbc_min(idim) + (dble(cell_b(idim)) - 0.5d0) * cell_length(idim)
         delta = dabs(delta_vector(center_a, center_b, pbc_length(idim)))
         gap = dmax1(0.d0, delta - cell_length(idim))
         cell_pair_min_dist2 = cell_pair_min_dist2 + gap*gap
      end do
   end function cell_pair_min_dist2

end subroutine computef

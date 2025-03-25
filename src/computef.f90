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
   use input, only : fix
   use pbc
   implicit none

   integer :: n, i, j, k, icell
   integer :: ilugan, ilubar, icart, itype, imol, iatom, idatom
   integer :: cell(3)

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
      else
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

                  if ( .not. hasfree(cell(1),cell(2),cell(3)) ) then
                     hasfree(cell(1),cell(2),cell(3)) = .true.
                     icell = index_cell(cell,ncells)
                     lcellnext(icell) = lcellfirst
                     lcellfirst = icell

                     ! Add cells with fixed atoms which are vicinal to this cell
                     if ( fix ) then
                        ! cells sharing faces
                        call add_cell_behind(cell, -1, 0, 0)
                        call add_cell_behind(cell, 0, -1, 0)
                        call add_cell_behind(cell, 0, 0, -1)
                        call add_cell_behind(cell, 1, 0, 0)
                        call add_cell_behind(cell, 0, 1, 0)
                        call add_cell_behind(cell, 0, 0, 1)
                        ! cells sharing edges
                        call add_cell_behind(cell, 0, 1, 1)
                        call add_cell_behind(cell, 0, 1, -1)
                        call add_cell_behind(cell, 0, -1, 1)
                        call add_cell_behind(cell, 0, -1, -1)
                        call add_cell_behind(cell, 1, 0, 1)
                        call add_cell_behind(cell, 1, 0, -1)
                        call add_cell_behind(cell, 1, 1, 0)
                        call add_cell_behind(cell, 1, -1, 0)
                        call add_cell_behind(cell, -1, 0, 1)
                        call add_cell_behind(cell, -1, 0, -1)
                        call add_cell_behind(cell, -1, 1, 0)
                        call add_cell_behind(cell, -1, -1, 0)
                        ! cells sharing vertices 
                        call add_cell_behind(cell, -1, 1, 1)
                        call add_cell_behind(cell, -1, 1, -1)
                        call add_cell_behind(cell, -1, -1, 1)
                        call add_cell_behind(cell, -1, -1, -1)
                        call add_cell_behind(cell, 1, 1, 1)
                        call add_cell_behind(cell, 1, 1, -1)
                        call add_cell_behind(cell, 1, -1, 1)
                        call add_cell_behind(cell, 1, -1, -1)
                     end if

                  end if

                  ibtype(icart) = itype
                  ibmol(icart) = imol

               end if

            end do

            ilugan = ilugan + 3
            ilubar = ilubar + 3

         end do
      end if
   end do

   if(init1) return

   ! Minimum distance function evaluation

   icell = lcellfirst
   do while( icell > 0 )

      call icell_to_cell(icell,ncells,cell)
      i = cell(1)
      j = cell(2)
      k = cell(3)

      icart = latomfirst(i,j,k)
      do while( icart > 0 )

         if(comptype(ibtype(icart))) then
            ! Interactions inside cell
            f = f + fparc(icart,latomnext(icart))
            ! Interactions of cells that share faces (6 faces)
            f = f + fparc(icart,latomfirst(cell_ind(i-1, ncells(1)),j,k)) ! 1 - (-1, 0, 0)
            f = f + fparc(icart,latomfirst(i,cell_ind(j-1, ncells(2)),k)) ! 2 - (0, -1, 0)
            f = f + fparc(icart,latomfirst(i,j,cell_ind(k-1, ncells(3)))) ! 3 - (0, 0, -1)
            f = f + fparc(icart,latomfirst(cell_ind(i+1, ncells(1)),j,k)) ! 4 - (1, 0, 0)
            f = f + fparc(icart,latomfirst(i,cell_ind(j+1, ncells(2)),k)) ! 5 - (0, 1, 0)
            f = f + fparc(icart,latomfirst(i,j,cell_ind(k+1, ncells(3)))) ! 6 - (0, 0, 1)
            ! Interactions of cells that share axes (12 edges)
            f = f + fparc(icart,latomfirst(cell_ind(i-1, ncells(1)),cell_ind(j-1, ncells(2)),k)) ! 1 - (-1, -1, 0)
            f = f + fparc(icart,latomfirst(cell_ind(i-1, ncells(1)),j,cell_ind(k-1, ncells(3)))) ! 2 - (-1, 0, -1)
            f = f + fparc(icart,latomfirst(i,cell_ind(j-1, ncells(2)),cell_ind(k-1, ncells(3)))) ! 3 - (0, -1, -1)
            f = f + fparc(icart,latomfirst(cell_ind(i+1, ncells(1)),cell_ind(j-1, ncells(2)),k)) ! 4 - (1, -1, 0)
            f = f + fparc(icart,latomfirst(cell_ind(i+1, ncells(1)),j,cell_ind(k-1, ncells(3)))) ! 5 - (1, 0, -1)
            f = f + fparc(icart,latomfirst(i,cell_ind(j+1, ncells(2)),cell_ind(k-1, ncells(3)))) ! 6 - (0, 1, -1)
            f = f + fparc(icart,latomfirst(cell_ind(i-1, ncells(1)),cell_ind(j+1, ncells(2)),k)) ! 7 - (-1, 1, 0)
            f = f + fparc(icart,latomfirst(cell_ind(i-1, ncells(1)),j,cell_ind(k+1, ncells(3)))) ! 8 - (-1, 0, 1)
            f = f + fparc(icart,latomfirst(i,cell_ind(j+1, ncells(2)),cell_ind(k+1, ncells(3)))) ! 9 - (0, 1, 1)
            f = f + fparc(icart,latomfirst(cell_ind(i+1, ncells(1)),cell_ind(j+1, ncells(2)),k)) ! 10 - (1, 1, 0)
            f = f + fparc(icart,latomfirst(cell_ind(i+1, ncells(1)),j,cell_ind(k+1, ncells(3)))) ! 11 - (1, 0, 1)
            f = f + fparc(icart,latomfirst(i,cell_ind(j-1, ncells(2)),cell_ind(k+1, ncells(3)))) ! 12 - (0, -1, 1)
            ! Interactions of cells that share vertices (8 vertices)
            f = f + fparc(icart,latomfirst(cell_ind(i-1, ncells(1)),cell_ind(j-1, ncells(2)),cell_ind(k-1, ncells(3)))) ! 1 - (-1, -1, -1)
            f = f + fparc(icart,latomfirst(cell_ind(i-1, ncells(1)),cell_ind(j-1, ncells(2)),cell_ind(k+1, ncells(3)))) ! 2 - (-1, -1, 1)
            f = f + fparc(icart,latomfirst(cell_ind(i-1, ncells(1)),cell_ind(j+1, ncells(2)),cell_ind(k-1, ncells(3)))) ! 3 - (-1, 1, -1)
            f = f + fparc(icart,latomfirst(cell_ind(i-1, ncells(1)),cell_ind(j+1, ncells(2)),cell_ind(k+1, ncells(3)))) ! 4 - (-1, 1, 1)
            f = f + fparc(icart,latomfirst(cell_ind(i+1, ncells(1)),cell_ind(j-1, ncells(2)),cell_ind(k-1, ncells(3)))) ! 5 - (1, -1, -1)
            f = f + fparc(icart,latomfirst(cell_ind(i+1, ncells(1)),cell_ind(j-1, ncells(2)),cell_ind(k+1, ncells(3)))) ! 6 - (1, -1, 1)
            f = f + fparc(icart,latomfirst(cell_ind(i+1, ncells(1)),cell_ind(j+1, ncells(2)),cell_ind(k-1, ncells(3)))) ! 7 - (1, 1, -1)
            f = f + fparc(icart,latomfirst(cell_ind(i+1, ncells(1)),cell_ind(j+1, ncells(2)),cell_ind(k+1, ncells(3)))) ! 8 - (1, 1, 1)
         end if

         icart = latomnext(icart)
      end do

      icell = lcellnext(icell)
   end do

   return
end subroutine computef

subroutine add_cell_behind(cell,i,j,k)
   use cell_indexing, only: index_cell
   use compute_data, only: hasfree, ncells, latomfix, lcellnext, lcellfirst, ncells
   use pbc, only: cell_ind
   implicit none
   integer, intent(in) :: cell(3), i, j, k
   integer :: icell, ic, jc, kc
   ic = cell_ind(cell(1)+i,ncells(1))
   jc = cell_ind(cell(2)+j,ncells(2))
   kc = cell_ind(cell(3)+k,ncells(3))
   if ( .not. hasfree(ic,jc,kc) .and. latomfix(ic,jc,kc) /= 0 ) then
      hasfree(ic,jc,kc) = .true.
      icell = index_cell(cell, ncells)
      lcellnext(icell) = lcellfirst
      lcellfirst = icell
   end if
end subroutine add_cell_behind


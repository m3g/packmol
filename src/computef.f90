!
!  Written by Leandro Martínez, 2009-2011.
!  Copyright (c) 2009-2018, Leandro Martínez, Jose Mario Martinez,
!  Ernesto G. Birgin.
!
! Subroutine that computes the function value
!

subroutine computef(n,x,f)

   use sizes
   use compute_data
   use input, only : fix
   use pbc
   implicit none

   integer :: n, i, j, k, icell
   integer :: ilugan, ilubar, icart, itype, imol, iatom, idatom, &
      ixcell, iycell, izcell

   double precision :: v1(3), v2(3), v3(3)
   double precision :: x(n)
   double precision :: f,fparc,fplus
   double precision :: xbar, ybar, zbar
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

            xbar = x(ilubar+1)
            ybar = x(ilubar+2)
            zbar = x(ilubar+3)

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

               call compcart(icart,xbar,ybar,zbar, &
                  coor(idatom,1),coor(idatom,2),coor(idatom,3), &
                  v1,v2,v3)

               ! Adding to f the value relative to constraints for this atom

               call comprest(icart,fplus)
               f = f + fplus
               frest = dmax1(frest,fplus)
               if(move) frest_atom(icart) = frest_atom(icart) + fplus

               ! Putting atoms in their cells

               if(.not.init1) then

                  !
                  call seticell(xcart(icart,1), xcart(icart,2), xcart(icart,3), ixcell, iycell, izcell)

                  ! Atom linked list

                  latomnext(icart) = latomfirst(ixcell,iycell,izcell)
                  latomfirst(ixcell,iycell,izcell) = icart

                  ! cell with atoms linked list

                  if ( .not. hasfree(ixcell,iycell,izcell) ) then
                     hasfree(ixcell,iycell,izcell) = .true.
                     call ijk_to_icell(ixcell,iycell,izcell,icell)
                     lcellnext(icell) = lcellfirst
                     lcellfirst = icell

                     ! Add cells with fixed atoms which are vicinal to this cell, and
                     ! are behind

                     if ( fix ) then
                        call add_cell_behind(cell_ind(ixcell-1, ncells(1)),iycell,izcell)
                        call add_cell_behind(ixcell,cell_ind(iycell-1, ncells(2)),izcell)
                        call add_cell_behind(ixcell,iycell,cell_ind(izcell-1, ncells(3)))

                        call add_cell_behind(ixcell,cell_ind(iycell-1,ncells(2)),cell_ind(izcell+1,ncells(3)))
                        call add_cell_behind(ixcell,cell_ind(iycell-1,ncells(2)),cell_ind(izcell-1,ncells(3)))
                        call add_cell_behind(cell_ind(ixcell-1,ncells(1)),cell_ind(iycell+1,ncells(2)),izcell)
                        call add_cell_behind(cell_ind(ixcell-1,ncells(1)),iycell,cell_ind(izcell+1,ncells(3)))
                        call add_cell_behind(cell_ind(ixcell-1,ncells(1)),cell_ind(iycell-1,ncells(2)),izcell)
                        call add_cell_behind(cell_ind(ixcell-1,ncells(1)),iycell,cell_ind(izcell-1,ncells(3)))

                        call add_cell_behind(cell_ind(ixcell-1,ncells(1)),cell_ind(iycell+1,ncells(2)),cell_ind(izcell+1,ncells(3)))
                        call add_cell_behind(cell_ind(ixcell-1,ncells(1)),cell_ind(iycell+1,ncells(2)),cell_ind(izcell-1,ncells(3)))
                        call add_cell_behind(cell_ind(ixcell-1,ncells(1)),cell_ind(iycell-1,ncells(2)),cell_ind(izcell+1,ncells(3)))
                        call add_cell_behind(cell_ind(ixcell-1,ncells(1)),cell_ind(iycell-1,ncells(2)),cell_ind(izcell-1,ncells(3)))
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

      call icell_to_ijk(icell,i,j,k)

      icart = latomfirst(i,j,k)
      do while( icart > 0 )

         if(comptype(ibtype(icart))) then
            ! Interactions inside cell
            f = f + fparc(icart,latomnext(icart))
            ! Interactions of cells that share faces
            f = f + fparc(icart,latomfirst(cell_ind(i+1, ncells(1)),j,k))
            f = f + fparc(icart,latomfirst(i,cell_ind(j+1, ncells(2)),k))
            f = f + fparc(icart,latomfirst(i,j,cell_ind(k+1, ncells(3))))
            ! Interactions of cells that share axes
            f = f + fparc(icart,latomfirst(cell_ind(i+1, ncells(1)),cell_ind(j+1, ncells(2)),k))
            f = f + fparc(icart,latomfirst(cell_ind(i+1, ncells(1)),j,cell_ind(k+1, ncells(3))))
            f = f + fparc(icart,latomfirst(cell_ind(i+1, ncells(1)),cell_ind(j-1, ncells(2)),k))
            f = f + fparc(icart,latomfirst(cell_ind(i+1, ncells(1)),j,cell_ind(k-1, ncells(3))))
            f = f + fparc(icart,latomfirst(i,cell_ind(j+1, ncells(2)),cell_ind(k+1, ncells(3))))
            f = f + fparc(icart,latomfirst(i,cell_ind(j+1, ncells(2)),cell_ind(k-1, ncells(3))))
            ! Interactions of cells that share vertices
            f = f + fparc(icart,latomfirst(cell_ind(i+1, ncells(1)),cell_ind(j+1, ncells(2)),cell_ind(k+1, ncells(3))))
            f = f + fparc(icart,latomfirst(cell_ind(i+1, ncells(1)),cell_ind(j+1, ncells(2)),cell_ind(k-1, ncells(3))))
            f = f + fparc(icart,latomfirst(cell_ind(i+1, ncells(1)),cell_ind(j-1, ncells(2)),cell_ind(k+1, ncells(3))))
            f = f + fparc(icart,latomfirst(cell_ind(i+1, ncells(1)),cell_ind(j-1, ncells(2)),cell_ind(k-1, ncells(3))))
         end if

         icart = latomnext(icart)
      end do

      icell = lcellnext(icell)
   end do

   return
end subroutine computef

subroutine add_cell_behind(i,j,k)

   use sizes
   use compute_data
   implicit none
   integer :: icell, i, j, k

   if ( .not. hasfree(i,j,k) .and. latomfix(i,j,k) /= 0 ) then
      hasfree(i,j,k) = .true.
      call ijk_to_icell(i,j,k,icell)
      lcellnext(icell) = lcellfirst
      lcellfirst = icell
   end if

end subroutine add_cell_behind


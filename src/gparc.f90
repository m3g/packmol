!
!  Written by Leandro Martínez, 2009-2011.
!  Copyright (c) 2009-2018, Leandro Martínez, Jose Mario Martinez,
!  Ernesto G. Birgin.
!
! Compute gradient relative to atom-to-atom distances
!

subroutine gparc(icart,firstjcart)

   use sizes
   use compute_data
   use pbc, only : pbc_length
   implicit none

   ! SCALAR ARGUMENTS
   integer, intent(in) :: icart, firstjcart

   ! LOCAL SCALARS
   integer :: jcart
   double precision :: datom, dtemp, xdiff, tol, short_tol, short_tol_scale
   double precision :: dx, dy, dz
   double precision :: xi, yi, zi, ri, fsi, short_ri, short_si
   double precision :: inv_pbcx, inv_pbcy, inv_pbcz
   logical :: use_short_i

   xi = xcart(icart,1)
   yi = xcart(icart,2)
   zi = xcart(icart,3)
   ri = radius(icart)
   fsi = fscale(icart)
   short_ri = short_radius(icart)
   short_si = short_radius_scale(icart)
   use_short_i = use_short_radius(icart)
   inv_pbcx = 1.d0 / pbc_length(1)
   inv_pbcy = 1.d0 / pbc_length(2)
   inv_pbcz = 1.d0 / pbc_length(3)

   jcart = firstjcart
   do while ( jcart > 0 )
      !
      ! Cycle if this type is not to be computed
      !
      if ( .not. comptype(ibtype(jcart))) then
         jcart = latomnext(jcart)
         cycle
      end if
      !
      ! Cycle if the atoms are from the same molecule
      !
      if ( ibmol(icart) == ibmol(jcart) .and. &
         ibtype(icart) == ibtype(jcart) ) then
         jcart = latomnext(jcart)
         cycle
      end if
      !
      ! Cycle if both atoms are from fixed molecules
      !
      if ( fixedatom(icart) .and. fixedatom(jcart) ) then
         jcart = latomnext(jcart)
         cycle
      end if
      !
      ! Otherwise, compute distance and evaluate function for this pair
      !
      tol = (ri+radius(jcart))**2
      dx = xi - xcart(jcart,1)
      dy = yi - xcart(jcart,2)
      dz = zi - xcart(jcart,3)
      dx = dx - pbc_length(1) * dnint(dx * inv_pbcx)
      dy = dy - pbc_length(2) * dnint(dy * inv_pbcy)
      dz = dz - pbc_length(3) * dnint(dz * inv_pbcz)
      datom = dx*dx + dy*dy + dz*dz
      if( datom < tol ) then
         dtemp = fsi*fscale(jcart) * 4.d0 * (datom - tol)
         xdiff = dtemp*dx
         gxcar(icart,1)= gxcar(icart,1) + xdiff
         gxcar(jcart,1)= gxcar(jcart,1) - xdiff
         xdiff = dtemp*dy
         gxcar(icart,2)= gxcar(icart,2) + xdiff
         gxcar(jcart,2)= gxcar(jcart,2) - xdiff
         xdiff = dtemp*dz
         gxcar(icart,3)= gxcar(icart,3) + xdiff
         gxcar(jcart,3)= gxcar(jcart,3) - xdiff
         if ( use_short_i .or. use_short_radius(jcart) ) then
            short_tol = ( short_ri + short_radius(jcart) )**2
            if ( datom < short_tol ) then
               short_tol_scale = dsqrt(short_si*short_radius_scale(jcart))
               short_tol_scale = short_tol_scale*( tol**2 / short_tol**2 )
               dtemp = fsi*fscale(jcart) * 4.d0 * short_tol_scale*(datom - short_tol)
               xdiff = dtemp*dx
               gxcar(icart,1)= gxcar(icart,1) + xdiff
               gxcar(jcart,1)= gxcar(jcart,1) - xdiff
               xdiff = dtemp*dy
               gxcar(icart,2)= gxcar(icart,2) + xdiff
               gxcar(jcart,2)= gxcar(jcart,2) - xdiff
               xdiff = dtemp*dz
               gxcar(icart,3)= gxcar(icart,3) + xdiff
               gxcar(jcart,3)= gxcar(jcart,3) - xdiff
            end if
         end if
      end if
      jcart = latomnext(jcart)
   end do
   return
end subroutine gparc

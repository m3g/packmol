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
   integer :: marker_i
   logical :: use_short_i, fixed_i

   xi = x(icart)
   yi = y(icart)
   zi = z(icart)
   ri = radius(icart)
   fsi = fscale(icart)
   short_ri = short_radius(icart)
   short_si = short_radius_scale(icart)
   marker_i = fixed_short_marker(icart)
   use_short_i = iand(marker_i,1) /= 0
   fixed_i = marker_i >= 2
   inv_pbcx = 1.d0 / pbc_length(1)
   inv_pbcy = 1.d0 / pbc_length(2)
   inv_pbcz = 1.d0 / pbc_length(3)

   if ( .not. fixed_i .and. .not. use_short_i ) then
      ! Fast path: movable atom i and no short-radius term from i.
      jcart = firstjcart
      do while ( jcart > 0 )
         if ( .not. comptype(ibtype_hot(jcart))) then
            jcart = latomnext(jcart)
            cycle
         end if
         if ( ibmol_hot(icart) == ibmol_hot(jcart) .and. ibtype_hot(icart) == ibtype_hot(jcart) ) then
            jcart = latomnext(jcart)
            cycle
         end if

         tol = (ri+radius(jcart))**2
         dx = xi - x(jcart)
         dy = yi - y(jcart)
         dz = zi - z(jcart)
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
            if ( iand(fixed_short_marker(jcart),1) /= 0 ) then
               short_tol = ( short_ri + short_radius(jcart) )**2
               if ( datom < short_tol ) then
                  short_tol_scale = dsqrt(short_si*short_radius_scale(jcart))
                  short_tol_scale = short_tol_scale*( tol**2 / short_tol**2 )
                  dtemp = fsi*fscale(jcart) * short_tol_scale * 4.d0 * (datom - short_tol)
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
   else if ( .not. fixed_i ) then
      ! Short-radius-enabled path for movable atoms.
      jcart = firstjcart
      do while ( jcart > 0 )
         if ( .not. comptype(ibtype_hot(jcart))) then
            jcart = latomnext(jcart)
            cycle
         end if
         if ( ibmol_hot(icart) == ibmol_hot(jcart) .and. ibtype_hot(icart) == ibtype_hot(jcart) ) then
            jcart = latomnext(jcart)
            cycle
         end if

         tol = (ri+radius(jcart))**2
         dx = xi - x(jcart)
         dy = yi - y(jcart)
         dz = zi - z(jcart)
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
            short_tol = ( short_ri + short_radius(jcart) )**2
            if ( datom < short_tol ) then
               short_tol_scale = dsqrt(short_si*short_radius_scale(jcart))
               short_tol_scale = short_tol_scale*( tol**2 / short_tol**2 )
               dtemp = fsi*fscale(jcart) * short_tol_scale * 4.d0 * (datom - short_tol)
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

         jcart = latomnext(jcart)
      end do
   else
      ! Fixed-atom path (keeps explicit fixed/fixed exclusion).
      jcart = firstjcart
      do while ( jcart > 0 )
         if ( .not. comptype(ibtype_hot(jcart))) then
            jcart = latomnext(jcart)
            cycle
         end if
         if ( ibmol_hot(icart) == ibmol_hot(jcart) .and. ibtype_hot(icart) == ibtype_hot(jcart) ) then
            jcart = latomnext(jcart)
            cycle
         end if
         if ( fixed_short_marker(jcart) >= 2 ) then
            jcart = latomnext(jcart)
            cycle
         end if

         tol = (ri+radius(jcart))**2
         dx = xi - x(jcart)
         dy = yi - y(jcart)
         dz = zi - z(jcart)
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
            if ( use_short_i .or. iand(fixed_short_marker(jcart),1) /= 0 ) then
               short_tol = ( short_ri + short_radius(jcart) )**2
               if ( datom < short_tol ) then
                  short_tol_scale = dsqrt(short_si*short_radius_scale(jcart))
                  short_tol_scale = short_tol_scale*( tol**2 / short_tol**2 )
                  dtemp = fsi*fscale(jcart) * short_tol_scale * 4.d0 * (datom - short_tol)
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
   end if

   return
end subroutine gparc

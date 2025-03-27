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
   use pbc, only : delta_vector, pbc_length
   implicit none

   ! SCALAR ARGUMENTS
   integer, intent(in) :: icart, firstjcart

   ! LOCAL SCALARS
   integer :: jcart
   double precision :: datom, dtemp, xdiff, tol, short_tol, short_tol_scale
   double precision :: vdiff(3)

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
      tol = (radius(icart)+radius(jcart))**2
      vdiff = delta_vector(xcart(icart,:), xcart(jcart,:), pbc_length)
      datom = (vdiff(1))**2 + (vdiff(2))**2 + (vdiff(3))**2
      if( datom < tol ) then
         dtemp = fscale(icart)*fscale(jcart) * 4.d0 * (datom - tol)
         xdiff = dtemp*vdiff(1)
         gxcar(icart,1)= gxcar(icart,1) + xdiff
         gxcar(jcart,1)= gxcar(jcart,1) - xdiff
         xdiff = dtemp*vdiff(2)
         gxcar(icart,2)= gxcar(icart,2) + xdiff
         gxcar(jcart,2)= gxcar(jcart,2) - xdiff
         xdiff = dtemp*vdiff(3)
         gxcar(icart,3)= gxcar(icart,3) + xdiff
         gxcar(jcart,3)= gxcar(jcart,3) - xdiff
         if ( use_short_radius(icart) .or. use_short_radius(jcart) ) then
            short_tol = ( short_radius(icart) + short_radius(jcart) )**2
            if ( datom < short_tol ) then
               short_tol_scale = dsqrt(short_radius_scale(icart)*short_radius_scale(jcart))
               short_tol_scale = short_tol_scale*( tol**2 / short_tol**2 )
               dtemp = fscale(icart)*fscale(jcart) * 4.d0 * short_tol_scale*(datom - short_tol)
               xdiff = dtemp*vdiff(1)
               gxcar(icart,1)= gxcar(icart,1) + xdiff
               gxcar(jcart,1)= gxcar(jcart,1) - xdiff
               xdiff = dtemp*vdiff(2)
               gxcar(icart,2)= gxcar(icart,2) + xdiff
               gxcar(jcart,2)= gxcar(jcart,2) - xdiff
               xdiff = dtemp*vdiff(3)
               gxcar(icart,3)= gxcar(icart,3) + xdiff
               gxcar(jcart,3)= gxcar(jcart,3) - xdiff
            end if
         end if
      end if
      jcart = latomnext(jcart)
   end do
   return
end subroutine gparc


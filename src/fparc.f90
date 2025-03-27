!
!  Written by Leandro Martínez, 2009-2011.
!  Copyright (c) 2009-2018, Leandro Martínez, Jose Mario Martinez,
!  Ernesto G. Birgin.
!
! Function that computes the atom-to-atom component of the objective
! function
!

double precision function fparc(icart,firstjcart)

   use sizes
   use compute_data
   use pbc, only : delta_vector, pbc_length
   implicit none

   ! SCALAR ARGUMENTS
   integer, intent(in) :: icart,firstjcart

   ! LOCAL SCALARS
   integer :: jcart
   double precision :: datom, tol, short_tol, short_tol_penalty, short_tol_scale
   double precision :: vdiff(3)

   fparc = 0.0d0
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
      vdiff = delta_vector(xcart(icart,:), xcart(jcart,:), pbc_length)
      datom = ( vdiff(1) )**2 + ( vdiff(2) )**2 + ( vdiff(3) )**2
      tol = (radius(icart)+radius(jcart))**2
      if ( datom < tol ) then
         fparc = fparc + fscale(icart)*fscale(jcart)*(datom-tol)**2
         if ( use_short_radius(icart) .or. use_short_radius(jcart) ) then
            short_tol = (short_radius(icart)+short_radius(jcart))**2
            if ( datom < short_tol ) then
               short_tol_penalty = datom-short_tol
               short_tol_scale = dsqrt(short_radius_scale(icart)*short_radius_scale(jcart))
               short_tol_scale = short_tol_scale*(tol**2/short_tol**2)
               fparc = fparc + fscale(icart)*fscale(jcart)*short_tol_scale*short_tol_penalty**2
            end if
         end if
      end if
      tol = (radius_ini(icart)+radius_ini(jcart))**2
      fdist = dmax1(tol-datom,fdist)
      if ( move ) then
         fdist_atom(icart) = dmax1(tol-datom,fdist_atom(icart))
         fdist_atom(jcart) = dmax1(tol-datom,fdist_atom(jcart))
      end if
      jcart = latomnext(jcart)
   end do

end function fparc


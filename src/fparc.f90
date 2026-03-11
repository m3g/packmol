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
   use pbc, only : pbc_length
   implicit none

   ! SCALAR ARGUMENTS
   integer, intent(in) :: icart,firstjcart

   ! LOCAL SCALARS
   integer :: jcart
   double precision :: datom, tol, short_tol, short_tol_penalty, short_tol_scale
   double precision :: dx, dy, dz
   double precision :: xi, yi, zi, ri, rii, fsi, short_ri, short_si
   double precision :: inv_pbcx, inv_pbcy, inv_pbcz
   logical :: use_short_i

   xi = xcart(icart,1)
   yi = xcart(icart,2)
   zi = xcart(icart,3)
   ri = radius(icart)
   rii = radius_ini(icart)
   fsi = fscale(icart)
   short_ri = short_radius(icart)
   short_si = short_radius_scale(icart)
   use_short_i = use_short_radius(icart)
   inv_pbcx = 1.d0 / pbc_length(1)
   inv_pbcy = 1.d0 / pbc_length(2)
   inv_pbcz = 1.d0 / pbc_length(3)

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
      dx = xi - xcart(jcart,1)
      dy = yi - xcart(jcart,2)
      dz = zi - xcart(jcart,3)
      dx = dx - pbc_length(1) * dnint(dx * inv_pbcx)
      dy = dy - pbc_length(2) * dnint(dy * inv_pbcy)
      dz = dz - pbc_length(3) * dnint(dz * inv_pbcz)
      datom = dx*dx + dy*dy + dz*dz

      tol = (ri + radius(jcart))**2
      if ( datom < tol ) then
         fparc = fparc + fsi*fscale(jcart)*(datom-tol)**2
         if ( use_short_i .or. use_short_radius(jcart) ) then
            short_tol = (short_ri + short_radius(jcart))**2
            if ( datom < short_tol ) then
               short_tol_penalty = datom-short_tol
               short_tol_scale = dsqrt(short_si*short_radius_scale(jcart))
               short_tol_scale = short_tol_scale*(tol**2/short_tol**2)
               fparc = fparc + fsi*fscale(jcart)*short_tol_scale*short_tol_penalty**2
            end if
         end if
      end if
      tol = (rii + radius_ini(jcart))**2
      fdist = dmax1(tol-datom,fdist)
      if ( move ) then
         fdist_atom(icart) = dmax1(tol-datom,fdist_atom(icart))
         fdist_atom(jcart) = dmax1(tol-datom,fdist_atom(jcart))
      end if
      jcart = latomnext(jcart)
   end do

end function fparc

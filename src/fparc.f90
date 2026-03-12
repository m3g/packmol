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
   use compute_data, only : comptype, fdist, fdist_atom, move, radius, radius_ini, fscale, short_radius, &
      short_radius_scale, fixed_short_marker, latomnext, xcart, ibmol, ibtype
   use pbc, only : pbc_length
   implicit none

   ! SCALAR ARGUMENTS
   integer, intent(in) :: icart,firstjcart

   ! LOCAL SCALARS
   integer :: jcart
   double precision :: datom, tol, short_tol, short_tol_penalty, short_tol_scale
   double precision :: sumr, short_sumr, delta, tol_sq, short_tol_sq
   double precision :: dx, dy, dz
   double precision :: xi, yi, zi, ri, rii, fsi, short_ri, short_si
   double precision :: inv_pbcx, inv_pbcy, inv_pbcz
   integer :: marker_i, itype_i, imol_i
   logical :: use_short_i, fixed_i

   xi = xcart(icart,1)
   yi = xcart(icart,2)
   zi = xcart(icart,3)
   ri = radius(icart)
   rii = radius_ini(icart)
   fsi = fscale(icart)
   short_ri = short_radius(icart)
   short_si = short_radius_scale(icart)
   marker_i = fixed_short_marker(icart)
   use_short_i = iand(marker_i,1) /= 0
   fixed_i = marker_i >= 2
   itype_i = ibtype(icart)
   imol_i = ibmol(icart)
   inv_pbcx = 1.d0 / pbc_length(1)
   inv_pbcy = 1.d0 / pbc_length(2)
   inv_pbcz = 1.d0 / pbc_length(3)

   fparc = 0.0d0
   if ( .not. fixed_i .and. .not. use_short_i ) then
      ! Fast path: movable atom i and no short-radius term from i.
      jcart = firstjcart
      do while ( jcart > 0 )
         if ( .not. comptype(ibtype(jcart))) then
            jcart = latomnext(jcart)
            cycle
         end if
         if ( imol_i == ibmol(jcart) .and. itype_i == ibtype(jcart) ) then
            jcart = latomnext(jcart)
            cycle
         end if

         dx = xi - xcart(jcart,1)
         dy = yi - xcart(jcart,2)
         dz = zi - xcart(jcart,3)
         dx = dx - pbc_length(1) * dnint(dx * inv_pbcx)
         dy = dy - pbc_length(2) * dnint(dy * inv_pbcy)
         dz = dz - pbc_length(3) * dnint(dz * inv_pbcz)
         datom = dx*dx + dy*dy + dz*dz

         sumr = ri + radius(jcart)
         tol = sumr*sumr
         if ( datom < tol ) then
            delta = datom - tol
            fparc = fparc + fsi*fscale(jcart)*delta*delta
            if ( iand(fixed_short_marker(jcart),1) /= 0 ) then
               short_sumr = short_ri + short_radius(jcart)
               short_tol = short_sumr*short_sumr
               if ( datom < short_tol ) then
                  short_tol_penalty = datom-short_tol
                  short_tol_scale = dsqrt(short_si*short_radius_scale(jcart))
                  tol_sq = tol*tol
                  short_tol_sq = short_tol*short_tol
                  short_tol_scale = short_tol_scale*(tol_sq/short_tol_sq)
                  fparc = fparc + fsi*fscale(jcart)*short_tol_scale*short_tol_penalty*short_tol_penalty
               end if
            end if
         end if
         sumr = rii + radius_ini(jcart)
         tol = sumr*sumr
         fdist = dmax1(tol-datom,fdist)
         if ( move ) then
            fdist_atom(icart) = dmax1(tol-datom,fdist_atom(icart))
            fdist_atom(jcart) = dmax1(tol-datom,fdist_atom(jcart))
         end if
         jcart = latomnext(jcart)
      end do
   else if ( .not. fixed_i ) then
      ! Short-radius-enabled path for movable atoms.
      jcart = firstjcart
      do while ( jcart > 0 )
         if ( .not. comptype(ibtype(jcart))) then
            jcart = latomnext(jcart)
            cycle
         end if
         if ( imol_i == ibmol(jcart) .and. itype_i == ibtype(jcart) ) then
            jcart = latomnext(jcart)
            cycle
         end if

         dx = xi - xcart(jcart,1)
         dy = yi - xcart(jcart,2)
         dz = zi - xcart(jcart,3)
         dx = dx - pbc_length(1) * dnint(dx * inv_pbcx)
         dy = dy - pbc_length(2) * dnint(dy * inv_pbcy)
         dz = dz - pbc_length(3) * dnint(dz * inv_pbcz)
         datom = dx*dx + dy*dy + dz*dz

         sumr = ri + radius(jcart)
         tol = sumr*sumr
         if ( datom < tol ) then
            delta = datom - tol
            fparc = fparc + fsi*fscale(jcart)*delta*delta
            short_sumr = short_ri + short_radius(jcart)
            short_tol = short_sumr*short_sumr
            if ( datom < short_tol ) then
               short_tol_penalty = datom-short_tol
               short_tol_scale = dsqrt(short_si*short_radius_scale(jcart))
               tol_sq = tol*tol
               short_tol_sq = short_tol*short_tol
               short_tol_scale = short_tol_scale*(tol_sq/short_tol_sq)
               fparc = fparc + fsi*fscale(jcart)*short_tol_scale*short_tol_penalty*short_tol_penalty
            end if
         end if
         sumr = rii + radius_ini(jcart)
         tol = sumr*sumr
         fdist = dmax1(tol-datom,fdist)
         if ( move ) then
            fdist_atom(icart) = dmax1(tol-datom,fdist_atom(icart))
            fdist_atom(jcart) = dmax1(tol-datom,fdist_atom(jcart))
         end if
         jcart = latomnext(jcart)
      end do
   else
      ! Fixed-atom path (keeps explicit fixed/fixed exclusion).
      jcart = firstjcart
      do while ( jcart > 0 )
         if ( .not. comptype(ibtype(jcart))) then
            jcart = latomnext(jcart)
            cycle
         end if
         if ( imol_i == ibmol(jcart) .and. itype_i == ibtype(jcart) ) then
            jcart = latomnext(jcart)
            cycle
         end if
         if ( fixed_short_marker(jcart) >= 2 ) then
            jcart = latomnext(jcart)
            cycle
         end if

         dx = xi - xcart(jcart,1)
         dy = yi - xcart(jcart,2)
         dz = zi - xcart(jcart,3)
         dx = dx - pbc_length(1) * dnint(dx * inv_pbcx)
         dy = dy - pbc_length(2) * dnint(dy * inv_pbcy)
         dz = dz - pbc_length(3) * dnint(dz * inv_pbcz)
         datom = dx*dx + dy*dy + dz*dz

         sumr = ri + radius(jcart)
         tol = sumr*sumr
         if ( datom < tol ) then
            delta = datom - tol
            fparc = fparc + fsi*fscale(jcart)*delta*delta
            if ( use_short_i .or. iand(fixed_short_marker(jcart),1) /= 0 ) then
               short_sumr = short_ri + short_radius(jcart)
               short_tol = short_sumr*short_sumr
               if ( datom < short_tol ) then
                  short_tol_penalty = datom-short_tol
                  short_tol_scale = dsqrt(short_si*short_radius_scale(jcart))
                  tol_sq = tol*tol
                  short_tol_sq = short_tol*short_tol
                  short_tol_scale = short_tol_scale*(tol_sq/short_tol_sq)
                  fparc = fparc + fsi*fscale(jcart)*short_tol_scale*short_tol_penalty*short_tol_penalty
               end if
            end if
         end if
         sumr = rii + radius_ini(jcart)
         tol = sumr*sumr
         fdist = dmax1(tol-datom,fdist)
         if ( move ) then
            fdist_atom(icart) = dmax1(tol-datom,fdist_atom(icart))
            fdist_atom(jcart) = dmax1(tol-datom,fdist_atom(jcart))
         end if
         jcart = latomnext(jcart)
      end do
   end if

end function fparc

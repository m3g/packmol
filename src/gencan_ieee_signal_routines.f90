subroutine gp_ieee_signal1(gpsupn, acgeps, bcgeps, cgepsf, cgepsi, cggpnf)
   implicit none
   double precision :: gpsupn, acgeps, cgepsf, cgepsi, cggpnf, bcgeps
   if ( gpsupn .gt. 0.d0 ) then
      acgeps = log10( cgepsf / cgepsi ) / log10( cggpnf / gpsupn )
      bcgeps = log10( cgepsi ) - acgeps * log10( gpsupn )
   else
      acgeps = 0.0d0
      bcgeps = cgepsf
   end if
end subroutine gp_ieee_signal1

subroutine gp_ieee_signal2(&
   cgmaxit, nind, nearlyq, ucgmaxit, cgscre, &
   kappa, gpeucn2, gpeucn20, epsgpen2, epsgpsn, &
   cgeps, acgeps, bcgeps, cgepsf, cgepsi, gpsupn, gpsupn0)
   implicit none
   logical :: nearlyq
   integer :: cgmaxit, nind, ucgmaxit, cgscre
   double precision :: kappa, gpeucn2, gpeucn20, epsgpen2, epsgpsn, &
      cgeps, acgeps, bcgeps, cgepsf, cgepsi, gpsupn, gpsupn0
   if( ucgmaxit .le. 0 ) then
      if ( nearlyq ) then
         cgmaxit = nind
      else
         if ( cgscre .eq. 1 ) then
            kappa = log10( gpeucn2 / gpeucn20 )/log10( epsgpen2 / gpeucn20 )
         else ! if ( cgscre .eq. 2 ) then
            kappa= log10( gpsupn / gpsupn0 ) / log10( epsgpsn / gpsupn0 )
         end if
         kappa = max( 0.0d0, min( 1.0d0, kappa ) )
         cgmaxit = int(( 1.0d0 - kappa ) * max( 1.0d0, 10.0d0 * log10( dfloat( nind ) ) ) + kappa * dfloat( nind ) )
         ! L. Martinez added to accelerate the iterations near the solution
         cgmaxit = min(20,cgmaxit)
      end if
   else
      cgmaxit = ucgmaxit
   end if
   if ( cgscre .eq. 1 ) then
      cgeps = sqrt( 10.0d0 ** ( acgeps * log10( gpeucn2 ) + bcgeps ) )
   else ! if ( cgscre .eq. 2 ) then
      cgeps = 10.0d0 ** ( acgeps * log10( gpsupn ) + bcgeps )
   end if
   cgeps = max( cgepsf, min( cgepsi, cgeps ) )
end subroutine gp_ieee_signal2

!
!  Written by Leandro Martínez, 2009-2011.
!  Copyright (c) 2009-2018, Leandro Martínez, Jose Mario Martinez,
!  Ernesto G. Birgin.
!
!
! Subroutine comprest: Compute the function value relative to
!                      to the restrictions for one atom
!

subroutine comprest(icart,f)

   use sizes
   use compute_data, only : xcart, restpars, scale, scale2, nratom, ityperest, iratom

   implicit none
   integer :: iratcount, irest, icart
   double precision :: xmin, ymin, zmin, clength, a1, a2, a3, w, b1, b2, b3, d, a4
   double precision :: f
   double precision :: xmax, ymax, zmax
   double precision :: v1, v2, v3
   double precision :: vnorm
   double precision :: xmed, ymed, zmed
   double precision :: x, y, z

   f = 0.d0
   do iratcount = 1, nratom(icart)
      irest = iratom(icart,iratcount)
      if(ityperest(irest).eq.2) then
         clength = restpars(irest,4)
         xmin = restpars(irest,1)
         ymin = restpars(irest,2)
         zmin = restpars(irest,3)
         xmax = restpars(irest,1) + clength
         ymax = restpars(irest,2) + clength
         zmax = restpars(irest,3) + clength
         a1 = dmin1(xcart(icart,1) - xmin, 0.d0)
         a2 = dmin1(xcart(icart,2) - ymin, 0.d0)
         a3 = dmin1(xcart(icart,3) - zmin, 0.d0)
         f = f + scale*(a1 * a1 + a2 * a2 + a3 * a3)
         a1 = dmax1(xcart(icart,1) - xmax, 0.d0)
         a2 = dmax1(xcart(icart,2) - ymax, 0.d0)
         a3 = dmax1(xcart(icart,3) - zmax, 0.d0)
         f = f + scale*(a1 * a1 + a2 * a2 + a3 * a3)
      else if(ityperest(irest).eq.3) then
         xmin = restpars(irest,1)
         ymin = restpars(irest,2)
         zmin = restpars(irest,3)
         xmax = restpars(irest,4)
         ymax = restpars(irest,5)
         zmax = restpars(irest,6)
         a1 = dmin1(xcart(icart,1) - xmin, 0.d0)
         a2 = dmin1(xcart(icart,2) - ymin, 0.d0)
         a3 = dmin1(xcart(icart,3) - zmin, 0.d0)
         f = f + scale*(a1 * a1 + a2 * a2 + a3 * a3)
         a1 = dmax1(xcart(icart,1) - xmax, 0.d0)
         a2 = dmax1(xcart(icart,2) - ymax, 0.d0)
         a3 = dmax1(xcart(icart,3) - zmax, 0.d0)
         f = f + scale*(a1 * a1 + a2 * a2 + a3 * a3)
      else if(ityperest(irest).eq.4) then
         w = (xcart(icart,1)-restpars(irest,1))**2 + &
            (xcart(icart,2)-restpars(irest,2))**2 + &
            (xcart(icart,3)-restpars(irest,3))**2 - &
            restpars(irest,4)**2
         a1 = dmax1(w,0.d0)
         f = f + scale2*a1*a1
      else if(ityperest(irest).eq.5) then
         a1 = (xcart(icart,1)-restpars(irest,1))**2 / restpars(irest,4)**2
         a2 = (xcart(icart,2)-restpars(irest,2))**2 / restpars(irest,5)**2
         a3 = (xcart(icart,3)-restpars(irest,3))**2 / restpars(irest,6)**2
         a4 = restpars(irest,7)**2
         w = a1 + a2 + a3 - a4
         a1 = dmax1(w,0.d0)
         f = f + scale2*a1*a1
      else if(ityperest(irest).eq.6) then ! outside cube
         xmin = restpars(irest,1)
         ymin = restpars(irest,2)
         zmin = restpars(irest,3)
         xmax = restpars(irest,1) + restpars(irest,4)
         ymax = restpars(irest,2) + restpars(irest,4)
         zmax = restpars(irest,3) + restpars(irest,4)
         x = xcart(icart, 1)
         y = xcart(icart, 2)
         z = xcart(icart, 3)
         a1 = 0.0
         a2 = 0.0
         a3 = 0.0
         if ( ( x > xmin .and. x < xmax ) .and. &
              ( y > ymin .and. y < ymax ) .and. &
              ( z > zmin .and. z < zmax ) ) then
            xmed = (xmax - xmin) / 2
            ymed = (ymax - ymin) / 2
            zmed = (zmax - zmin) / 2
            if ( x <= xmed ) a1 = x - xmin 
            if ( x > xmed ) a1 = xmax - x
            if ( y <= ymed ) a2 = y - ymin
            if ( y > ymed ) a2 = ymax - y
            if ( z <= zmed ) a3 = z - zmin  
            if ( z > zmed ) a3 = zmax - z
         end if
         f = f + scale * (a1 + a2 + a3)
      else if(ityperest(irest).eq.7) then ! outside box
         xmin = restpars(irest,1)
         ymin = restpars(irest,2)
         zmin = restpars(irest,3)
         xmax = restpars(irest,4)
         ymax = restpars(irest,5)
         zmax = restpars(irest,6)
         x = xcart(icart, 1)
         y = xcart(icart, 2)
         z = xcart(icart, 3)
         a1 = 0.0
         a2 = 0.0
         a3 = 0.0
         if ( ( x > xmin .and. x < xmax ) .and. &
              ( y > ymin .and. y < ymax ) .and. &
              ( z > zmin .and. z < zmax ) ) then
            xmed = (xmax - xmin) / 2
            ymed = (ymax - ymin) / 2
            zmed = (zmax - zmin) / 2
            if ( x <= xmed ) a1 = x - xmin 
            if ( x > xmed ) a1 = xmax - x
            if ( y <= ymed ) a2 = y - ymin
            if ( y > ymed ) a2 = ymax - y
            if ( z <= zmed ) a3 = z - zmin  
            if ( z > zmed ) a3 = zmax - z
         end if
         f = f + scale * (a1 + a2 + a3)
      else if(ityperest(irest).eq.8) then
         w = (xcart(icart,1)-restpars(irest,1))**2 + &
            (xcart(icart,2)-restpars(irest,2))**2 + &
            (xcart(icart,3)-restpars(irest,3))**2 - &
            restpars(irest,4)**2
         a1 = dmin1(w,0.d0)
         f = f + scale2*a1*a1
      else if(ityperest(irest).eq.9) then
         a1 = (xcart(icart,1)-restpars(irest,1))**2 / restpars(irest,4)**2
         a2 = (xcart(icart,2)-restpars(irest,2))**2 / restpars(irest,5)**2
         a3 = (xcart(icart,3)-restpars(irest,3))**2 / restpars(irest,6)**2
         a4 = restpars(irest,7)**2
         w = a1 + a2 + a3 - a4
         a1 = dmin1(w,0.d0)
         f = f + a1*a1
      else if(ityperest(irest).eq.10) then
         w = restpars(irest,1)*xcart(icart,1) + &
            restpars(irest,2)*xcart(icart,2) + &
            restpars(irest,3)*xcart(icart,3) - &
            restpars(irest,4)
         a1 = dmin1(w,0.d0)
         f = f + scale * a1*a1
      else if(ityperest(irest).eq.11) then
         w = restpars(irest,1)*xcart(icart,1) + &
            restpars(irest,2)*xcart(icart,2) + &
            restpars(irest,3)*xcart(icart,3) - &
            restpars(irest,4)
         a1 = dmax1(w,0.d0)
         f = f + scale * a1*a1
      else if(ityperest(irest).eq.12) then
         a1 = xcart(icart,1) - restpars(irest,1)
         a2 = xcart(icart,2) - restpars(irest,2)
         a3 = xcart(icart,3) - restpars(irest,3)
         vnorm = sqrt(restpars(irest,4)**2 + restpars(irest,5)**2 + restpars(irest,6)**2)
         v1 = restpars(irest,4)/vnorm
         v2 = restpars(irest,5)/vnorm
         v3 = restpars(irest,6)/vnorm
         b1 = v1 * a1
         b2 = v2 * a2
         b3 = v3 * a3
         w = b1 + b2 + b3
         d = ( a1 - v1*w )**2 + ( a2 - v2*w )**2 + ( a3 - v3*w )**2
         f = f + scale2 * ( &
            dmax1(-w , 0.d0)**2 + &
            dmax1(w - restpars(irest,9), 0.d0)**2 + &
            dmax1(d - restpars(irest,7)**2 , 0.d0 )**2 )
      else if(ityperest(irest).eq.13) then
         a1 = xcart(icart,1) - restpars(irest,1)
         a2 = xcart(icart,2) - restpars(irest,2)
         a3 = xcart(icart,3) - restpars(irest,3)
         vnorm = sqrt(restpars(irest,4)**2 + restpars(irest,5)**2 + restpars(irest,6)**2)
         v1 = restpars(irest,4)/vnorm
         v2 = restpars(irest,5)/vnorm
         v3 = restpars(irest,6)/vnorm
         b1 = v1 * a1
         b2 = v2 * a2
         b3 = v3 * a3
         w = b1 + b2 + b3
         d = ( a1 - v1*w )**2 +( a2 - v2*w )**2 + ( a3 - v3*w )**2
         f = f + scale2 * ( &
            dmin1(-w , 0.d0)**2 * &
            dmin1(w - restpars(irest,9), 0.d0)**2 * &
            dmin1(d - restpars(irest,7)**2 , 0.d0 )**2 )
      else if(ityperest(irest).eq.14) then
         a1 = -(xcart(icart,1) - restpars(irest,1))**2/(2*restpars(irest,3)**2)
         a2 = -(xcart(icart,2) - restpars(irest,2))**2/(2*restpars(irest,4)**2)
         if(a1+a2<=-50) then
            w = -(xcart(icart,3)-restpars(irest,5))
         else
            w = restpars(irest,6)*exp(a1+a2)-(xcart(icart,3)-restpars(irest,5))
         end if
         a1 = dmax1(w,0.d0)
         f = f + scale * a1*a1
      else if(ityperest(irest).eq.15) then
         a1 = -(xcart(icart,1) - restpars(irest,1))**2/(2*restpars(irest,3)**2)
         a2 = -(xcart(icart,2) - restpars(irest,2))**2/(2*restpars(irest,4)**2)
         if(a1+a2<=-50) then
            w = -(xcart(icart,3)-restpars(irest,5))
         else
            w = restpars(irest,6)*exp(a1+a2)-(xcart(icart,3)-restpars(irest,5))
         end if
         a1 = dmin1(w,0.d0)
         f = f + scale * a1*a1
      end if
   end do
   return
end subroutine comprest

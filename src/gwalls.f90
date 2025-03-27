!
!  Written by Leandro Martínez, 2009-2011.
!  Copyright (c) 2009-2018, Leandro Martínez, Jose Mario Martinez,
!  Ernesto G. Birgin.
!
! Gradient relative to restraints
!

subroutine gwalls(icart,irest)

   use sizes
   use compute_data

   implicit none
   integer :: icart, irest
   double precision :: a1, a2, a3, a4, a5, a6, xmin, ymin, zmin, &
      xmax, ymax, zmax, &
      clength, b1, b2, b3, c1, c2, w, d, rg(3), &
      vnorm, vv1, vv2, vv3, frab, frac, frbc, &
      dfra(3), dfrb(3), dfrc(3)
   double precision :: xmed, ymed, zmed
   double precision :: x, y, z

   if(ityperest(irest).eq.2) then
      clength = restpars(irest,4)
      xmin = restpars(irest,1)
      ymin = restpars(irest,2)
      zmin = restpars(irest,3)
      xmax = restpars(irest,1) + clength
      ymax = restpars(irest,2) + clength
      zmax = restpars(irest,3) + clength
      a1 = xcart(icart,1) - xmin
      a2 = xcart(icart,2) - ymin
      a3 = xcart(icart,3) - zmin
      if(a1.lt.0.d0) gxcar(icart,1) = gxcar(icart,1) + scale * 2.d0 * a1
      if(a2.lt.0.d0) gxcar(icart,2) = gxcar(icart,2) + scale * 2.d0 * a2
      if(a3.lt.0.d0) gxcar(icart,3) = gxcar(icart,3) + scale * 2.d0 * a3
      a1 = xcart(icart,1) - xmax
      a2 = xcart(icart,2) - ymax
      a3 = xcart(icart,3) - zmax
      if(a1.gt.0.d0) gxcar(icart,1) = gxcar(icart,1) + scale * 2.d0 * a1
      if(a2.gt.0.d0) gxcar(icart,2) = gxcar(icart,2) + scale * 2.d0 * a2
      if(a3.gt.0.d0) gxcar(icart,3) = gxcar(icart,3) + scale * 2.d0 * a3
   else if(ityperest(irest).eq.3) then
      xmin = restpars(irest,1)
      ymin = restpars(irest,2)
      zmin = restpars(irest,3)
      xmax = restpars(irest,4)
      ymax = restpars(irest,5)
      zmax = restpars(irest,6)
      a1 = xcart(icart,1) - xmin
      a2 = xcart(icart,2) - ymin
      a3 = xcart(icart,3) - zmin
      if(a1.lt.0.d0) gxcar(icart,1) = gxcar(icart,1) + scale * 2.d0 * a1
      if(a2.lt.0.d0) gxcar(icart,2) = gxcar(icart,2) + scale * 2.d0 * a2
      if(a3.lt.0.d0) gxcar(icart,3) = gxcar(icart,3) + scale * 2.d0 * a3
      a1 = xcart(icart,1) - xmax
      a2 = xcart(icart,2) - ymax
      a3 = xcart(icart,3) - zmax
      if(a1.gt.0.d0) gxcar(icart,1) = gxcar(icart,1) + scale * 2.d0 * a1
      if(a2.gt.0.d0) gxcar(icart,2) = gxcar(icart,2) + scale * 2.d0 * a2
      if(a3.gt.0.d0) gxcar(icart,3) = gxcar(icart,3) + scale * 2.d0 * a3
   else if(ityperest(irest).eq.4) then
      d = (xcart(icart,1)-restpars(irest,1))**2 + &
         (xcart(icart,2)-restpars(irest,2))**2 + &
         (xcart(icart,3)-restpars(irest,3))**2 - &
         restpars(irest,4)**2
      if(d.gt.0.d0) then
         gxcar(icart,1) = gxcar(icart,1) + 4.d0 * scale2 * &
            (xcart(icart,1)-restpars(irest,1))*d
         gxcar(icart,2) = gxcar(icart,2) + 4.d0 * scale2 * &
            (xcart(icart,2)-restpars(irest,2))*d
         gxcar(icart,3) = gxcar(icart,3) + 4.d0 * scale2 * &
            (xcart(icart,3)-restpars(irest,3))*d
      end if
   else if(ityperest(irest).eq.5) then
      a1 = xcart(icart,1)-restpars(irest,1)
      b1 = xcart(icart,2)-restpars(irest,2)
      c1 = xcart(icart,3)-restpars(irest,3)
      a2 = restpars(irest,4)**2
      b2 = restpars(irest,5)**2
      c2 = restpars(irest,6)**2
      d = a1**2/a2+b1**2/b2+c1**2/c2-restpars(irest,7)**2
      if(d.gt.0) then
         gxcar(icart,1) = gxcar(icart,1) + scale2*4.d0*d*a1/a2
         gxcar(icart,2) = gxcar(icart,2) + scale2*4.d0*d*b1/b2
         gxcar(icart,3) = gxcar(icart,3) + scale2*4.d0*d*c1/c2
      end if
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
      a4 = 0.0
      a5 = 0.0
      a6 = 0.0
      if ( ( x > xmin .and. x < xmax ) .and. &
           ( y > ymin .and. y < ymax ) .and. &
           ( z > zmin .and. z < zmax ) ) then
         xmed = (xmax - xmin) / 2
         ymed = (ymax - ymin) / 2
         zmed = (zmax - zmin) / 2
         if ( x <= xmed ) a1 = 1.0
         if ( x > xmed ) a4 = -1.0
         if ( y <= ymed ) a2 = 1.0
         if ( y > ymed ) a5 = -1.0
         if ( z <= zmed ) a3 = 1.0
         if ( z > zmed ) a6 = -1.0
      end if
      gxcar(icart,1) = gxcar(icart,1) + scale * (a1 + a4)
      gxcar(icart,2) = gxcar(icart,2) + scale * (a2 + a5) 
      gxcar(icart,3) = gxcar(icart,3) + scale * (a3 + a6)
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
      a4 = 0.0
      a5 = 0.0
      a6 = 0.0
      if ( ( x > xmin .and. x < xmax ) .and. &
           ( y > ymin .and. y < ymax ) .and. &
           ( z > zmin .and. z < zmax ) ) then
         xmed = (xmax - xmin) / 2
         ymed = (ymax - ymin) / 2
         zmed = (zmax - zmin) / 2
         if ( x <= xmed ) a1 = 1.0
         if ( x > xmed ) a4 = -1.0
         if ( y <= ymed ) a2 = 1.0
         if ( y > ymed ) a5 = -1.0
         if ( z <= zmed ) a3 = 1.0
         if ( z > zmed ) a6 = -1.0
      end if
      gxcar(icart,1) = gxcar(icart,1) + scale * (a1 + a4)
      gxcar(icart,2) = gxcar(icart,2) + scale * (a2 + a5)
      gxcar(icart,3) = gxcar(icart,3) + scale * (a3 + a6)
   else if(ityperest(irest).eq.8) then
      d = (xcart(icart,1)-restpars(irest,1))**2 + &
         (xcart(icart,2)-restpars(irest,2))**2 + &
         (xcart(icart,3)-restpars(irest,3))**2 - &
         restpars(irest,4)**2
      if(d.lt.0.d0) then
         gxcar(icart,1) = gxcar(icart,1) + 4.d0 * scale2 * &
            (xcart(icart,1)-restpars(irest,1))*d
         gxcar(icart,2) = gxcar(icart,2) + 4.d0 * scale2 * &
            (xcart(icart,2)-restpars(irest,2))*d
         gxcar(icart,3) = gxcar(icart,3) + 4.d0 * scale2 * &
            (xcart(icart,3)-restpars(irest,3))*d
      end if
   else if(ityperest(irest).eq.9) then
      a1 = xcart(icart,1)-restpars(irest,1)
      b1 = xcart(icart,2)-restpars(irest,2)
      c1 = xcart(icart,3)-restpars(irest,3)
      a2 = restpars(irest,4)**2
      b2 = restpars(irest,5)**2
      c2 = restpars(irest,6)**2
      d = a1**2/a2+b1**2/b2+c1**2/c2-restpars(irest,7)**2
      if(d.lt.0) then
         d = scale2 * d
         gxcar(icart,1) = gxcar(icart,1) + 4.d0*d*a1/a2
         gxcar(icart,2) = gxcar(icart,2) + 4.d0*d*b1/b2
         gxcar(icart,3) = gxcar(icart,3) + 4.d0*d*c1/c2
      end if
   else if(ityperest(irest).eq.10) then
      d = restpars(irest,1)*xcart(icart,1) + &
         restpars(irest,2)*xcart(icart,2) + &
         restpars(irest,3)*xcart(icart,3) - &
         restpars(irest,4)
      if(d.lt.0.d0) then
         d = scale * d
         gxcar(icart,1) = gxcar(icart,1) + 2.d0*restpars(irest,1)*d
         gxcar(icart,2) = gxcar(icart,2) + 2.d0*restpars(irest,2)*d
         gxcar(icart,3) = gxcar(icart,3) + 2.d0*restpars(irest,3)*d
      end if
   else if(ityperest(irest).eq.11) then
      d = restpars(irest,1)*xcart(icart,1) + &
         restpars(irest,2)*xcart(icart,2) + &
         restpars(irest,3)*xcart(icart,3) - &
         restpars(irest,4)
      if(d.gt.0.d0) then
         d = scale * d
         gxcar(icart,1) = gxcar(icart,1) + 2.d0*restpars(irest,1)*d
         gxcar(icart,2) = gxcar(icart,2) + 2.d0*restpars(irest,2)*d
         gxcar(icart,3) = gxcar(icart,3) + 2.d0*restpars(irest,3)*d
      end if
   else if(ityperest(irest).eq.12) then
      rg(1) = 0.0d0
      rg(2) = 0.0d0
      rg(3) = 0.0d0
      a1 = xcart(icart,1) - restpars(irest,1)
      a2 = xcart(icart,2) - restpars(irest,2)
      a3 = xcart(icart,3) - restpars(irest,3)
      vnorm = sqrt(restpars(irest,4)**2 + restpars(irest,5)**2 &
         + restpars(irest,6)**2)
      vv1 = restpars(irest,4)/vnorm
      vv2 = restpars(irest,5)/vnorm
      vv3 = restpars(irest,6)/vnorm
      b1 = vv1 * a1
      b2 = vv2 * a2
      b3 = vv3 * a3
      w = b1 + b2 + b3
      d = (a1 - vv1*w)**2 + (a2 - vv2*w)**2 + (a3 - vv3*w)**2
      rg(1) = scale2 * ( &
         -2*dmax1(-w , 0.d0) * vv1 + &
         2*dmax1(w - restpars(irest,9), 0.d0) * vv1 + &
         2*dmax1(d - restpars(irest,7)**2 , 0.d0) * &
         (2*(a1 - vv1*w)*(1 - vv1**2)+ &
         2*(a2 - vv2*w)*(-vv2*vv1)+ &
         2*(a3 - vv3*w)*(-vv3*vv1) ))
      rg(2) = scale2 * ( &
         -2*dmax1(-w , 0.d0) * vv2 + &
         2*dmax1(w - restpars(irest,9), 0.d0) * vv2 + &
         2*dmax1(d - restpars(irest,7)**2 , 0.d0) * &
         (2*(a1 - vv1*w)*(-vv1*vv2)+ &
         2*(a2 - vv2*w)*(1 - vv2**2)+ &
         2*(a3 - vv3*w)*(-vv3*vv2) ))
      rg(3) = scale2 * ( &
         -2*dmax1(-w , 0.d0) * vv3 + &
         2*dmax1(w - restpars(irest,9), 0.d0) * vv3 + &
         2*dmax1(d - restpars(irest,7)**2 , 0.d0) * &
         (2*(a1 - vv1*w)*(-vv1*vv3)+ &
         2*(a2 - vv2*w)*(-vv2*vv3)+ &
         2*(a3 - vv3*w)*(1 - vv3**2) ))
      gxcar(icart,1) = gxcar(icart,1) + rg(1)
      gxcar(icart,2) = gxcar(icart,2) + rg(2)
      gxcar(icart,3) = gxcar(icart,3) + rg(3)
   else if(ityperest(irest).eq.13) then
      rg(1) = 0.0d0
      rg(2) = 0.0d0
      rg(3) = 0.0d0
      a1 = xcart(icart,1) - restpars(irest,1)
      a2 = xcart(icart,2) - restpars(irest,2)
      a3 = xcart(icart,3) - restpars(irest,3)
      vnorm = sqrt(restpars(irest,4)**2 + restpars(irest,5)**2 &
         + restpars(irest,6)**2)
      vv1 = restpars(irest,4)/vnorm
      vv2 = restpars(irest,5)/vnorm
      vv3 = restpars(irest,6)/vnorm
      b1 = vv1 * a1
      b2 = vv2 * a2
      b3 = vv3 * a3
      w = b1 + b2 + b3
      d = (a1 - vv1*w)**2 + (a2 - vv2*w)**2 + (a3 - vv3*w)**2
      frab = dmin1(-w , 0.d0)**2 * dmin1(w - restpars(irest,9), 0.d0)**2
      frac = dmin1(-w , 0.d0)**2 * dmin1(d - restpars(irest,7)**2 , 0.d0 )**2
      frbc = dmin1(w - restpars(irest,9), 0.d0)**2 * &
         dmin1(d - restpars(irest,7)**2 , 0.d0 )**2
      dfra(1) = -2*dmin1(-w , 0.d0) * vv1
      dfrb(1) = 2*dmin1(w - restpars(irest,9), 0.d0) * vv1
      dfrc(1) = 2*dmin1(d - restpars(irest,7)**2 , 0.d0) * &
         (2*(a1 - vv1*w)*(1 - vv1**2)+ &
         2*(a2 - vv2*w)*(-vv2*vv1)+  &
         2*(a3 - vv3*w)*(-vv3*vv1) )
      dfra(2) = -2*dmin1(-w , 0.d0) * vv2
      dfrb(2) = 2*dmin1(w - restpars(irest,9), 0.d0) * vv2
      dfrc(2) = 2*dmin1(d - restpars(irest,7)**2 , 0.d0) * &
         (2*(a1 - vv1*w)*(-vv1*vv2)+  &
         2*(a2 - vv2*w)*(1 - vv2**2)+ &
         2*(a3 - vv3*w)*(-vv3*vv2) )
      dfra(3) = -2*dmin1(-w , 0.d0) * vv3
      dfrb(3) = 2*dmin1(w - restpars(irest,9), 0.d0) * vv3
      dfrc(3) = 2*dmin1(d - restpars(irest,7)**2 , 0.d0) * &
         (2*(a1 - vv1*w)*(-vv1*vv3)+ &
         2*(a2 - vv2*w)*(-vv2*vv3)+ &
         2*(a3 - vv3*w)*(1 - vv3**2) )
      rg(1) = scale2 * ( dfra(1)*frbc + dfrb(1)*frac + dfrc(1)*frab)
      rg(2) = scale2 * ( dfra(2)*frbc + dfrb(2)*frac + dfrc(2)*frab)
      rg(3) = scale2 * ( dfra(3)*frbc + dfrb(3)*frac + dfrc(3)*frab)
      gxcar(icart,1) = gxcar(icart,1) + rg(1)
      gxcar(icart,2) = gxcar(icart,2) + rg(2)
      gxcar(icart,3) = gxcar(icart,3) + rg(3)

      ! Addition of Gaussian surface on xy plane
      ! Based on eq. of type h*exp(-(x-a)**2/2c**2 -(y-b)**2/2d**2)-(z-g)
   else if(ityperest(irest).eq.14) then
      a1 = -(xcart(icart,1) - restpars(irest,1))**2/(2*restpars(irest,3)**2)
      a2 = -(xcart(icart,2) - restpars(irest,2))**2/(2*restpars(irest,4)**2)
      if(a1+a2<=-50) then
         d = -(xcart(icart,3)-restpars(irest,5))
      else
         d = restpars(irest,6)*exp(a1+a2)-(xcart(icart,3)-restpars(irest,5))
      end if
      if(d.gt.0.d0) then
         d = scale * d
         gxcar(icart,1) = gxcar(icart,1) - 2.d0*d*(xcart(icart,1)-restpars(irest,1)) &
            * (d+(xcart(icart,3)-restpars(irest,5))) / restpars(irest,3)**2
         gxcar(icart,2) = gxcar(icart,2) - 2.d0*d*(xcart(icart,2)-restpars(irest,2)) &
            * (d+(xcart(icart,3)-restpars(irest,5))) / restpars(irest,4)**2
         gxcar(icart,3) = gxcar(icart,3) - 2.d0*d
      end if
   else if(ityperest(irest).eq.15) then
      a1 = -(xcart(icart,1) - restpars(irest,1))**2/(2*restpars(irest,3)**2)
      a2 = -(xcart(icart,2) - restpars(irest,2))**2/(2*restpars(irest,4)**2)
      if(a1+a2<=-50) then
         d = -(xcart(icart,3)-restpars(irest,5))
      else
         d = restpars(irest,6)*exp(a1+a2)-(xcart(icart,3)-restpars(irest,5))
      end if
      if(d.lt.0.d0) then
         d = scale * d
         gxcar(icart,1) = gxcar(icart,1) - 2.d0*d*(xcart(icart,1)-restpars(irest,1)) &
            * (d+(xcart(icart,3)-restpars(irest,5))) / restpars(irest,3)**2
         gxcar(icart,2) = gxcar(icart,2) - 2.d0*d*(xcart(icart,2)-restpars(irest,2)) &
            * (d+(xcart(icart,3)-restpars(irest,5))) / restpars(irest,4)**2
         gxcar(icart,3) = gxcar(icart,3) - 2.d0*d
      end if
   end if

   return
end subroutine gwalls


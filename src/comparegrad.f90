!
!  Written by Leandro Martínez, 2009-2011.
!  Copyright (c) 2009-2018, Leandro Martínez, Jose Mario Martinez,
!  Ernesto G. Birgin.
!

!
! Subroutine that performs finite difference and analytical gradient
! comparision. Used only for test purpouses
!

subroutine comparegrad(n,x)

   use sizes
   implicit none

   integer :: n, i, iworst
   double precision :: x(n), fx, step, gcomp, gbest, eworst, &
      error, steperror, stepbest
   double precision, allocatable :: g(:)
   real :: time0, tarray(2), etime

   ! Allocate local array

   allocate(g(nn))

   write(*,*)
   write(*,*) ' Comparing analytical and finite-difference '
   write(*,*) ' gradients... may take a while. '
   write(*,*)
   write(*,*) ' Five first center of masses and angles of tested point: '
   do i = 1, 15, 3
      write(*,"( i4,6(tr2,f8.3) )") (i+2)/3, x(i), x(i+1), x(i+2), x(n/2+i),&
         x(n/2+i+1),x(n/2+i+2)
   end do
   write(*,*)
   write(*,*) ' Computing gradient ... '

   call computef(n,x,fx)
   write(*,*) ' Function value on test point: ', fx
   open(98, file = 'chkgrad.log',status='unknown')
   write(98, *)'Function Value = ', fx
   call computeg(n,x,g)

   write(98,"( t2,'Component',t16,'Analytical',t33,'Discrete', &
   &t51,'Error',t62,'Best step' )")
   time0 = etime(tarray)
   eworst = 0.d0
   do i = 1, n
      if(etime(tarray)-time0.gt.10.) then
         time0 = etime(tarray)
         write(*,*) ' Computing the ',i,'th of ',n,' components. Worst error: ', eworst
      end if
      error = 1.d20
      step = 1.d-2
      do while(error.gt.1.d-6.and.step.ge.1.d-20)
         call discret(i,n,x,gcomp,step)
         if(dmin1(abs(g(i)),abs(gcomp)).gt.1.d-10) then
            steperror = abs( ( gcomp - g(i) ) / g(i) )
         else
            steperror = abs( gcomp - g(i) )
         end if
         if( steperror .lt. error ) then
            error = steperror
            gbest = gcomp
            stepbest = step
         end if
         step = step / 10.d0
      end do
      write(98,"(i10,5(tr2,d13.7))") i, g(i), gbest, error, stepbest
      if(error.gt.eworst) then
         iworst = i
         eworst = error
      end if
   end do
   write(98,*) 'Maximum difference = ', iworst,' Error= ', eworst
   write(*,*) ' Done. '
   stop

end subroutine comparegrad

subroutine discret(icomp,n,x,gcomp,step)

   implicit none
   integer :: n, icomp
   double precision :: save, step, x(n), fplus, fminus, gcomp

   save = x(icomp)
   x(icomp) = save + step
   call computef(n,x,fplus)
   x(icomp) = save - step
   call computef(n,x,fminus)
   gcomp = (fplus - fminus) / (2.d0 * step)
   x(icomp) = save

   return
end subroutine discret


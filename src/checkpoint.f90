!
!  Written by Leandro Martínez, 2009-2011.
!  Copyright (c) 2009-2018, Leandro Martínez, Jose Mario Martinez,
!  Ernesto G. Birgin.
!

!
! Subroutine that writes the last point obtained when
! a solution was not found
!

subroutine checkpoint(n,x)

   use sizes
   use compute_data
   use input
   use usegencan
   use ahestetic

   implicit none
   integer :: i
   integer :: n
   double precision :: x(n)
   double precision :: fx
   logical :: movebadprint
   character(len=strl) :: xyzout_forced

   ! All molecules are important

   do i = 1, ntype_with_fixed
      comptype(i) = .true.
   end do

   ! Call the subroutine that computes de function value

   call computef(n,x,fx)

   write(*,dash3_line)
   write(*,"(&
   &' Packmol was not able to find a solution to your',/,&
   &' packing problem with the desired distance tolerance.',/,/,&
   &' First of all, be sure if the molecules fit in the',/,&
   &' regions specified and if the constraints were set',/,&
   &' correctly. ',/,/,&
   &' Secondly, try simply running it again with a different ',/,&
   &' seed for the random number generator of the initial ',/,&
   &' point. This is done by adding the keyword seed to the',/,&
   &' input file, as in: ',/,/,&
   &' seed 192911 ',/,/,&
   &' The best configuration found has a function value of',/,&
   &' f = ', e14.7,/,/,&
   &' IMPORTANT: ',/,&
   &' If the number of molecules and the restraints are',/,&
   &' correct, it is still very likely that the current point',/,&
   &' fits your needs if your purpose is to run a MD',/,&
   &' simulation.',/,&
   &' Therefore, we recommend to minimize the energy of the',/,&
   &' solution found, equilibrate it and run with it as well.',/&
   &)") fx
   write(*,dash3_line)

   call output(n,x,xyzout)

   write(*,*) ' The solution with the best function value was '
   write(*,*) ' written to the output file: ', trim(adjustl(xyzout))
   if ( crd ) write(*,*) ' ... and to CRD file: ', trim(adjustl(crdfile))
   write(*,dash1_line)
   write(*,*) ' Forcing the solution to fit the constraints...'

   ! CALL GENCAN

   init1 = .true.
   do i = 1, nloop
      iprint1 = 0
      iprint2 = 0
      call pgencan(n,x,fx)
      movebadprint = .false.
      call movebad(n,x,fx,movebadprint)
   end do
   init1 = .false.

   write(*,*)
   write(*,dash1_line)
   xyzout_forced = trim(adjustl(xyzout))//'_FORCED'
   call output(n,x,xyzout_forced)

   write(*,*) ' The forced point was writen to the '
   write(*,*) ' output file: ', trim(adjustl(xyzout_forced))
   if ( crd ) write(*,*) ' ... and to CRD file: ', trim(adjustl(crdfile))
   write(*,*)
   write(*,*) ' If you want that the packing procedure continues'
   write(*,*) ' for a longer time, add the following keyword '
   write(*,*) ' to the input file: '
   write(*,*)
   write(*,*) ' nloop [integer]      (ex: nloop 200) '
   write(*,*)
   write(*,*) ' The default nloop value is 50 for each molecule.'
   write(*,*)

   write(*,hash1_line)
   write(*,*) ' ENDED WITHOUT PERFECT PACKING: '
   write(*,*) ' The output file:'
   write(*,*)
   write(*,*) '   ', trim(adjustl(xyzout))
   if ( crd ) write(*,*) ' (... and to CRD file: ', trim(adjustl(crdfile)), ')'
   write(*,*)
   write(*,*) ' contains the best solution found. '
   write(*,*)
   write(*,*) ' Very likely, if the input data was correct, '
   write(*,*) ' it is a reasonable starting configuration.'
   write(*,*) ' Check commentaries above for more details. '
   write(*,hash1_line)

   return
end subroutine checkpoint


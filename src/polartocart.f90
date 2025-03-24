!
!  Written by Leandro Martínez, 2009-2011.
!  Copyright (c) 2009-2018, Leandro Martínez, Jose Mario Martinez,
!  Ernesto G. Birgin.
!
! Subroutine eulerrmat: Computes the rotation matrix from the
!                       Euler angles
!
! Note that:
! In this routine, beta is a rotation about the y-axis
!                  gama is a rotation about the z-axis
!                  teta is a rotation about the x-axis

subroutine eulerrmat(beta,gama,teta,v1,v2,v3)

   implicit none
   double precision :: beta, gama, teta
   double precision :: cb, sb, cg, sg, ct, st
   double precision :: v1(3), v2(3), v3(3)

   cb = dcos(beta)
   sb = dsin(beta)
   cg = dcos(gama)
   sg = dsin(gama)
   ct = dcos(teta)
   st = dsin(teta)

   v1(1)=-sb * sg * ct + cb * cg
   v1(2)=-sb * cg * ct - cb * sg
   v1(3)= sb * st

   v2(1)= cb * sg * ct + sb * cg
   v2(2)= cb * cg * ct - sb * sg
   v2(3)=-cb * st

   v3(1)= sg * st
   v3(2)= cg * st
   v3(3)= ct

   return
end subroutine eulerrmat

!
! Subroutine compcart: Compute cartesian coordinates using
!                      the center of mass, the canonical coordinates
!                      and the rotation matrix
!

subroutine compcart(x,xcm,xref,v1,v2,v3)
   implicit none
   double precision, intent(inout) :: x(3)
   double precision, intent(in) :: xcm(3), xref(3)
   double precision, intent(in) :: v1(3), v2(3), v3(3)
   x(1) = xcm(1) + xref(1)*v1(1) + xref(2)*v2(1) + xref(3)*v3(1)
   x(2) = xcm(2) + xref(1)*v1(2) + xref(2)*v2(2) + xref(3)*v3(2)
   x(3) = xcm(3) + xref(1)*v1(3) + xref(2)*v2(3) + xref(3)*v3(3)
   return
end subroutine compcart

!
! Subroutine eulerfixed: This routine was added because it defines
!                        the rotation in the "human" way, an is thus used
!                        to set the position of the fixed molecules.
!     That means: beta is a counterclockwise rotation around x axis.
!                 gama is a counterclockwise rotation around y axis.
!                 teta is a counterclockwise rotation around z axis.
!     The other routine should better do this as well, but then we need to change
!     all the derivative calculations, just for the sake of human interpretation
!     of the rotation which, in that case, is not really important. Maybe some day.
!

subroutine eulerfixed(beta,gama,teta,v1,v2,v3)

   implicit none
   double precision :: beta, gama, teta
   double precision :: c1, s1, c2, s2, c3, s3
   double precision :: v1(3), v2(3), v3(3)

   c1 = dcos(beta)
   s1 = dsin(beta)
   c2 = dcos(gama)
   s2 = dsin(gama)
   c3 = dcos(teta)
   s3 = dsin(teta)

   v1(1) = c2*c3
   v1(2) = c1*s3 + c3*s1*s2
   v1(3) = s1*s3 - c1*c3*s2

   v2(1) = -c2*s3
   v2(2) = c1*c3 - s1*s2*s3
   v2(3) = c1*s2*s3 + c3*s1

   v3(1) = s2
   v3(2) = -c2*s1
   v3(3) = c1*c2

   return
end subroutine eulerfixed


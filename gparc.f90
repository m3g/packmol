!  
!  Written by Leandro Martínez, 2009-2011.
!  Copyright (c) 2009-2011, Leandro Martínez, Jose Mario Martinez,
!  Ernesto G. Birgin.
!  
! Compute gradient relative to atom-to-atom distances
!

subroutine gparc(icart,firstjcart)

  use sizes
  use compute_data
  implicit none

  ! SCALAR ARGUMENTS
  integer :: icart,firstjcart

  ! LOCAL SCALARS
  integer :: jcart
  double precision :: a1,a2,a3,datom,dtemp,xdiff,tol

  jcart = firstjcart
  do while ( jcart .ne. 0 )
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
    a1 = xcart(icart, 1)-xcart(jcart, 1) 
    a1 = a1 * a1
    if(a1.lt.tol) then
      a2 = xcart(icart, 2)-xcart(jcart, 2) 
      a2 = a1 + a2 * a2
      if(a2.lt.tol) then
        a3 = xcart(icart, 3)-xcart(jcart, 3)
        datom = a2 + a3 * a3 
        if(datom.lt.tol) then 
          dtemp = 4.d0 * (datom - tol)
          xdiff = dtemp*(xcart(icart,1) - xcart(jcart,1)) 
          gxcar(icart,1)= gxcar(icart,1) + xdiff
          gxcar(jcart,1)= gxcar(jcart,1) - xdiff 
          xdiff = dtemp*(xcart(icart,2) - xcart(jcart,2)) 
          gxcar(icart,2)= gxcar(icart,2) + xdiff
          gxcar(jcart,2)= gxcar(jcart,2) - xdiff 
          xdiff = dtemp*(xcart(icart,3) - xcart(jcart,3)) 
          gxcar(icart,3)= gxcar(icart,3) + xdiff
          gxcar(jcart,3)= gxcar(jcart,3) - xdiff 
        end if
      end if
    end if 
    jcart = latomnext(jcart)
  end do
  return
end subroutine gparc


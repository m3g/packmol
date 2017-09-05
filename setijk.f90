!  
!  Written by Leandro Martínez, 2009-2011.
!  Copyright (c) 2009-2011, Leandro Martínez, Jose Mario Martinez,
!  Ernesto G. Birgin.
!  
! Subroutines that set the indexes of a three-dimensional array
! given the undimensional counter of the vector (for an array
! with dimensions (0:nboxes(1)+1,0:nboxes(2)+1,0:nboxes(3)+1), and
! vice-versa.
!

subroutine ibox_to_ijk(ibox,i,j,k)

  use compute_data, only : nb2
  implicit none
  integer :: ibox, i, j, k, iibox

  k = mod(ibox,nb2(3))
  if ( k == 0 ) k = nb2(3)

  iibox = ibox - k 
  iibox = iibox / nb2(3) + 1 
  j = mod(iibox,nb2(2))
  if ( j == 0 ) j = nb2(2)

  iibox = iibox - j
  iibox = iibox / nb2(2) + 1
  i = mod(iibox,nb2(1))
  if ( i == 0 ) i = nb2(1)

  k = k - 1
  j = j - 1
  i = i - 1

end subroutine ibox_to_ijk

subroutine ijk_to_ibox(i,j,k,ibox)

  use compute_data, only : nb2
  implicit none
  integer :: i, j, k, ibox

  ibox = i*nb2(2)*nb2(3) + j*nb2(3) + k + 1

end subroutine ijk_to_ibox


!  
!  Written by Yi Yao (yaoyi92@gmail.com)
!

module pbc
 
  double precision, public :: pbc_box(3) 
  logical, public :: is_pbc = .false.

  public pbc_vector, pbc_idx_box

  contains

  subroutine pbc_vector(v)

      implicit none

      integer icoord

      double precision, intent(inout) :: v(3)

      do icoord = 1, 3
          v(icoord) = v(icoord) - pbc_box(icoord) * nint(v(icoord) / pbc_box(icoord))
      end do

  end subroutine pbc_vector

  integer function pbc_idx_box(ibox, nbox)

      implicit none
      integer ibox, nbox

      pbc_idx_box = modulo(ibox - 1 + nbox, nbox) + 1

  end function pbc_idx_box

end module pbc


!  
!  Written by Yi Yao (yaoyi92@gmail.com)
!

module pbc
 
  double precision, public :: pbc_box(3) 
  logical, public :: using_pbc = .false.

  public delta_vector, idx_box

  contains

  elemental double precision function delta_vector(v1,v2,pbc_box)
      implicit none
      double precision, intent(in) :: v1, v2, pbc_box
      delta_vector = v1 - v2
      if (using_pbc) then
        delta_vector = delta_vector - pbc_box * nint(delta_vector/pbc_box)
      end if
 end function delta_vector

  integer function idx_box(ibox, nbox)
      implicit none
      integer ibox, nbox
      if (using_pbc) then
        idx_box = modulo(ibox - 1 + nbox, nbox) + 1
      else
        idx_box = ibox
      end if
  end function idx_box

end module pbc


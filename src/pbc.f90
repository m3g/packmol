!
!  Written by Yi Yao (yaoyi92@gmail.com)
!

module pbc

   implicit none

   logical, public :: using_pbc = .false.
   double precision, public :: pbc_min(3), pbc_max(3), pbc_length(3)
   public v_in_box, delta_vector, cell_ind

contains

   elemental double precision function v_in_box(v, pbc_min, pbc_length)
      double precision, intent(in) :: v, pbc_min, pbc_length
      v_in_box = pbc_min + modulo((v - pbc_min), pbc_length)
   end function v_in_box

   elemental double precision function delta_vector(v1,v2, pbc_length)
      double precision, intent(in) :: v1, v2, pbc_length
      delta_vector = v1 - v2
      delta_vector =  delta_vector - pbc_length * nint(delta_vector/pbc_length)
   end function delta_vector

   integer function cell_ind(icell, ncells)
      integer :: icell, ncells
      cell_ind = modulo(icell - 1 + ncells, ncells) + 1
   end function cell_ind

end module pbc


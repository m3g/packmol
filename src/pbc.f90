!
!  Written by Yi Yao (yaoyi92@gmail.com)
!

module pbc

   implicit none

   double precision, public :: pbc_box(6), pbc_sides(3)
   logical, public :: using_pbc = .false.

   public delta_vector, cell_ind

contains

   elemental double precision function delta_vector(v1,v2,system_length)
      double precision, intent(in) :: v1, v2, system_length
      delta_vector = v1 - v2
      delta_vector =  delta_vector - system_length * nint(delta_vector/system_length)
   end function delta_vector

   integer function cell_ind(icell, ncells)
      integer :: icell, ncells
      cell_ind = modulo(icell - 1 + ncells, ncells) + 1
   end function cell_ind

end module pbc


!
!  Written by Yi Yao (yaoyi92@gmail.com)
!

module pbc

   implicit none

   double precision, public :: pbc_box(6), pbc_sides(3)
   logical, public :: using_pbc = .false.
   public v_in_box, delta_vector, cell_ind

contains

   elemental double precision function v_in_box(v,sizemin,system_length)
      double precision, intent(in) :: v, system_length, sizemin
      v_in_box = sizemin + modulo((v - sizemin), system_length)
   end function v_in_box

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


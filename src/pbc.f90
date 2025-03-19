!
!  Written by Yi Yao (yaoyi92@gmail.com)
!

module pbc

   implicit none

   double precision, public :: pbc_box(6), pbc_sides(3)
   logical, public :: using_pbc = .false.

   public delta_vector, idx_box

contains

   elemental double precision function delta_vector(v1,v2,using_pbc,pbc_sides)
      logical, intent(in) :: using_pbc
      double precision, intent(in) :: v1, v2, pbc_sides
      delta_vector = v1 - v2
      if (using_pbc) then
         delta_vector = mod(delta_vector, pbc_sides)
         if (delta_vector >= pbc_sides / 2) then
            delta_vector = pbc_sides - delta_vector
         end if
      end if
   end function delta_vector

   integer function idx_box(ibox, nbox)
      integer :: ibox, nbox
      if (using_pbc) then
         idx_box = modulo(ibox - 1 + nbox, nbox) + 1
      else
         idx_box = ibox
      end if
   end function idx_box

end module pbc


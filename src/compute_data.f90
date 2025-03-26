!
!  Written by Leandro Martínez, 2009-2011.
!  Copyright (c) 2009-2018, Leandro Martínez, Jose Mario Martinez,
!  Ernesto G. Birgin.
!
module compute_data

   use sizes

   integer :: ntotmol, ntype, nfixedat, ntotat
   integer :: ncells(3)

   integer, allocatable :: nmols(:) ! (ntype)
   integer, allocatable :: natoms(:) ! (ntype)
   integer, allocatable :: idfirst(:) ! (ntype)
   integer, allocatable :: nratom(:) ! (ntotat)
   integer, allocatable :: iratom(:,:) ! (ntotat,mrperatom)
   integer, allocatable :: ityperest(:) ! (maxrest)
   integer, allocatable :: ibmol(:) ! (ntotat)
   integer, allocatable :: ibtype(:) ! (ntotat)

   double precision :: scale, scale2
   double precision :: fdist, frest
   double precision :: sizemin(3), sizemax(3)
   double precision :: cell_length(3), system_length(3)
   double precision :: radmax

   double precision, allocatable :: xcart(:,:) ! (ntotat,3)
   double precision, allocatable :: coor(:,:) ! (ntotat,3)
   double precision, allocatable :: restpars(:,:) ! (maxrest,9)
   double precision, allocatable :: rot_bound(:,:,:) ! (ntype,3,2)
   double precision, allocatable :: radius(:), radius_ini(:), fscale(:) ! (ntotat)
   double precision, allocatable :: short_radius(:), short_radius_scale(:) ! ntotat
   double precision, allocatable :: gxcar(:,:) ! (ntotat,3)

   double precision, allocatable :: fdist_atom(:), frest_atom(:) ! (ntotat)
   double precision, allocatable :: dmax(:) ! (ntype)

   logical, allocatable :: constrain_rot(:,:) ! (ntype,3)
   logical, allocatable :: comptype(:) ! (ntype)
   logical, allocatable :: fixedatom(:) ! (ntotat)
   logical, allocatable :: use_short_radius(:) ! ntotat
   logical :: init1, move

   ! For linked lists
   integer, allocatable :: latomnext(:) ! (ntotat)
   integer, allocatable :: latomfirst(:,:,:) !  (ncells(1),ncells(2),ncells3))
   integer, allocatable :: latomfix(:,:,:) ! (ncells(1),ncells(2),ncells(3))

   ! For movebad
   double precision, allocatable :: fmol(:), radiuswork(:) ! (ntotat)

   ! For restmol
   double precision, allocatable :: xmol(:) ! (nn)
   logical, allocatable :: compsafe(:) ! (ntype)

   ! For cells with atoms linked lists
   integer :: lcellfirst
   integer, allocatable :: lcellnext(:) ! (ncells(1)*ncells(2)*ncells(3))
   logical, allocatable :: empty_cell(:,:,:) ! (ncells(1),ncells(2),ncells(3))

end module compute_data

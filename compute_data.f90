!  
!  Written by Leandro Martínez, 2009-2011.
!  Copyright (c) 2009-2011, Leandro Martínez, Jose Mario Martinez,
!  Ernesto G. Birgin.
!
module compute_data

  use sizes

  integer :: ntotmol, ntype, natfix, ntotat
  integer :: nboxes(3), nb2(3)

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
  double precision :: boxl(3)

  double precision, allocatable :: xcart(:,:) ! (ntotat,3)
  double precision, allocatable :: coor(:,:) ! (ntotat,3)
  double precision, allocatable :: restpars(:,:) ! (maxrest,9)
  double precision, allocatable :: rot_bound(:,:,:) ! (ntype,3,2)
  double precision, allocatable :: radius(:), radius_ini(:) ! (ntotat)
  double precision, allocatable :: gxcar(:,:) ! (ntotat,3)
  
  double precision, allocatable :: fdist_atom(:), frest_atom(:) ! (ntotat)
  double precision, allocatable :: dmax(:) ! (ntype)
  double precision, allocatable :: cmxmin(:), cmymin(:), cmzmin(:) ! (ntype)
  double precision, allocatable :: cmxmax(:), cmymax(:), cmzmax(:) ! (ntype)

  logical, allocatable :: constrain_rot(:,:) ! (ntype,3)
  logical, allocatable :: comptype(:) ! (ntype)
  logical, allocatable :: fixedatom(:) ! (ntotat)
  logical :: init1, move

  ! For linked lists
  integer, allocatable :: latomnext(:) ! (ntotat)
  integer, allocatable :: latomfirst(:,:,:) !  (0:nbp+1,0:nbp+1,0:nbp+1)
  integer, allocatable :: latomfix(:,:,:) ! (0:nbp+1,0:nbp+1,0:nbp+1)
 
  ! For movebad
  double precision, allocatable :: fmol(:), radiuswork(:) ! (ntotat)

  ! For restmol
  double precision, allocatable :: xmol(:) ! (nn)
  logical, allocatable :: compsafe(:) ! (ntype)

  ! For boxes with atoms linked lists
  integer :: lboxfirst
  integer, allocatable :: lboxnext(:) ! ((nbp+2)**3)
  logical, allocatable :: hasfree(:,:,:) ! (0:nbp+1,0:nbp+1,0:nbp+1) 

end module compute_data

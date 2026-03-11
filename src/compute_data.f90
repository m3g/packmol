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
   integer, allocatable :: ibmol_hot(:) ! packed stream mirror for hot paths
   integer, allocatable :: ibtype_hot(:) ! packed stream mirror for hot paths
   integer, allocatable :: fixed_short_marker(:) ! 0: movable/no-short, 1: movable/short, 2: fixed/no-short, 3: fixed/short

   double precision :: scale, scale2
   double precision :: fdist, frest
   double precision :: sizemin(3), sizemax(3)
   double precision :: cell_length(3), system_length(3)
   double precision :: radmax

   double precision, allocatable :: xcart(:,:) ! (ntotat,3)
   double precision, allocatable :: x_hot(:), y_hot(:), z_hot(:) ! hot scalar streams for xcart
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
   double precision, allocatable :: cell_max_radius(:,:,:) ! (ncells(1),ncells(2),ncells(3))
   double precision, allocatable :: cell_max_short_radius(:,:,:) ! (ncells(1),ncells(2),ncells(3))

contains

   subroutine refresh_hot_buffers_full()
      implicit none

      if ( .not. allocated(x_hot) ) return
      x_hot(:) = xcart(:,1)
      y_hot(:) = xcart(:,2)
      z_hot(:) = xcart(:,3)
      ibtype_hot(:) = ibtype(:)
      ibmol_hot(:) = ibmol(:)
      fixed_short_marker(:) = merge(2, 0, fixedatom(:)) + merge(1, 0, use_short_radius(:))
   end subroutine refresh_hot_buffers_full

   subroutine refresh_hot_buffers_atom(icart)
      implicit none
      integer, intent(in) :: icart

      if ( .not. allocated(x_hot) ) return
      x_hot(icart) = xcart(icart,1)
      y_hot(icart) = xcart(icart,2)
      z_hot(icart) = xcart(icart,3)
      ibtype_hot(icart) = ibtype(icart)
      ibmol_hot(icart) = ibmol(icart)
      fixed_short_marker(icart) = merge(2, 0, fixedatom(icart)) + merge(1, 0, use_short_radius(icart))
   end subroutine refresh_hot_buffers_atom

end module compute_data

!
!  Written by Leandro Martínez, 2009-2011.
!  Copyright (c) 2009-2018, Leandro Martínez, Jose Mario Martinez,
!  Ernesto G. Birgin.
!
! Module that carries the input parameters read from the input file
!

module input

   use sizes
   implicit none

   integer :: nlines
   integer :: nrest
   integer :: seed
   integer :: nloop, nloop_all
   integer :: writeout
   integer :: ntfix
   integer :: ntcon(9)

   integer, allocatable :: nconnect(:,:)  ! (ntotat,9)
   integer, allocatable :: irestline(:) ! (maxrest)
   integer, allocatable :: linestrut(:,:) ! (ntype,2)
   integer, allocatable :: resnumbers(:) ! (ntype)
   integer, allocatable :: maxcon(:) ! (ntotat)
   integer, allocatable :: input_itype(:) ! (ntype)
   integer, allocatable :: nloop_type(:) ! (ntype)
   integer, allocatable :: nloop0_type(:) ! (ntype)
   integer, allocatable :: maxmove(:) ! (ntype)

   double precision :: dism
   double precision :: precison
   double precision :: sidemax
   double precision :: discale
   double precision :: movefrac
   double precision :: add_sides_fix
   double precision :: precision
   double precision :: fbins
   double precision :: short_tol_dist
   double precision :: short_tol_scale

   double precision, allocatable :: amass(:) ! (ntotat)
   double precision, allocatable :: charge(:) ! (ntotat)

   logical :: writebad
   logical :: tinker
   logical :: pdb
   logical :: crd
   logical :: xyz
   logical :: towhee
   logical :: moldy
   logical :: check
   logical :: chkgrad
   logical :: randini
   logical :: movebadrandom
   logical :: add_amber_ter, amber_ter_preserve
   logical :: add_box_sides
   logical :: fix
   logical :: avoidoverlap
   logical :: packall
   logical :: use_short_tol

   logical, allocatable :: changechains(:) ! (ntype)
   logical, allocatable :: fixedoninput(:) ! (ntype)
   logical, allocatable :: connect(:) ! (ntype)

   character(len=1), parameter :: forbidden_char = '~'
   character(len=strl) :: xyzout
   character(len=strl) :: crdfile

   character(len=1), allocatable :: chain(:) ! (ntype)
   character(len=3), allocatable :: ele(:) ! (ntotat)
   character(len=8), allocatable :: segid(:) ! (segment identifier)
   character(len=strl), allocatable :: pdbfile(:) ! (ntype)
   character(len=strl), allocatable :: name(:) ! (ntype)
   character(len=strl), allocatable :: keyword(:,:) ! (nlines,maxkeywords)
   character(len=strl), allocatable :: inputfile(:) ! (nlines)
   character(len=strl), allocatable :: restart_from(:), restart_to(:) ! (0:ntype)

end module input

!  
!  Written by Leandro Martínez, 2009-2011.
!  Copyright (c) 2009-2018, Leandro Martínez, Jose Mario Martinez,
!  Ernesto G. Birgin.
!  
!-----------------------------------------------------------------------------
!
! http://m3g.iqm.unicamp.br/packmol
!
! Usage (see the page above for further information):
!
! ./packmol < inputfile.inp
!
! References:
!
! L. Martinez, R. Andrade, E. G. Birgin, J. M. Martinez,
! PACKMOL: A package for building initial configurations for
! molecular dynamics simulations, J. Comp. Chem. 30:2157-2164, 2009.
!
! J. M. Martinez and L. Martinez, 
! Packing optimization for the automated generation of complex
! system's initial configurations for molcular dynamics and
! docking. J. Comp. Chem. 24:819-825, 2003.
!
! This version of Packmol uses the optimization method GENCAN which
! is a part of the TANGO (Trustable Algorithms for Nonlinear General
! Optimization) project.
! Reference:
! E. G. Birgin, J. M. Martinez, Comp. Opt. Appl. 23:101-125, 2002.
! http://www.ime.usp.br/~egbirgin/tango
!
!

program packmol

  use exit_codes
  use sizes
  use compute_data
  use input
  use usegencan
  use flashsort
  use swaptypemod
  use ahestetic
  use pbc
  implicit none

  integer :: itype, irest, idatom, iatom
  integer :: idtemp, nmtemp, natemp, input_itypetemp
  integer :: linesttmp1, linesttmp2, jtype
  integer :: ntmol, n, iftype, icart, imol, iicart, iline_atoms
  integer :: i, iline, iiatom, iat, iirest, iratcount, ival
  integer :: loop
  integer :: resntemp, nloop_tmp
  integer :: ioerr
  integer :: maxmove_tmp
  integer :: exit_code = 0
      
  double precision, allocatable :: x(:), xprint(:) ! (nn)
  double precision :: v1(3),v2(3),v3(3)
  double precision :: radscale, value
  double precision :: xcm(3), beta, gama, teta
  double precision :: xtemp(3)
  double precision :: fx, bestf, flast, fprint, all_type_fx
  double precision :: fimp, fimprov
  double precision, parameter :: pi=4.d0*datan(1.d0)

  real :: etime, tarray(2), time0
  
  character(len=strl) :: record, restart_from_temp, restart_to_temp
  character(len=strl) :: xyzfile
  character(len=1) :: chain_tmp

  logical :: fixtmp
  logical :: rests
  logical :: movebadprint
  logical :: changechains_tmp, connecttmp

  logical, allocatable :: fixed(:) ! ntype

  ! Printing title

  call title()
      
  ! Set dimensions of all arrays

  call setsizes()

  ! Allocate local array

  allocate(fixed(ntype),x(nn),xprint(nn),xfull(nn))

  ! Start time computation

  time0 = etime(tarray)

  ! Reading input file

  call getinp()

  ! Put molecules in their center of mass

  call cenmass()
 
  ! Writting some input data
     
  write(*,*) ' Total number of atoms: ', ntotat

  ! Put fixed molecules in the specified position

  fixed(1:ntype) = .false.
  do irest = 1, nrest
    if(ityperest(irest).eq.1) then
      do itype = 1, ntype
        if(irestline(irest).gt.linestrut(itype,1).and.&
           irestline(irest).lt.linestrut(itype,2)) then
          xcm = restpars(irest,1:3)
          beta = restpars(irest,4) 
          gama = restpars(irest,5) 
          teta = restpars(irest,6) 

          ! Compute rotation matrix from euler angles

          call eulerfixed(beta,gama,teta,v1,v2,v3)                 

          idatom = idfirst(itype) - 1
          do iatom = 1, natoms(itype)
            idatom = idatom + 1
            xtemp(1) = coor(idatom,1)*v1(1) &
                     + coor(idatom,2)*v2(1) &
                     + coor(idatom,3)*v3(1) 
            xtemp(2) = coor(idatom,1)*v1(2) &
                     + coor(idatom,2)*v2(2) &
                     + coor(idatom,3)*v3(2) 
            xtemp(3) = coor(idatom,1)*v1(3) &
                     + coor(idatom,2)*v2(3) &
                     + coor(idatom,3)*v3(3) 
            coor(idatom, 1:3) = xtemp + xcm
          end do
          record = name(itype)
          write(*,*) ' Molecule ',trim(adjustl(record)),'(',itype,') will be fixed.' 
          fixed(itype) = .true.
          if(nmols(itype).gt.1) then
            write(*,*)' ERROR: Cannot set number > 1',' for fixed molecules. '
            write(*,*) '       Structure: ', itype,': ', trim(adjustl(record))
            stop exit_code_input_error
          end if
          if ( restart_from(itype) /= 'none' .or. &
               restart_to(itype) /= 'none' ) then
            write(*,*) ' ERROR: Restart files cannot be used for fixed molecules. '
            write(*,*) '        Structure: ', itype,': ', trim(adjustl(record))
            stop exit_code_input_error
          end if
        end if
      end do
    end if
  end do 

  ! Reseting parameters for removing the fixed molecules
  ! fix is the logical variable that informs that there are fixed molecules

  fix = .false.
  ntemp = 0
  do itype = 1, ntype

    ! input_itype and fixedoninput vectors are used only to preserve the
    ! order of input in the output files

    input_itype(itype) = itype
    if(fixed(itype)) then
      fix = .true.
      fixedoninput(itype) = .true.
    else
      ntemp = ntemp + 1
      fixedoninput(itype) = .false.
    end if
  end do
  ntype_with_fixed = ntype
  ntype = ntemp     

  do i = 1, ntype_with_fixed - ntype 
    do itype = 1, ntype_with_fixed - 1
      if(fixed(itype)) then
        record = name(itype)
        restart_to_temp = restart_to(itype)
        restart_from_temp = restart_from(itype)
        fixtmp = fixed(itype)
        idtemp = idfirst(itype)
        input_itypetemp = input_itype(itype)
        nmtemp = nmols(itype)
        natemp = natoms(itype)
        resntemp = resnumbers(itype)
        connecttmp = connect(itype)
        if(pdb) xyzfile = pdbfile(itype)
        linesttmp1 = linestrut(itype,1)
        linesttmp2 = linestrut(itype,2)
        changechains_tmp = changechains(itype)
        maxmove_tmp = maxmove(itype)
        chain_tmp = chain(itype)
        nloop_tmp = nloop_type(itype)
        jtype = itype + 1
        if(.not.fixed(jtype)) then
          name(itype) = name(jtype)
          name(jtype) = record(1:10)
          restart_to(itype) = restart_to(jtype)
          restart_to(jtype) = restart_to_temp
          restart_from(itype) = restart_from(jtype)
          restart_from(jtype) = restart_from_temp
          idfirst(itype) = idfirst(jtype)
          idfirst(jtype) = idtemp
          input_itype(itype) = input_itype(jtype)
          input_itype(jtype) = input_itypetemp
          fixed(itype) = fixed(jtype)
          fixed(jtype) = fixtmp
          nmols(itype) = nmols(jtype)
          nmols(jtype) = nmtemp
          natoms(itype) = natoms(jtype)
          natoms(jtype) = natemp
          resnumbers(itype) = resnumbers(jtype)
          resnumbers(jtype) = resntemp
          connect(itype) = connect(jtype)
          connect(jtype) = connecttmp
          changechains(itype) = changechains(jtype)
          changechains(jtype) = changechains_tmp
          maxmove(itype) = maxmove(jtype)
          maxmove(jtype) = maxmove_tmp
          chain(itype) = chain(jtype)
          chain(jtype) = chain_tmp
          nloop_type(itype) = nloop_type(jtype)
          nloop_type(jtype) = nloop_tmp
          if(pdb) then
            pdbfile(itype) = pdbfile(jtype) 
            pdbfile(jtype) = xyzfile
          end if
          linestrut(itype,1) = linestrut(jtype,1)
          linestrut(itype,2) = linestrut(jtype,2)
          linestrut(jtype,1) = linesttmp1
          linestrut(jtype,2) = linesttmp2
        end if
      end if
    end do
  end do

  ! Computing the number of variables
  !
  ! ntype: 1...ntype (counter for the number of free structures)
  !
  ! ntype_with_fixed: 1...ntype...ntype_with_fixed (counter for the total number of structures)
  !

  ntmol = 0
  do itype = 1, ntype_with_fixed
    ntmol = ntmol + nmols(itype)
  end do
  ntotmol = 0 
  do itype = 1, ntype 
    ntotmol = ntotmol + nmols(itype)       
  end do     
  n = ntotmol * 6
  write(*,*) ' Total number of molecules: ', ntmol
  write(*,*) ' Number of fixed molecules: ', ntmol - ntotmol
  write(*,*) ' Number of free molecules: ', ntotmol
  write(*,*) ' Number of variables: ', n 

  ! Computing the total number of fixed atoms

  nfixedat = 0
  if(fix) then
    do iftype = ntype + 1, ntype_with_fixed
      nfixedat = nfixedat + natoms(iftype)
    end do
  end if       
  write(*,*) ' Total number of fixed atoms: ', nfixedat

  ! Setting the vector that contains the default tolerances

  do i = 1, ntotat
    radius(i) = dism/2.d0
    fscale(i) = 1.d0
    if ( use_short_tol ) then
      use_short_radius(i) = .true.
    else
      use_short_radius(i) = .false.
    end if
    short_radius(i) = short_tol_dist/2.d0
    short_radius_scale(i) = short_tol_scale
  end do

  ! Setting the radius defined for atoms of each molecule, 
  ! but not atom-specific, first

  icart = 0
  do itype = 1, ntype_with_fixed
    iline = linestrut(itype,1)
    iline_atoms = 0 
    do while( iline <= linestrut(itype,2) )
      if ( keyword(iline,1) == "atoms" ) then
        iline_atoms = iline
        iline = iline + 1
        cycle
      end if
      if ( keyword(iline,1) == "end" .and. &    
           keyword(iline,2) == "atoms" ) then
        iline_atoms = 0  
        iline = iline + 1
        cycle
      end if
      if ( iline_atoms == 0 ) then
        !
        ! Read radius
        !
        if ( keyword(iline,1) == "radius" ) then
          read(keyword(iline,2),*,iostat=ioerr) value
          if ( ioerr /= 0 ) then
            write(*,*) ' ERROR: Could not read radius from keyword. '
            stop exit_code_input_error
          end if
          iicart = icart
          do imol = 1, nmols(itype)
            do iatom = 1, natoms(itype)
              iicart = iicart + 1
              radius(iicart) = value
            end do
          end do
        end if
        !
        ! Read minimum-distance function scale
        !
        if ( keyword(iline,1) == "fscale" ) then
          read(keyword(iline,2),*,iostat=ioerr) value
          if ( ioerr /= 0 ) then
            write(*,*) ' ERROR: Could not read fscale value from keyword. '
            stop exit_code_input_error
          end if
          iicart = icart
          do imol = 1, nmols(itype)
            do iatom = 1, natoms(itype)
              iicart = iicart + 1
              fscale(iicart) = value
            end do
          end do
        end if
        !
        ! Read short_radius
        !  
        if ( keyword(iline,1) == "short_radius" ) then
          read(keyword(iline,2),*,iostat=ioerr) value
          if ( ioerr /= 0 ) then
            write(*,*) ' ERROR: Could not read short_radius value from keyword. '
            stop exit_code_input_error
          end if
          iicart = icart
          do imol = 1, nmols(itype)
            do iatom = 1, natoms(itype)
              iicart = iicart + 1
              short_radius(iicart) = value
              use_short_radius(iicart) = .true.
            end do
          end do
        end if
        !
        ! Read short_radius scale
        !  
        if ( keyword(iline,1) == "short_radius_scale" ) then
          read(keyword(iline,2),*,iostat=ioerr) value
          if ( ioerr /= 0 ) then
            write(*,*) ' ERROR: Could not read short_radius_scale value from keyword. '
            stop exit_code_input_error
          end if
          iicart = icart
          do imol = 1, nmols(itype)
            do iatom = 1, natoms(itype)
              iicart = iicart + 1
              short_radius_scale(iicart) = value
              use_short_radius(iicart) = .true.
            end do
          end do
        end if
      end if
      iline = iline + 1
    end do
    icart = icart + nmols(itype)*natoms(itype)
  end do

  ! If some radius was defined using atom-specific definitions, overwrite
  ! the general radius defined for the molecule

  icart = 0
  do itype = 1, ntype_with_fixed
    iline = linestrut(itype,1)
    iline_atoms = 0 
    do while( iline <= linestrut(itype,2) )
      if ( keyword(iline,1) == "atoms" ) then
        iline_atoms = iline
        iline = iline + 1
        cycle
      end if
      if ( keyword(iline,1) == "end" .and. &    
           keyword(iline,2) == "atoms" ) then
        iline_atoms = 0  
        iline = iline + 1
        cycle
      end if
      if ( iline_atoms /= 0 ) then
        !
        ! Read atom specific radius
        !
        if ( keyword(iline,1) == "radius" ) then
          read(keyword(iline,2),*,iostat=ioerr) value
          if ( ioerr /= 0 ) then
            write(*,*) ' ERROR: Could not read radius from keyword. '
            stop exit_code_input_error
          end if
          ival = 2
          do
            read(keyword(iline_atoms,ival),*,iostat=ioerr) iat
            if ( ioerr /= 0 ) exit
            if ( iat > natoms(itype) ) then
              write(*,*) ' ERROR: atom selection with index greater than number of '
              write(*,*) '        atoms in structure ', itype
              stop exit_code_input_error
            end if
            radius(icart+iat) = value
            ival = ival + 1
          end do
        end if
        !
        ! Read atom specific function scale
        !
        if ( keyword(iline,1) == "fscale" ) then
          read(keyword(iline,2),*,iostat=ioerr) value
          if ( ioerr /= 0 ) then
            write(*,*) ' ERROR: Could not read fscale value from keyword. '
            stop exit_code_input_error
          end if
          ival = 2
          do
            read(keyword(iline_atoms,ival),*,iostat=ioerr) iat
            if ( ioerr /= 0 ) exit
            if ( iat > natoms(itype) ) then
              write(*,*) ' ERROR: atom selection with index greater than number of '
              write(*,*) '        atoms in structure ', itype
              stop exit_code_input_error
            end if
            fscale(icart+iat) = value
            ival = ival + 1
          end do
        end if
        !
        ! Read atom specific short radius
        !
        if ( keyword(iline,1) == "short_radius" ) then
          read(keyword(iline,2),*,iostat=ioerr) value
          if ( ioerr /= 0 ) then
            write(*,*) ' ERROR: Could not read short_radius value from keyword. '
            stop exit_code_input_error
          end if
          ival = 2
          do
            read(keyword(iline_atoms,ival),*,iostat=ioerr) iat
            if ( ioerr /= 0 ) exit
            if ( iat > natoms(itype) ) then
              write(*,*) ' ERROR: atom selection with index greater than number of '
              write(*,*) '        atoms in structure ', itype
              stop exit_code_input_error
            end if
            short_radius(icart+iat) = value
            use_short_radius(icart+iat) = .true.
            ival = ival + 1
          end do
        end if
        !
        ! Read atom specific short radius function scale
        !
        if ( keyword(iline,1) == "short_radius_scale" ) then
          read(keyword(iline,2),*,iostat=ioerr) value
          if ( ioerr /= 0 ) then
            write(*,*) ' ERROR: Could not read short_radius_scale value from keyword. '
            stop exit_code_input_error
          end if
          ival = 2
          do
            read(keyword(iline_atoms,ival),*,iostat=ioerr) iat
            if ( ioerr /= 0 ) exit
            if ( iat > natoms(itype) ) then
              write(*,*) ' ERROR: atom selection with index greater than number of '
              write(*,*) '        atoms in structure ', itype
              stop exit_code_input_error
            end if
            short_radius_scale(icart+iat) = value
            use_short_radius(icart+iat) = .true.
            ival = ival + 1
          end do
        end if
      end if
      iline = iline + 1
    end do
    iicart = icart
    icart = icart + natoms(itype)
    do imol = 2, nmols(itype)
      do iatom = 1, natoms(itype)
        icart = icart + 1
        radius(icart) = radius(iicart+iatom)
        fscale(icart) = fscale(iicart+iatom)
        short_radius(icart) = short_radius(iicart+iatom)
        short_radius_scale(icart) = short_radius_scale(iicart+iatom)
        use_short_radius(icart) = use_short_radius(iicart+iatom)
      end do
    end do
  end do

  ! Check if the short radii were set correctly, if the case

  ioerr = 0
  do i = 1, ntotat
    if ( use_short_radius(i) ) then
     if ( short_radius(i) >= radius(i) ) then 
       write(*,*) ' ERROR: The short radius must be smaller than the default radius. '
       write(*,*) '        (the default radius is one half of the default tolerance).'
       stop exit_code_input_error
     end if
   end if
  end do

  ! If using PBC, add the extra constraint

  radmax = 0.d0
  do i = 1, ntotat
    radmax = dmax1(radmax,2.d0*radius(i))
  end do

  if (using_pbc) then
    nrest = nrest + 1
    irestline(nrest) = -1
    ityperest(nrest) = 3
    restpars(nrest,1:3) = pbc_min
    restpars(nrest,4:6) = pbc_max
    write(*,*) " PBC on: Constraint automatically added to non-fixed atoms:"
    write(*,"(a, 6f8.2)") "  -> inside box ", restpars(nrest,1:6)
    write(*,*) ' Updated total number of restrictions: ', nrest
  end if

  ! Setting the array that contains the restrictions per atom

  icart = 0
  do itype = 1, ntype
    rests = .false.
    do imol = 1, nmols(itype)
      idatom = idfirst(itype) - 1      
      do iatom = 1, natoms(itype) 
        icart = icart + 1
        idatom = idatom + 1
        nratom(icart) = 0
        iratcount = 0
        do i = 1, mrperatom
          iratom(icart,i) = 0
        end do
        iline = linestrut(itype,1)
        do while(iline.lt.linestrut(itype,2))
          iline = iline + 1
          if(keyword(iline,1).eq.'atoms') then
            iiatom = -1
            do iat = 2, maxkeywords
              read(keyword(iline,iat),*,iostat=ioerr) iiatom
              if ( ioerr /= 0 ) then
                if ( iiatom == -1 ) then 
                  write(*,*) ' ERROR: Could not read atom selection for type: ', itype
                  stop exit_code_input_error
                else
                  exit
                end if
              end if
              if ( iiatom > natoms(itype) ) then
                write(*,*) ' ERROR: atom selection with index greater than number of '
                write(*,*) '        atoms in structure ', itype
                stop exit_code_input_error
              end if
              if(iatom.eq.iiatom) exit
            end do
            do while(keyword(iline,1).ne.'end'.and.&
                     keyword(iline,2).ne.'atoms')
              iline = iline + 1
              if(iatom.eq.iiatom) then
                if(keyword(iline,1).eq.'inside'.or.&
                   keyword(iline,1).eq.'outside'.or.&
                   keyword(iline,1).eq.'over'.or.&
                   keyword(iline,1).eq.'above'.or.&
                   keyword(iline,1).eq.'below') then
                  nratom(icart) = nratom(icart) + 1
                  iratcount = iratcount + 1
                  do irest = 1, nrest
                    if(irestline(irest).eq.iline) iirest = irest
                  end do
                  iratom(icart,iratcount) = iirest
                end if
              end if
            end do
            iline = iline - 1
          else if(keyword(iline,1).eq.'inside'.or.&
                  keyword(iline,1).eq.'outside'.or.&
                  keyword(iline,1).eq.'over'.or.&
                  keyword(iline,1).eq.'above'.or.&
                  keyword(iline,1).eq.'below') then
            nratom(icart) = nratom(icart) + 1    
            iratcount = iratcount + 1
            do irest = 1, nrest
              if(irestline(irest).eq.iline) iirest = irest
            end do
            iratom(icart,iratcount) = iirest
          end if
        end do
        if (using_pbc) then
          nratom(icart) = nratom(icart) + 1
          iratcount = iratcount + 1
          do irest = 1, nrest
            if (irestline(irest).eq.-1) iratom(icart,iratcount) = irest
          end do
        end if
        if(nratom(icart).gt.0) rests = .true.
      end do 
      if(.not.rests) then
        write(*,*) ' ERROR: Some molecule has no geometrical',&
                   ' restriction defined: nothing to do.'
        stop exit_code_input_error
      end if
    end do
  end do

  ! Read the constraints to rotations about axis, if set

  do itype = 1, ntype
    constrain_rot(itype,1) = .false.
    constrain_rot(itype,2) = .false.
    constrain_rot(itype,3) = .false.
    iline = linestrut(itype,1)
    do while(iline.lt.linestrut(itype,2))
      iline = iline + 1
      if(keyword(iline,1).eq.'constrain_rotation') then
        if(iline.gt.linestrut(itype,1).and.&
           iline.lt.linestrut(itype,2)) then

           ! Note that for movable molecules, teta is a rotation on the x-axis,
           !                                  gama is a rotation on the z-axis,
           !                                  beta is a rotation on the y-axis
           !                                  (see eulerrmat routine)

          if(keyword(iline,2).eq.'x') then
            constrain_rot(itype,3) = .true.
            read(keyword(iline,3),*) rot_bound(itype,3,1)
            read(keyword(iline,4),*) rot_bound(itype,3,2)
            rot_bound(itype,3,1) = rot_bound(itype,3,1)*pi/180.d0
            rot_bound(itype,3,2) = rot_bound(itype,3,2)*pi/180.d0
  
            write(*,*) ' Rotations about x axis of molecules of ',&
                       ' type ', itype, ' will be constrained. '
          end if
          if(keyword(iline,2).eq.'y') then
            constrain_rot(itype,1) = .true.
            read(keyword(iline,3),*) rot_bound(itype,1,1)
            read(keyword(iline,4),*) rot_bound(itype,1,2)
            rot_bound(itype,1,1) = rot_bound(itype,1,1)*pi/180.d0
            rot_bound(itype,1,2) = rot_bound(itype,1,2)*pi/180.d0

            write(*,*) ' Rotations about y axis of molecules of ',&
                       ' type ', itype, ' will be constrained. '
          end if
          if(keyword(iline,2).eq.'z') then
            constrain_rot(itype,2) = .true.
            read(keyword(iline,3),*) rot_bound(itype,2,1)
            read(keyword(iline,4),*) rot_bound(itype,2,2)
            rot_bound(itype,2,1) = rot_bound(itype,2,1)*pi/180.d0
            rot_bound(itype,2,2) = rot_bound(itype,2,2)*pi/180.d0

            write(*,*) ' Rotations about z axis of molecules of ',&
                       ' type ', itype, ' will be constrained. '
          end if
          if ( keyword(iline,2) /= 'x' .and. &
               keyword(iline,2) /= 'y' .and. &
               keyword(iline,2) /= 'z' ) then
            write(*,*) ' ERROR: constrain_rotation option not properly defined (not x, y, or z) '
            stop exit_code_input_error
          end if
        end if
      end if
    end do
  end do
 
  ! If there are no variables (only fixed molecules, stop)

  if(n.eq.0) then
    call output(n,x, xyzout)
    write(*,dash1_line)
    write(*,*) ' Success! There are only fixed molecules, therefore there is nothing to pack. '
    write(*,*) ' The output file contains the fixed molecules in the desired positions. '
    write(*,dash1_line)
    write(*,*) ' Wrote output file: ', trim(adjustl(xyzout))
    if ( crd ) write(*,*) ' ... and to CRD file: ', trim(adjustl(crdfile))
    write(*,dash1_line)
    stop
  end if
  
  !
  ! (Re)setting parameters and building initial point
  !

  call initial(n,x)

  ! Computing the energy at the initial point

  radscale = 1.d0
  do i = 1, ntotat
    radius_ini(i) = radius(i)
  end do
  call computef(n,x,all_type_fx)
  write(*,*) ' Objective function at initial point: ', all_type_fx
  fprint = all_type_fx
  do i = 1, n 
    xprint(i) = x(i)
  end do

  ! Stop if only checking the initial approximation

  if(check) then
    call output(n,x,xyzout)
    write(*,*) ' Wrote initial point to output file: ', trim(adjustl(xyzout)) 
    if ( crd ) write(*,*) ' ... and to CRD file: ', trim(adjustl(crdfile))
    stop
  end if

  !
  ! Main loop: first pack types of molecules separately, then
  ! pack all molecules together
  !

  call swaptype(n,x,itype,0) ! Save all-molecule vector data
  itype = 0
  main : do while(itype <= ntype)
    itype = itype + 1
    if ( packall ) itype = ntype + 1
 
    ! Use larger tolerance than required to improve separation

    radscale = discale
    do i = 1, ntotat
      radius(i) = discale*radius_ini(i)
    end do

    ! Set vectors for specific or all-molecule packing
    
    if ( itype <= ntype ) then
      call swaptype(n,x,itype,1) ! Set vectors to pack only this type of molecule
    else 
      call swaptype(n,x,itype,3) ! Restore all-molecule vectors
    end if

    ! Print titles

    write(*,hash3_line)
    if ( itype <= ntype ) then
      write(*,*) ' Packing molecules of type: ', input_itype(itype)
    else
      write(*,*) ' Packing all molecules together '
    end if
    write(*,hash3_line)

    ! Checking if first approximation is a solution

    call computef(n,x,fx)

    if ( fdist < precision .and. frest < precision ) then

      write(*,*)
      write(*,*) ' Initial approximation is a solution. Nothing to do. '
      write(*,*)
      call swaptype(n,x,itype,3) ! Restore all-molecule vectors
      call output(n,x,xyzout)
      if( itype == ntype + 1 ) then
        write(*,*) ' Solution written to file: ', trim(adjustl(xyzout))
        if ( crd ) write(*,*) ' ... and to CRD file: ', trim(adjustl(crdfile))
      else
        write(*,*) ' Current point written to file: ', trim(adjustl(xyzout))
        if ( crd ) write(*,*) ' ... and to CRD file: ', trim(adjustl(crdfile))
      end if
      call writesuccess(itype,fdist,frest,fx)

    ! Otherwise, pack the molecules
    
    else 

      loop = -1

      ! Initializing parameters relative to the improvement of the function
      fimp = 1.d99
      fimprov = fimp
      do i = 1, ntotat
        radiuswork(i) = radius(i) 
        radius(i) = radius_ini(i)
      end do
      call computef(n,x,fx)
      do i = 1, ntotat
        radius(i) = radiuswork(i)
      end do
      bestf = fx
      flast = fx

      gencanloop : do while(loop.lt.nloop)
        loop = loop + 1

        ! Moving bad molecules
        if(radscale == 1.d0 .and. fimp.le.10.d0) then
          movebadprint = .true.
          call movebad(n,x,fx,movebadprint)
          flast = fx
        end if


        write(*,dash3_line)
        write(*,*) ' Starting GENCAN loop: ', loop
        write(*,*) ' Scaling radii by: ', radscale
        write(*,*)

        ! CALL GENCAN

        write(*,prog1_line)
        call pgencan(n,x,fx)

        !
        ! Compute the statistics of the last optimization loop
        !

        ! Use the user-specified radii for statistics

        do i = 1, ntotat
          radiuswork(i) = radius(i)
          radius(i) = radius_ini(i)
        end do
        call computef(n,x,fx)

        if(bestf.gt.0.d0) fimprov = -100.d0 * (fx - bestf) / bestf
        if(bestf.eq.0.d0) fimprov = 100.d0
        if(flast.gt.0.d0) fimp = -100.d0 * (fx - flast) / flast
        if(flast.eq.0.d0) fimp = 100.d0
        fimp = dmin1(99.99d0,dmax1(-99.99d0,fimp))
        fimprov = dmin1(99.99d0,dmax1(-99.99d0,fimprov))

        write(*,"(/&
                  &'  Function value from last GENCAN loop: f = ', e10.5, /&
                  &'  Best function value before: f = ', e10.5,           /&
                  &'  Improvement from best function value: ', f8.2, ' %',/&
                  &'  Improvement from last loop: ', f8.2, ' %',          /&
                  &'  Maximum violation of target distance: ', f12.6,     /&
                  &'  Maximum violation of the constraints: ', e10.5       &
                  &)") fx, bestf, fimprov, fimp, fdist, frest
        flast = fx

        !
        ! Analysis of final loop packing and output data
        !
        
        if ( itype <= ntype ) then

          ! Save best function value for this packing

          if ( fx < bestf ) bestf = fx

          ! Check if this point is a solution

          call swaptype(n,x,itype,2) ! Save this type current point 
          ! If the solution was found for this type
          if( fdist < precision .and. frest < precision ) then
            call swaptype(n,x,itype,3) ! Restore all molecule vectors
            call output(n,x,xyzout)
            write(*,*) ' Current structure written to file: ', trim(adjustl(xyzout))
            if ( crd ) write(*,*) ' ... and to CRD file: ', trim(adjustl(crdfile))
            call writesuccess(itype,fdist,frest,fx)
            exit gencanloop
          end if

          ! Compute and report function value for all-type packing

          call swaptype(n,x,itype,3) ! Restore all molecule vectors
          call computef(n,x,all_type_fx)
          write(*,"('  All-type function value: ', e10.5 )") all_type_fx

        else

          call computef(n,x,fx)
          all_type_fx = fx
          if ( fx < bestf ) bestf = fx
          ! If solution was found for all system
          if ( fdist < precision .and. frest < precision ) then
            call output(n,x,xyzout)
            call writesuccess(itype,fdist,frest,fx)
            write(*,*) ' Solution written to file: ', trim(adjustl(xyzout))
            if ( crd ) write(*,*) ' ... and to CRD file: ', trim(adjustl(crdfile))
            write(*,dash3_line)
            exit main
          end if

        end if
        write(*,dash3_line)

        ! If this is the best structure so far
        if( mod(loop+1,writeout) == 0 .and. all_type_fx < fprint ) then
          call output(n,x,xyzout)
          write(*,*) ' Current solution written to file: ', trim(adjustl(xyzout))
          if ( crd ) write(*,*) ' ... and to CRD file: ', trim(adjustl(crdfile))
          fprint = all_type_fx
          do i = 1, n
            xprint(i) = x(i)
          end do

        ! If the user required printing even bad structures
        else if ( mod(loop+1,writeout) == 0 .and. writebad ) then
          call output(n,x,xyzout)
          write(*,*) ' Writing current (perhaps bad) structure to file: ', trim(adjustl(xyzout))
          if ( crd ) write(*,*) ' ... and to CRD file: ', trim(adjustl(crdfile))
        end if

        ! Restore vector for packing this type of molecule, if the case

        if ( itype <= ntype ) then
          call swaptype(n,x,itype,0) ! Reset type vectors
          call swaptype(n,x,itype,1) ! Set vector for molecules of this type
          call computef(n,x,fx)
        end if

        ! Restore the working radii 

        do i = 1, ntotat
          radius(i) = radiuswork(i)
        end do
        if ( radscale > 1.d0 ) then
          if( ( fdist < precision .and. fimp < 10.d0 ) .or. &
                fimp < 2.d0 ) then
            radscale = dmax1(0.9*radscale,1.d0)
            do i = 1, ntotat
              radius(i) = dmax1(radius_ini(i),0.9d0*radius(i))
            end do
          end if
        end if

        if(loop.eq.nloop) then
          if ( itype .eq. ntype+1 ) then
            write(*,*)' STOP: Maximum number of GENCAN loops achieved.'
            call checkpoint(n,xprint)
            exit_code = exit_code_failed_to_converge
            exit main
          else
            write(*,*)' Maximum number of GENCAN loops achieved.'
          end if
        end if

      end do gencanloop

    end if

  end do main

  write(*,*) '  Running time: ', etime(tarray) - time0,' seconds. ' 
  write(*,dash3_line)
  write(*,*) 

  ! Fortran < 2008 doesn't support non-constant exit codes
  if (exit_code == 0) then
    stop
  elseif (exit_code == exit_code_failed_to_converge) then
    stop exit_code_failed_to_converge
  else
    stop exit_code_general_error
  end if

end program packmol


!  
!  Written by Leandro Martínez, 2009-2011.
!  Copyright (c) 2009-2018, Leandro Martínez, Jose Mario Martinez,
!  Ernesto G. Birgin.
!  
! Subroutine initial: Subroutine that reset parameters and
!                     builds the initial point
!

subroutine initial(n,x)

  use exit_codes
  use sizes
  use compute_data
  use input, only : randini, ntfix, fix, moldy, chkgrad, avoidoverlap,&
                    discale, precision, sidemax, restart_from, input_itype,&
                    nloop0_type
  use usegencan
  use ahestetic
  use pbc
  implicit none
  integer :: n, i, j, k, idatom, iatom, ilubar, ilugan, icart, itype, &
             imol, ntry, nb, iboxx, iboxy, iboxz, ifatom, &
             idfatom, iftype, jatom, ioerr

  double precision :: x(n), cmx, cmy, beta, gamma, theta, &
                      cmz, fx, xlength, dbox, rnd, &
                      radmax, v1(3), v2(3), v3(3), xbar, ybar, zbar
  double precision, parameter :: twopi = 8.d0*datan(1.d0)
     
  logical :: overlap, movebadprint, hasbad 
  logical, allocatable :: hasfixed(:,:,:)

  character(len=strl) :: record

  ! Allocate hasfixed array

  allocate(hasfixed(0:nbp+1,0:nbp+1,0:nbp+1))

  ! We need to initialize the move logical variable

  move = .false.

  ! Default status of the function evaluation

  init1 = .false.
  lboxfirst = 0

  ! Initialize the comptype logical array

  do i = 1, ntfix
    comptype(i) = .true.
  end do

  ! Penalty factors for the objective function relative to restrictions
  ! Default values: scale = 1.d2, scale2 = 1.d1

  scale = 1.d0
  scale2 = 1.d-2

  ! Move molecules to their center of mass (not for moldy)
  if(.not.moldy) call tobar()

  ! Compute maximum internal distance within each type of molecule

  do itype = 1, ntype
    dmax(itype) = 0.d0
    idatom = idfirst(itype) - 1
    do iatom = 1, natoms(itype) - 1
      do jatom = iatom + 1, natoms(itype)
        dmax(itype) = dmax1 ( dmax(itype),&
               (coor(idatom+iatom,1)-coor(idatom+jatom,1))**2+&
               (coor(idatom+iatom,2)-coor(idatom+jatom,2))**2+&
               (coor(idatom+iatom,3)-coor(idatom+jatom,3))**2 )
      end do
    end do
    dmax(itype) = dsqrt(dmax(itype))
    write(*,*) ' Maximum internal distance of type ',itype,': ',&
               dmax(itype)
    if(dmax(itype).eq.0.) dmax(itype) = 1.d0
  end do

  ! Maximum size of the system: if you system is very large (about
  ! 80 nm wide), increase the sidemax parameter.
  ! Otherwise, the packing can be slow and unsucesful

  cmxmin(1) = -sidemax
  cmymin(1) = -sidemax
  cmzmin(1) = -sidemax
  cmxmax(1) = sidemax
  cmymax(1) = sidemax
  cmzmax(1) = sidemax
  do i = 1, 3
    x(i) = 0.d0
    x(i+ntotmol*3) = 0.d0
  end do
  call restmol(1,0,n,x,fx,.true.)
  sizemin(1) = x(1) - sidemax 
  sizemax(1) = x(1) + sidemax
  sizemin(2) = x(2) - sidemax
  sizemax(2) = x(2) + sidemax
  sizemin(3) = x(3) - sidemax
  sizemax(3) = x(3) + sidemax
  write(*,*) ' All atoms must be within these coordinates: '
  write(*,*) '  x: [ ', sizemin(1),', ', sizemax(1), ' ] '
  write(*,*) '  y: [ ', sizemin(2),', ', sizemax(2), ' ] '
  write(*,*) '  z: [ ', sizemin(3),', ', sizemax(3), ' ] '
  write(*,*) ' If the system is larger than this, increase the sidemax parameter. '

  ! Create first aleatory guess

  i = 0
  j = ntotmol*3
  do itype = 1, ntype
    do imol = 1, nmols(itype)
      x(i+1) = sizemin(1) + rnd()*(sizemax(1)-sizemin(1))
      x(i+2) = sizemin(2) + rnd()*(sizemax(2)-sizemin(2))
      x(i+3) = sizemin(3) + rnd()*(sizemax(3)-sizemin(3))
      if ( constrain_rot(itype,1) ) then
        x(j+1) = ( rot_bound(itype,1,1) - dabs(rot_bound(itype,1,2)) ) + &
               2.d0*rnd()*dabs(rot_bound(itype,1,2))
      else
        x(j+1) = twopi*rnd()
      end if
      if ( constrain_rot(itype,2) ) then
        x(j+2) = ( rot_bound(itype,2,1) - dabs(rot_bound(itype,2,2)) ) + &
                 2.d0*rnd()*dabs(rot_bound(itype,2,2))
      else
        x(j+2) = twopi*rnd()
      end if
      if ( constrain_rot(itype,3) ) then
        x(j+3) = ( rot_bound(itype,3,1) - dabs(rot_bound(itype,3,2)) ) + &
                 2.d0*rnd()*dabs(rot_bound(itype,3,2))
      else
        x(j+3) = twopi*rnd()
      end if
      i = i + 3
      j = j + 3
    end do
  end do

  ! Initialize cartesian coordinate array for the first time

  ilubar = 0
  ilugan = ntotmol*3
  icart = 0
  do itype = 1, ntype
    do imol = 1, nmols(itype)
      xbar = x(ilubar+1)
      ybar = x(ilubar+2)
      zbar = x(ilubar+3)
      beta = x(ilugan+1)
      gamma = x(ilugan+2)
      theta = x(ilugan+3)
      call eulerrmat(beta,gamma,theta,v1,v2,v3)
      idatom = idfirst(itype) - 1
      do iatom = 1, natoms(itype)
        icart = icart + 1
        idatom = idatom + 1
        call compcart(icart,xbar,ybar,zbar,&
                      coor(idatom,1),coor(idatom,2),coor(idatom,3),&
                      v1,v2,v3)
        fixedatom(icart) = .false.
      end do
    end do
  end do
  if(fix) then
    icart = ntotat - natfix
    do iftype = ntype + 1, ntfix
      idfatom = idfirst(iftype) - 1
      do ifatom = 1, natoms(iftype)
        idfatom = idfatom + 1
        icart = icart + 1
        xcart(icart,1) = coor(idfatom,1)
        xcart(icart,2) = coor(idfatom,2)
        xcart(icart,3) = coor(idfatom,3)
        fixedatom(icart) = .true.
      end do
    end do
  end if

  ! Use the largest radius as the reference for binning the box

  radmax = 0.d0
  do i = 1, ntotat
    radmax = dmax1(radmax,2.d0*radius(i))
  end do

  ! Performing some steps of optimization for the restrictions only
  
  write(*,hash3_line)
  write(*,"('  Building initial approximation ... ' )")
  write(*,hash3_line)
  write(*,"('  Adjusting initial point to fit the constraints ')")
  write(*,dash2_line)
  init1 = .true.
  call swaptype(n,x,itype,0) ! Initialize swap arrays
  itype = 0
  do while( itype <= ntype )
    itype = itype + 1
    if ( itype <= ntype ) then
      call swaptype(n,x,itype,1) ! Set arrays for this type
    else
      call swaptype(n,x,itype,3) ! Restore arrays if itype = ntype + 1
      exit
    end if
    write(*,dash3_line)
    write(*,*) ' Molecules of type: ', input_itype(itype)
    write(*,*)
    i = 0
    hasbad = .true.
    call computef(n,x,fx)
    do while( frest > precision .and. i.le. nloop0_type(itype)-1 .and. hasbad)
      i = i + 1 
      write(*,prog1_line)
      call pgencan(n,x,fx)
      call computef(n,x,fx)
      if(frest > precision) then 
        write(*,"( a,i6,a,i6 )")'  Fixing bad orientations ... ', i,' of ', nloop0_type(itype)
        movebadprint = .true.
        call movebad(n,x,fx,movebadprint) 
      end if
    end do
    write(*,*) 
    write(*,*) ' Restraint-only function value: ', fx
    write(*,*) ' Maximum violation of the restraints: ', frest
    call swaptype(n,x,itype,2) ! Save current type results

    if( hasbad .and. frest > precision ) then
      write(*,*) ' ERROR: Packmol was unable to put the molecules'
      write(*,*) '        in the desired regions even without'
      write(*,*) '        considering distance tolerances. '
      write(*,*) '        Probably there is something wrong with'
      write(*,*) '        the constraints, since it seems that'
      write(*,*) '        the molecules cannot satisfy them at'
      write(*,*) '        at all. '
      write(*,*) '        Please check the spatial constraints and' 
      write(*,*) '        try again.'
      if ( i .ge. nloop0_type(itype)-1 ) then
      end if
        write(*,*) ' >The maximum number of cycles (',nloop0_type(itype),') was achieved.' 
        write(*,*) '  You may try increasing it with the',' nloop0 keyword, as in: nloop0 1000 '
      stop exit_code_failed_to_converge
    end if
  end do
  init1 = .false.

  ! Rescaling sizemin and sizemax in order to build the patch of boxes

  write(*,dash3_line)
  write(*,*) ' Rescaling maximum and minimum coordinates... '
  do i = 1, 3
    sizemin(i) = 1.d20
    sizemax(i) = -1.d20
  end do                       

  icart = 0
  do itype = 1, ntfix
    do imol = 1, nmols(itype)
      do iatom = 1, natoms(itype) 
        icart = icart + 1
        sizemin(1) = dmin1(sizemin(1),xcart(icart,1))
        sizemin(2) = dmin1(sizemin(2),xcart(icart,2))
        sizemin(3) = dmin1(sizemin(3),xcart(icart,3))
        sizemax(1) = dmax1(sizemax(1),xcart(icart,1))
        sizemax(2) = dmax1(sizemax(2),xcart(icart,2))
        sizemax(3) = dmax1(sizemax(3),xcart(icart,3))
      end do 
    end do
  end do             

  if (is_pbc) then
    sizemin = 0.0d0
    sizemax = pbc_box
  end if

  ! Computing the size of the patches

  write(*,*) ' Computing size of patches... '
  dbox = discale * radmax + 0.01d0 * radmax 
  do i = 1, 3
    xlength = sizemax(i) - sizemin(i)
    nb = int(xlength/dbox + 1.d0)  
    if(nb.gt.nbp) nb = nbp
    boxl(i) = dmax1(xlength/dfloat(nb),dbox)
    nboxes(i) = nb
    nb2(i) = nboxes(i) + 2
  end do

  ! Reseting latomfix array

  do i = 0, nbp + 1
    do j = 0, nbp + 1
      do k = 0, nbp + 1
        latomfix(i,j,k) = 0
        latomfirst(i,j,k) = 0
        hasfixed(i,j,k) = .false.
        hasfree(i,j,k) = .false.
      end do
    end do
  end do   
 
  ! If there are fixed molecules, add them permanently to the latomfix array

  write(*,*) ' Add fixed molecules to permanent arrays... '
  if(fix) then
    icart = ntotat - natfix
    do iftype = ntype + 1, ntfix
      idfatom = idfirst(iftype) - 1
      do ifatom = 1, natoms(iftype)
        idfatom = idfatom + 1
        icart = icart + 1
        call setibox(xcart(icart,1),xcart(icart,2),xcart(icart,3),&
                     sizemin,boxl,nboxes,iboxx,iboxy,iboxz)
        latomnext(icart) = latomfix(iboxx,iboxy,iboxz)
        latomfix(iboxx,iboxy,iboxz) = icart
        latomfirst(iboxx,iboxy,iboxz) = icart
        ibtype(icart) = iftype
        ibmol(icart) = 1
        hasfixed(iboxx,iboxy,iboxz) = .true.
      end do
    end do
  end if

  ! Reseting mass centers to be within the regions

  write(*,*) ' Reseting center of mass... '
  do itype = 1, ntype
    cmxmin(itype) = 1.d20
    cmymin(itype) = 1.d20
    cmzmin(itype) = 1.d20
    cmxmax(itype) = -1.d20
    cmymax(itype) = -1.d20
    cmzmax(itype) = -1.d20
  end do

  icart = 0
  do itype = 1, ntype
    do imol = 1, nmols(itype)
      cmx = 0.d0
      cmy = 0.d0
      cmz = 0.d0
      do iatom = 1, natoms(itype)
        icart = icart + 1
        cmx = cmx + xcart(icart,1)
        cmy = cmy + xcart(icart,2)
        cmz = cmz + xcart(icart,3)
      end do
      cmx = cmx / dfloat(natoms(itype))
      cmy = cmy / dfloat(natoms(itype))
      cmz = cmz / dfloat(natoms(itype))
      cmxmin(itype) = dmin1(cmxmin(itype),cmx)
      cmymin(itype) = dmin1(cmymin(itype),cmy)
      cmzmin(itype) = dmin1(cmzmin(itype),cmz)
      cmxmax(itype) = dmax1(cmxmax(itype),cmx)
      cmymax(itype) = dmax1(cmymax(itype),cmy)
      cmzmax(itype) = dmax1(cmzmax(itype),cmz)
    end do
  end do

  ! If there is a restart file for all system, read it

  if ( restart_from(0) /= 'none' ) then
    record = restart_from(0)
    write(*,*) ' Restarting all system from file: ', trim(adjustl(record))
    open(10,file=restart_from(0),status='old',action='read',iostat=ioerr)
    ilubar = 0
    ilugan = ntotmol*3
    do i = 1, ntotmol
      read(10,*,iostat=ioerr) x(ilubar+1), x(ilubar+2), x(ilubar+3), &
                              x(ilugan+1), x(ilugan+2), x(ilugan+3)
      if ( ioerr /= 0 ) then
        write(*,*) ' ERROR: Could not read restart file: ', trim(adjustl(record))
        stop exit_code_open_file
      end if
      ilubar = ilubar + 3
      ilugan = ilugan + 3
    end do
    close(10)
    return
  end if

  ! Building random initial point 

  write(*,dash3_line)
  write(*,*) ' Setting initial trial coordinates ... '
  write(*,dash2_line)

  if ( chkgrad ) then 
     write(*,*) ' For checking gradient, will set avoidoverlap to false. '
     avoidoverlap = .false. 
  end if

  ! Setting random center of mass coordinates, within size limits

  ilubar = 0
  do itype = 1, ntype
    if ( restart_from(itype) /= 'none' ) then
      ilubar = ilubar + nmols(itype)*3
      cycle
    end if
    do imol = 1, nmols(itype)
      if ( .not. avoidoverlap ) then
        fx = 1.d0
        ntry = 0
        do while((fx.gt.precision).and.ntry.le.20) 
          ntry = ntry + 1
          x(ilubar+1) = cmxmin(itype) + rnd()*(cmxmax(itype)-cmxmin(itype))
          x(ilubar+2) = cmymin(itype) + rnd()*(cmymax(itype)-cmymin(itype))
          x(ilubar+3) = cmzmin(itype) + rnd()*(cmzmax(itype)-cmzmin(itype))
          call restmol(itype,ilubar,n,x,fx,.false.)
        end do
      else
        fx = 1.d0
        ntry = 0
        overlap = .false.
        do while((overlap.or.fx.gt.precision).and.ntry.le.20) 
          ntry = ntry + 1
          x(ilubar+1) = cmxmin(itype) + rnd()*(cmxmax(itype)-cmxmin(itype))
          x(ilubar+2) = cmymin(itype) + rnd()*(cmymax(itype)-cmymin(itype))
          x(ilubar+3) = cmzmin(itype) + rnd()*(cmzmax(itype)-cmzmin(itype))
          if(fix) then
            call setibox(x(ilubar+1),x(ilubar+2),x(ilubar+3),&
                         sizemin,boxl,nboxes,iboxx,iboxy,iboxz)
            if(hasfixed(iboxx,  iboxy,  iboxz  ).or.&
               hasfixed(iboxx+1,iboxy,  iboxz  ).or.&
               hasfixed(iboxx,  iboxy+1,iboxz  ).or.&
               hasfixed(iboxx,  iboxy,  iboxz+1).or.&
               hasfixed(iboxx-1,iboxy,  iboxz  ).or.&
               hasfixed(iboxx,  iboxy-1,iboxz  ).or.&
               hasfixed(iboxx,  iboxy,  iboxz-1).or.&
               hasfixed(iboxx+1,iboxy+1,iboxz  ).or.&
               hasfixed(iboxx+1,iboxy,  iboxz+1).or.&
               hasfixed(iboxx+1,iboxy-1,iboxz  ).or.&
               hasfixed(iboxx+1,iboxy,  iboxz-1).or.&
               hasfixed(iboxx,  iboxy+1,iboxz+1).or.&
               hasfixed(iboxx,  iboxy+1,iboxz-1).or.&
               hasfixed(iboxx,  iboxy-1,iboxz+1).or.&
               hasfixed(iboxx,  iboxy-1,iboxz-1).or.&
               hasfixed(iboxx-1,iboxy+1,iboxz  ).or.&
               hasfixed(iboxx-1,iboxy,  iboxz+1).or.&
               hasfixed(iboxx-1,iboxy-1,iboxz  ).or.&
               hasfixed(iboxx-1,iboxy,  iboxz-1).or.&
               hasfixed(iboxx+1,iboxy+1,iboxz+1).or.&
               hasfixed(iboxx+1,iboxy+1,iboxz-1).or.&
               hasfixed(iboxx+1,iboxy-1,iboxz+1).or.&
               hasfixed(iboxx+1,iboxy-1,iboxz-1).or.&
               hasfixed(iboxx-1,iboxy+1,iboxz+1).or.&
               hasfixed(iboxx-1,iboxy+1,iboxz-1).or.&
               hasfixed(iboxx-1,iboxy-1,iboxz+1).or.&
               hasfixed(iboxx-1,iboxy-1,iboxz-1)) then
              overlap = .true.
            else
              overlap = .false.
            end if
          end if  
          if(.not.overlap) call restmol(itype,ilubar,n,x,fx,.false.)
        end do
      end if
      ilubar = ilubar + 3
    end do
  end do

  ! Setting random angles, except if the rotations were constrained

  ilugan = ntotmol*3
  do itype = 1, ntype
    if ( restart_from(itype) /= 'none' ) then
      ilugan = ilugan + nmols(itype)*3
      cycle
    end if
    do imol = 1, nmols(itype)
      if ( constrain_rot(itype,1) ) then
        x(ilugan+1) = ( rot_bound(itype,1,1) - dabs(rot_bound(itype,1,2)) ) + &
               2.d0*rnd()*dabs(rot_bound(itype,1,2))
      else
        x(ilugan+1) = twopi*rnd()
      end if
      if ( constrain_rot(itype,2) ) then
        x(ilugan+2) = ( rot_bound(itype,2,1) - dabs(rot_bound(itype,2,2)) ) + &
                 2.d0*rnd()*dabs(rot_bound(itype,2,2))
      else
        x(ilugan+2) = twopi*rnd()
      end if
      if ( constrain_rot(itype,3) ) then
        x(ilugan+3) = ( rot_bound(itype,3,1) - dabs(rot_bound(itype,3,2)) ) + &
                 2.d0*rnd()*dabs(rot_bound(itype,3,2))
      else
        x(ilugan+3) = twopi*rnd()
      end if
      ilugan = ilugan + 3
    end do
  end do

  ! Compare analytical and finite-difference gradients

  if(chkgrad) then
    dbox = discale * radmax + 0.01d0 * radmax
    do i = 1, 3
      xlength = sizemax(i) - sizemin(i)
      nb = int(xlength/dbox + 1.d0)  
      if(nb.gt.nbp) nb = nbp
      boxl(i) = dmax1(xlength/dfloat(nb),dbox)
      nboxes(i) = nb
      nb2(i) = nboxes(i) + 2
    end do
    call comparegrad(n,x)
    stop
  end if

  !
  ! Reading restart files of specific molecule types, if available
  !

  ilubar = 0
  ilugan = ntotmol*3
  do itype = 1, ntype
    if ( restart_from(itype) /= 'none' ) then
      record = restart_from(itype)
      write(*,dash3_line)
      write(*,*) ' Molecules of type: ', input_itype(itype)
      write(*,*) ' Will restart coordinates from: ', trim(adjustl(record))
      open(10,file=record,status='old',action='read',iostat=ioerr)
      if ( ioerr /= 0 ) then
        write(*,*) ' ERROR: Could not open restart file: ', trim(adjustl(record))
        stop exit_code_open_file
      end if
      do i = 1, nmols(itype)
        read(10,*,iostat=ioerr) x(ilubar+1), x(ilubar+2), x(ilubar+3), &
                                x(ilugan+1), x(ilugan+2), x(ilugan+3)
        if ( ioerr /= 0 ) then
          write(*,*) ' ERROR: Could not read restart file: ', trim(adjustl(record))
          stop exit_code_open_file
        end if
        ilubar = ilubar + 3
        ilugan = ilugan + 3
      end do
      close(10)
      call swaptype(n,x,itype,0) ! Initialize swap arrays
      call swaptype(n,x,itype,1) ! Set arrays for this type
      call computef(n,x,fx)
      write(*,*) ' Maximum violation of the restraints: ', frest
      write(*,*) ' Maximum violation of minimum atom distances: ', fdist
      call swaptype(n,x,itype,3) ! Restore all-molecule arrays 
    else
      ilubar = ilubar + nmols(itype)*3
      ilugan = ilugan + nmols(itype)*3
    end if
  end do

  ! Return with current random point (not default)

  if(randini) return
 
  ! Adjusting current point to fit the constraints

  init1 = .true.
  call swaptype(n,x,itype,0) ! Initialize swap arrays
  itype = 0
  do while( itype <= ntype )
    itype = itype + 1
    if ( itype == ntype + 1 ) then
      call swaptype(n,x,itype,3) ! Restore arrays for all molecules
      exit
    end if
    if ( restart_from(itype) /= 'none' ) cycle
    call swaptype(n,x,itype,1) ! Set arrays for this type
    write(*,dash3_line)
    write(*,*) ' Molecules of type: ', input_itype(itype)
    write(*,*) ' Adjusting random positions to fit the constraints. '
    i = 0
    call computef(n,x,fx)
    hasbad = .true.
    do while( frest > precision .and. i <= nloop0_type(itype)-1 .and. hasbad)
      i = i + 1 
      write(*,prog1_line)
      call pgencan(n,x,fx)
      call computef(n,x,fx)
      if(frest > precision) then
        write(*,"( a,i6,a,i6 )")'  Fixing bad orientations ... ', i,' of ', nloop0_type(itype)
        movebadprint = .true.
        call movebad(n,x,fx,movebadprint)
      end if
    end do
    write(*,*) ' Restraint-only function value: ', fx
    write(*,*) ' Maximum violation of the restraints: ', frest
    call swaptype(n,x,itype,2) ! Save results for this type
  end do
  init1 = .false.
  write(*,hash3_line)

  ! Deallocate hasfixed array

  deallocate(hasfixed)

  return
end subroutine initial


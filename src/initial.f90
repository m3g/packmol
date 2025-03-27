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
   use cell_indexing, only: setcell
   use compute_data
   use input, only : randini, ntype_with_fixed, fix, chkgrad, avoidoverlap,&
      discale, precision, sidemax, restart_from, input_itype,&
      nloop0_type
   use usegencan
   use ahestetic
   use pbc
   implicit none
   integer :: n, i, j, idatom, iatom, ilubar, ilugan, icart, itype, &
      imol, ntry, cell(3), ic, jc, kc, ifatom, &
      idfatom, iftype, jatom, ioerr, max_guess_try

   double precision :: x(n), beta, gamma, theta, &
      fx, cell_side, rnd, xrnd(3), v1(3), v2(3), v3(3), xcm(3)
   double precision :: scale_rnd_guess
   double precision, parameter :: twopi = 8.d0*datan(1.d0)
   double precision, allocatable :: cm_min(:,:), cm_max(:,:)

   logical :: overlap, movebadprint, hasbad

   character(len=strl) :: record

   ! We need to initialize the move logical variable
   move = .false.

   ! Default status of the function evaluation
   init1 = .false.
   lcellfirst = 0

   ! Initialize the comptype logical array
   comptype(1:ntype_with_fixed) = .true.

   ! Penalty factors for the objective function relative to restrictions
   ! Default values: scale = 1.d2, scale2 = 1.d1

   scale = 1.d0
   scale2 = 1.d-2
   call tobar()

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
   allocate(cm_min(ntype,3), cm_max(ntype,3))
   x(:) = 0.d0
   call restmol(1,0,n,x,fx,.true.)
   sizemin(1:3) = x(1:3) - sidemax
   sizemax(1:3) = x(1:3) + sidemax
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
         xcm = x(ilubar+1:ilubar+3)
         beta = x(ilugan+1)
         gamma = x(ilugan+2)
         theta = x(ilugan+3)
         call eulerrmat(beta,gamma,theta,v1,v2,v3)
         idatom = idfirst(itype) - 1
         do iatom = 1, natoms(itype)
            icart = icart + 1
            idatom = idatom + 1
            call compcart(xcart(icart,1:3),xcm,coor(idatom,1:3),v1,v2,v3)
            fixedatom(icart) = .false.
         end do
      end do
   end do
   if(fix) then
      icart = ntotat - nfixedat
      do iftype = ntype + 1, ntype_with_fixed
         idfatom = idfirst(iftype) - 1
         do ifatom = 1, natoms(iftype)
            idfatom = idfatom + 1
            icart = icart + 1
            xcart(icart,1) = coor(idfatom,1)
            xcart(icart,2) = coor(idfatom,2)
            xcart(icart,3) = coor(idfatom,3)
            fixedatom(icart) = .true.
            ! Check if fixed molecules are compatible with PBC given
            if (using_pbc) then
               do i = 1, 3
                  if (xcart(icart, i) < pbc_min(i) .or. xcart(icart, i) > pbc_max(i)) then
                     write(*,*) "ERROR: Fixed molecule are outside the PBC box:"
                     write(*,*) "   Atom: ", ifatom, " of molecule: ", iftype, " - coordinate: ", i
                     write(*,*) "  ", xcart(icart, i), " not in [", pbc_min(i), ", ", pbc_max(i), "]"
                     write(*,*) "(after translating/rotation the fixed molecule with the given orientation)"
                     stop exit_code_input_error
                  end if
               end do
            end if
         end do
      end do
   end if

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

   ! Rescaling sizemin and sizemax in order to build the patch of cells

   write(*,dash3_line)
   write(*,*) ' Rescaling maximum and minimum coordinates... '
   sizemin(1:3) = 1.d20
   sizemax(1:3) = -1.d20
   ! Maximum and minimum coordinates of fixed molecules
   icart = ntotat - nfixedat
   do itype = ntype + 1, ntype_with_fixed
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
   icart = 0
   do itype = 1, ntype
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
   write(*,*) ' Mininum and maximum coordinates after constraint fitting: '
   write(*,*) '  x: [ ', sizemin(1),', ', sizemax(1), ' ] '
   write(*,*) '  y: [ ', sizemin(2),', ', sizemax(2), ' ] '
   write(*,*) '  z: [ ', sizemin(3),', ', sizemax(3), ' ] '
   sizemin = sizemin - 1.1d0 * radmax
   sizemax = sizemax + 1.1d0 * radmax
   ! When *not* using PBC, actually PBCs are used, but particles
   ! wont interact across the PBC because there is an empty layer
   ! of cells, given by the shift in the sizemin and sizemax dimensions,
   ! above defined. 
   if (.not. using_pbc) then
      pbc_min = sizemin
      pbc_max = sizemax
      pbc_length = pbc_max - pbc_min
   end if 

   ! Computing the size of the patches
   write(*,*) ' Computing size of patches... '
   cell_side = discale * (1.01d0 * radmax)
   do i = 1, 3
      ncells(i) = max(1,floor(pbc_length(i)/cell_side))
      cell_length(i) = pbc_length(i) / ncells(i)
   end do
   write(*,*) ' Number of cells in each direction and cell sides: '
   write(*,*) '  x: ', ncells(1), ' cells of size ', cell_length(1)
   write(*,*) '  y: ', ncells(2), ' cells of size ', cell_length(2)
   write(*,*) '  z: ', ncells(3), ' cells of size ', cell_length(3)
   write(*,"(a, 3(f10.5))") '  Cell-system length: ', pbc_length(1:3) 

   ! Allocate arrays depending on the number of cells
   allocate(latomfirst(ncells(1),ncells(2),ncells(3)))
   allocate(latomfix(ncells(1),ncells(2),ncells(3)))
   allocate(lcellnext(ncells(1)*ncells(2)*ncells(3)))
   allocate(empty_cell(ncells(1),ncells(2),ncells(3)))

   ! Reseting linked lists arrays
   latomfix(:,:,:) = 0
   latomfirst(:,:,:) = 0
   latomnext(:) = 0
   empty_cell(:,:,:) = .true.

   ! If there are fixed molecules, add them permanently to the latomfix array
   if(fix) then
      write(*,*) ' Add fixed molecules to permanent arrays... '
      icart = ntotat - nfixedat
      do iftype = ntype + 1, ntype_with_fixed
         do ifatom = 1, natoms(iftype)
            icart = icart + 1
            call setcell(xcart(icart,:),cell)
            latomnext(icart) = latomfix(cell(1),cell(2),cell(3))
            latomfix(cell(1),cell(2),cell(3)) = icart
            latomfirst(cell(1),cell(2),cell(3)) = icart
            ibtype(icart) = iftype
            ibmol(icart) = 1
         end do
      end do
   end if

   ! Reseting mass centers to be within the regions

   write(*,*) ' Reseting center of mass... '
   cm_min = 1.d20
   cm_max = -1.d20
   icart = 0
   do itype = 1, ntype
      do imol = 1, nmols(itype)
         xcm(:) = 0.d0
         do iatom = 1, natoms(itype)
            icart = icart + 1
            xcm(:) = xcm(:) + xcart(icart,1:3)
         end do
         xcm(:) = xcm(:) / natoms(itype)
         cm_min(itype,:) = dmin1(cm_min(itype,:),xcm(:))
         cm_max(itype,:) = dmax1(cm_max(itype,:),xcm(:))
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
      max_guess_try = 1
      scale_rnd_guess = 1.5d0 ! move guess outside the constraints/box
   else
      max_guess_try = 20
      scale_rnd_guess = 1.d0
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
            do while((fx > precision) .and. (ntry < max_guess_try))
               ntry = ntry + 1
               call random_number(xrnd)
               x(ilubar+1:ilubar+3) = &
                  cm_min(itype,:) + xrnd(:)*(cm_max(itype,:)-cm_min(itype,:)) * scale_rnd_guess
               call restmol(itype,ilubar,n,x,fx,.false.)
            end do
         else
            fx = 1.d0
            ntry = 0
            overlap = .false.
            do while(overlap .or. (fx > precision) .and. (ntry < max_guess_try))
               overlap = .false.
               ntry = ntry + 1
               call random_number(xrnd)
               x(ilubar+1:ilubar+3) = &
                  cm_min(itype,:) + xrnd(:)*(cm_max(itype,:)-cm_min(itype,:)) * scale_rnd_guess
               if(fix) then
                  call setcell(x(ilubar+1:ilubar+3),cell)
                  icell: do ic = -1, 1 
                     do jc = -1, 1
                        do kc = -1, 1
                           if(latomfix(cell_ind(cell(1)+ic,ncells(1)),&
                                       cell_ind(cell(2)+jc,ncells(2)),&
                                       cell_ind(cell(3)+kc,ncells(3)) &
                                      ) /= 0) then
                              overlap = .true.
                              exit icell
                           end if
                        end do
                     end do
                  end do icell
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
      cell_side = discale * (1.01d0 * radmax)
      do i = 1, 3
         ncells(i) = max(1,floor(pbc_length(i)/cell_side))
         cell_length(i) = pbc_length(i) / ncells(i)
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

   ! Deallocate allocated arrays
   deallocate(cm_min, cm_max)

   return
end subroutine initial


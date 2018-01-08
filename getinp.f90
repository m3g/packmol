!  
!  Written by Leandro Martínez, 2009-2011.
!  Copyright (c) 2009-2011, Leandro Martínez, Jose Mario Martinez,
!  Ernesto G. Birgin.
!
! Subroutine getinp: subroutine that reads the input file
!

subroutine getinp()

  use sizes
  use compute_data, only : ntype, natoms, idfirst, nmols, ityperest, coor, restpars
  use input
  use usegencan

  implicit none
  integer :: i, k, ii, iarg, iline, idatom, iatom, in, lixo, irest, itype, itest,&
             imark, strlength, ioerr
  double precision :: clen
  character(len=200) :: record, blank

  ! Clearing the blank character arrays

  do i = 1, 200
    blank(i:i) = ' '
  end do

  ! Getting random seed and optional optimization parameters if set

  seed = 1234567
  randini = .false.
  check = .false.
  chkgrad = .false.
  iprint1 = 2
  iprint2 = 2
  discale = 1.1d0
  writeout = 10
  maxit = 20
  nloop = 0
  nloop_all = 0
  movefrac = 0.05
  movebadrandom = .false.
  precision = 1.d-2
  writebad = .false.
  add_amber_ter = .false.
  add_box_sides = .false.
  add_sides_fix = 0.d0
  sidemax = 1000.d0
  ioerr = 0
  avoidoverlap = .true.
  packall = .false.
  do i = 1, nlines
    if(keyword(i,1).eq.'seed') then
      read(keyword(i,2),*,iostat=ioerr) seed
      if ( ioerr /= 0 ) exit
      if ( seed == -1 ) call seed_from_time(seed)
    else if(keyword(i,1).eq.'randominitialpoint') then
      randini = .true.
    else if(keyword(i,1).eq.'check') then
      check = .true.
    else if(keyword(i,1).eq.'writebad') then
      writebad = .true.
    else if(keyword(i,1).eq.'precision') then
      read(keyword(i,2),*,iostat=ioerr) precision
      if ( ioerr /= 0 ) exit
      write(*,*) ' Optional precision set: ', precision
    else if(keyword(i,1).eq.'movefrac') then
      read(keyword(i,2),*,iostat=ioerr) movefrac
      if ( ioerr /= 0 ) exit
      write(*,*) ' Optional movefrac set: ', movefrac
    else if(keyword(i,1).eq.'movebadrandom') then
      movebadrandom = .true.
      write(*,*) ' Will move randomly bad molecues (movebadrandom) '
    else if(keyword(i,1).eq.'chkgrad') then
      chkgrad = .true.
    else if(keyword(i,1).eq.'writeout') then
      read(keyword(i,2),*,iostat=ioerr) writeout
      if ( ioerr /= 0 ) exit
      write(*,*) ' Output frequency: ', writeout
    else if(keyword(i,1).eq.'maxit') then
      read(keyword(i,2),*,iostat=ioerr) maxit
      if ( ioerr /= 0 ) exit
      write(*,*) ' User defined GENCAN number of iterations: ', maxit
    else if(keyword(i,1).eq.'nloop') then
      read(keyword(i,2),*,iostat=ioerr) nloop
      if ( ioerr /= 0 ) exit
      write(*,*) ' User defined number of GENCAN loops: ', nloop
    else if(keyword(i,1).eq.'nloop_all') then
      read(keyword(i,2),*,iostat=ioerr) nloop_all
      if ( ioerr /= 0 ) exit
      write(*,*) ' User defined diferential number of GENCAN loops for All-together packing: ', nloop_all
    else if(keyword(i,1).eq.'discale') then
      read(keyword(i,2),*,iostat=ioerr) discale
      if ( ioerr /= 0 ) exit
      write(*,*) ' Optional initial tolerance scale: ', discale
    else if(keyword(i,1).eq.'sidemax') then
      read(keyword(i,2),*,iostat=ioerr) sidemax
      if ( ioerr /= 0 ) exit
      write(*,*) ' User set maximum system dimensions: ', sidemax
    else if(keyword(i,1).eq.'fbins') then
      read(keyword(i,2),*,iostat=ioerr) fbins
      if ( ioerr /= 0 ) exit
      write(*,*) ' User set linked-cell bin parameter: ', fbins
    else if(keyword(i,1).eq.'add_amber_ter') then
      add_amber_ter = .true.
      write(*,*) ' Will add the TER flag between molecules. '
    else if(keyword(i,1).eq.'avoid_overlap') then
      if ( keyword(i,2).eq.'yes') then
        avoidoverlap = .true.
        write(*,*) ' Will avoid overlap to fixed molecules at initial point. '
      else
        avoidoverlap = .false.
        write(*,*) ' Will NOT avoid overlap to fixed molecules at initial point. '
      end if
    else if(keyword(i,1).eq.'packall') then
      packall = .true.
      write(*,*) ' Will pack all molecule types from the beginning. '
    else if(keyword(i,1).eq.'add_box_sides') then
      add_box_sides = .true.
      write(*,*) ' Will print BOX SIDE informations. '
      read(keyword(i,2),*,iostat=ioerr) add_sides_fix
      if ( ioerr /= 0 ) then
        ioerr = 0
        cycle
      end if
      write(*,*) ' Will sum ', add_sides_fix,' to each side length on print'
    else if(keyword(i,1).eq.'iprint1') then 
      read(keyword(i,2),*,iostat=ioerr) iprint1
      if ( ioerr /= 0 ) exit
      write(*,*) ' Optional printvalue 1 set: ', iprint1
    else if(keyword(i,1).eq.'iprint2') then 
      read(keyword(i,2),*,iostat=ioerr) iprint2
      if ( ioerr /= 0 ) exit
      write(*,*) ' Optional printvalue 2 set: ', iprint2
    else if( keyword(i,1) /= 'tolerance' .and. &
             keyword(i,1) /= 'structure' .and. &
             keyword(i,1) /= 'end' .and. &
             keyword(i,1) /= 'atoms' .and. &
             keyword(i,1) /= 'output' .and. &
             keyword(i,1) /= 'filetype' .and. &
             keyword(i,1) /= 'number' .and. &
             keyword(i,1) /= 'inside' .and. &
             keyword(i,1) /= 'outside' .and. &
             keyword(i,1) /= 'fixed' .and. &
             keyword(i,1) /= 'center' .and. &
             keyword(i,1) /= 'centerofmass' .and. &
             keyword(i,1) /= 'over' .and. &
             keyword(i,1) /= 'below' .and. &
             keyword(i,1) /= 'constrain_rotation' .and. &
             keyword(i,1) /= 'radius' .and. &
             keyword(i,1) /= 'resnumbers' .and. &
             keyword(i,1) /= 'changechains' .and. &
             keyword(i,1) /= 'chain' .and. &
             keyword(i,1) /= 'discale' .and. &
             keyword(i,1) /= 'maxit' .and. &
             keyword(i,1) /= 'movebadrandom' .and. &
             keyword(i,1) /= 'add_amber_ter' .and. &
             keyword(i,1) /= 'sidemax' .and. &
             keyword(i,1) /= 'seed' .and. &
             keyword(i,1) /= 'randominitialpoint' .and. &
             keyword(i,1) /= 'restart_from' .and. &
             keyword(i,1) /= 'restart_to' .and. &
             keyword(i,1) /= 'nloop' .and. &
             keyword(i,1) /= 'nloop_all' .and. &
             keyword(i,1) /= 'writeout' .and. &
             keyword(i,1) /= 'writebad' .and. &
             keyword(i,1) /= 'check' .and. &
             keyword(i,1) /= 'iprint1' .and. &
             keyword(i,1) /= 'iprint2' .and. &
             keyword(i,1) /= 'chkgrad' ) then
      write(*,*) ' ERROR: Keyword not recognized: ', trim(keyword(i,1))
      stop
    end if
  end do
  if ( ioerr /= 0 ) then
    write(*,*) ' ERROR: Some optional keyword was not used correctly: ', trim(keyword(i,1))
    stop
  end if
  write(*,*) ' Seed for random number generator: ', seed
  call init_random_number(seed)

  ! Checking for the name of the output file to be created

  xyzout = '####'
  do iline = 1, nlines
    if(keyword(iline,1).eq.'output') then
      xyzout = keyword(iline,2)
      xyzout = xyzout(1:strlength(xyzout))
    end if
  end do
  if(xyzout(1:4) == '####') then
    write(*,*)' ERROR: Output file not (correctly?) specified. '
    stop
  end if
  write(*,*)' Output file: ', xyzout(1:strlength(xyzout))

  ! Reading structure files

  itype = 0
  do iline = 1, nlines
    if(keyword(iline,1).eq.'structure') then
      itype = itype + 1
     
      record = keyword(iline,2)
      write(*,*) ' Reading coordinate file: ', record(1:strlength(record))

      ! Reading pdb input files

      if(pdb) then
        name(itype) = record(1:strlength(record))
        record = keyword(iline,2)
        pdbfile(itype) = record(1:80)
        idfirst(itype) = 1
        do ii = itype - 1, 1, -1
          idfirst(itype) = idfirst(itype) + natoms(ii)
        end do
        open(10,file=keyword(iline,2),status='old',iostat=ioerr)
        if ( ioerr /= 0 ) call failopen(keyword(iline,2))
        record(1:6) = '######'
        do while(record(1:4).ne.'ATOM'.and.record(1:6).ne.'HETATM')
          read(10,"( a200 )") record
        end do
        idatom = idfirst(itype) - 1
        do while(idatom.lt.natoms(itype)+idfirst(itype)-1)
          if(record(1:4).eq.'ATOM'.or.record(1:6).eq.'HETATM') then
            idatom = idatom + 1
            amass(idatom) = 1.d0
            read(record,"( t31,f8.3,t39,f8.3,t47,f8.3 )",iostat=ioerr) &
                 (coor(idatom,k),k=1,3)
            if( ioerr /= 0 ) then
              record = keyword(iline,2) 
              write(*,*) ' ERROR: Failed to read coordinates from', &
                         ' file: ', record(1:strlength(record))
              write(*,*) ' Probably the coordinates are not in', &
                         ' standard PDB file format. '
              write(*,*) ' Standard PDB format specifications', &
                         ' can be found at: '
              write(*,*) ' www.rcsb.org/pdb '
              stop
            end if

            ! This only tests if residue numbers can be read, they are used 
            ! only for  output

            read(record(23:26),*,iostat=ioerr) itest
            if( ioerr /= 0 ) then
              record = pdbfile(itype)
              write(*,*) ' ERROR: Failed reading residue number',&
                         ' from PDB file: ',record(1:strlength(record))
              write(*,*) ' Residue numbers are integers that',&
                         ' must be within columns 23 and 26. '
              write(*,*) ' Other characters within these columns',&
                         ' will cause input/output errors. '
              write(*,*) ' Standard PDB format specifications',&
                         ' can be found at: '
              write(*,*) ' www.rcsb.org/pdb '
              stop
            end if   
          end if
          read(10,"( a200 )",iostat=ioerr) record
          if ( ioerr /= 0 ) exit
        end do
        close(10)
      end if

      ! Reading tinker input files

      if(tinker) then
        open(10,file=keyword(iline,2),status='old',iostat=ioerr)
        if ( ioerr /= 0 ) call failopen(keyword(iline,2))
        idfirst(itype) = 1
        do ii = itype - 1, 1, -1
          idfirst(itype) = idfirst(itype) + natoms(ii)
        end do
        record = keyword(iline,2)
        call setcon(record(1:64),idfirst(itype))
        open(10,file = keyword(iline,2), status = 'old')
        record = blank
        do while(record.le.blank)
          read(10,"( a200 )") record
        end do
        i = 1
        do while(record(i:i).le.' ')
          i = i + 1
          if ( i > 200 ) exit
        end do
        iarg = i
        if ( i < 200 ) then
          do while(record(i:i).gt.' ')
            i = i + 1
            if ( i > 200 ) exit
          end do
        end if
        read(record(iarg:i-1),*) natoms(itype)
        if ( i < 200 ) then
          do while(record(i:i).le.' ')
            i = i + 1
            if ( i > 200 ) exit
          end do
        end if
        iarg = i
        if ( i < 200 ) then
          do while(record(i:i).gt.' ')
            i = i + 1
            if ( i > 200 ) exit
          end do
        end if
        read(record(iarg:i-1),"( a200 )") name(itype)
        record = name(itype)
        name(itype) = record(1:strlength(record))
        if(name(itype).lt.' ') name(itype) = 'Without_title'
        idatom = idfirst(itype) - 1
        do iatom = 1, natoms(itype)
          idatom = idatom + 1
          record = blank
          do while(record.le.blank)
            read(10,"( a200 )") record
          end do
          i = 1
          do while(record(i:i).le.' ')
            i = i + 1
            if ( i > 200 ) exit
          end do
          iarg = i
          if ( i < 200 ) then
            do while(record(i:i).gt.' ')
              i = i + 1
              if ( i > 200 ) exit
            end do
          end if
          read(record(iarg:i-1),*) in
          if ( i < 200 ) then
            do while(record(i:i).le.' ')
              i = i + 1
              if ( i > 200 ) exit
            end do
          end if
          iarg = i
          if ( i < 200 ) then
            do while(record(i:i).gt.' ')
              i = i + 1
              if ( i > 200 ) exit
            end do
          end if
          read(record(iarg:i-1),*) ele(idatom)    
          read(record(i:200),*) (coor(idatom,k), k = 1, 3),&
               (nconnect(idatom, k), k = 1, maxcon(idatom))
          amass(idatom) = 1.d0
        end do
        close(10)
      end if

      ! Reading xyz input files

      if(xyz) then
        open(10,file=keyword(iline,2),status='old',iostat=ioerr)
        if ( ioerr /= 0 ) call failopen(keyword(iline,2))
        read(10,*) natoms(itype)
        read(10,"( a200 )") name(itype)
        if(name(itype).lt.' ') name(itype) = 'Without_title'
        idfirst(itype) = 1
        do ii = itype - 1, 1, -1
          idfirst(itype) = idfirst(itype) + natoms(ii)
        end do
        idatom = idfirst(itype) - 1
        do iatom = 1, natoms(itype)
          idatom = idatom + 1
          record = blank
          read(10,"( a200 )") record
          read(record,*) ele(idatom), (coor(idatom,k),k=1,3)
          amass(idatom) = 1.d0
        end do
        close(10)
      end if
      
      ! Reading moldy input files
  
      if(moldy) then
        open(10,file=keyword(iline,2), status ='old',iostat=ioerr)
        if ( ioerr /= 0 ) call failopen(keyword(iline,2))
        read(10,*) name(itype), nmols(itype)
        natoms(itype) = 0
        do while(.true.)
          read(10,"( a200 )",iostat=ioerr) record
          if ( ioerr /= 0 ) exit
          if(record.gt.' '.and.record(1:3).ne.'end') & 
            natoms(itype) = natoms(itype) + 1
        end do
        close(10)
        idfirst(itype) = 1
        do ii = itype - 1, 1, -1
          idfirst(itype) = idfirst(itype) + natoms(ii)
        end do
        open(10,file=keyword(iline,2),status='old')
        read(10,"( a200 )") record
        idatom = idfirst(itype) - 1
        do iatom = 1, natoms(itype)
          idatom = idatom + 1
          read(10,"( a200 )") record
          read(record,*) lixo, (coor(idatom,k), k = 1, 3),&
                       amass(idatom), charge(idatom), ele(idatom)
        end do
        close(10)
      end if
    end if

  end do
  ntype = itype

  write(*,*) ' Number of independent structures: ', ntype
  write(*,*) ' The structures are: '

  do itype = 1, ntype
    record = name(itype)
    write(*,*) ' Structure ', itype, ':',record(1:strlength(record)),&
               '(',natoms(itype),' atoms)'
  end do
  if(nloop.eq.0) nloop = 200*ntype
  if(nloop_all.eq.0) nloop_all = nloop
      
  ! Reading the restrictions that were set

  irest = 0
  ioerr = 0
  do iline = 1, nlines

    if(keyword(iline,1).eq.'fixed') then
      irest = irest + 1
      irestline(irest) = iline
      ityperest(irest) = 1
      read(keyword(iline,2),*,iostat=ioerr) restpars(irest,1)
      read(keyword(iline,3),*,iostat=ioerr) restpars(irest,2)
      read(keyword(iline,4),*,iostat=ioerr) restpars(irest,3)
      read(keyword(iline,5),*,iostat=ioerr) restpars(irest,4)
      read(keyword(iline,6),*,iostat=ioerr) restpars(irest,5)
      read(keyword(iline,7),*,iostat=ioerr) restpars(irest,6)
    end if
  
    if(keyword(iline,1).eq.'inside') then
      irest = irest + 1
      irestline(irest) = iline
      if(keyword(iline,2).eq.'cube') then
        ityperest(irest) = 2
        read(keyword(iline,3),*,iostat=ioerr) restpars(irest,1)
        read(keyword(iline,4),*,iostat=ioerr) restpars(irest,2)
        read(keyword(iline,5),*,iostat=ioerr) restpars(irest,3)
        read(keyword(iline,6),*,iostat=ioerr) restpars(irest,4)
      else if(keyword(iline,2).eq.'box') then
        ityperest(irest) = 3
        read(keyword(iline,3),*,iostat=ioerr) restpars(irest,1)
        read(keyword(iline,4),*,iostat=ioerr) restpars(irest,2)
        read(keyword(iline,5),*,iostat=ioerr) restpars(irest,3)
        read(keyword(iline,6),*,iostat=ioerr) restpars(irest,4)
        read(keyword(iline,7),*,iostat=ioerr) restpars(irest,5)
        read(keyword(iline,8),*,iostat=ioerr) restpars(irest,6)
      else if(keyword(iline,2).eq.'sphere') then
        ityperest(irest) = 4
        read(keyword(iline,3),*,iostat=ioerr) restpars(irest,1)
        read(keyword(iline,4),*,iostat=ioerr) restpars(irest,2)
        read(keyword(iline,5),*,iostat=ioerr) restpars(irest,3)
        read(keyword(iline,6),*,iostat=ioerr) restpars(irest,4)
      else if(keyword(iline,2).eq.'ellipsoid') then
        ityperest(irest) = 5
        read(keyword(iline,3),*,iostat=ioerr) restpars(irest,1)
        read(keyword(iline,4),*,iostat=ioerr) restpars(irest,2)
        read(keyword(iline,5),*,iostat=ioerr) restpars(irest,3)
        read(keyword(iline,6),*,iostat=ioerr) restpars(irest,4)
        read(keyword(iline,7),*,iostat=ioerr) restpars(irest,5)
        read(keyword(iline,8),*,iostat=ioerr) restpars(irest,6)
        read(keyword(iline,9),*,iostat=ioerr) restpars(irest,7)
      else if(keyword(iline,2).eq.'cylinder') then
        ityperest(irest) = 12
        read(keyword(iline,3),*,iostat=ioerr) restpars(irest,1) 
        read(keyword(iline,4),*,iostat=ioerr) restpars(irest,2)
        read(keyword(iline,5),*,iostat=ioerr) restpars(irest,3)
        read(keyword(iline,6),*,iostat=ioerr) restpars(irest,4)
        read(keyword(iline,7),*,iostat=ioerr) restpars(irest,5)
        read(keyword(iline,8),*,iostat=ioerr) restpars(irest,6)
        read(keyword(iline,9),*,iostat=ioerr) restpars(irest,7)  
        read(keyword(iline,10),*,iostat=ioerr) restpars(irest,9)  
        restpars(irest,8) = restpars(irest,4)**2 + &
                            restpars(irest,5)**2 + &
                            restpars(irest,6)**2
        if(restpars(irest,8).lt.1.d-10) then
           write(*,*) ' ERROR: The norm of the director vector', &
                      ' of the cylinder constraint cannot be zero.'
           ioerr = 1
        else
          clen = dsqrt(restpars(irest,8))
          restpars(irest,4) = restpars(irest,4) / clen
          restpars(irest,5) = restpars(irest,5) / clen
          restpars(irest,6) = restpars(irest,6) / clen
        end if                                     
      else
        ioerr = 1
      end if
    end if

    if(keyword(iline,1).eq.'outside') then
      irest = irest + 1
      irestline(irest) = iline
      if(keyword(iline,2).eq.'cube') then
        ityperest(irest) = 6
        read(keyword(iline,3),*,iostat=ioerr) restpars(irest,1)
        read(keyword(iline,4),*,iostat=ioerr) restpars(irest,2)
        read(keyword(iline,5),*,iostat=ioerr) restpars(irest,3)
        read(keyword(iline,6),*,iostat=ioerr) restpars(irest,4)
      else if(keyword(iline,2).eq.'box') then
        ityperest(irest) = 7
        read(keyword(iline,3),*,iostat=ioerr) restpars(irest,1)
        read(keyword(iline,4),*,iostat=ioerr) restpars(irest,2)
        read(keyword(iline,5),*,iostat=ioerr) restpars(irest,3)
        read(keyword(iline,6),*,iostat=ioerr) restpars(irest,4)
        read(keyword(iline,7),*,iostat=ioerr) restpars(irest,5)
        read(keyword(iline,8),*,iostat=ioerr) restpars(irest,6)
      else if(keyword(iline,2).eq.'sphere') then
        ityperest(irest) = 8
        read(keyword(iline,3),*,iostat=ioerr) restpars(irest,1)
        read(keyword(iline,4),*,iostat=ioerr) restpars(irest,2)
        read(keyword(iline,5),*,iostat=ioerr) restpars(irest,3)
        read(keyword(iline,6),*,iostat=ioerr) restpars(irest,4)
      else if(keyword(iline,2).eq.'ellipsoid') then
        ityperest(irest) = 9
        read(keyword(iline,3),*,iostat=ioerr) restpars(irest,1)
        read(keyword(iline,4),*,iostat=ioerr) restpars(irest,2)
        read(keyword(iline,5),*,iostat=ioerr) restpars(irest,3)
        read(keyword(iline,6),*,iostat=ioerr) restpars(irest,4)
        read(keyword(iline,7),*,iostat=ioerr) restpars(irest,5)
        read(keyword(iline,8),*,iostat=ioerr) restpars(irest,6)
        read(keyword(iline,9),*,iostat=ioerr) restpars(irest,7)
      else if(keyword(iline,2).eq.'cylinder') then
        ityperest(irest) = 13
        read(keyword(iline,3),*,iostat=ioerr) restpars(irest,1) 
        read(keyword(iline,4),*,iostat=ioerr) restpars(irest,2)
        read(keyword(iline,5),*,iostat=ioerr) restpars(irest,3)
        read(keyword(iline,6),*,iostat=ioerr) restpars(irest,4)
        read(keyword(iline,7),*,iostat=ioerr) restpars(irest,5)
        read(keyword(iline,8),*,iostat=ioerr) restpars(irest,6)
        read(keyword(iline,9),*,iostat=ioerr) restpars(irest,7)
        read(keyword(iline,10),*,iostat=ioerr) restpars(irest,9)
        restpars(irest,8) = restpars(irest,4)**2 + &
                            restpars(irest,5)**2 + &
                            restpars(irest,6)**2
        if(restpars(irest,8).lt.1.d-10) then
           write(*,*) ' ERROR: The norm of the director vector',&
                      ' of the cylinder constraint cannot be zero.'
           ioerr = 1
        else
          clen = dsqrt(restpars(irest,8))
          restpars(irest,4) = restpars(irest,4) / clen
          restpars(irest,5) = restpars(irest,5) / clen
          restpars(irest,6) = restpars(irest,6) / clen
        end if                                     
      else
        ioerr = 1
      end if
    end if

    if(keyword(iline,1).eq.'over') then
      irest = irest + 1
      irestline(irest) = iline
      ityperest(irest) = 10
      read(keyword(iline,3),*,iostat=ioerr) restpars(irest,1)
      read(keyword(iline,4),*,iostat=ioerr) restpars(irest,2)
      read(keyword(iline,5),*,iostat=ioerr) restpars(irest,3)
      read(keyword(iline,6),*,iostat=ioerr) restpars(irest,4)
      if(keyword(iline,2).ne.'plane') ioerr = 1
    end if

    if(keyword(iline,1).eq.'below') then
      irest = irest + 1
      irestline(irest) = iline
      ityperest(irest) = 11
      read(keyword(iline,3),*,iostat=ioerr) restpars(irest,1)
      read(keyword(iline,4),*,iostat=ioerr) restpars(irest,2)
      read(keyword(iline,5),*,iostat=ioerr) restpars(irest,3)
      read(keyword(iline,6),*,iostat=ioerr) restpars(irest,4)
      if(keyword(iline,2).ne.'plane') ioerr = 1 
    end if

    if ( ioerr /= 0 ) then
      write(*,*) ' ERROR: Some restriction is not set correctly. '
      stop
    end if

  end do
  nrest = irest 
  write(*,*) ' Total number of restrictions: ', nrest

  ! Getting the tolerance

  ioerr = 1
  dism = -1.d0
  do iline = 1, nlines
    if(keyword(iline,1).eq.'tolerance') then
      read(keyword(iline,2),*,iostat=ioerr) dism
      if ( ioerr /= 0 ) then
        write(*,*) ' ERROR: Failed reading tolerance. '
        stop
      end if
      exit
    end if
  end do
  if ( ioerr /= 0 ) then
    write(*,*) ' ERROR: Overall tolerance not set. Use, for example: tolerance 2.0 '
    stop
  end if
  write(*,*) ' Distance tolerance: ', dism

  ! Assigning the input lines that correspond to each structure

  itype = 0
  iline = 0
  do while(iline < nlines)
    iline = iline + 1
    if(keyword(iline,1).eq.'structure') then
      itype = itype + 1
      linestrut(itype,1) = iline
      iline = iline + 1
      do while(keyword(iline,1).ne.'end'.or.&
               keyword(iline,2).ne.'structure')
        if(keyword(iline,1) == 'structure'.or.&
           iline == nlines) then
          write(*,*) ' Input file ERROR: structure specification',&
                     ' not ending with "end structure"'
          stop
        end if
        iline = iline + 1
      end do
      linestrut(itype,2) = iline 
    end if
  end do

  ! If pdb files, get the type of residue numbering output for each
  ! molecule

  if(pdb) then             
    do itype = 1, ntype
      resnumbers(itype) = -1
      changechains(itype) = .false.
      chain(itype) = "#"
      do iline = 1, nlines
        if(iline.gt.linestrut(itype,1).and.&
             iline.lt.linestrut(itype,2)) then
          if(keyword(iline,1).eq.'changechains') then
            changechains(itype) = .true.
          end if
          if(keyword(iline,1).eq.'resnumbers') then
            read(keyword(iline,2),*) resnumbers(itype)
          end if
          if(keyword(iline,1).eq.'chain') then
            read(keyword(iline,2),*) chain(itype)
          end if
        end if
      end do
      if ( resnumbers(itype) == -1 ) then
        write(*,*) ' Warning: Type of residue numbering not',&
                   ' set for structure ',itype
        call setrnum(pdbfile(itype),imark)
        if(imark.eq.1) resnumbers(itype) = 0
        if(imark.gt.1) resnumbers(itype) = 1 
      end if
      write(*,*) ' Residue numbering set for structure ',itype,':',&
                 resnumbers(itype)
      write(*,*) ' Swap chains of molecules of structure ',&
                 itype,':', changechains(itype) 
      if ( chain(itype) /= "#" ) then
        write(*,*) ' Specific chain identifier set for structure ',itype,':',chain(itype)
      end if
      if ( chain(itype) /= "#" .and. changechains(itype) ) then
        write(*,*) " ERROR: 'changechains' and 'chain' input parameters are not compatible "
        write(*,*) "        for a single structure. "
        stop
      end if
    end do
  end if

  ! Write the number of molecules of each type 

  do itype = 1, ntype
    write(*,*) ' Number of molecules of type ', itype, ': ', nmols(itype)
    if(pdb.and.nmols(itype).gt.9999) then
      write(*,*) ' Warning: There will be more than 9999 molecules of type ',itype
      write(*,*) '          Residue numbering is reset after 9999. '
      if ( chain(itype) == "#" ) then
        write(*,*) ' Each set be will be assigned a different chain in the PDB output file. '
      end if
    end if
  end do

  ! Checking if restart files will be used for each structure or for the whole system

  restart_from(0) = "none"
  restart_to(0) = "none"
  do itype = 1, ntype
    restart_from(itype) = "none"
    restart_to(itype) = "none"
  end do
  lines: do iline = 1, nlines
    if ( keyword(iline,1) == 'restart_from' ) then
      do itype = 1, ntype
        if(iline.gt.linestrut(itype,1).and.&
           iline.lt.linestrut(itype,2)) then
          restart_from(itype) = keyword(iline,2)
          cycle lines
        end if
      end do
      if( restart_from(0) == 'none' ) then
        restart_from(0) = keyword(iline,2)
      else
        write(*,*) ' ERROR: More than one definition of restart_from file for all system. '
        stop
      end if
    end if
    if ( keyword(iline,1) == 'restart_to' ) then
      do itype = 1, ntype
        if(iline.gt.linestrut(itype,1).and.&
           iline.lt.linestrut(itype,2)) then
          restart_to(itype) = keyword(iline,2)
          cycle lines
        end if
      end do
      if( restart_to(0) == 'none' ) then
        restart_to(0) = keyword(iline,2)
      else
        write(*,*) ' ERROR: More than one definition of restart_to file for all system. '
        stop
      end if
    end if
  end do lines
 
  return
end subroutine getinp

!
! Subroutine that stops if failed to open file
!

subroutine failopen(record)
  character(len=200) :: record
  write(*,*) 
  write(*,*) ' ERROR: Could not open file. '
  write(*,*) '        Could not find file: ',trim(record)
  write(*,*) '        Please check if all the input and structure ' 
  write(*,*) '        files are in the current directory or if the' 
  write(*,*) '        correct paths are provided.'
  write(*,*) 
  stop 
end subroutine failopen

!
! Subroutine that checks if a pdb structure has one or more than
! one residue
!

subroutine setrnum(file,nres)

  implicit none
  integer :: iread, ires, ireslast, nres, ioerr
  character(len=80) :: file
  character(len=200) :: record

  open(10,file=file,status='old')
  iread = 0
  nres = 1
  do while(nres.eq.1)
    read(10,"( a200 )",iostat=ioerr) record
    if ( ioerr /= 0 ) exit
    if(record(1:4).eq.'ATOM'.or.record(1:6).eq.'HETATM') then
      read(record(23:26),*,iostat=ioerr) ires
      if ( ioerr /= 0 ) cycle
      iread = iread + 1
      if(iread.gt.1) then
        if(ires.ne.ireslast) then
          nres = 2
          close(10)
          return
        end if
      end if
      ireslast = ires
    end if
  end do
  close(10)

  return
end subroutine setrnum

!
!  Subroutine that computes de number of connections of each atom
!  for tinker xyz files
! 

subroutine setcon(xyzfile,idfirst)

  use sizes
  use input, only : maxcon
  implicit none

  integer :: idfirst
  integer :: natoms, idatom, iatom, ic, i 
  character(len=64) :: xyzfile
  character(len=120) :: record

  open(10, file = xyzfile, status='old')
  read(10,*) natoms
  idatom = idfirst - 1
  do iatom = 1, natoms
    idatom = idatom + 1
    read(10,"( a120 )") record
    ic = 0
    do i = 1, 119
      if(record(i:i).gt.' '.and.record(i+1:i+1).le.' ') ic = ic + 1
    end do
    maxcon(idatom) = ic - 5
  end do
  close(10)

  return
end subroutine setcon

!
! Subroutine getkeywords: gets keywords and values from the input
!                         file in a more robust way
!

subroutine getkeywords()

  use sizes
  use input, only : keyword, nlines, inputfile
  implicit none
  character(len=200) :: record
  integer :: iline, i, j, ilast, ival, ioerr

  ! Clearing keyword array

  do i = 1, nlines
    do j = 1, maxkeywords
      keyword(i,j) = 'none'
    end do            
  end do

  ! Filling keyword array

  do iline = 1, nlines
    read(inputfile(iline),"( a200 )",iostat=ioerr) record
    if ( ioerr /= 0 ) exit
    i = 0
    ival = 0
    do while(i < 200)
      i = i + 1
      ilast = i
      do while(record(i:i) > ' '.and. i < 200)
        i = i + 1
      end do
      if(i.gt.ilast) then
        ival = ival + 1
        keyword(iline,ival) = record(ilast:i)
      end if
    end do
  end do

  return
end subroutine getkeywords

! Subroutine that returns the chain character given an index

subroutine chainc(i,chain)

  implicit none
  integer :: i
  character :: chain

  if(i.eq.0) chain = ' '
  if(i.eq.1) chain = 'A'
  if(i.eq.2) chain = 'B'
  if(i.eq.3) chain = 'C'
  if(i.eq.4) chain = 'D'
  if(i.eq.5) chain = 'E'
  if(i.eq.6) chain = 'F'
  if(i.eq.7) chain = 'G'
  if(i.eq.8) chain = 'H'
  if(i.eq.9) chain = 'I'
  if(i.eq.10) chain = 'J'
  if(i.eq.11) chain = 'K'
  if(i.eq.12) chain = 'L'
  if(i.eq.13) chain = 'M'
  if(i.eq.14) chain = 'N'
  if(i.eq.15) chain = 'O'
  if(i.eq.16) chain = 'P'
  if(i.eq.17) chain = 'Q'
  if(i.eq.18) chain = 'R'
  if(i.eq.19) chain = 'S'
  if(i.eq.20) chain = 'T'
  if(i.eq.21) chain = 'U'
  if(i.eq.22) chain = 'V'
  if(i.eq.23) chain = 'W'
  if(i.eq.24) chain = 'X'
  if(i.eq.25) chain = 'Y'
  if(i.eq.26) chain = 'Z'
  if(i.eq.27) chain = '1'
  if(i.eq.28) chain = '2'
  if(i.eq.29) chain = '3'
  if(i.eq.30) chain = '4'
  if(i.eq.31) chain = '5'
  if(i.eq.32) chain = '6'
  if(i.eq.33) chain = '7'
  if(i.eq.34) chain = '8'
  if(i.eq.35) chain = '9'
  if(i.eq.36) chain = '0'
  if(i.gt.36) chain = '#'

  return
end subroutine chainc

! Subroutine that clears a character variable

subroutine clear(record)
      
  integer :: i
  character(len=80) :: record

  do i = 1, 80
    record(i:i) = ' '
  end do
      
  return
end subroutine clear


!
!  Written by Leandro Martínez, 2009-2011.
!  Copyright (c) 2009-2018, Leandro Martínez, Jose Mario Martinez,
!  Ernesto G. Birgin.
!
! Subroutine getinp: subroutine that reads the input file
!

subroutine getinp()

   use exit_codes
   use sizes
   use compute_data, only : ntype, natoms, idfirst, nmols, ityperest, coor, restpars
   use input
   use usegencan
   use pbc

   implicit none
   integer :: i, k, ii, iarg, iline, idatom, iatom, in, irest, itype, itest,&
      imark, ioerr, nloop0, iread, idfirstatom
   double precision :: clen
   character(len=strl) :: record, blank
   logical :: inside_structure

   ! Clearing the blank character arrays

   do i = 1, strl
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
   nloop0 = 0
   movefrac = 0.05
   movebadrandom = .false.
   precision = 1.d-2
   writebad = .false.
   add_amber_ter = .false.
   amber_ter_preserve = .false.
   add_box_sides = .false.
   add_sides_fix = 0.d0
   sidemax = 1000.d0
   ioerr = 0
   avoidoverlap = .true.
   packall = .false.
   use_short_tol = .false.
   crd = .false.
   hexadecimal_indices = .false.
   inside_structure = .false.

   do i = 1, nlines

      if ( keyword(i,1).eq.'structure') inside_structure = .true.
      if ( keyword(i,1).eq.'end' .and. &
         keyword(i,2).eq.'structure') inside_structure = .false.

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
      else if(keyword(i,1).eq.'hexadecimal_indices') then
         hexadecimal_indices = .true.
      else if(keyword(i,1).eq.'maxit') then
         read(keyword(i,2),*,iostat=ioerr) maxit
         if ( ioerr /= 0 ) exit
         write(*,*) ' User defined GENCAN number of iterations: ', maxit
      else if(keyword(i,1).eq.'nloop') then
         if( .not. inside_structure ) then
            read(keyword(i,2),*,iostat=ioerr) nloop
            if ( ioerr /= 0 ) exit
         end if
      else if(keyword(i,1).eq.'nloop0') then
         if( .not. inside_structure ) then
            read(keyword(i,2),*,iostat=ioerr) nloop0
            if ( ioerr /= 0 ) exit
         end if
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
      else if(keyword(i,1).eq.'amber_ter_preserve') then
         amber_ter_preserve = .true.
         write(*,*) ' TER flags for fixed molecules will be kept if found. '
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
      else if(keyword(i,1).eq.'use_short_tol') then
         use_short_tol = .true.
         write(*,*) ' Will use a short distance penalty for all atoms. '
      else if(keyword(i,1).eq.'writecrd') then
         crd = .true.
         write(*,*) ' Will write output also in CRD format '
         read(keyword(i,2),*,iostat=ioerr) crdfile
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
      else if(keyword(i,1).eq.'pbc') then
         read(keyword(i,2),*,iostat=ioerr) pbc_min(1)
         read(keyword(i,3),*,iostat=ioerr) pbc_min(2)
         read(keyword(i,4),*,iostat=ioerr) pbc_min(3)
         if ( ioerr /= 0 ) exit
         if (maxkeywords > 4)  then
            if (trim(adjustl(keyword(i,5))) /= "none") then 
               read(keyword(i,5),*,iostat=ioerr) pbc_max(1)
               read(keyword(i,6),*,iostat=ioerr) pbc_max(2)
               read(keyword(i,7),*,iostat=ioerr) pbc_max(3)
               if ( ioerr /= 0 ) exit
            else
               pbc_max = pbc_min
               pbc_min = 0.0
            end if
         else 
            pbc_max = pbc_min
            pbc_min = 0.0
         end if
         if ( ioerr /= 0 ) exit
         using_pbc = .true.
         pbc_length = pbc_max - pbc_min
         if ( any(pbc_length <= 0.0) ) then
            write(*,*) ' ERROR: Length of PBC box must be positive in all directions, got: ', &
              pbc_length
            write(*,*) '        Lower PBC coordinates read: ', pbc_min
            write(*,*) '        Upper PBC coordinates read: ', pbc_max
            stop exit_code_input_error
         end if
         write(*,"(a)") '  Periodic boundary condition activated: '
         write(*,"(a, 3f8.2)") "    Minimum coordinates: ", pbc_min
         write(*,"(a, 3f8.2)") "    Maximum coordinates: ", pbc_max
      else if( keyword(i,1) /= 'tolerance' .and. &
         keyword(i,1) /= 'short_tol_dist' .and. &
         keyword(i,1) /= 'short_tol_scale' .and. &
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
         keyword(i,1) /= 'above' .and. &
         keyword(i,1) /= 'below' .and. &
         keyword(i,1) /= 'constrain_rotation' .and. &
         keyword(i,1) /= 'radius' .and. &
         keyword(i,1) /= 'fscale' .and. &
         keyword(i,1) /= 'short_radius' .and. &
         keyword(i,1) /= 'short_radius_scale' .and. &
         keyword(i,1) /= 'resnumbers' .and. &
         keyword(i,1) /= 'connect' .and. &
         keyword(i,1) /= 'changechains' .and. &
         keyword(i,1) /= 'chain' .and. &
         keyword(i,1) /= 'discale' .and. &
         keyword(i,1) /= 'maxit' .and. &
         keyword(i,1) /= 'movebadrandom' .and. &
         keyword(i,1) /= 'maxmove' .and. &
         keyword(i,1) /= 'add_amber_ter' .and. &
         keyword(i,1) /= 'amber_ter_preserve' .and. &
         keyword(i,1) /= 'sidemax' .and. &
         keyword(i,1) /= 'seed' .and. &
         keyword(i,1) /= 'randominitialpoint' .and. &
         keyword(i,1) /= 'restart_from' .and. &
         keyword(i,1) /= 'restart_to' .and. &
         keyword(i,1) /= 'nloop' .and. &
         keyword(i,1) /= 'nloop0' .and. &
         keyword(i,1) /= 'writeout' .and. &
         keyword(i,1) /= 'hexadecimal_indices' .and. &
         keyword(i,1) /= 'writebad' .and. &
         keyword(i,1) /= 'check' .and. &
         keyword(i,1) /= 'iprint1' .and. &
         keyword(i,1) /= 'iprint2' .and. &
         keyword(i,1) /= 'writecrd' .and. &
         keyword(i,1) /= 'segid' .and. &
         keyword(i,1) /= 'chkgrad' ) then
         write(*,*) ' ERROR: Keyword not recognized: ', trim(keyword(i,1))
         stop exit_code_input_error
      end if
   end do
   if ( ioerr /= 0 ) then
      write(*,*) ' ERROR: Some optional keyword was not used correctly: ', trim(keyword(i,1))
      stop exit_code_input_error
   end if
   write(*,*) ' Seed for random number generator: ', seed
   call init_random_number(seed)

   ! Checking for the name of the output file to be created

   xyzout = '####'
   do iline = 1, nlines
      if(keyword(iline,1).eq.'output') then
         xyzout = keyword(iline,2)
         xyzout = trim(adjustl(xyzout))
      end if
   end do
   if(xyzout(1:4) == '####') then
      write(*,*)' ERROR: Output file not (correctly?) specified. '
      stop exit_code_input_error
   end if
   write(*,*)' Output file: ', trim(adjustl(xyzout))

   ! Reading structure files

   itype = 0
   do iline = 1, nlines
      if(keyword(iline,1).eq.'structure') then
         itype = itype + 1

         record = keyword(iline,2)
         write(*,*) ' Reading coordinate file: ', trim(adjustl(record))

         ! Reading pdb input files

         if(pdb) then
            name(itype) = trim(adjustl(record))
            record = keyword(iline,2)
            pdbfile(itype) = trim(record)
            idfirst(itype) = 1
            idfirstatom = 0
            do ii = itype - 1, 1, -1
               idfirst(itype) = idfirst(itype) + natoms(ii)
            end do
            open(10,file=keyword(iline,2),status='old',iostat=ioerr)
            if ( ioerr /= 0 ) call failopen(keyword(iline,2))
            ! Read coordinates
            record(1:6) = '######'
            do while(record(1:4).ne.'ATOM'.and.record(1:6).ne.'HETATM')
               read(10,str_format) record
            end do
            idatom = idfirst(itype) - 1
            do while(idatom.lt.natoms(itype)+idfirst(itype)-1)
               if(record(1:4).eq.'ATOM'.or.record(1:6).eq.'HETATM') then
                  idatom = idatom + 1
                  amass(idatom) = 1.d0
                  maxcon(idatom) = 0
                  nconnect(idatom,1:9) = 0
                  ! Read the index of the first atom, to adjust connectivities, if any
                  if(idfirstatom == 0) read(record(7:11),*,iostat=ioerr) idfirstatom
                  read(record,"( t31,f8.3,t39,f8.3,t47,f8.3 )",iostat=ioerr) &
                     (coor(idatom,k),k=1,3)
                  if( ioerr /= 0 ) then
                     record = keyword(iline,2)
                     write(*,*) ' ERROR: Failed to read coordinates from', &
                        ' file: ', trim(adjustl(record))
                     write(*,*) ' Probably the coordinates are not in', &
                        ' standard PDB file format. '
                     write(*,*) ' Standard PDB format specifications', &
                        ' can be found at: '
                     write(*,*) ' www.rcsb.org/pdb '
                     stop exit_code_input_error
                  end if

                  ! This only tests if residue numbers can be read, they are used
                  ! only for  output
                  read(record(23:26),*,iostat=ioerr) itest
                  if( ioerr /= 0 ) then
                     record = pdbfile(itype)
                     write(*,*) ' ERROR: Failed reading residue number',&
                        ' from PDB file: ', trim(adjustl(record))
                     write(*,*) ' Residue numbers are integers that',&
                        ' must be within columns 23 and 26. '
                     write(*,*) ' Other characters within these columns',&
                        ' will cause input/output errors. '
                     write(*,*) ' Standard PDB format specifications',&
                        ' can be found at: '
                     write(*,*) ' www.rcsb.org/pdb '
                     stop exit_code_input_error
                  end if
               end if
               read(10,str_format,iostat=ioerr) record
            end do
            !
            ! Read connectivity, if there is any specified
            !
            do while(.true.)
               if ( ioerr /= 0 ) exit
               if(record(1:6).eq.'CONECT') then
                  iread = 7
                  read(record(7:len(record)),*,iostat=ioerr) iatom
                  if(ioerr /= 0) then
                     write(*,*) " ERROR: Could not read atom index from CONECT line: "
                     write(*,*) trim(adjustl(record))
                     stop exit_code_input_error
                  end if
                  iatom = iatom - idfirstatom + 1
                  idatom = idfirst(itype) - 1 + iatom
                  ! count the number of connections
                  read(record(7:len(record)), *, iostat=ioerr)  iatom, &
                      (nconnect(idatom, i), i=maxcon(idatom)+1, 9)
                  if(ioerr > 0) then
                     write(*,*) " ERROR: Error reading CONECT line: "
                     write(*,*) trim(adjustl(record))
                     stop exit_code_input_error
                  end if
                  do i = maxcon(idatom)+1, 9
                     if (nconnect(idatom,i) /= 0) then
                        nconnect(idatom, i) = nconnect(idatom, i) - idfirstatom + 1
                     end if
                  end do
                  do i = 1, 9
                     if (nconnect(idatom, i) /= 0) then
                        maxcon(idatom) = i
                     end if
                  end do
               end if
               read(10,str_format,iostat=ioerr) record
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
               read(10,str_format) record
            end do
            i = 1
            do while(record(i:i).le.' ')
               i = i + 1
               if ( i > strl ) exit
            end do
            iarg = i
            if ( i < strl ) then
               do while(record(i:i).gt.' ')
                  i = i + 1
                  if ( i > strl ) exit
               end do
            end if
            read(record(iarg:i-1),*) natoms(itype)
            if ( i < strl ) then
               do while(record(i:i).le.' ')
                  i = i + 1
                  if ( i > strl ) exit
               end do
            end if
            iarg = i
            if ( i < strl ) then
               do while(record(i:i).gt.' ')
                  i = i + 1
                  if ( i > strl ) exit
               end do
            end if
            read(record(iarg:i-1),str_format) name(itype)
            record = name(itype)
            name(itype) = trim(adjustl(record))
            if(name(itype).lt.' ') name(itype) = 'Without_title'
            idatom = idfirst(itype) - 1
            do iatom = 1, natoms(itype)
               idatom = idatom + 1
               record = blank
               do while(record.le.blank)
                  read(10,str_format) record
               end do
               i = 1
               do while(record(i:i).le.' ')
                  i = i + 1
                  if ( i > strl ) exit
               end do
               iarg = i
               if ( i < strl ) then
                  do while(record(i:i).gt.' ')
                     i = i + 1
                     if ( i > strl ) exit
                  end do
               end if
               read(record(iarg:i-1),*) in
               if ( i < strl ) then
                  do while(record(i:i).le.' ')
                     i = i + 1
                     if ( i > strl ) exit
                  end do
               end if
               iarg = i
               if ( i < strl ) then
                  do while(record(i:i).gt.' ')
                     i = i + 1
                     if ( i > strl ) exit
                  end do
               end if
               read(record(iarg:i-1),*) ele(idatom)
               read(record(i:strl),*) (coor(idatom,k), k = 1, 3),&
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
            read(10,str_format) name(itype)
            if(name(itype).lt.' ') name(itype) = 'Without_title'
            idfirst(itype) = 1
            do ii = itype - 1, 1, -1
               idfirst(itype) = idfirst(itype) + natoms(ii)
            end do
            idatom = idfirst(itype) - 1
            do iatom = 1, natoms(itype)
               idatom = idatom + 1
               record = blank
               read(10,str_format) record
               read(record,*) ele(idatom), (coor(idatom,k),k=1,3)
               amass(idatom) = 1.d0
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
      write(*,*) ' Structure ', itype, ':', trim(adjustl(record)),&
         '(',natoms(itype),' atoms)'
   end do

   ! Setting the vectors for the number of GENCAN loops

   if(nloop.eq.0) then
      nloop_all = 200*ntype
      nloop = nloop_all
   else
      nloop_all = nloop
   end if
   write(*,*) ' Maximum number of GENCAN loops for all molecule packing: ', nloop_all
   do itype = 1, ntype
      if ( nloop_type(itype) == 0 ) then
         nloop_type(itype) = nloop_all
      else
         write(*,*) ' Maximum number of GENCAN loops for type: ', itype, ': ', nloop_type(itype)
      end if
   end do

   ! nloop0 are the number of loops for the initial phase packing

   if(nloop0.eq.0) then
      nloop0 = 20*ntype
   else
      write(*,*) ' Maximum number of GENCAN loops-0 for all molecule packing: ', nloop0
   end if
   do itype = 1, ntype
      if ( nloop0_type(itype) == 0 ) then
         nloop0_type(itype) = nloop0
      else
         write(*,*) ' Maximum number of GENCAN loops-0 for type: ', itype, ': ', nloop0_type(itype)
      end if
   end do

   ! Getting the tolerance

   ioerr = 1
   dism = -1.d0
   do iline = 1, nlines
      if(keyword(iline,1).eq.'tolerance') then
         read(keyword(iline,2),*,iostat=ioerr) dism
         if ( ioerr /= 0 ) then
            write(*,*) ' ERROR: Failed reading tolerance. '
            stop exit_code_input_error
         end if
         exit
      end if
   end do
   if ( ioerr /= 0 ) then
      write(*,*) ' ERROR: Overall tolerance not set. Use, for example: tolerance 2.0 '
      stop exit_code_input_error
   end if
   write(*,*) ' Distance tolerance: ', dism

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

      if(keyword(iline,1).eq.'over' .or. keyword(iline,1).eq.'above') then
         irest = irest + 1
         irestline(irest) = iline
         read(keyword(iline,3),*,iostat=ioerr) restpars(irest,1)
         read(keyword(iline,4),*,iostat=ioerr) restpars(irest,2)
         read(keyword(iline,5),*,iostat=ioerr) restpars(irest,3)
         read(keyword(iline,6),*,iostat=ioerr) restpars(irest,4)
         if (keyword(iline,2).eq.'plane') then
            ityperest(irest) = 10
         else if(keyword(iline,2).eq.'xygauss') then
            ityperest(irest) = 14
            read(keyword(iline,7),*,iostat=ioerr) restpars(irest,5)
            read(keyword(iline,8),*,iostat=ioerr) restpars(irest,6)
         else
            ioerr = 1
         end if
      end if

      if(keyword(iline,1).eq.'below') then
         irest = irest + 1
         irestline(irest) = iline
         read(keyword(iline,3),*,iostat=ioerr) restpars(irest,1)
         read(keyword(iline,4),*,iostat=ioerr) restpars(irest,2)
         read(keyword(iline,5),*,iostat=ioerr) restpars(irest,3)
         read(keyword(iline,6),*,iostat=ioerr) restpars(irest,4)
         if (keyword(iline,2).eq.'plane') then
            ityperest(irest) = 11
         else if(keyword(iline,2).eq.'xygauss') then
            ityperest(irest) = 15
            read(keyword(iline,7),*,iostat=ioerr) restpars(irest,5)
            read(keyword(iline,8),*,iostat=ioerr) restpars(irest,6)
         else
            ioerr = 1
         end if
      end if

      if ( ioerr /= 0 ) then
         write(*,*) ' ERROR: Some restriction is not set correctly. '
         stop exit_code_input_error
      end if

   end do

   ! Reading, if defined, the short distance penalty parameters

   ioerr = 1
   short_tol_dist = dism/2.d0
   ! Reading short_tol_dist
   do iline = 1, nlines
      if(keyword(iline,1).eq.'short_tol_dist') then
         read(keyword(iline,2),*,iostat=ioerr) short_tol_dist
         if ( ioerr /= 0 ) then
            write(*,*) ' ERROR: Failed reading short_tol_dist. '
            stop exit_code_input_error
         end if
         if ( short_tol_dist > dism ) then
            write(*,*) ' ERROR: The short_tol_dist parameter must be smaller than the tolerance. '
            stop exit_code_input_error
         end if
         write(*,*) ' User defined short tolerance distance: ', short_tol_dist
         short_tol_dist = short_tol_dist**2
         exit
      end if
   end do
   ! Reading short_tol_scale
   short_tol_scale = 3.d0
   do iline = 1, nlines
      if(keyword(iline,1).eq.'short_tol_scale') then
         read(keyword(iline,2),*,iostat=ioerr) short_tol_scale
         if ( ioerr /= 0 ) then
            write(*,*) ' ERROR: Failed reading short_tol_scale. '
            stop exit_code_input_error
         end if
         if ( short_tol_dist <= 0.d0 ) then
            write(*,*) ' ERROR: The short_tol_scale parameter must be positive. '
            stop exit_code_input_error
         end if
         write(*,*) ' User defined short tolerance scale: ', short_tol_scale
         exit
      end if
   end do

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
               write(*,*) ' ERROR: Structure specification not ending with "end structure"'
               stop exit_code_input_error
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
         connect(itype) = .true.
         resnumbers(itype) = -1
         changechains(itype) = .false.
         chain(itype) = "#"
         segid(itype) = ""
         maxmove(itype) = nmols(itype)
         do iline = 1, nlines
            if(iline.gt.linestrut(itype,1).and.&
               iline.lt.linestrut(itype,2)) then
               if(keyword(iline,1).eq.'changechains') then
                  changechains(itype) = .true.
               end if
               if(keyword(iline,1).eq.'maxmove') then
                  read(keyword(iline,2),*) maxmove(itype)
               end if
               if(keyword(iline,1).eq.'resnumbers') then
                  read(keyword(iline,2),*) resnumbers(itype)
               end if
               if(keyword(iline,1).eq.'connect') then
                  if(keyword(iline,2) == "no") then
                     connect(itype) = .false.
                  end if
               end if
               if(keyword(iline,1).eq.'chain') then
                  read(keyword(iline,2),*) chain(itype)
               end if
               if(keyword(iline,1).eq.'segid') then
                  read(keyword(iline,2),*) segid(itype)
               end if
            end if
         end do
         if (crd) then
            if (itype.gt.1 .and. segid(itype)=="") then
               if (segid(itype-1) /= "")  then
                  write(*,*) ' Warning: Type of segid not defined for ', itype,'. Keeping it same as previous'
               endif
               segid(itype) = segid(itype-1)
            endif
         endif
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
            stop exit_code_input_error
         end if
      end do
   end if

   ! Write the number of molecules of each type

   do itype = 1, ntype
      write(*,*) ' Number of molecules of type ', itype, ': ', nmols(itype)
      if(pdb.and.nmols(itype).gt.9999) then
         write(*,*) ' Warning: There will be more than 9999 molecules of type ',itype
         if (.not. crd)  write(*,*) '          Residue numbering is reset after 9999. '
         if (crd)  write(*,*) '          Residue numbering is reset after 9999 in pdb but not in crd. '
         if ( chain(itype) == "#" ) then
            write(*,*) ' Each set be will be assigned a different chain in the PDB output file. '
         end if
      end if
      if(crd.and.nmols(itype).gt.99999999) then
         write(*,*) ' Warning: There will be more than 99999999 molecules of type ',itype
         write(*,*) '          Residue numbering is reset after 99999999 in crd. '
      endif
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
            stop exit_code_input_error
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
            stop exit_code_input_error
         end if
      end if
   end do lines

   ! total number of restrictions
   nrest = irest
   write(*,*) ' Total number of restrictions: ', nrest

   return
end subroutine getinp

!
! Subroutine that stops if failed to open file
!

subroutine failopen(record)
   use exit_codes
   use sizes
   character(len=strl) :: record
   write(*,*)
   write(*,*) ' ERROR: Could not open file. '
   write(*,*) '        Could not find file: ',trim(record)
   write(*,*) '        Please check if all the input and structure '
   write(*,*) '        files are in the current directory or if the'
   write(*,*) '        correct paths are provided.'
   write(*,*)
   stop exit_code_open_file
end subroutine failopen

!
! Subroutine that checks if a pdb structure has one or more than
! one residue
!

subroutine setrnum(file,nres)

   use sizes
   implicit none
   integer :: iread, ires, ireslast, nres, ioerr
   character(len=strl) :: file
   character(len=strl) :: record

   open(10,file=file,status='old')
   iread = 0
   nres = 1
   do while(nres.eq.1)
      read(10,str_format,iostat=ioerr) record
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
   use input, only : keyword, nlines, inputfile, forbidden_char
   implicit none
   character(len=strl) :: record
   integer :: iline, i, j, k, ilast, ival, ioerr

   ! Clearing keyword array

   do i = 1, nlines
      do j = 1, maxkeywords
         keyword(i,j) = 'none'
      end do
   end do

   ! Filling keyword array
   do iline = 1, nlines
      read(inputfile(iline),str_format,iostat=ioerr) record
      if ( ioerr /= 0 ) exit
      i = 0
      ival = 0
      do while(i < strl)
         i = i + 1
         ilast = i
         do while(record(i:i) > ' '.and. i < strl)
            i = i + 1
         end do
         if(i.gt.ilast) then
            ival = ival + 1
            keyword(iline,ival) = record(ilast:i)
         end if
      end do
   end do

   ! Remove quotes and the forbidden_char from keywords
   do i = 1, nlines
      do j = 1, maxkeywords
         record = keyword(i,j)
         do k = 1,strl
            if (record(k:k) == forbidden_char .or. record(k:k) == '"') then
               record(k:k) = " "
            end if
         end do
         keyword(i,j) = trim(adjustl(record))
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

   use sizes
   integer :: i
   character(len=strl) :: record

   do i = 1, strl
      record(i:i) = ' '
   end do

   return
end subroutine clear


!
!  Written by Leandro Martínez, 2009-2011.
!  Copyright (c) 2009-2018, Leandro Martínez, Jose Mario Martinez,
!  Ernesto G. Birgin.
!
! Subroutine output: Subroutine that writes the output file
!

subroutine output(n, x, output_file_name)

   use exit_codes
   use sizes
   use compute_data
   use input
   use pbc

   implicit none
   integer :: n, k, i, ilugan, ilubar, itype, imol, idatom,&
      iimol, ichain, iatom, irec, ilres, ifres,&
      iires, ciires, irescount,&
      icart, i_ref_atom, ioerr, ifirst_mol
   integer :: nres, imark
   integer :: i_fixed, i_not_fixed

   double precision :: x(n)
   double precision :: v1(3), v2(3), v3(3)
   double precision :: xcm(3), beta, gama, teta 

   character :: write_chain, even_chain, odd_chain
   character(len=64) :: title
   character(len=strl) :: pdb_atom_line, tinker_atom_line, crd_format
   character(len=8) :: crdires,crdresn,crdsegi,atmname
   character(len=strl) :: record
   character(len=strl) :: output_file_name
   character(len=5) :: i5hex
   character(len=4) :: i4hex

   ! Job title

   title = ' Built with Packmol '

   !
   ! Write restart files, if required
   !

   ! Restart file for all system

   if ( restart_to(0) /= 'none' ) then
      record = restart_to(0)
      open(10,file=restart_to(0),iostat=ioerr)
      if ( ioerr /= 0 ) then
         write(*,*) ' ERROR: Could not open restart_to file: ', trim(adjustl(record))
         stop exit_code_open_file
      end if
      ilubar = 0
      ilugan = ntotmol*3
      do i = 1, ntotmol
         write(10,"(6(tr1,es23.16))") x(ilubar+1:ilubar+3), x(ilugan+1:ilugan+3)
         ilubar = ilubar + 3
         ilugan = ilugan + 3
      end do
      close(10)
      write(*,*) ' Wrote restart file for all system: ', trim(adjustl(record))
   end if

   ! Restart files for specific molecule types

   ilubar = 0
   ilugan = ntotmol*3
   do itype = 1, ntype
      if ( restart_to(itype) /= 'none' ) then
         record = restart_to(itype)
         open(10,file=record,iostat=ioerr)
         if ( ioerr /= 0 ) then
            write(*,*) ' ERROR: Could not open restart_to file: ', trim(adjustl(record))
            stop exit_code_open_file
         end if
         do i = 1, nmols(itype)
            write(10,"(6(tr1,es23.16))") x(ilubar+1:ilubar+3), x(ilugan+1:ilugan+3)
            ilubar = ilubar + 3
            ilugan = ilugan + 3
         end do
         close(10)
         write(*,*) ' Wrote restart file: ', trim(adjustl(record))
      else
         ilubar = ilubar + nmols(itype)*3
         ilugan = ilugan + nmols(itype)*3
      end if
   end do

   ! Write the output (xyz file)

   if(xyz) then
      open(30,file=output_file_name,status='unknown')
      write(30,*) ntotat
      write(30,*) title
      ilubar = 0
      ilugan = ntotmol*3
      icart = 0
      i_not_fixed = 0
      i_fixed = ntype
      do itype = 1, ntype_with_fixed
         if ( .not. fixedoninput(itype) ) then
            i_not_fixed = i_not_fixed + 1
            do imol = 1, nmols(i_not_fixed)
               xcm = x(ilubar+1:ilubar+3)
               beta = x(ilugan+1)
               gama = x(ilugan+2)
               teta = x(ilugan+3)
               call eulerrmat(beta,gama,teta,v1,v2,v3)
               idatom = idfirst(i_not_fixed) - 1
               do iatom = 1, natoms(i_not_fixed)
                  icart = icart + 1
                  idatom = idatom + 1
                  call compcart(xcart(icart,1:3),xcm,coor(idatom,1:3),v1,v2,v3)
                  write(30,"( tr2,a3,tr2,3(tr2,f14.6) )") ele(idatom), xcart(icart, 1:3)
               end do
               ilugan = ilugan + 3
               ilubar = ilubar + 3
            end do
         else
            i_fixed = i_fixed + 1
            idatom = idfirst(i_fixed) - 1
            do iatom = 1, natoms(i_fixed)
               idatom = idatom + 1
               write(30,"( tr2,a3,tr2,3(tr2,f14.6) )") ele(idatom), coor(idatom,1:3)
            end do
         end if
      end do
      close(30)
   end if

   ! write the output as pdb file
   if(pdb) then
      pdb_atom_line = "( t1,a6,t7,a5,t12,a10,t22,a1,t23,&
      &a4,t27,a1,t31,f8.3,t39,f8.3,t47,&
      &f8.3,t55,a26 )"
      crd_format='(2I10,2X,A8,2X,A8,3F20.10,2X,A8,2X,A8,F20.10)'

      open(30,file=output_file_name,status='unknown')
      if ( crd ) then
         open(40,file=crdfile,status='unknown')
         write(40,'("* TITLE ", a64,/&
         &"* Packmol generated CHARMM CRD File",/&
         &"* Home-Page:",/&
         &"* http://m3g.iqm.unicamp.br/packmol",/&
         &"* ")') title
         write(40,'(i10,2x,a)') ntotat,'EXT'
      end if

      write(30,"( &
      &'HEADER ',/&
      &'TITLE    ', a64,/&
      &'REMARK   Packmol generated pdb file ',/&
      &'REMARK   Home-Page: ',&
      &'http://m3g.iqm.unicamp.br/packmol',/,&
      &'REMARK' )" ) title

      if (hexadecimal_indices) then
          write(30,"(a)") 'REMARK  Atom and residue indices are in hexadecimal format.'
          write(30,"(a)") 'REMARK'
      end if

      if(add_box_sides .or. using_pbc) then
         write(30,"( 'CRYST1',t7,f9.2,t16,f9.2,t25,f9.2,t34,f7.2,t41,f7.2,t48,f7.2,t56,'P 1           1' )") &
            pbc_max-pbc_min,&
            90., 90., 90.
      end if

      ilubar = 0
      ilugan = ntotmol*3
      icart = 0
      i_ref_atom = 0
      iimol = 0
      ichain = 0
      i_not_fixed = 0
      i_fixed = ntype
      irescount = 1
      do itype = 1, ntype_with_fixed
         if ( .not. fixedoninput(itype) ) then
            i_not_fixed = i_not_fixed + 1

            ! Counting the number of residues of this molecule

            open(15,file=pdbfile(i_not_fixed),status='old')
            ifres = 0
            do
               read(15,str_format,iostat=ioerr) record
               if ( ioerr /= 0 ) exit
               if ( record(1:4).eq.'ATOM'.or.record(1:6).eq.'HETATM' ) then
                  read(record(23:26),*,iostat=ioerr) imark
                  if ( ioerr /= 0 ) then
                     record = pdbfile(i_not_fixed)
                     write(*,*) ' ERROR: Failed reading residue number ',&
                        ' from PDB file: ', trim(adjustl(record))
                     write(*,*) ' Residue numbers are integers that must',&
                        ' be between columns 23 and 26. '
                     write(*,*) ' Other characters within these columns',&
                        ' will cause input/output errors. '
                     write(*,*) ' Standard PDB format specifications can',&
                        ' be found at: '
                     write(*,*) ' www.rcsb.org/pdb '
                     stop exit_code_input_error
                  end if
                  if ( ifres .eq. 0 ) ifres = imark
                  ilres = imark
               end if
            end do
            nres = ilres - ifres + 1

            do irec = 1, strl
               record(irec:irec) = ' '
            end do

            mol: do imol = 1, nmols(i_not_fixed)
               iimol = iimol + 1

               if( chain(i_not_fixed) == "#" ) then
                  if(imol.eq.1.or.mod(imol,9999).eq.1) then
                     ichain = ichain + 1
                     if( changechains(i_not_fixed) ) then
                        call chainc(ichain,odd_chain)
                        ichain = ichain + 1
                        call chainc(ichain,even_chain)
                     else
                        call chainc(ichain,even_chain)
                        odd_chain = even_chain
                     end if
                  end if
                  if ( mod(imol,2) == 0 ) write_chain = even_chain
                  if ( mod(imol,2) /= 0 ) write_chain = odd_chain
               else
                  write_chain = chain(i_not_fixed)
               end if

               xcm = x(ilubar+1:ilubar+3)
               beta = x(ilugan+1)
               gama = x(ilugan+2)
               teta = x(ilugan+3)
               call eulerrmat(beta,gama,teta,v1,v2,v3)

               rewind(15)
               idatom = idfirst(i_not_fixed) - 1
               iatom = 0
               do while(iatom.lt.natoms(i_not_fixed))

                  read(15,str_format,iostat=ioerr) record
                  if ( ioerr /= 0 ) exit mol
                  if(record(1:4).ne.'ATOM'.and.record(1:6).ne.'HETATM') then
                     if(amber_ter_preserve .and. record(1:3).eq.'TER') then
                        write(30,"('TER')")
                     end if
                     cycle
                  end if

                  iatom = iatom + 1
                  icart = icart + 1
                  idatom = idatom + 1
                  i_ref_atom = i_ref_atom + 1
                  call compcart(xcart(icart,1:3),xcm,coor(idatom,1:3),v1,v2,v3)

                  ! Setting residue numbers for this molecule

                  imark = 0
                  read(record(23:26),*,iostat=ioerr) imark
                  if ( ioerr /= 0 ) imark = 1
                  if(resnumbers(i_not_fixed).eq.0) then
                     iires = mod(imol,9999)
                     ciires = mod(imol,99999999)
                  else if(resnumbers(i_not_fixed).eq.1) then
                     iires = imark
                     ciires = imark
                  else if(resnumbers(i_not_fixed).eq.2) then
                     iires = mod(imark-ifres+irescount,9999)
                     ciires = mod(imark-ifres+irescount,99999999)
                  else if(resnumbers(i_not_fixed).eq.3) then
                     iires = mod(iimol,9999)
                     ciires = mod(iimol,99999999)
                  end if
                  if(iires.eq.0) iires = 9999
                  if(ciires.eq.0) ciires = 99999999

                  ! Writing output line
                  if(record(1:4).eq.'ATOM') then
                     write(30,pdb_atom_line) "ATOM  ", i5hex(i_ref_atom),&
                        record(12:21), write_chain, i4hex(iires),&
                        record(27:27),&
                        xcart(icart,1:3),&
                        record(55:80)
                  end if
                  if(record(1:6).eq.'HETATM') then
                     write(30,pdb_atom_line) "HETATM", i5hex(i_ref_atom),&
                        record(12:21), write_chain, i4hex(iires),&
                        record(27:27),&
                        xcart(icart,1:3),&
                        record(55:80)
                  end if

                  if ( crd ) then
                     write(crdires,'(I8)') ciires
                     crdires = adjustl(crdires)
                     crdresn = trim(adjustl(record(18:21)))
                     crdsegi = crdresn
                     if (len(trim(adjustl(segid(i_not_fixed))))/=0) crdsegi = trim(adjustl(segid(i_not_fixed)))
                     atmname = adjustl(record(13:16))
                     write(40,crd_format) i_ref_atom, ciires,crdresn, atmname, &
                        (xcart(icart,k), k = 1, 3), crdsegi,&
                        crdires, 0.
                  end if

               end do
               irescount = irescount + nres
               ilugan = ilugan + 3
               ilubar = ilubar + 3

               if(add_amber_ter) write(30,"('TER')")
            end do mol
            close(15)

            ! If fixed molecule on input:
         else
            i_fixed = i_fixed + 1

            ! Counting the number of residues of this molecule

            open(15,file=pdbfile(i_fixed),status='old')
            ifres = 0
            do
               read(15,str_format,iostat=ioerr) record
               if ( ioerr /= 0 ) exit
               if ( record(1:4).eq.'ATOM'.or.record(1:6).eq.'HETATM' ) then
                  read(record(23:26),*,iostat=ioerr) imark
                  if ( ioerr /= 0 ) then
                     record = pdbfile(i_not_fixed)
                     write(*,*) ' ERROR: Failed reading residue number ',&
                        ' from PDB file: ', trim(adjustl(record))
                     write(*,*) ' Residue numbers are integers that must',&
                        ' be between columns 23 and 26. '
                     write(*,*) ' Other characters within these columns',&
                        ' will cause input/output errors. '
                     write(*,*) ' Standard PDB format specifications can',&
                        ' be found at: '
                     write(*,*) ' www.rcsb.org/pdb '
                     stop exit_code_input_error
                  end if
                  if ( ifres .eq. 0 ) ifres = imark
                  ilres = imark
               end if
            end do
            nres = ilres - ifres + 1

            iimol = iimol + 1
            idatom = idfirst(i_fixed) - 1

            rewind(15)
            iatom = 0
            do while(iatom.lt.natoms(i_fixed))

               read(15,str_format,iostat=ioerr) record
               if ( ioerr /= 0 ) exit
               if(record(1:4).ne.'ATOM'.and.record(1:6).ne.'HETATM') then
                  if(amber_ter_preserve .and. record(1:3).eq.'TER') then
                     write(30,"('TER')")
                  end if
                  !write(30,"( a80 )") record(1:80)
                  cycle
               end if

               iatom = iatom + 1
               idatom = idatom + 1
               i_ref_atom = i_ref_atom + 1

               read(record(23:26),*) imark
               if(resnumbers(i_fixed).eq.0) then
                  iires = 1
                  ciires = 1
               else if(resnumbers(i_fixed).eq.1) then
                  iires = imark
                  ciires = imark
               else if(resnumbers(i_fixed).eq.2) then
                  iires = mod(imark-ifres+irescount,9999)
                  ciires = mod(imark-ifres+irescount,99999999)
               else if(resnumbers(i_fixed).eq.3) then
                  iires = mod(iimol,9999)
                  ciires = mod(iimol,99999999)
               end if

               if ( chain(i_fixed) == "#" ) then
                  write_chain = record(22:22)
               else
                  write_chain = chain(i_fixed)
               end if

               if(record(1:4).eq.'ATOM') then
                  write(30,pdb_atom_line) "ATOM  ", i5hex(i_ref_atom),&
                     record(12:21), write_chain, i4hex(iires),&
                     record(27:27),&
                     coor(idatom,1:3),&
                     record(55:80)
               end if
               if(record(1:6).eq.'HETATM') then
                  write(30,pdb_atom_line) "HETATM", i5hex(i_ref_atom),&
                     record(12:21), write_chain, i4hex(iires),&
                     record(27:27),&
                     coor(idatom,1:3),&
                     record(55:80)
               end if

               if ( crd ) then
                  write(crdires,'(I8)') ciires
                  crdires = adjustl(crdires)
                  crdresn = trim(adjustl(record(18:21)))
                  crdsegi = crdresn
                  if (len(trim(adjustl(segid(i_fixed))))/=0) crdsegi = trim(adjustl(segid(i_fixed)))
                  atmname = adjustl(record(13:16))
                  write(40,crd_format) i_ref_atom, iires,crdresn, atmname, xcart(icart,1:3), crdsegi, crdires, 0.
               end if

            end do
            irescount = irescount + nres
            close(15)
            if(add_amber_ter) write(30,"('TER')")
         end if
      end do
      !
      ! Write connectivity if available
      !
      i_ref_atom = 0
      i_not_fixed = 0
      i_fixed = ntype
      do itype = 1, ntype_with_fixed
         if ( .not. fixedoninput(itype) ) then
            i_not_fixed = i_not_fixed + 1
            idatom = idfirst(i_not_fixed) - 1
            do imol = 1, nmols(i_not_fixed)
               iatom = 0
               ifirst_mol = i_ref_atom + 1
               do while(iatom.lt.natoms(i_not_fixed))
                  iatom = iatom + 1
                  i_ref_atom = i_ref_atom + 1
                  if(connect(itype)) then
                     call write_connect(30,idatom,iatom,ifirst_mol)
                  end if
               end do
            end do
            close(15)
            ! If fixed molecule on input:
         else
            i_fixed = i_fixed + 1
            idatom = idfirst(i_fixed) - 1
            iatom = 0
            ifirst_mol = i_ref_atom + 1
            idatom = idfirst(i_fixed) - 1
            do while(iatom.lt.natoms(i_fixed))
               iatom = iatom + 1
               i_ref_atom = i_ref_atom + 1
               if(connect(itype)) then
                  call write_connect(30,idatom,iatom,ifirst_mol)
               end if
            end do
         end if
      end do
      write(30,"('END')")
      close(30)
      if ( crd ) close(40)
   end if

   ! Write the output (tinker xyz file)

   if(tinker) then

      tinker_atom_line = "( i7,tr2,a3,3(tr2,f10.6),9(tr2,i7) )"

      open(30, file = output_file_name,status='unknown')

      write(30,"( i6,tr2,a64 )") ntotat, title

      ilubar = 0
      ilugan = ntotmol*3
      icart = 0
      i_ref_atom = 0
      i_not_fixed = 0
      i_fixed = ntype

      do itype = 1, ntype_with_fixed

         if ( .not. fixedoninput(itype) ) then
            i_not_fixed = i_not_fixed + 1

            do imol = 1, nmols(i_not_fixed)

               xcm = x(ilubar+1:ilubar+3)
               beta = x(ilugan+1)
               gama = x(ilugan+2)
               teta = x(ilugan+3)

               call eulerrmat(beta,gama,teta,v1,v2,v3)

               idatom = idfirst(i_not_fixed) - 1
               do iatom = 1, natoms(i_not_fixed)
                  icart = icart + 1
                  idatom = idatom + 1
                  call compcart(xcart(icart,1:3),xcm,coor(idatom,1:3),v1,v2,v3)
                  ntcon(1) = nconnect(idatom,1)
                  ntcon(2:maxcon(idatom)) = nconnect(idatom,2:maxcon(idatom)) + i_ref_atom
                  write(30,tinker_atom_line) i_ref_atom+iatom, ele(idatom), xcart(icart, 1:3), ntcon(1:maxcon(idatom))
               end do
               i_ref_atom = i_ref_atom + natoms(i_not_fixed)

               ilugan = ilugan + 3
               ilubar = ilubar + 3

            end do

         else

            i_fixed = i_fixed + 1
            idatom = idfirst(i_fixed) - 1
            do iatom = 1, natoms(i_fixed)
               idatom = idatom + 1
               ntcon(1) = nconnect(idatom,1)
               do k = 2, maxcon(idatom)
                  ntcon(k) = nconnect(idatom,k) + i_ref_atom
               end do
               write(30,tinker_atom_line) i_ref_atom+iatom, ele(idatom), coor(idatom,1:3), ntcon(1:maxcon(idatom))
            end do
            i_ref_atom = i_ref_atom + natoms(i_fixed)

         end if

      end do
      close(30)
   end if

   return
end subroutine output

function i5hex(i)
   use input, only: hexadecimal_indices
   implicit none
   integer :: i
   character(len=5) i5hex
   if(hexadecimal_indices .or. i > 99999) then
      write(i5hex,"(z5)") i
   else
      write(i5hex,"(i5)") i
   end if
end

function i4hex(i)
   use input, only: hexadecimal_indices
   implicit none
   integer :: i
   character(len=4) i4hex
   if(hexadecimal_indices .or. i > 9999) then
      write(i4hex,"(z4)") i
   else
      write(i4hex,"(i4)") i
   end if
end

subroutine write_connect(iostream,idatom,iatom,ifirst)
   use sizes
   use input
   implicit none
   integer :: iostream, iatom, idatom, ifirst, iiat, icon
   character(len=5) :: i5hex, idatom_i5hex, str_conat
   character(len=strl) :: str
   iiat = iatom + idatom
   if(maxcon(iiat) == 0) return
   idatom_i5hex = i5hex(iatom+ifirst-1)
   icon = 1
   ! start CONECT line
   write(str, "(a7,a5)") "CONECT", idatom_i5hex
   do 
      str_conat = i5hex(nconnect(iiat,icon)+ifirst-1)
      ! finish CONECT line
      if (icon >= maxcon(iiat)) then
         write(str, "(a,a5)") trim(str), str_conat
         write(iostream, "(a)") trim(adjustl(str))
         exit
      end if
      ! Next atom in CONECT line (for a maximum of 4, must be in increasing order)
      if (nconnect(iiat,icon+1) > nconnect(iiat,icon) .and. mod(icon,5) /= 0) then
         write(str, "(a,a5)") trim(str), str_conat
      else
         ! finish connect line
         write(str, "(a,a5)") trim(str), str_conat
         write(iostream, "(a)") trim(adjustl(str))
         ! Start a new CONECT line
         write(str, "(a7,a5)") "CONECT", idatom_i5hex
      end if
      icon = icon + 1
   end do
end subroutine write_connect

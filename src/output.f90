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
      irest, iimol, ichain, iatom, irec, ilres, ifres,&
      iires, ciires, irescount,&
      icart, i_ref_atom, ioerr, ifirst_mol
   integer :: nr, nres, imark
   integer :: i_fixed, i_not_fixed

   double precision :: x(n)
   double precision :: tens(4,4), v(4,4), dv(4)
   double precision :: v1(3), v2(3), v3(3)
   double precision :: xbar, ybar, zbar, beta, gama, teta, xcm, ycm, zcm
   double precision :: xlength, ylength, zlength
   double precision :: xxyx, xxyy, xxyz, xyyz, xyyy, xzyx,&
      xzyy, xzyz, xyyx, xq, yq, zq, q0, q1, q2, q3
   double precision :: xtemp, ytemp, ztemp
   double precision :: sxmin, symin, szmin, sxmax, symax, szmax

   character :: write_chain, even_chain, odd_chain
   character(len=64) :: title
   character(len=strl) :: pdb_atom_line, tinker_atom_line, crd_format
   character(len=8) :: crdires,crdresn,crdsegi,atmname
   character(len=strl) :: record
   character(len=strl) :: output_file_name
   character(len=5) :: i5hex, tmp_i5hex

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
         write(10,"(6(tr1,es23.16))") x(ilubar+1), x(ilubar+2), x(ilubar+3), &
            x(ilugan+1), x(ilugan+2), x(ilugan+3)
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
            write(10,"(6(tr1,es23.16))") x(ilubar+1), x(ilubar+2), x(ilubar+3), &
               x(ilugan+1), x(ilugan+2), x(ilugan+3)
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
      do itype = 1, ntfix
         if ( .not. fixedoninput(itype) ) then
            i_not_fixed = i_not_fixed + 1
            do imol = 1, nmols(i_not_fixed)
               xbar = x(ilubar+1)
               ybar = x(ilubar+2)
               zbar = x(ilubar+3)
               beta = x(ilugan+1)
               gama = x(ilugan+2)
               teta = x(ilugan+3)
               call eulerrmat(beta,gama,teta,v1,v2,v3)
               idatom = idfirst(i_not_fixed) - 1
               do iatom = 1, natoms(i_not_fixed)
                  icart = icart + 1
                  idatom = idatom + 1
                  call compcart(icart,xbar,ybar,zbar,&
                     coor(idatom,1),coor(idatom,2),&
                     coor(idatom,3),&
                     v1,v2,v3)
                  write(30,"( tr2,a3,tr2,3(tr2,f14.6) )") ele(idatom), (xcart(icart, k), k = 1, 3)
               end do
               ilugan = ilugan + 3
               ilubar = ilubar + 3
            end do
         else
            i_fixed = i_fixed + 1
            idatom = idfirst(i_fixed) - 1
            do iatom = 1, natoms(i_fixed)
               idatom = idatom + 1
               write(30,"( tr2,a3,tr2,3(tr2,f14.6) )") ele(idatom), (coor(idatom,k),k=1,3)
            end do
         end if
      end do
      close(30)
   end if

   ! write the output as a MOLDY file

   if(moldy) then
      open(30,file=output_file_name,status='unknown')
      ! For square moldy boxes, this must be the side dimensions of the box
      sxmin = 1.d30
      symin = 1.d30
      szmin = 1.d30
      sxmax = -1.d30
      symax = -1.d30
      szmax = -1.d30
      do irest = 1, nrest
         if(ityperest(irest).eq.2) then
            sxmin = dmin1(restpars(irest,1),sxmin)
            symin = dmin1(restpars(irest,2),symin)
            szmin = dmin1(restpars(irest,3),szmin)
            sxmax = dmax1(restpars(irest,4)+restpars(irest,1),sxmax)
            symax = dmax1(restpars(irest,4)+restpars(irest,2),symax)
            szmax = dmax1(restpars(irest,4)+restpars(irest,3),szmax)
         else if(ityperest(irest).eq.3) then
            sxmin = dmin1(restpars(irest,1),sxmin)
            symin = dmin1(restpars(irest,2),symin)
            szmin = dmin1(restpars(irest,3),szmin)
            sxmax = dmax1(restpars(irest,4),sxmax)
            symax = dmax1(restpars(irest,5),symax)
            szmax = dmax1(restpars(irest,6),szmax)
         else
            write(*,*) ' WARNING: The first line of the moldy output'
            write(*,*) ' file contains the size of the sides of the'
            write(*,*) ' paralelogram that defines the system. '
            write(*,*) ' The numbers printed may not be correct in '
            write(*,*) ' this case because regions other than cubes '
            write(*,*) ' or boxes were used. '
            sxmin = dmin1(sxmin,sizemin(1))
            symin = dmin1(symin,sizemin(2))
            szmin = dmin1(szmin,sizemin(3))
            sxmax = dmax1(sxmax,sizemax(1))
            symax = dmax1(symax,sizemax(2))
            szmax = dmax1(szmax,sizemax(3))
         end if
      end do
      xlength = sxmax - sxmin
      ylength = symax - symin
      zlength = szmax - szmin
      write(30,"( 3(tr1,f12.6),' 90 90 90 1 1 1 ' )") xlength, ylength, zlength
      ilubar = 0
      ilugan = ntotmol*3
      i_not_fixed = 0
      i_fixed = ntype
      do itype = 1, ntfix
         if ( .not. fixedoninput(itype) ) then
            i_not_fixed = i_not_fixed + 1
            record = name(i_not_fixed)
            do imol = 1, nmols(i_not_fixed)
               xbar = (x(ilubar+1) - sxmin) / xlength
               ybar = (x(ilubar+2) - symin) / ylength
               zbar = (x(ilubar+3) - szmin) / zlength
               beta = x(ilugan+1)
               gama = x(ilugan+2)
               teta = x(ilugan+3)
               call eulerrmat(beta,gama,teta,v1,v2,v3)

               ! Computing cartesian coordinates and quaternions

               xxyx = 0.d0
               xxyy = 0.d0
               xxyz = 0.d0
               xyyx = 0.d0
               xyyy = 0.d0
               xyyz = 0.d0
               xzyx = 0.d0
               xzyy = 0.d0
               xzyz = 0.d0
               idatom = idfirst(i_not_fixed) - 1
               do iatom = 1, natoms(i_not_fixed)
                  idatom = idatom + 1
                  xq =   coor(idatom, 1)*v1(1) &
                     + coor(idatom, 2)*v2(1) &
                     + coor(idatom, 3)*v3(1)
                  yq =   coor(idatom, 1)*v1(2) &
                     + coor(idatom, 2)*v2(2) &
                     + coor(idatom, 3)*v3(2)
                  zq =   coor(idatom, 1)*v1(3) &
                     + coor(idatom, 2)*v2(3) &
                     + coor(idatom, 3)*v3(3)

                  ! Recovering quaternions for molecule imol

                  xxyx = xxyx + xq * coor(idatom,1) * amass(idatom)
                  xxyy = xxyy + xq * coor(idatom,2) * amass(idatom)
                  xxyz = xxyz + xq * coor(idatom,3) * amass(idatom)
                  xyyx = xyyx + yq * coor(idatom,1) * amass(idatom)
                  xyyy = xyyy + yq * coor(idatom,2) * amass(idatom)
                  xyyz = xyyz + yq * coor(idatom,3) * amass(idatom)
                  xzyx = xzyx + zq * coor(idatom,1) * amass(idatom)
                  xzyy = xzyy + zq * coor(idatom,2) * amass(idatom)
                  xzyz = xzyz + zq * coor(idatom,3) * amass(idatom)
               end do

               tens(1,1) = xxyx + xyyy + xzyz
               tens(1,2) = xzyy - xyyz
               tens(2,2) = xxyx - xyyy - xzyz
               tens(1,3) = xxyz - xzyx
               tens(2,3) = xxyy + xyyx
               tens(3,3) = xyyy - xzyz - xxyx
               tens(1,4) = xyyx - xxyy
               tens(2,4) = xzyx + xxyz
               tens(3,4) = xyyz + xzyy
               tens(4,4) = xzyz - xxyx - xyyy
               nr = 16
               call jacobi (tens, 4, 4, dv, v, nr)
               q0 = v(1,4)
               q1 = v(2,4)
               q2 = v(3,4)
               q3 = v(4,4)
               record = name(i_not_fixed)
               xbar = dmin1(0.999999d0,xbar)
               ybar = dmin1(0.999999d0,ybar)
               zbar = dmin1(0.999999d0,zbar)
               write(30,"( a10,tr1,7(f12.6) )") trim(adjustl(record)), xbar, ybar, zbar, &
                  q0, q1, q2, q3
               ilugan = ilugan + 3
               ilubar = ilubar + 3
            end do
         else
            i_fixed = i_fixed + 1
            idatom = idfirst(i_fixed) - 1

            ! Getting the specified position of the molecule

            do irest = 1, nrest
               if(irestline(irest).gt.linestrut(i_fixed,1).and.&
                  irestline(irest).lt.linestrut(i_fixed,2)) then
                  xcm = restpars(irest,1) - sxmin
                  ycm = restpars(irest,2) - symin
                  zcm = restpars(irest,3) - szmin
                  beta = -restpars(irest,4)
                  gama = -restpars(irest,5)
                  teta = -restpars(irest,6)
               end if
            end do
            call eulerrmat(beta,gama,teta,v1,v2,v3)

            ! Computing cartesian coordinates and quaternions

            xxyx = 0.d0
            xxyy = 0.d0
            xxyz = 0.d0
            xyyx = 0.d0
            xyyy = 0.d0
            xyyz = 0.d0
            xzyx = 0.d0
            xzyy = 0.d0
            xzyz = 0.d0
            idatom = idfirst(i_fixed) - 1
            do iatom = 1, natoms(i_fixed)
               idatom = idatom + 1
               xtemp = coor(idatom,1) - xcm
               ytemp = coor(idatom,2) - ycm
               ztemp = coor(idatom,3) - zcm
               xq =   xtemp*v1(1) + ytemp*v2(1) + ztemp*v3(1)
               yq =   xtemp*v1(2) + ytemp*v2(2) + ztemp*v3(2)
               zq =   xtemp*v1(3) + ytemp*v2(3) + ztemp*v3(3)
               xxyx = xxyx + xtemp * xq * amass(idatom)
               xxyy = xxyy + xtemp * yq * amass(idatom)
               xxyz = xxyz + xtemp * zq * amass(idatom)
               xyyx = xyyx + ytemp * xq * amass(idatom)
               xyyy = xyyy + ytemp * yq * amass(idatom)
               xyyz = xyyz + ytemp * zq * amass(idatom)
               xzyx = xzyx + ztemp * xq * amass(idatom)
               xzyy = xzyy + ztemp * yq * amass(idatom)
               xzyz = xzyz + ztemp * zq * amass(idatom)
            end do
            tens(1,1) = xxyx + xyyy + xzyz
            tens(1,2) = xzyy - xyyz
            tens(2,2) = xxyx - xyyy - xzyz
            tens(1,3) = xxyz - xzyx
            tens(2,3) = xxyy + xyyx
            tens(3,3) = xyyy - xzyz - xxyx
            tens(1,4) = xyyx - xxyy
            tens(2,4) = xzyx + xxyz
            tens(3,4) = xyyz + xzyy
            tens(4,4) = xzyz - xxyx - xyyy
            nr = 16
            call jacobi (tens, 4, 4, dv, v, nr)
            q0 = v(1,4)
            q1 = v(2,4)
            q2 = v(3,4)
            q3 = v(4,4)
            xcm = xcm / xlength
            ycm = ycm / ylength
            zcm = zcm / zlength
            record = name(itype)
            xcm = dmin1(0.999999d0,xcm)
            ycm = dmin1(0.999999d0,ycm)
            zcm = dmin1(0.999999d0,zcm)
            write(30,"( a10,tr1,7(f12.6) )") trim(adjustl(record)),&
               xcm, ycm, zcm, q0, q1, q2, q3
         end if
      end do
      close(30)
   end if

   ! write the output as pdb file

   if(pdb) then
      pdb_atom_line = "( t1,a6,t7,a5,t12,a10,t22,a1,t23,&
      &i4,t27,a1,t31,f8.3,t39,f8.3,t47,&
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

      if(add_box_sides .or. using_pbc) then
         write(30,"( 'CRYST1',t7,f9.2,t16,f9.2,t25,f9.2,&
         &t34,f7.2,t41,f7.2,t48,f7.2,&
         &t56,'P 1           1' )") &
            sizemax(1)-sizemin(1) + add_sides_fix,&
            sizemax(2)-sizemin(2) + add_sides_fix,&
            sizemax(3)-sizemin(3) + add_sides_fix,&
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
      do itype = 1, ntfix
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

               xbar = x(ilubar+1)
               ybar = x(ilubar+2)
               zbar = x(ilubar+3)
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
                  call compcart(icart,xbar,ybar,zbar,&
                     coor(idatom,1),coor(idatom,2),&
                     coor(idatom,3),v1,v2,v3)

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
                     tmp_i5hex = i5hex(i_ref_atom)
                     write(30,pdb_atom_line) "ATOM  ", tmp_i5hex,&
                        record(12:21), write_chain, iires,&
                        record(27:27),&
                        (xcart(icart,k), k = 1, 3),&
                        record(55:80)
                  end if
                  if(record(1:6).eq.'HETATM') then
                     tmp_i5hex = i5hex(i_ref_atom)
                     write(30,pdb_atom_line) "HETATM", tmp_i5hex,&
                        record(12:21), write_chain, iires,&
                        record(27:27),&
                        (xcart(icart,k), k = 1, 3),&
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
                  tmp_i5hex = i5hex(i_ref_atom)
                  write(30,pdb_atom_line) "ATOM  ", tmp_i5hex,&
                     record(12:21), write_chain, iires,&
                     record(27:27),&
                     (coor(idatom,k), k = 1, 3),&
                     record(55:80)
               end if
               if(record(1:6).eq.'HETATM') then
                  tmp_i5hex = i5hex(i_ref_atom)
                  write(30,pdb_atom_line) "HETATM", tmp_i5hex,&
                     record(12:21), write_chain, iires,&
                     record(27:27),&
                     (coor(idatom,k), k = 1, 3),&
                     record(55:80)
               end if

               if ( crd ) then
                  write(crdires,'(I8)') ciires
                  crdires = adjustl(crdires)
                  crdresn = trim(adjustl(record(18:21)))
                  crdsegi = crdresn
                  if (len(trim(adjustl(segid(i_fixed))))/=0) crdsegi = trim(adjustl(segid(i_fixed)))
                  atmname = adjustl(record(13:16))
                  write(40,crd_format) i_ref_atom, iires,crdresn, atmname, &
                     (xcart(icart,k), k = 1, 3), crdsegi,&
                     crdires, 0.
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
      do itype = 1, ntfix
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

      do itype = 1, ntfix

         if ( .not. fixedoninput(itype) ) then
            i_not_fixed = i_not_fixed + 1

            do imol = 1, nmols(i_not_fixed)

               xbar = x(ilubar+1)
               ybar = x(ilubar+2)
               zbar = x(ilubar+3)
               beta = x(ilugan+1)
               gama = x(ilugan+2)
               teta = x(ilugan+3)

               call eulerrmat(beta,gama,teta,v1,v2,v3)

               idatom = idfirst(i_not_fixed) - 1
               do iatom = 1, natoms(i_not_fixed)
                  icart = icart + 1
                  idatom = idatom + 1
                  call compcart(icart,xbar,ybar,zbar,&
                     coor(idatom,1),coor(idatom,2),&
                     coor(idatom,3),&
                     v1,v2,v3)

                  ntcon(1) = nconnect(idatom,1)
                  do k = 2, maxcon(idatom)
                     ntcon(k) = nconnect(idatom,k) + i_ref_atom
                  end do
                  write(30,tinker_atom_line) i_ref_atom+iatom,&
                     ele(idatom), (xcart(icart, k), k = 1, 3),&
                     (ntcon(k), k = 1, maxcon(idatom))
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
               write(30,tinker_atom_line) i_ref_atom+iatom, ele(idatom),&
                  (coor(idatom,k), k = 1, 3),&
                  (ntcon(k), k = 1, maxcon(idatom))
            end do
            i_ref_atom = i_ref_atom + natoms(i_fixed)

         end if

      end do
      close(30)
   end if

   return
end subroutine output

function i5hex(i)
   implicit none
   integer :: i
   character(len=5) i5hex
   if(i <= 99999) then
      write(i5hex,"(i5)") i
   else
      write(i5hex,"(z5)") i
   end if
end

subroutine write_connect(iostream,idatom,iatom,ifirst)
   use sizes
   use input
   implicit none
   integer :: i, j, iostream, iatom, idatom, ifirst
   character(len=5) :: i5hex, tmp_i5hex
   character(len=strl) :: str
   if(maxcon(iatom+idatom) == 0) return
   tmp_i5hex = i5hex(iatom+ifirst-1)
   write(str,"(a7,9a5)") "CONECT", tmp_i5hex, & 
       (i5hex(nconnect(iatom+idatom,i)+ifirst-1), i=1, maxcon(iatom+idatom))
   write(iostream,"(a)") trim(adjustl(str))
end subroutine write_connect







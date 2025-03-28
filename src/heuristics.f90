!
!  Written by Leandro Martínez, 2009-2011.
!  Copyright (c) 2009-2018, Leandro Martínez, Jose Mario Martinez,
!  Ernesto G. Birgin.
!
! subroutine movebad: Move the worse molecules to new positions
!

subroutine movebad(n,x,fx,movebadprint)

   use sizes
   use compute_data
   use input, only : movefrac, movebadrandom, precision, maxmove, maxmove_total
   use usegencan
   use flashsort
   use ahestetic
   implicit none

   ! Internal variables
   integer :: n, i, j, icart, itype, iatom, imol, ilubar, ilugan, &
      ilubar2, ilugan2, nbad, igood, ibad, nmove, nmove_total, itype_tmp
   double precision :: x(n), fx, rnd, frac
   double precision :: fdist_mol, frest_mol
   logical :: movebadprint, hasbad

   if(movebadprint) write(*,*) ' Moving worst molecules ... '

   icart = 0
   do itype = 1, ntype
      if(.not.comptype(itype)) then
         icart = icart + nmols(itype)*natoms(itype)
      else
         do imol = 1, nmols(itype)
            do iatom = 1, natoms(itype)
               icart = icart + 1
               fdist_atom(icart) = 0.d0
               frest_atom(icart) = 0.d0
            end do
         end do
      end if
   end do

   move = .true.
   if(movebadprint) write(*,*) ' Function value before moving molecules:',fx
   do i = 1, ntotat
      radiuswork(i) = radius(i)
      radius(i) = radius_ini(i)
   end do
   call computef(n,x,fx)
   move = .false.


   ! Moving the worst molecules
   if (init1) then
      do i = 1, ntype
         rand_type(i) = i
      end do
   else
      call shuffle_types(ntype, rand_type)
   end if
   nmove_total = 0
   hasbad = .false.
   icart = 0
   move_itype : do itype_tmp = 1, ntype
      itype = rand_type(itype_tmp)
      if(.not.comptype(itype)) then
         icart = icart + nmols(itype)*natoms(itype)
      else

         ! Checking the function value for each molecule

         nbad = 0
         do imol = 1, nmols(itype)
            fdist_mol = 0.d0
            frest_mol = 0.d0
            do iatom = 1, natoms(itype)
               icart = icart + 1
               fdist_mol = dmax1(fdist_mol,fdist_atom(icart))
               frest_mol = dmax1(frest_mol,frest_atom(icart))
            end do
            if(fdist_mol > precision .or. frest_mol > precision ) then
               hasbad = .true.
               nbad = nbad + 1
               fmol(imol) = fdist_mol + frest_mol
            else
               fmol(imol) = 0.d0
            end if
         end do
         frac = dfloat(nbad)/dfloat(nmols(itype))
         if(movebadprint) write(*,"( a,i9,a,f8.2,a,$ )") &
            '  Type ',itype,' molecules with non-zero contributions:', &
            100.d0*frac,'%'

         if(nbad == 0) then
            write(*,*)
         else
            frac = dmin1(movefrac,frac)

            ! Ordering molecules from best to worst

            mflash = 1 + nmols(itype)/10
            call flash1(fmol,nmols(itype),lflash,mflash,indflash)

            ! Moving molecules

            nmove = min0(min0(maxmove(itype),maxmove_total-nmove_total),max0(int(nmols(itype)*frac),1))
            nmove_total = nmove_total + nmove
            if(movebadprint) then
               write(*,"( a,i9,a )") '  Moving ',nmove,' molecule(s).'
               if (nmove > 0) then
                  if ( movebadrandom ) then
                     write(*,*) ' New positions will be aleatory (movebadrandom is set) '
                  else
                     write(*,*) ' New positions will be based on good molecules (movebadrandom is not set) '
                  end if
               else
                  cycle move_itype
               end if
            end if
            imol = 0
            do i = 1, itype - 1
               if(comptype(i)) imol = imol + nmols(i)
            end do
            write(*,prog2_line)
            write(*,"( '          |',$)")
            j = 0
            do i = 1, nmove
               ibad = nmols(itype) - i + 1
               igood = int(rnd()*nmols(itype)*frac) + 1
               ilubar = 3*(indflash(ibad)+imol-1)
               ilugan = 3*(indflash(ibad)+imol-1)+3*ntotmol
               ilubar2 = 3*(indflash(igood)+imol-1)
               ilugan2 = 3*(indflash(igood)+imol-1)+3*ntotmol
               if ( movebadrandom ) then
                  x(ilubar+1) = sizemin(1) + rnd()*(sizemax(1)-sizemin(1))
                  x(ilubar+2) = sizemin(2) + rnd()*(sizemax(2)-sizemin(2))
                  x(ilubar+3) = sizemin(3) + rnd()*(sizemax(3)-sizemin(3))
               else
                  x(ilubar+1) = x(ilubar2+1) - 0.3*dmax(itype)+0.6*rnd()*dmax(itype)
                  x(ilubar+2) = x(ilubar2+2) - 0.3*dmax(itype)+0.6*rnd()*dmax(itype)
                  x(ilubar+3) = x(ilubar2+3) - 0.3*dmax(itype)+0.6*rnd()*dmax(itype)
               end if
               x(ilugan+1) = x(ilugan2+1)
               x(ilugan+2) = x(ilugan2+2)
               x(ilugan+3) = x(ilugan2+3)
               call restmol(itype,ilubar,n,x,fx,.true.)
               do while( j <= 60*i/nmove )
                  write(*,"('*',$)")
                  j = j + 1
               end do
            end do
            write(*,"('|')")
         end if
      end if
   end do move_itype

   call computef(n,x,fx)
   if(movebadprint) write(*,*) ' Function value after moving molecules:', fx
   do i = 1, ntotat
      radius(i) = radiuswork(i)
   end do

   return
end subroutine movebad

subroutine shuffle_types(ntype, sequence)
    implicit none
    integer, intent(in) :: ntype
    integer, intent(inout) :: sequence(ntype)
    integer :: i, j, temp
    double precision :: rand_real

    ! Initialize the sequence with numbers 1 to N
    do i = 1, ntype
      sequence(i) = i
    end do

    ! Fisher-Yates shuffle
    do i = ntype, 2, -1
       call random_number(rand_real)
       j = int(rand_real * i) + 1  ! Generate a random index in [1, i]
       ! Swap sequence(i) and sequence(j)
       temp = sequence(i)
       sequence(i) = sequence(j)
       sequence(j) = temp
    end do
end subroutine shuffle_types

!  
!  Written by Leandro Martínez, 2009-2011.
!  Copyright (c) 2009-2011, Leandro Martínez, Jose Mario Martinez,
!  Ernesto G. Birgin.
!  
! Subroutine that computes the analytical derivatives
!

subroutine computeg(n,x,g) 

  use sizes
  use compute_data
  use input, only : fix
  implicit none

  integer :: n
  integer :: idatom, iatom, irest 
  integer :: i, j, k, ilubar, ilugan, icart, itype, imol
  integer :: ibox, iboxx, iboxy, iboxz
  integer :: k1, k2
  integer :: iratcount

  double precision :: x(n), g(n) 
  double precision :: dv1beta(3), dv1gama(3), dv1teta(3),&
                      dv2beta(3), dv2gama(3), dv2teta(3),& 
                      dv3beta(3), dv3gama(3), dv3teta(3) 
  double precision :: v1(3), v2(3), v3(3)
  double precision :: xbar, ybar, zbar
  double precision :: xtemp, ytemp, ztemp
  double precision :: beta, gama, teta, cb, sb, cg, sg, ct, st

  ! Reset gradients

  do i = 1, ntotat
    do j = 1, 3 
      gxcar(i,j) = 0.d0 
    end do 
  end do 

  ! Reset boxes

  if(.not.init1) call resetboxes()

  ! Transform baricenter and angles into cartesian coordinates 

  ! Computes cartesian coordinates from vector x and coor 
 
  ilubar = 0 
  ilugan = ntotmol*3 
  icart = natfix

  do itype = 1, ntype 

    if(.not.comptype(itype)) then
      icart = icart + nmols(itype)*natoms(itype)
    else
    do imol = 1, nmols(itype) 

      xbar = x(ilubar + 1) 
      ybar = x(ilubar + 2) 
      zbar = x(ilubar + 3) 
 
      ! Compute the rotation matrix 

      beta = x(ilugan + 1)
      gama = x(ilugan + 2)
      teta = x(ilugan + 3)

      call eulerrmat(beta,gama,teta,v1,v2,v3)  
  
      idatom = idfirst(itype) - 1
      do iatom = 1, natoms(itype) 
  
        icart = icart + 1
        idatom = idatom + 1

        call compcart(icart,xbar,ybar,zbar, &
                      coor(idatom,1),coor(idatom,2),coor(idatom,3), &
                      v1,v2,v3)

        ! Gradient relative to the wall distace

        do iratcount = 1, nratom(icart)
          irest = iratom(icart,iratcount)
          call gwalls(icart,irest)
        end do

        if(.not.init1) then    
          xtemp = xcart(icart,1) - sizemin(1)
          ytemp = xcart(icart,2) - sizemin(2) 
          ztemp = xcart(icart,3) - sizemin(3) 
  
          iboxx = int(xtemp/boxl(1)) + 1
          iboxy = int(ytemp/boxl(2)) + 1
          iboxz = int(ztemp/boxl(3)) + 1
  
          if(xtemp.le.0) iboxx = 1
          if(ytemp.le.0) iboxy = 1
          if(ztemp.le.0) iboxz = 1 
          if(iboxx.gt.nboxes(1)) iboxx = nboxes(1)
          if(iboxy.gt.nboxes(2)) iboxy = nboxes(2)
          if(iboxz.gt.nboxes(3)) iboxz = nboxes(3)

          ! Atom linked list

          latomnext(icart) = latomfirst(iboxx,iboxy,iboxz)
          latomfirst(iboxx,iboxy,iboxz) = icart

          ! Box with atoms linked list

          if ( .not. hasfree(iboxx,iboxy,iboxz) ) then
            hasfree(iboxx,iboxy,iboxz) = .true.
            call ijk_to_ibox(iboxx,iboxy,iboxz,ibox)
            lboxnext(ibox) = lboxfirst
            lboxfirst = ibox

            ! Add boxes with fixed atoms which are vicinal to this box, and
            ! are behind 

            if ( fix ) then

              call add_box_behind(iboxx-1,iboxy,iboxz)
              call add_box_behind(iboxx,iboxy-1,iboxz)
              call add_box_behind(iboxx,iboxy,iboxz-1)

              call add_box_behind(iboxx,iboxy-1,iboxz+1)
              call add_box_behind(iboxx,iboxy-1,iboxz-1)
              call add_box_behind(iboxx-1,iboxy+1,iboxz)
              call add_box_behind(iboxx-1,iboxy,iboxz+1)
              call add_box_behind(iboxx-1,iboxy-1,iboxz)
              call add_box_behind(iboxx-1,iboxy,iboxz-1)

              call add_box_behind(iboxx-1,iboxy+1,iboxz+1)
              call add_box_behind(iboxx-1,iboxy+1,iboxz-1)
              call add_box_behind(iboxx-1,iboxy-1,iboxz+1)
              call add_box_behind(iboxx-1,iboxy-1,iboxz-1)

            end if                                 

          end if

          ibtype(icart) = itype
          ibmol(icart) = imol  
        end if
  
      end do
      ilugan = ilugan + 3 
      ilubar = ilubar + 3
    end do
    end if
  end do

  if( .not. init1 ) then

    !
    ! Gradient relative to minimum distance
    !

    ibox = lboxfirst
    do while( ibox > 0 )

      call ibox_to_ijk(ibox,i,j,k)

      icart = latomfirst(i,j,k)
      do while ( icart .ne. 0 )

        if(comptype(ibtype(icart))) then

          ! Interactions inside box

          call gparc(icart,latomnext(icart))

          ! Interactions of boxes that share faces

          call gparc(icart,latomfirst(i+1,j,k))
          call gparc(icart,latomfirst(i,j+1,k))
          call gparc(icart,latomfirst(i,j,k+1))

          ! Interactions of boxes that share axes

          call gparc(icart,latomfirst(i+1,j+1,k))
          call gparc(icart,latomfirst(i+1,j,k+1))
          call gparc(icart,latomfirst(i+1,j-1,k))
          call gparc(icart,latomfirst(i+1,j,k-1))
          call gparc(icart,latomfirst(i,j+1,k+1))
          call gparc(icart,latomfirst(i,j+1,k-1))

          ! Interactions of boxes that share vertices

          call gparc(icart,latomfirst(i+1,j+1,k+1))
          call gparc(icart,latomfirst(i+1,j+1,k-1))
          call gparc(icart,latomfirst(i+1,j-1,k+1))
          call gparc(icart,latomfirst(i+1,j-1,k-1))

        end if

        icart = latomnext(icart)
      end do

      ibox = lboxnext(ibox)
    end do

  end if

  ! Computing the gradient using chain rule 
 
  do i = 1, n 
    g(i) = 0.d0 
  end do 

  k1 = 0 
  k2 = ntotmol * 3 

  icart = natfix 
  do itype = 1, ntype 

    if(.not.comptype(itype)) then
      icart = icart + nmols(itype)*natoms(itype)
    else
    do imol = 1, nmols(itype) 

      beta = x(k2 + 1) 
      gama = x(k2 + 2) 
      teta = x(k2 + 3) 

      cb = dcos(beta) 
      sb = dsin(beta) 
      cg = dcos(gama) 
      sg = dsin(gama) 
      ct = dcos(teta) 
      st = dsin(teta) 
  
      dv1beta(1) = - cb * sg * ct - sb * cg 
      dv2beta(1) = - sb * sg * ct + cb * cg 
      dv3beta(1) = 0.d0 
  
      dv1gama(1) = - sb * cg * ct - cb * sg 
      dv2gama(1) =   cb * cg * ct - sb * sg 
      dv3gama(1) =   cg * st 
  
      dv1teta(1) =   sb * sg * st 
      dv2teta(1) = - cb * sg * st 
      dv3teta(1) =   sg * ct 
       
      dv1beta(2) = - cb * cg * ct + sb * sg 
      dv2beta(2) = - sb * cg * ct - cb * sg 
      dv3beta(2) = 0.d0 
  
      dv1gama(2) =   sb * sg * ct - cb * cg 
      dv2gama(2) = - sg * cb * ct - cg * sb 
      dv3gama(2) = - sg * st 
  
      dv1teta(2) =   sb * cg * st 
      dv2teta(2) = - cb * cg * st 
  
      dv3teta(2) =   cg * ct 
  
      dv1beta(3) =   cb * st 
      dv2beta(3) =   sb * st 
      dv3beta(3) = 0.d0 
  
      dv1gama(3) = 0.d0 
      dv2gama(3) = 0.d0 
      dv3gama(3) = 0.d0 
  
      dv1teta(3) =   sb * ct 
      dv2teta(3) = - cb * ct 
      dv3teta(3) = - st 
  
      idatom = idfirst(itype) - 1
      do iatom = 1, natoms(itype)
      
        icart = icart + 1
        idatom = idatom + 1 

        do k = 1, 3 
          g(k1+k) = g(k1+k) + gxcar(icart, k) 
        end do 
  
        do k = 1, 3 
          g(k2 + 1) = g(k2 + 1) &
                      + (coor(idatom,1) * dv1beta(k) &
                      + coor(idatom, 2) * dv2beta(k) & 
                      + coor(idatom, 3) * dv3beta(k)) &
                      * gxcar(icart, k) 
      
          g(k2 + 2) = g(k2 + 2) &
                      + (coor(idatom,1) * dv1gama(k) &
                      + coor(idatom, 2) * dv2gama(k) &
                      + coor(idatom, 3) * dv3gama(k)) &
                      * gxcar(icart, k) 
      
          g(k2 + 3) = g(k2 + 3) &
                      + (coor(idatom,1)  * dv1teta(k) &
                      + coor(idatom, 2) * dv2teta(k) &
                      + coor(idatom, 3) * dv3teta(k)) &
                      * gxcar(icart, k) 
        end do 

      end do 
      k2 = k2 + 3 
      k1 = k1 + 3
    end do 
    end if
  end do 

  return 
end subroutine computeg


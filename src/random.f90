!
!  Written by Leandro Martínez, 2009-2011.
!  Copyright (c) 2009-2018, Leandro Martínez, Jose Mario Martinez,
!  Ernesto G. Birgin.
!

!
! Function that returns a real random number between 0. and 1.
!

double precision function rnd()
   call random_number(rnd)
   return
end function rnd

!
! Subroutine that initializes the random number generator given a seed
!
subroutine init_random_number(iseed)
   integer :: size
   integer :: i, iseed
   integer, allocatable :: seed(:)
   call random_seed(size=size)
   allocate(seed(size))
   do i = 1, size
      seed(i) = i*iseed
   end do
   call random_seed(put=seed)
   deallocate(seed)
   return
end subroutine init_random_number

!
! Subroutine that uses the date to create a random seed
!

subroutine seed_from_time(seed)

   implicit none
   integer :: seed, value(8)
   character(len=10) :: b(3)
   call date_and_time( b(1), b(2), b(3), value )
   seed = value(1)+value(2)+value(3)+value(4)+value(5)+value(6)+value(7)+value(8)
   seed = seed + value(1)+value(2)+value(3)+value(4)+value(5)/100+value(6)*100+value(7)/10+value(8)*10

end subroutine seed_from_time


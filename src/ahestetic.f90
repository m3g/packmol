!
!  Written by Leandro Martínez, 2009-2011.
!  Copyright (c) 2009-2018, Leandro Martínez, Jose Mario Martinez,
!  Ernesto G. Birgin.
!
!  Module that contains some ahestetic output definitions
!
module ahestetic

   character(len=13), parameter :: dash1_line = "(  80('-')  )",&
      dash2_line = "(/,80('-')  )",&
      dash3_line = "(/,80('-'),/)"

   character(len=13), parameter :: hash1_line = "(  80('#')  )",&
      hash2_line = "(/,80('#')  )",&
      hash3_line = "(/,80('#'),/)"

   character(len=31), parameter :: prog1_line = "('  Packing:|0 ',tr55,'100%|' )",&
      prog2_line = "('   Moving:|0 ',tr55,'100%|' )"

end module ahestetic


subroutine printbar(iprint, iter, maxit)
   integer :: iter, maxit
   itprint = 60 / (maxit)
   if( iprint .ge. 2 ) then
      if( iter.eq.0) then
         write(*,"('          |')",advance='no')
      else if(iter.eq.maxit) then
         do i = itprint*(maxit-1), 60 
            write(*,"('*')", advance='no')
         end do
         write(*,"('|')")
      else
         do i = 1, itprint
            write(*,"('*')", advance='no')
         end do
      end if
   end if
end subroutine printbar

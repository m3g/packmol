!
!  Written by Leandro Martínez, 2009-2011.
!  Copyright (c) 2009-2018, Leandro Martínez, Jose Mario Martinez,
!  Ernesto G. Birgin.
!
! Function that determines the length of a string (better than
! intrinsic "len" because considers tabs as empty characters)
!
function strlength(string)

   use sizes
   implicit none
   integer :: strlength
   character(len=strl) :: string
   logical empty_char

   strlength = strl
   do while(empty_char(string(strlength:strlength)))
      strlength = strlength - 1
      if ( strlength == 0 ) exit
   end do

end function strlength

!
! Function that determines if a character is empty (empty, space, or tab)
! (nice suggestion from Ian Harvey -IanH0073- at github)
!

function empty_char(ch)
   character :: ch
   logical empty_char
   empty_char = .false.
   if ( ch == '' .or. &
      ch == achar(9) .or. &
      ch == achar(32) ) then
      empty_char = .true.
   end if
end function empty_char

!
! Function that replaces all non-space empty characters by spaces
!

function alltospace(record)

   use sizes
   implicit none
   integer :: i
   logical :: empty_char
   character(len=strl) :: alltospace, record

   do i = 1, strl
      if ( empty_char(record(i:i)) ) then
         alltospace(i:i) = " "
      else
         alltospace(i:i) = record(i:i)
      end if
   end do

end function alltospace

subroutine parse_spaces(record)
   use exit_codes
   use input, only : forbidden_char
   use sizes
   implicit none
   integer :: i, strlength
   character(len=strl) :: record
   ! Replace spaces within quotes by ~
   i = 0
   do while(i < strlength(record))
      i = i + 1
      if ( record(i:i) == '"' ) then
         i = i + 1
         do while(record(i:i) /= '"')
            i = i + 1
            if( i > strlength(record) ) then
               write(*,*) ' ERROR: Could not find ending quotes in line: ', trim(record)
               stop exit_code_input_error
            end if
            if(record(i:i) == " ") then
               record(i:i) = forbidden_char
            end if
         end do
      end if
   end do
   ! Replace spaces after \ by the forbidden_char and remove the \
   i = 0
   do while(i < strlength(record)-1)
      i = i + 1
      if (record(i:i) == "\" .and. record(i+1:i+1) == " ") then
         record(i:i) = forbidden_char
         record = record(1:i)//record(i+2:strlength(record))
      end if
   end do
end

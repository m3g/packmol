!
!  Written by Alexandr Fonari, 2022.
!  Copyright (c) 2009-2018, Leandro Martínez, Jose Mario Martinez,
!  Ernesto G. Birgin.
!

module exit_codes

   implicit none

   ! Codes 1, 2, 126 – 165 and 255 have special meaning
   integer, parameter :: exit_code_success = 0
   integer, parameter :: exit_code_general_error = 170
   integer, parameter :: exit_code_input_error = 171
   integer, parameter :: exit_code_open_file = 172
   integer, parameter :: exit_code_failed_to_converge = 173

end module exit_codes

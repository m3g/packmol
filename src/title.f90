!  
!  Written by Leandro Martínez, 2009-2011.
!  Copyright (c) 2009-2018, Leandro Martínez, Jose Mario Martinez,
!  Ernesto G. Birgin.
!  

! Routine to print the title 

subroutine title()

    use ahestetic
    write(*,hash3_line)
    write(*,"(' PACKMOL - Packing optimization for the automated generation of', /&
             &' starting configurations for molecular dynamics simulations.', /&
             &' ',/&
             &t61,' Version 21.0.1 ')")
    write(*,hash3_line)

end subroutine title

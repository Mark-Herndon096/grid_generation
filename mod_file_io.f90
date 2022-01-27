!======================================================================
!	Author: Mark Herndon
!	Date: 01/27/2022
!	Description: Module for grid generation program file I/O
!======================================================================
MODULE mod_file_io
    IMPLICIT NONE
    INTEGER :: ng !> number of grids to generate
    INTEGER, DIMENSION(:), ALLOCATABLE :: ni, nj, nk
    CHARACTER(LEN=100),    ALLOCATABLE :: fname(:)
    !!TODO: VARIABLE AND INTERFACE DECLARATIONS

CONTAINS

!! SUBROUTINES AND FUNCTIONS
!======================================================================
SUBROUTINE read_grid_parameters
    USE, INTRINSIC :: iso_fortran_env, ONLY : stderr => error_unit
    IMPLICIT NONE
    CHARACTER(len=100) :: input_fname
    INTEGER :: i, f_unit, io_stat
    LOGICAL :: ex_stat
    
    input_fname = 'grid_parameters.dat'
    NAMELIST /General_Data/ ng
    NAMELIST /Grid_Dimensions/ ni, nj, nk
    NAMELIST /Grid_Filenames/ fname

    INQUIRE (FILE=TRIM(input_fname), EXIST=ex_stat)
    IF (ex_stat .EQ. 0) THEN
        WRITE(stderr, '(3a)') 'Error: file "', trim(input_fname), '" not found'
    END IF
   
 
    OPEN(FILE=TRIM(input_fname), newunit=f_unit, ACTION='READ',STATUS='OLD')
    READ(NML=General_Data, UNIT=f_unit, iostat=io_stat) 
    IF (io_stat /= 0) THEN
        WRITE(stderr, '(3a)') 'Error reading namelist General_Data in "', trim(input_fname),'"'
    END IF
    ALLOCATE(ni(ng)) 
    ALLOCATE(nj(ng)) 
    ALLOCATE(nk(ng))
    ALLOCATE(fname(ng))
 
    READ(NML=Grid_Dimensions, UNIT=f_unit, iostat=io_stat) 
    IF (io_stat /= 0) THEN
        WRITE(stderr, '(3a)') 'Error reading namelist Grid_Dimensions in "', trim(input_fname),'"'
    END IF

    WRITE(*,*) 'ni(1) = ', ni(1)    
    WRITE(*,*) 'nj(1) = ', nj(1)    
    WRITE(*,*) 'nk(1) = ', nk(1)    
    WRITE(*,*) 'ni(2) = ', ni(2)    
    WRITE(*,*) 'nj(2) = ', nj(2)    
    WRITE(*,*) 'nk(2) = ', nk(2)    

    READ(NML=Grid_Filenames, UNIT=f_unit, iostat=io_stat) 
    IF (io_stat /= 0) THEN
        WRITE(stderr, '(3a)') 'Error reading namelist Grid_Dimensions in "', trim(input_fname),'"'
    END IF
    
    WRITE(*,*) 'fname(1) : ', TRIM(fname(1))
    WRITE(*,*) 'fname(2) : ', TRIM(fname(2))
    
    CLOSE(f_unit)
    

END SUBROUTINE read_grid_parameters
!=======================================================================
!=======================================================================
END MODULE mod_file_io
!======================================================================

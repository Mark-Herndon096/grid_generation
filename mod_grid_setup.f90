!======================================================================
!	Author: Mark Herndon
!	Date: 01/27/2022
!	Description: Module for grid generation file I/O and parameters
!======================================================================
MODULE mod_grid_setup
    IMPLICIT NONE
    INTEGER :: ng_total !> total number of grids to generate
    INTEGER :: ng       !> number of grids to generate 
    INTEGER, DIMENSION(:),      ALLOCATABLE :: ni, nj, nk
    REAL(KIND=8), DIMENSION(:), ALLOCATABLE :: xc, yc, zc 
    REAL(KIND=8), DIMENSION(:), ALLOCATABLE :: xspan 
    REAL(KIND=8), DIMENSION(:), ALLOCATABLE :: yspan 
    REAL(KIND=8), DIMENSION(:), ALLOCATABLE :: zspan 
    CHARACTER(LEN=100),         ALLOCATABLE :: fname(:)
    CHARACTER(LEN=20),          ALLOCATABLE :: grid_type(:)
    ! PERMISSIBLE GRID TYPE PARAMETERS
    CHARACTER(LEN=20), PARAMETER :: gtype1  = 'cylinder'
    CHARACTER(LEN=20), PARAMETER :: gtype2  = 'box_uniform'
    CHARACTER(LEN=20), PARAMETER :: gtype3  = 'box_stretched_x'
    CHARACTER(LEN=20), PARAMETER :: gtype4  = 'box_stretched_y'
    CHARACTER(LEN=20), PARAMETER :: gtype5  = 'box_stretched_z'
    CHARACTER(LEN=20), PARAMETER :: gtype6  = 'box_stretched_xy'
    CHARACTER(LEN=20), PARAMETER :: gtype7  = 'box_stretched_xz'
    CHARACTER(LEN=20), PARAMETER :: gtype8  = 'box_stretched_yz'
    CHARACTER(LEN=20), PARAMETER :: gtype9  = 'box_stretched_xyz'
    CHARACTER(LEN=20), PARAMETER :: gtype10 = 'wall_boundary_x'
    CHARACTER(LEN=20), PARAMETER :: gtype11 = 'wall_boundary_y'
    CHARACTER(LEN=20), PARAMETER :: gtype12 = 'wall_boundary_z'
    CHARACTER(LEN=20), PARAMETER :: gtype13 = 'wall_boundary_xy'
    CHARACTER(LEN=20), PARAMETER :: gtype14 = 'wall_boundary_xz'
    CHARACTER(LEN=20), PARAMETER :: gtype15 = 'wall_boundary_yz'
    CHARACTER(LEN=20), PARAMETER :: gtype16 = 'wall_boundary_xyz'
    CHARACTER(LEN=20)            :: grid_type_selector
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
    NAMELIST /Grid_Filenames/ fname
    NAMELIST /Grid_Type_Data/ grid_type

    INQUIRE (FILE=TRIM(input_fname), EXIST=ex_stat)
    IF (ex_stat .EQ. 0) THEN
        WRITE(stderr, '(3a)') 'Error: file "', trim(input_fname), '" not found'
    END IF
   
 
    OPEN(FILE=TRIM(input_fname), newunit=f_unit, ACTION='READ',STATUS='OLD')
    READ(NML=General_Data, UNIT=f_unit, iostat=io_stat) 
    IF (io_stat /= 0) THEN
        WRITE(stderr, '(3a)') 'Error reading namelist General_Data in "', trim(input_fname),'"'
    END IF

    ALLOCATE(fname(ng))
    ALLOCATE(grid_type(ng))
    !============================================================= 

    READ(NML=Grid_Filenames, UNIT=f_unit, iostat=io_stat) 
    IF (io_stat /= 0) THEN
        WRITE(stderr, '(3a)') 'Error reading namelist Grid_Filenames in "', trim(input_fname),'"'
    END IF
    
    READ(NML=Grid_Type_Data, UNIT=f_unit, iostat=io_stat) 
    IF (io_stat /= 0) THEN
        WRITE(stderr, '(3a)') 'Error reading namelist Grid_Type_Data in "', trim(input_fname),'"'
    END IF
    CLOSE(f_unit)
    
    DO i = 1, ng
        grid_type_selector = grid_type(i)
         
        SELECT CASE ( grid_type_selector ) 
            CASE ( gtype1 )
                WRITE(*,*) 'gtype == ', TRIM(gtype1)

            CASE ( gtype2 )
                WRITE(*,*) 'gtype == ', TRIM(gtype2)

            CASE ( gtype3 )
                WRITE(*,*) 'gtype == ', TRIM(gtype3)

            CASE ( gtype4 )
                WRITE(*,*) 'gtype == ', TRIM(gtype4)

            CASE ( gtype5 )
                WRITE(*,*) 'gtype == ', TRIM(gtype5)

            CASE ( gtype6 )
                WRITE(*,*) 'gtype == ', TRIM(gtype6)

            CASE ( gtype7 )
                WRITE(*,*) 'gtype == ', TRIM(gtype7)
        
            CASE ( gtype8 )
                WRITE(*,*) 'gtype == ', TRIM(gtype8)

            CASE ( gtype9 )
                WRITE(*,*) 'gtype == ', TRIM(gtype9)

            CASE ( gtype10 )
                WRITE(*,*) 'gtype == ', TRIM(gtype10)

            CASE ( gtype11 )
                WRITE(*,*) 'gtype == ', TRIM(gtype11)

            CASE ( gtype12 )
                WRITE(*,*) 'gtype == ', TRIM(gtype12)

            CASE ( gtype13 )
                WRITE(*,*) 'gtype == ', TRIM(gtype13)

            CASE DEFAULT
                WRITE(*,*) 'SPECIFY VALID GRID TYPE'
        END SELECT
    END DO
    
END SUBROUTINE read_grid_parameters
!=======================================================================
!=======================================================================
END MODULE mod_grid_setup
!======================================================================

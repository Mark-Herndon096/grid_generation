!======================================================================
!	Author: Mark Herndon
!	Date: 01/27/2022
!	Description: Module for grid generation file I/O and parameters
!======================================================================
MODULE mod_grid_setup
    IMPLICIT NONE
    INTEGER :: ng_total !> total number of grids to generate
    INTEGER, DIMENSION(:),      ALLOCATABLE :: ni, nj, nk
    INTEGER, DIMENSION(:,:),    ALLOCATABLE :: dims
    REAL(KIND=8), DIMENSION(:), ALLOCATABLE :: xc, yc, zc 
    REAL(KIND=8), DIMENSION(:), ALLOCATABLE :: rmin, rmax, b
    REAL(KIND=8), DIMENSION(:), ALLOCATABLE :: xspan, xstart
    REAL(KIND=8), DIMENSION(:), ALLOCATABLE :: yspan, ystart 
    REAL(KIND=8), DIMENSION(:), ALLOCATABLE :: zspan, zstart

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
    CHARACTER(LEN=20), ALLOCATABLE :: grid_type_array(:)
    INTEGER, DIMENSION(16) :: grid_type_counter 

    NAMELIST /general_data/           ng_total
    NAMELIST /grid_filenames/         fname
    NAMELIST /grid_type_data/         grid_type
    NAMELIST /cylinder_data/          dims, rmin, rmax, b, xc, yc, zspan
    NAMELIST /box_uniform_data/       dims, xspan, yspan, zspan, xstart, ystart, zstart 
    NAMELIST /box_stretched_x_data/   dims, xspan, yspan, zspan, xstart, ystart, zstart
    NAMELIST /box_stretched_y_data/   dims, xspan, yspan, zspan, xstart, ystart, zstart 
    NAMELIST /box_stretched_z_data/   dims, xspan, yspan, zspan, xstart, ystart, zstart 
    NAMELIST /box_stretched_xy_data/  dims, xspan, yspan, zspan, xstart, ystart, zstart 
    NAMELIST /box_stretched_xz_data/  dims, xspan, yspan, zspan, xstart, ystart, zstart 
    NAMELIST /box_stretched_yz_data/  dims, xspan, yspan, zspan, xstart, ystart, zstart 
    NAMELIST /box_stretched_xyx_data/ dims, xspan, yspan, zspan, xstart, ystart, zstart 
    NAMELIST /wall_boundary_x_data/   dims, xspan, yspan, zspan, xstart, ystart, zstart 
    NAMELIST /wall_boundary_y_data/   dims, xspan, yspan, zspan, xstart, ystart, zstart 
    NAMELIST /wall_boundary_z_data/   dims, xspan, yspan, zspan, xstart, ystart, zstart 
    NAMELIST /wall_boundary_xy_data/  dims, xspan, yspan, zspan, xstart, ystart, zstart
    NAMELIST /wall_boundary_xz_data/  dims, xspan, yspan, zspan, xstart, ystart, zstart
    NAMELIST /wall_boundary_yz_data/  dims, xspan, yspan, zspan, xstart, ystart, zstart
    NAMELIST /wall_boundary_xyx_data/ dims, xspan, yspan, zspan, xstart, ystart, zstart  
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



    INQUIRE (FILE=TRIM(input_fname), EXIST=ex_stat)
    IF (ex_stat .EQ. 0) THEN
        WRITE(stderr, '(3a)') 'Error: file "', trim(input_fname), '" not found'
    END IF
   
 
    OPEN(FILE=TRIM(input_fname), newunit=f_unit, ACTION='READ',STATUS='OLD')
    READ(NML=general_data, UNIT=f_unit, iostat=io_stat) 
    IF (io_stat /= 0) THEN
        WRITE(stderr, '(3a)') 'Error reading namelist general_data in "', trim(input_fname),'"'
    END IF

    ALLOCATE(fname(ng_total))
    ALLOCATE(grid_type(ng_total))
    !============================================================= 

    READ(NML=grid_filenames, UNIT=f_unit, iostat=io_stat) 
    IF (io_stat /= 0) THEN
        WRITE(stderr, '(3a)') 'Error reading namelist Grid_Filenames in "', trim(input_fname),'"'
    END IF
    
    READ(NML=grid_type_data, UNIT=f_unit, iostat=io_stat) 
    IF (io_stat /= 0) THEN
        WRITE(stderr, '(3a)') 'Error reading namelist grid_type_data in "', trim(input_fname),'"'
    END IF

    CALL get_grid_type_count
    
    CLOSE(f_unit)

    !DO i = 1, ng_total
    !    grid_type_selector = grid_type(i)
    !    SELECT CASE ( grid_type_selector ) 
    !        CASE ( gtype1 )
    !            WRITE(*,*) 'gtype == ', TRIM(gtype1)

    !        CASE ( gtype2 )
    !            WRITE(*,*) 'gtype == ', TRIM(gtype2)

    !        CASE ( gtype3 )
    !            WRITE(*,*) 'gtype == ', TRIM(gtype3)

    !        CASE ( gtype4 )
    !            WRITE(*,*) 'gtype == ', TRIM(gtype4)

    !        CASE ( gtype5 )
    !            WRITE(*,*) 'gtype == ', TRIM(gtype5)

    !        CASE ( gtype6 )
    !            WRITE(*,*) 'gtype == ', TRIM(gtype6)

    !        CASE ( gtype7 )
    !            WRITE(*,*) 'gtype == ', TRIM(gtype7)
    !    
    !        CASE ( gtype8 )
    !            WRITE(*,*) 'gtype == ', TRIM(gtype8)

    !        CASE ( gtype9 )
    !            WRITE(*,*) 'gtype == ', TRIM(gtype9)

    !        CASE ( gtype10 )
    !            WRITE(*,*) 'gtype == ', TRIM(gtype10)

    !        CASE ( gtype11 )
    !            WRITE(*,*) 'gtype == ', TRIM(gtype11)

    !        CASE ( gtype12 )
    !            WRITE(*,*) 'gtype == ', TRIM(gtype12)

    !        CASE ( gtype13 )
    !            WRITE(*,*) 'gtype == ', TRIM(gtype13)

    !        CASE DEFAULT
    !            WRITE(*,*) 'SPECIFY VALID GRID TYPE'
    !    END SELECT
    !END DO
    
END SUBROUTINE read_grid_parameters
!=======================================================================
!=======================================================================
SUBROUTINE prepare_grid_variables(ng,grid_type_str)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: ng
    CHARACTER(LEN=*), INTENT(IN) :: grid_type_str
    WRITE(*,'(A,I0,A,A)') 'Creating ', ng, ' grids of type ', TRIM(grid_type_str)

END SUBROUTINE prepare_grid_variables
!=======================================================================
!=======================================================================
SUBROUTINE get_grid_type_count
    IMPLICIT NONE
    INTEGER :: i, j, k, n 
    grid_type_counter(:) = 0;
    ALLOCATE(grid_type_array(16))
    grid_type_array(1)  = gtype1
    grid_type_array(2)  = gtype2
    grid_type_array(3)  = gtype3
    grid_type_array(4)  = gtype4
    grid_type_array(5)  = gtype5
    grid_type_array(6)  = gtype6
    grid_type_array(7)  = gtype7
    grid_type_array(8)  = gtype8
    grid_type_array(9)  = gtype9
    grid_type_array(10) = gtype10
    grid_type_array(11) = gtype11
    grid_type_array(12) = gtype12
    grid_type_array(13) = gtype13
    grid_type_array(14) = gtype14
    grid_type_array(15) = gtype15
    grid_type_array(16) = gtype16
    
    DO n = 1, ng_total
        DO i = 1, 16
            IF ( grid_type(n) .EQ. grid_type_array(i) ) THEN
                grid_type_counter(i) = grid_type_counter(i) + 1
            END IF
        END DO
    END DO
    
    

END SUBROUTINE get_grid_type_count
!=======================================================================
!=======================================================================
END MODULE mod_grid_setup
!======================================================================

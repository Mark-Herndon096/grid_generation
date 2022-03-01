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
    REAL(KIND=8), DIMENSION(:), ALLOCATABLE :: xspan, xstart, x_off, tau_x
    REAL(KIND=8), DIMENSION(:), ALLOCATABLE :: yspan, ystart, y_off, tau_y 
    REAL(KIND=8), DIMENSION(:), ALLOCATABLE :: zspan, zstart, z_off, tau_z

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
    NAMELIST /cylinder_data/          ni, nj, nk, rmin, rmax, b, xc, yc, zspan
    NAMELIST /box_uniform_data/       ni, nj, nk, xspan, yspan, zspan, xstart, ystart, zstart
    NAMELIST /box_stretched_x_data/   ni, nj, nk, xspan, yspan, zspan, xstart, ystart, zstart, x_off
    NAMELIST /box_stretched_y_data/   ni, nj, nk, xspan, yspan, zspan, xstart, ystart, zstart, y_off 
    NAMELIST /box_stretched_z_data/   ni, nj, nk, xspan, yspan, zspan, xstart, ystart, zstart, z_off 
    NAMELIST /box_stretched_xy_data/  ni, nj, nk, xspan, yspan, zspan, xstart, ystart, zstart, x_off, y_off 
    NAMELIST /box_stretched_xz_data/  ni, nj, nk, xspan, yspan, zspan, xstart, ystart, zstart, x_off, z_off 
    NAMELIST /box_stretched_yz_data/  ni, nj, nk, xspan, yspan, zspan, xstart, ystart, zstart, y_off, z_off, tau_y, tau_z 
    NAMELIST /box_stretched_xyx_data/ ni, nj, nk, xspan, yspan, zspan, xstart, ystart, zstart, x_off, y_off, z_off 
    NAMELIST /wall_boundary_x_data/   ni, nj, nk, xspan, yspan, zspan, xstart, ystart, zstart  
    NAMELIST /wall_boundary_y_data/   ni, nj, nk, xspan, yspan, zspan, xstart, ystart, zstart  
    NAMELIST /wall_boundary_z_data/   ni, nj, nk, xspan, yspan, zspan, xstart, ystart, zstart  
    NAMELIST /wall_boundary_xy_data/  ni, nj, nk, xspan, yspan, zspan, xstart, ystart, zstart 
    NAMELIST /wall_boundary_xz_data/  ni, nj, nk, xspan, yspan, zspan, xstart, ystart, zstart 
    NAMELIST /wall_boundary_yz_data/  ni, nj, nk, xspan, yspan, zspan, xstart, ystart, zstart 
    NAMELIST /wall_boundary_xyx_data/ ni, nj, nk, xspan, yspan, zspan, xstart, ystart, zstart   
CONTAINS

!! SUBROUTINES AND FUNCTIONS
!======================================================================
SUBROUTINE read_grid_parameters
    USE, INTRINSIC :: iso_fortran_env, only : stderr => error_unit
    IMPLICIT NONE
    CHARACTER(LEN=100) :: input_fname
    INTEGER :: i, f_unit, io_stat
    LOGICAL :: ex_stat
    
    input_fname = 'grid_parameters.dat'



    inquire (file=trim(input_fname), exist=ex_stat)
    if (ex_stat .eq. 0) then
        write(stderr, '(3a)') 'error: file "', trim(input_fname), '" not found'
    end if
   
 
    open(file=trim(input_fname), newunit=f_unit, action='read',status='old')
    read(nml=general_data, unit=f_unit, iostat=io_stat) 
    if (io_stat /= 0) then
        write(stderr, '(3a)') 'error reading namelist general_data in "', trim(input_fname),'"'
    end if

    allocate(fname(ng_total))
    allocate(grid_type(ng_total))
    !============================================================= 

    read(nml=grid_filenames, unit=f_unit, iostat=io_stat) 
    if (io_stat /= 0) then
        write(stderr, '(3a)') 'error reading namelist grid_filenames in "', trim(input_fname),'"'
    end if
    
    read(nml=grid_type_data, unit=f_unit, iostat=io_stat) 
    if (io_stat /= 0) then
        write(stderr, '(3a)') 'error reading namelist grid_type_data in "', trim(input_fname),'"'
    end if

    call get_grid_type_count
    
    close(f_unit)

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
    INTEGER :: n
    WRITE(*,'(A,I0,A,A)') 'Creating ', ng, ' grids of type ', TRIM(grid_type_str)
    
        
    grid_type_selector = grid_type_str
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
            CALL allocate_vars(ng)
            CALL box_stretched_yz(ng)
            CALL deallocate_vars(ng)
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

    

END SUBROUTINE prepare_grid_variables
!=======================================================================
!=======================================================================
SUBROUTINE allocate_vars(ng)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: ng
    ALLOCATE(ni(ng))
    ALLOCATE(nj(ng))
    ALLOCATE(nk(ng))
    ALLOCATE(rmin(ng))
    ALLOCATE(rmax(ng))
    ALLOCATE(b(ng))
    ALLOCATE(xc(ng))
    ALLOCATE(yc(ng))
    ALLOCATE(xspan(ng))
    ALLOCATE(yspan(ng))
    ALLOCATE(zspan(ng))
    ALLOCATE(xstart(ng))
    ALLOCATE(ystart(ng))
    ALLOCATE(zstart(ng))
    ALLOCATE(x_off(ng))
    ALLOCATE(y_off(ng))
    ALLOCATE(z_off(ng))
    ALLOCATE(tau_x(ng))
    ALLOCATE(tau_y(ng))
    ALLOCATE(tau_z(ng))
END SUBROUTINE allocate_vars
!=======================================================================
!=======================================================================
!=======================================================================
!=======================================================================
SUBROUTINE deallocate_vars(ng)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: ng
    DEALLOCATE(ni)
    DEALLOCATE(nj)
    DEALLOCATE(nk)
    DEALLOCATE(rmin)
    DEALLOCATE(rmax)
    DEALLOCATE(b)
    DEALLOCATE(xc)
    DEALLOCATE(yc)
    DEALLOCATE(xspan)
    DEALLOCATE(yspan)
    DEALLOCATE(zspan)
    DEALLOCATE(xstart)
    DEALLOCATE(ystart)
    DEALLOCATE(zstart)
    DEALLOCATE(x_off)
    DEALLOCATE(y_off)
    DEALLOCATE(z_off)
    DEALLOCATE(tau_x)
    DEALLOCATE(tau_y)
    DEALLOCATE(tau_z)
END SUBROUTINE deallocate_vars
!=======================================================================
!=======================================================================
SUBROUTINE box_stretched_yz(ng)
    USE, INTRINSIC :: iso_fortran_env, only : stderr => error_unit
    IMPLICIT NONE
    CHARACTER(LEN=100) :: input_fname
    INTEGER :: i, f_unit, io_stat, n
    LOGICAL :: ex_stat
    INTEGER, INTENT(IN) :: ng

    input_fname = 'grid_parameters.dat'

    open(file=trim(input_fname), newunit=f_unit, action='read',status='old')
    read(nml=box_stretched_yz_data, unit=f_unit, iostat=io_stat) 
    if (io_stat /= 0) then
        write(stderr, '(3a)') 'error reading namelist general_data in "', trim(input_fname),'"'
    end if
    
    DO n = 1, ng
    

    
    END DO 

END SUBROUTINE box_stretched_yz
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

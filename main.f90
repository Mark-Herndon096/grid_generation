!======================================================================
!	Author: Mark Herndon
!	Date: 01/27/22
!	Description: Main driver for grid generation program
!======================================================================
PROGRAM main
    USE mod_grid_setup, ONLY : read_grid_parameters, grid_type, &
                               ng_total, grid_type_array,       &
                               grid_type_counter
    IMPLICIT NONE
    INTEGER :: i, j, k, n

    ! read general grid parameters and grid types
    CALL read_grid_parameters
    
    ! Cycle through grids of each type and create/write grid files
    DO i = 1, 16
        IF ( grid_type_counter(i) .NE. 0 ) THEN
            WRITE(*,'(I0,A,A,A,A)') grid_type_counter(i), ' grid of type ', TRIM(grid_type_array(i)), ' detected'
        END IF
    END DO
    
     
    
     
    

    
END PROGRAM main
!======================================================================

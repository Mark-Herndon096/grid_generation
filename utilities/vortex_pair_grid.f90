!======================================================================
!	Author: Mark Herndon
!	Date: 02/28/22
!	Description: Wall grid with constant spacing in center region
!======================================================================
PROGRAM vortex_pair_grid
    IMPLICIT NONE
    INTEGER :: ng
    INTEGER :: ni, nj, nk
    INTEGER :: ni_c, ni_e
    INTEGER :: i, j, k, ii, jj 
    
    REAL(KIND=8) :: dx_min, dy_min
    REAL(KIND=8) :: s, dx, dy, dz
    REAL(KIND=8) :: xspan, xcspan, ymax
    REAL(KIND=8), ALLOCATABLE, DIMENSION(:,:,:) :: x, y, z
    CHARACTER(LEN=100) :: fname

    WRITE(*,'(A\)') 'Specify output grid name: '
    READ (*,*) fname

    ng = 1
    ni = 575
    nj = 450
    nk = 3

    xspan  = 100.d0;
    xcspan = 3.d0;
    dx_min = 0.008d0;
    ymax   = 50.d0;
    
    dy_min = dx_min/2.d0; 
    
    dz = 0.5d0;

    ni_c = INT(xcspan/dx_min)
    ni_e = INT((ni-ni_c)/2)
    WRITE(*,*) 'ni   == ', ni
    WRITE(*,*) 'ni_c == ', ni_c
    WRITE(*,*) 'ni_e == ', ni_e
    ni = INT(ni_c + 2*ni_e)
    WRITE(*,*) 'ni   == ', ni

    ALLOCATE(x(ni,nj,nk))
    ALLOCATE(y(ni,nj,nk))
    ALLOCATE(z(ni,nj,nk))

    !! Create x grid with equal spaced grid in -xcspan/2.0 --> xcspan/2.0
    DO k = 1, nk
        DO j = 1, nj 
            DO i = 1, ni_e
                ii = ni_e + 1 - i
                s  = (REAL(ii-1,KIND=8))/REAL(ni_e-1,KIND=8)
                dx = -xcspan/2.d0 - dx_min*(REAL(ii-1,KIND=8)) - (dx_min - dx_min*REAL(ni_e,KIND=8) + xspan/2.d0 - xcspan/2.d0)*s**4
                x(i,j,k) = dx
            END DO
        END DO
    END DO

    DO k = 1, nk
        DO j = 1, nj 
            DO i = ni_e+1, ni_e+ni_c
                x(i,j,k) = -xcspan/2.d0 + REAL(i-1,KIND=8)*dx_min - REAL(ni_e-1,KIND=8)*dx_min
            END DO
        END DO
    END DO
    
    DO k = 1, nk
        DO j = 1, nj 
            DO i = ni_e+ni_c+1, ni
                ii = i - ni_e - ni_c
                s  = (REAL(ii-1,KIND=8))/REAL(ni_e-1,KIND=8)
                dx = xcspan/2.d0 + dx_min*(REAL(ii-1,KIND=8)) + (dx_min - dx_min*REAL(ni_e,KIND=8) + xspan/2.d0 - xcspan/2.d0)*s**4
                x(i,j,k) = dx
            END DO
        END DO
    END DO

    !! Create y grid with wall normal streching function
    DO k = 1, nk
        DO j = 1, nj
            s = REAL(j-1,KIND=8)/REAL(nj-1,KIND=8)
            dy = dy_min*REAL(j-1,KIND=8) + ( dy_min - dy_min*REAL(nj,KIND=8) + ymax)*s**4
            DO i = 1, ni
                y(i,j,k) = dy
            END DO
        END DO
    END DO

    !! 2D in k-direction for z grid
    DO k = 1, nk
        DO j = 1, nj
            DO i = 1, ni
                z(i,j,k) = REAL(k-1,KIND=8)*dz
            END DO
        END DO
    END DO

   OPEN(1,FILE=TRIM(fname),FORM='UNFORMATTED',ACTION='WRITE',STATUS='REPLACE')
   WRITE(1) ng
   WRITE(1) ni, nj, nk
   WRITE(1) x, y, z
   CLOSE(1)
END PROGRAM vortex_pair_grid
!======================================================================

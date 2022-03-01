PROGRAM xy_stretch
    IMPLICIT NONE
    INTEGER      :: num_grds = 1
    INTEGER      :: ni, nj, nk          !> Grid indices
    INTEGER      ::  i,  j,  k          !> Looping indices
    INTEGER      :: frL = 2, frR = 2, z3 = 1
    REAL(KIND=8) :: xspan, yspan, zspan !> Physical grid spans
    REAL(KIND=8) ::    dx,    dy,    dz !> Differential 
    REAL(KIND=8) :: y_c, x_c, tau_y, tau_x, y_bar, x_bar, B       !> grid stretching parameters
    REAL(KIND=8) :: x_val, y_val, z_val, x_off, y_off
    REAL(KIND=8) :: t1, t2, t3, t4
    REAL(KIND=8), ALLOCATABLE, DIMENSION(:,:,:) :: x, y, z
    CHARACTER(LEN=100) :: fname

    zspan = 0.5d0   ! Domain length in z-direction
    
    WRITE(*,'(A\)') 'Specify grid dimensions (ni,nj,nk): '
    READ (*,*) ni, nj, nk
     
    WRITE(*,'(A\)') 'Specify grid span (xspan,yspan): '
    READ (*,*) xspan, yspan

    WRITE(*,'(A\)') 'Specify offsets (x_off, y_off): '
    READ (*,*) x_off, y_off

    WRITE(*,'(A\)') 'Specify grid span (tau_x,tau_y): '
    READ (*,*) tau_x, tau_y

    WRITE(*,'(A\)') 'Specify output grid name: '
    READ (*,*) fname
    
    IF (nk <= 3) THEN
       WRITE(*,*) 'Setting k-dimension for 2D simulation (nk = 3)'
       nk = 3
       z3 = 0
    END IF
    z3 = 0;
    ALLOCATE(x(ni,nj,1-z3*frL:nk+z3*frR)) ! Add k- dimensions
    ALLOCATE(y(ni,nj,1-z3*frL:nk+z3*frR)) ! Add k- dimensions
    ALLOCATE(z(ni,nj,1-z3*frL:nk+z3*frR)) ! Add k- dimensions

    dz = zspan/REAL(nk-1,KIND=8)
    dy = yspan/REAL(nj-1,KIND=8)
    dx = xspan/REAL(ni-1,KIND=8)
    
    x_c = x_off + xspan/2.d0; y_c = y_off + yspan/2.d0;


    DO k = 1-z3*frL, nk+z3*frR
            z_val    = REAL(k-1,KIND=8)*dz;
        DO j = 1, nj
                y_bar = REAL(j-1,KIND=8)*dy
                t1    = 1.d0 + (EXP( tau_y ) - 1.d0)*y_c/yspan;
                t2    = 1.d0 + (EXP(-tau_y ) - 1.d0)*y_c/yspan;
                B     = (1.d0/(2.d0*tau_y))*LOG(t1/t2);
                t3    = SINH(tau_y*(y_bar/yspan - B));
                t4    = SINH(tau_y*B);
                y_val = y_c*(1.d0 + t3/t4);
            DO i = 1, ni
                x_bar = REAL(i-1,KIND=8)*dx
                t1    = 1.d0 + (EXP( tau_x ) - 1.d0)*x_c/xspan;
                t2    = 1.d0 + (EXP(-tau_x ) - 1.d0)*x_c/xspan;
                B     = (1.d0/(2.d0*tau_x))*LOG(t1/t2);
                t3    = SINH(tau_x*(x_bar/xspan - B));
                t4    = SINH(tau_x*B);
                x_val = x_c*(1.d0 + t3/t4);
                x(i,j,k) = -xspan/2.d0 + x_val;
                y(i,j,k) = -yspan/2.d0 + y_val;
                z(i,j,k) = z_val;
            END DO
        END DO
    END DO
    

   OPEN(1,FILE=TRIM(fname),FORM='UNFORMATTED',ACTION='WRITE',STATUS='REPLACE')
   WRITE(1) num_grds
   WRITE(1) ni, nj, nk
   WRITE(1) x, y, z
   CLOSE(1)
END PROGRAM xy_stretch

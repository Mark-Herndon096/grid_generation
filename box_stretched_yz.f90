SUBROUTINE box_stretched_yz(ni, nj, nk, xspan, yspan, zspan, x_off, y_off, z_off, tau_x, tau_y, tau_z, grid_name)
    IMPLICIT NONE
    INTEGER                      :: num_grds = 1
    INTEGER                      :: frL = 2, frR = 2, z3 = 1
    INTEGER, INTENT(IN)          :: ni, nj, nk
    REAL(KIND=8), INTENT(IN)     :: xspan, yspan, zspan
    REAL(KIND=8), INTENT(IN)     :: x_off, y_off, z_off
    REAL(KIND=8), INTENT(IN)     :: tau_x, tau_y, tau_z
    CHARACTER(LEN=*), INTENT(IN) :: grid_name
    REAL(KIND=8), ALLOCATABLE, DIMENSION(:,:,:) :: x, y, z
    REAL(KIND=8) :: dx, dy, dz, x_c, y_c, z_c
    REAL(KIND=8) :: x_val, y_val, z_val, x_bar, y_bar, z_bar
    REAL(KIND=8) :: t1, t2, t3, t4, B
    INTEGER      :: i, j, k


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




END SUBROUTINE box_stretched_yz

PROGRAM main
    USE ISO_C_BINDING
    IMPLICIT NONE
    INTEGER :: N, v, u
    INTEGER, ALLOCATABLE, DIMENSION(:, :) :: z_array_2d
    
    PRINT*, "Enter the size of the Array (Must be 2, 4, 8, or 16):"
    READ(*, *) N

    ALLOCATE(z_array_2d(N, N))

    IF (N /= 2) THEN
        IF (N /= 4) THEN
            IF (N /= 8) THEN
                IF (N /= 16) THEN
                    PRINT*, "Input was not one of 2, 4, , 8. or 16"
                    STOP 1
                ENDIF
            ENDIF
        ENDIF
    ENDIF

    DO v = 1, N
        DO u = 1, N
            z_array_2d(u, v) = z_order2d(v, u)
        ENDDO
    ENDDO 

   
    DO v = 1, N
        IF (v == 1) THEN
            PRINT*, "A = [", z_array_2d(v, :)
        ELSE
            PRINT*, z_array_2d(v, :)
        ENDIF   
    ENDDO
    
        
    CONTAINS

        FUNCTION z_order2d(i, j) RESULT(z)
            !USE ISO_C_BINDING
            !IMPLICIT NONE

            INTEGER(C_INT), INTENT(IN) :: i, j
            INTEGER(C_INT) :: z

            INTEGER(C_INT) :: k,ik,jk,zk,ord
            INTEGER(C_INT8_T) :: ib(32), jb(32), zb(64)

            ib=0; jb=0; zb=0
            ik=i-1; jk=j-1
            DO k=1, 32
                ib(k)=INT(MOD(ik,2), KIND=C_INT8_T); ik=ik/2
                jb(k)=INT(MOD(jk,2), KIND=C_INT8_T); jk=jk/2
            ENDDO

            zk=1
            DO k=1, 32
                zb(zk)=ib(k);
                zb(zk+1)=jb(k);
                zk = zk+2
            ENDDO

            z=1; ord=1
            DO k=1, 64
                z=z+zb(k)*ord
                ord=ord+ord
            ENDDO
        ENDFUNCTION
ENDPROGRAM

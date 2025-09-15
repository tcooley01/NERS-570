FUNCTION z_order2d_fortran(i, j) RESULT(z) bind (C, name="z_order2d_fortran")

    USE ISO_C_BINDING
    IMPLICIT NONE

    INTEGER(C_INT), INTENT(IN) :: i, j
    INTEGER(C_INT) :: z
    INTEGER(C_INT) :: k, ik, jk, zk, ord
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
        zk=zk+2
    ENDDO

    z=1; ord=1
    DO k=1, 64
        z=z+zb(k)*ord
        ord=ord+ord
    ENDDO
ENDFUNCTION




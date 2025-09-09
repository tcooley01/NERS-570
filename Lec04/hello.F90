

SUBROUTINE myHello()   
#ifdef __GFORTRAN__ ! W: Illegal preprocessor directive                                          
 WRITE (0,'(a,i2,a )') 'File: "'//__FILE__//'", line',__LINE__, & ! E: Syntax error in expression
 " was compiled with gfortran!"
#else ! W: Illegal preprocessor directive
 WRITE (0,'(a,i2,a )') 'File : "'//__FILE__//'", line ',__LINE__, & ! E: Syntax error in expressi
 " was NOT compiled with gfortran ! "
#endif ! W: Illegal preprocessor directive
ENDSUBROUTINE

PROGRAM main
CALL myHello()
ENDPROGRAM



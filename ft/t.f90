PROGRAM ext
INTRINSIC SIN
EXTERNAL fun,proc2,prt
REAL fun, f1
f1=fun(3.1415926/6,SIN) 
!用标准函数SIN调用函数fun
PRINT * ,'f1=',f1
CALL proc1(10,20,prt)
!用子程序prt调用子程序proc1
CALL proc2(10,20,prt)
!用子程序prt调用子程序proc2

contains
SUBROUTINE proc1(x, y, p) 
!子程序p为形式子程序
INTEGER X, Y
CALL p(x*y)
END SUBROUTINE

END PROGRAM ext
FUNCTION fun (x,f)  !函数f为形式函数
REAL X
fun=f(x)
END FUNCTION

SUBROUTINE proc2(x, y, p) 
!子程序p为形式子程序
INTEGER X, Y
CALL p(x+y)
END SUBROUTINE

SUBROUTINE prt (x)
INTEGER X
PRINT *,'x=',X
END SUBROUTINE


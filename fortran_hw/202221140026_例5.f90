PROGRAM main
implicit none
integer :: m,n
integer :: r1,r2,r3
integer :: gcd,lcm
PRINT*, "input two Natural Numbers: "
READ*,m,n

r1=m
r2=n
do

if(r2==0) exit
 r3=mod(r1,r2)
 r1=r2
 r2=r3
end do
gcd=r1
lcm=m*n/gcd
  

PRINT"('Greatest common divisor of',I6,' and',I6,' is',I6,'.')",M,N,gcd
PRINT"('Lowest common multiple of',I6,' and',I6,' is',I6,'.')",M,N,lcm
END program main


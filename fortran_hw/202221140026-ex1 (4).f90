program main
implicit none
real,parameter :: pi=acos(-1.0)
real :: x1=0,x2=2*pi,INTF
print*,intf(x1,x2,100000)
end

real function intf(xi,xf,n)
implicit none
real :: xi,xf,h,x,f
integer n,i
intf=0
h=(xf-xi)/n
x=xi
do i=0,n,2
 intf=intf+h/3.0*(f(x)+4.0*f(x+h)+f(x+2*h))
 x=x+2.0*h
enddo
end

real function f(x)
implicit none
real x
f=sin(x)
end

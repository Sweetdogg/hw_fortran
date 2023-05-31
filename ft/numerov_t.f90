!-----------!
module wfunc
integer :: nx
real(8) :: mx,dx
real(8),allocatable :: psi(:)
end module wfunc

!--------------!
program Quantum
use wfunc
implicit none
real(8),parameter :: pi=4.d0*atan(1.d0)
integer :: i
real(8) :: e1,e2,e3
real(8) :: bound1,bound2,bound3

mx=5.d0
dx=0.01d0
nx=mx/dx
allocate(psi(-nx:nx))

e1=0.d0
call Numerov(e1,bound1)

e2=e1
do
e2=e2+0.1d0
call Numerov(e2,bound2)
if(bound1*bound2<0.d0)exit
e1=e2
end do

do 
e3=(e1+e2)/2.d0
call Numerov(e3,bound3)
if(abs(bound3)<1.d-12.or.abs(e3-e1)<1.d-12)exit
if(bound3*bound1>0.d0)then
  e1=e3
else
  e2=e3
endif
enddo
print*,'calculated:',e3,psi(nx)
print*,'   correct:',1.50

open(10,file='psi.dat',status='replace')
do i=-nx,nx
   write(10,*)dx*i,psi(i)
enddo
close(10)

deallocate(psi)

end program Quantum

!--------------------------!
subroutine Numerov(e,bound)
use wfunc
implicit none
integer :: i
real(8) :: e,f,bound
real(8) :: q0,q1,q2

psi(-nx)=0.d0
psi(-nx+1)=1.d0
!psi(nx)=0.d0

 f=-2.d0*e+(dx*i)**2

q0=psi(-nx)*(1.d0-dx**2.d0*f/12.d0)
q1=psi(-nx+1)*(1.d0-dx**2.d0*f/12.d0)

do i=-nx+2,nx
   q2=dx**2.d0*f*psi(i-1)+2.d0*q1-q0
   q0=q1
   q1=q2
   f=-2.d0*e+(dx*i)**2
   psi(i)=q1/(1.d0-dx**2.d0*f/12.d0)
enddo

psi=psi/sqrt(sum(psi(:)**2.d0)*dx)

bound=psi(nx)

end subroutine Numerov

real*8 function f(e,dx,i)
implicit none
integer i
real(8) e,dx
f=-2.d0*e+(dx*i)**2
end

program main
implicit none
real(8),parameter :: pi=4.d0*atan(1.d0)
real(8) :: v0,a,b
real(8),allocatable :: h(:,:),vec(:,:),eig(:)
integer :: i,j,n
real(8) :: intf,f,x,psi
print*,"input n:"
read*,n
v0=50.d0
a=-0.5d0
b=0.5d0
allocate(h(n,n))
allocate(vec(n,n))
allocate(eig(n))
do i=1,n
  do j=1,n
     h(i,j)=intf(i,j,a,b,v0)
  enddo
enddo
do i=1,n
   h(i,i)=h(i,i)+dble(i)**2*pi**2/8.d0
enddo
call diagonalize(n,h,vec,eig)

write(*,"(i6,f12.6)")n,eig(1)

open(10,file='gs.dat',status='replace')
x=-1.d0
do while (x<=1.d0) 
psi=0.d0
do i=1,n
   psi=psi+f(i,x)*vec(i,1)
enddo
write(10,*)x,psi
x=x+0.01
enddo
close(10)

deallocate(h)
deallocate(vec)
deallocate(eig)
end program main

function f(n,x)
implicit none
real(8),parameter :: pi=4.d0*atan(1.d0)
real(8) :: f,x
integer :: n
if(mod(n,2)==0)then
   f=sin(dble(n)*pi*x/2.d0)
else
   f=cos(dble(n)*pi*x/2.d0)
endif
end function f

function intf(p,k,x1,x2,v0)
implicit none
real(8) :: x1,x2,h,x,f,intf,v0
integer ::n,i,p,k
n=10000
h=(x2-x1)/dble(n)
x=x1
intf=v0*f(k,x)*f(p,x)
do i=1,n-1
   x=x+h
   intf=intf+dble((mod(i,2)+1)*2)*v0*f(k,x)*f(p,x)
enddo
x=x+h
intf=intf+v0*f(k,x)*f(p,x)
intf=intf*h/3.d0
end function intf

subroutine diagonalize(n,mat,vec,eig)
implicit none
integer :: n,info
real(8) :: mat(n,n),vec(n,n),eig(n),work(n*(3+n/2))
vec=mat
call dsyev('V','U',n,vec,n,eig,work,n*(3+n/2),info)
end subroutine diagonalize

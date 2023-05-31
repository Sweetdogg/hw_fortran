module sys

end module

program main
implicit none
real :: dx=0.1

open(1,file='psi.dat')
do while(x<=1)
	call nume(xi,xf,e,dx)
	write(1,*)
enddo


end

subroutine nume(e,dx)
implicit none
real :: dx,x,e,xi,f
	x=xi
	x=x+dx
enddo

e

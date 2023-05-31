program main
implicit none
integer*8 :: i,j
real*8 :: r(201)=(/(real(i*0.001),i=800,1000)/),x=0.5,x_a(201)
open(1,file='out.dat',status='unknown')

do j=1,201
	x_a(j)=4*r(j)*x*(1-x)	
end do

do i=1,1000
	do j=1,201
		x_a(j)=4*r(j)*x_a(j)*(1-x_a(j))
		write(1,*)r(j),x_a(j)
	end do 
end do
close(1)
end

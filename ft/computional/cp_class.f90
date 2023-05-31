program main
implicit none
real*8 :: a=0.1,w=0.86*4.69,k=22,c=0.8,hf=0,h=3,v=0,t=0,vf,dt=0.001
open(1,file='out.dat',status='unknown')

do while(t<100)
	hf=h-a*cos(w*t)
	vf=v+w*a*sin(w*t)
	call euler_v(v,k,hf,c,vf,theta,dt,t,h)
	write(1,*)t,hf
end do
close(1)

contains
	function theta(x)
	implicit none
	real*8 :: x,theta
	if(x>=0) then
	  theta=1
	else
	  theta=0
	end if
	end function
	
	subroutine euler_v(v,k,hf,c,vf,theta,dt,t,h)
	implicit none
	real*8 :: v,k,hf,c,vf,theta,dt,t,h
	t=t+dt
	v=v+(-k*hf-c*vf)*theta(-hf)*dt-dt
	h=h+v*dt
	end subroutine
	
	
end

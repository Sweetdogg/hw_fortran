program main
implicit none
real*8 :: r=0.1,w=400,dt=0.001,x=0,v=1,t=0

open(1,file='osc.dat',status='unknown')
do while(t<1)
	call euler_osc(x,v,t,dt,r,w)
	write(1,*) x,t
end do 
close(1)


contains 
	subroutine euler_osc(x,v,t,dt,r,w)	
	real*8 :: x,y,t,dt,r,w,v
	
	x=x+dt*v
	v=v-2*r*v*dt-w**2*x*dt
	t=dt+t
	
	end subroutine
end

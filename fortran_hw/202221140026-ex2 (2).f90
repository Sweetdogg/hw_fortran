program main
implicit none
 real*8 :: x=1,vx=1,t=0,dt=0.01,y=1,vy=0,r
open(1,file='trace')
do while(t<100)
r=x**2+y**2
vx=vx-dt*x*r**(-1.5)
vy=vy-dt*y*r**(-1.5)
x=x+dt*vx
y=y+dt*vy
t=t+dt
write(1,*)x,y
enddo
close(1)
end





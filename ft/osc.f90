program main
implicit none 
real*8 :: x=0,v=1,t=0,dt=0.01,wf=(2)**0.5d0

open(1,file='output2.dat')
do while(t<50)
 call step(x,v,t,dt,wf)
 write(1,*)t,x,v
enddo
end

subroutine step(x,v,t,dt,wf)
implicit none
real*8 :: x,v,t,dt,wf
v=((1.0-dt*0.5d0)*v+dt*(-x+sin(wf*t)))/(1+dt*0.5d0)
x=x+dt*v
t=t+dt
end


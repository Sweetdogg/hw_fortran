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
real*8 :: x,v,t,dt,wf,e,a
real*8 k1,l1,k2,l2,k3,l3,k4,l4
   a=-v-x+sin(wf*t)
   k1=dt*a
   l1=dt*v
   a=-(x+l1/2.0)-(v+k1/2.0)+sin(wf*(t+0.5*dt))
   k2=dt*a
   l2=dt*(v+k1/2.0)
   a=-(x+l2/2.0)-(v+k2/2.0)+sin(wf*(t+0.5*dt))
   k3=dt*a
   l3=dt*(v+k2/2.0)
   a=-(x+l3)-(v+k3)+sin(wf*(t+0.5*dt))
   k4=dt*a
   l4=dt*(v+k3)
   v=v+(k1+2.0*k2+2.0*k3+k4)/6.0
   x=x+(l1+2.0*l2+2.0*l3+l4)/6.0
   t=t+dt
end

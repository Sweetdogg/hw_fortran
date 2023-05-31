program main
implicit none
real :: x,v,a,e,t,dt
x=1;v=0;e=0;dt=0.1;a=0;e=0
open(1,file='output.dat')
do while(t<50)
 call euler(x,v,a,e,t,dt)
! call frog(x,v,a,e,t,dt)
 write(1,*)t,x,v,e
 t=t+dt
enddo


!goto 110
do while(t>0)
 call euler(x,v,a,e,t,dt)
! call frog(x,v,a,e,t,dt)
 write(1,*)t,x,v,e
 t=t-dt
enddo
110 continue

close(1)
end

subroutine euler(x,v,a,e,t,dt)
implicit none
real :: x,v,a,e,t,dt
 a=-x
 x=x+v*dt
 v=v+a*dt
 e=0.5d0*x**2+0.5d0*v**2
! t=t+dt
end

subroutine frog(x,v,a,e,t,dt)
implicit none
real :: x,v,a,e,t,dt
 v=v+a*dt
 x=x+v*dt
 a=-x
 e=0.5d0*x**2+0.5d0*v**2
! t=t+dt
end

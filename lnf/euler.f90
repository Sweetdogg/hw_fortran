program euler


real t,x,y,vx,vy,c,g,dt
t       =  0.0; 
x       =  0.0; 
y       =  0.0; 
vx      = 10.0*cos(0.25*pi);  
vy      = 10.0*sin(0.25*pi);   
c       = 0;
g       = 9.8;   
dt	= 0.01


open(1,file='out.dat',status='replace')
do while(t<1)
    
    call euler2d(x,y,vx,vy,t,c,g,dt)
    
    write(1,*) x,y
    print*,vx,vy
    
enddo
close(1)

contains
    subroutine euler2d(x,y,vx,vy,t,c,g,dt)
    real :: x,y,vx,vy,t,c,g,dt
    real ax,ay,v
    
    v=(vx*vx+vy*vy)**0.5
    ax =     - c * v * vx
    ay = - g - c * v * vy
    vx   = vx + ax * dt
    vy   = vy + ay * dt
    x    =  x + vx * dt
    y    =  y + vy * dt
    t    =  t +      dt
    
    end subroutine euler2d
end

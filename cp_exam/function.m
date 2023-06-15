dt=0.01;
t=0;
x=0;
v=1;
xr(1:1000)=0;
vr(1:1000)=0;
for i=1:1000
   xr(i)=x;
   vr(i)=v;
   [x,v]= euler(x,v,dt);
end
plot(xr,vr)
function [x,v]=euler(x,v,dt)
a=-x;
v=v+a*dt;
x=x+v*dt;
end
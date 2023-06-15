syms x;
b(10)=0;
a(10)=0;  
f=abs(sin(x));
a0=int(f,0,pi)/pi;
for k=1:10
    f=abs(sin(x))*sin(k*x/2);
    b(k)=int(f,-pi,pi)/pi;
    f=abs(sin(x))*cos(k*x/2);
    a(k)=int(f,-pi,pi)/pi;
end
% bk=0 ; ak=[0.848,0,-0.509,-0.424,-0.121,0,-0.056,-0.084,-0.033,0] ;
% a0=1/pi 
% syms t u;
% a=[0 0 t -t ; 0 0 -t t ; t -t u 0 ; -t t 0 u];
% [v,x]=eig(a);
% 
% syms t
% f=((-6*x^5)+sqrt(x^2+2)*(6*x^4+4*x^2-16)-10*x^3+15*x)/30;
% l=limit(f,x,0);
% 
% 
% syms x y r;
% r=(x^2+y^2)^0.5;
% f=(sin(x)/x+sin(y)/y)*r/(4-(cos(x)+cos(y))^2)^0.5;
% l=limit(limit(f,x,0),y,0);
% 
% syms x y r;
% r=(x^2+y^2)^0.5;
% f=(4-(cos(x)+cos(y))^2)^0.5;
% 
% 
% f=x^2*(x*(x^2+2)^0.5-x^2-1+1/(2*x^2));
% l=int(f,0,inf);
% 
% 
% 

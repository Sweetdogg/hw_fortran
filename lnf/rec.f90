program mian
implicit none
real :: x,p
integer :: n
print*,p(3,0.5)
end
recursive real function p(n,x) result(ans)
implicit none
real :: x
integer :: n
if(n==0) then
ans=1
else if(n==1) then
ans=x
else
ans=((2*n-1)*x*p(n-1,x)-(n-1)*p(n-2,x))/n
endif
end

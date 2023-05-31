program main 
implicit none
real :: x=0.5,sum
integer :: n=3
call p(sum,n,x)
print*,sum
end
recursive subroutine p(sum,i,x)
implicit none
real :: x,sum0,sum1,sum
integer :: i

if(i==0) then
sum=1
else if(i==1) then
sum=x
else
call p(sum,i-1,x)
sum0=sum
call p(sum,i-2,x)
sum1=sum
sum=((1-i)*sum1)/i+((2*i-1)*x*sum0)/i
endif


end

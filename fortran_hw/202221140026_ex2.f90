program main 
implicit none
integer,parameter :: n=5
integer :: a(n,n),i,asum(2)
a=reshape((/(i,i=1,n**2)/),(/n,n/))
call summatix(a,n)
asum=suma(a)

contains 
function suma(b)
integer :: suma(2)
integer :: b(:,:)
logical :: mask(n,n)
mask=.false.
b=transpose(b)
mask(1,:)=.true.
mask(:,1)=.true.
mask(n,:)=.true.
mask(:,n)=.true.
suma(1)=sum(b,mask)
print*,suma(1)
mask=.false.
mask(2:(n-1),2:(n-1))=.true.
suma(2)=sum(b,mask)
print*,suma(2)
end function

end

subroutine summatix(a,n)
integer :: a(n,n)
logical :: mask(n,n)
mask=.false.
a=transpose(a)
mask(1,:)=.true.
mask(:,1)=.true.
mask(n,:)=.true.
mask(:,n)=.true.
print*,sum(a,mask)
mask=.false.
mask(2:(n-1),2:(n-1))=.true.
print*,sum(a,mask)
end

program main 
implicit none
integer :: n,a(5,5)=reshape((/(n,n=1,25)/),(/5,5/))
logical :: mask(5,5)
mask=.false.

write(*,"(5i6)")a
mask(1,:)=.true.
mask(:,1)=.true.
mask(5,:)=.true.
mask(:,5)=.true.
print*,sum(a,mask)
mask=.false.
mask(2:4,2:4)=.true.
print*,sum(a,mask)
end
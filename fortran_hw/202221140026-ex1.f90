program main
implicit none
integer :: a(2,2),b(2,2),c(2,2),i,j,k
logical :: log
data a /2,4,6,8/
data b /3,6,9,18/
c=0
do i=1,2
 do j=1,2
  do k=1,2
   c(i,j)=c(i,j)+a(i,k)*b(k,j)
  end do
 end do
end do 
log=all(c==matmul(a,b))
print*,c,log
end
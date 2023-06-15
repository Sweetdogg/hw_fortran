program main
 implicit none
 integer :: i,j,k,sum1,sum2=0
 print*,'please input integer : '
 read*,j

 do i=1,j
   sum1=1
   do k=mod(i,2),i,2
     if(k==0) cycle
     sum1=sum1*k
   end do
   sum2=sum2+sum1
 end do

print*,sum2

end
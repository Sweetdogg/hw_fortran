program main
 integer,dimension(3) :: a,b
 integer :: c=0
 integer :: d(2,2),e(2,2)
 e = 0
 data a  /1,2,3/ 
 data b  /3,2,1/
 data d /2,3,4,5/
 do i = 1,3
   c = c+a(i)*b(i) 
 end do
 
 
 do i=1,2
  do j=1,2
   e(i,j)=d(j,i)
  end do
 end do 
 
 print*, c
 write(*,"(2i2)") ((e(i,j),i=1,2),j=1,2)
end

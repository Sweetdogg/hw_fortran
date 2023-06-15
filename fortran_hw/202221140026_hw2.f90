program main
implicit none
integer :: i
real :: a(3),b(3)
real :: res,a_product1,res1=0

print*,'please input a'
read*,(a(i),i=1,3)
print*,'input b'
read*,(b(i),i=1,3)
res=a_product1(a,b)

call a_product2(a,b,res1)
print*,res,res1

end program main

 	function a_product1(a,b)
 	implicit none
 	integer :: i
 	real :: a(3),b(3)
 	real :: a_product1
 	a_product1=0
 	do i=1,3
 		a_product1=a_product1+a(i)*b(i)
 	end do
 	end function a_product1
 	
 	subroutine a_product2(a,b,res1)
 	implicit none
 	integer :: i
 	real :: a(3),b(3)
 	real :: res1
 	
 	do i=1,3
 		res1=res1+a(i)*b(i)
 	end do
 	
 	end subroutine a_product2
 

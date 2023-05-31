program main
implicit none
integer :: i,n,j
real,allocatable :: a(:),b(:)
real :: res,res1=0
print*,'n='
read*,n
allocate(a(n),b(n))
print*,'please input a'
read*,(a(i),i=1,n)
print*,'input b'
read*,(b(i),i=1,n)
res=a_product1()

call a_product2()
print*,res,res1

contains
 	function a_product1()
 	real :: a_product1
 	a_product1=0
 	do j=1,n
 		a_product1=a_product1+a(j)*b(j)
 	end do
 	end function a_product1
 	
 	subroutine a_product2()
 	implicit none
 	
 	do i=1,3
 		res1=res1+a(i)*b(i)
 	end do	
 	end subroutine a_product2
 end

program main
implicit none
integer :: i,n
real,allocatable :: a(:),b(:)
real :: res,a_product1,res1=0
print*,'n='
read*,n
allocate(a(n),b(n))
print*,'please input a'
read*,(a(i),i=1,n)
print*,'input b'
read*,(b(i),i=1,n)
res=a_product1(a,b,n)

call a_product2(a,b,res1)
print*,res,res1

contains
 	function a_product1(a,b,n)
 	implicit none
 	integer :: i
 	integer :: n
 	real :: a(n),b(n)
 	real :: a_product1
 	a_product1=0
 	do i=1,n
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
 end

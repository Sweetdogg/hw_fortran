module array
implicit none
integer,parameter :: n=5
integer :: i
integer :: a(n,n)=reshape((/(i,i=1,n**2)/),(/n,n/)),res
end

program main
use array
implicit none

call margin(a,res)
call center(a,res)
contains 
	subroutine center(b,c)
	implicit none
	integer :: b(:,:),j,k,c
	c=0
	do j=2,n-1
		do k=2,n-1
		c=c+b(j,k)
		enddo
	enddo
	! sum(b(2:n-1,2:n-1))
	print*,c
	end
	
	subroutine margin(b,c)
	implicit none
	integer :: b(:,:),j,k,c
	c=0
	do j=2,n-1
		do k=2,n-1
		c=c+b(j,k)
		enddo
	enddo
	c=sum(b)-c
	print*,c
	end
end

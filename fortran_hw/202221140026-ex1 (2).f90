module array
implicit none
integer,parameter :: n=5

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

program main
use array
implicit none
integer :: a(n,n),i,res
a=reshape((/(i,i=1,n**2)/),(/n,n/))
call margin(a,res )
call center(a,res)

end

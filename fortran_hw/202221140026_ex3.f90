program main 
implicit none

integer,parameter :: n=5
integer :: a(n,n),i,p=0
a=reshape((/(i,i=1,n**2)/),(/n,n/))

call choose(a,p,center)
	
contains
	subroutine center(b,c)
	implicit none
	integer :: b(:,:),j,k,c
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
	do j=2,n-1
		do k=2,n-1
		c=c+b(j,k)
		enddo
	enddo
	c=sum(b)-c
	print*,c
	end
	
	subroutine choose(b,c,f)
	implicit none
	integer :: b(:,:),c
	interface
          subroutine f(b, c)
          integer :: b(:,:), c
          end subroutine f
        end interface
        call f(b, c)
	end	
	
end



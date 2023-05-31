program main
implicit none
integer,parameter :: n=10
integer :: i,j,k
real(8) :: mat(n,n),vec(10,10),eig(10)
k=1
do i=1,n
		mat(i:,i:)=k
		k=k+1
enddo

call diagonalize(10,mat,vec,eig)
do j=1,10
	print*,eig(j)
enddo

end program main

subroutine diagonalize(n,mat,vec,eig)
implicit none
integer :: n,info
real(8) :: mat(n,n),vec(n,n),eig(n),work(n*(3+n/2))
vec=mat
call dsyev('V','U',n,vec,n,eig,work,n*(3+n/2),info)
end subroutine diagonalize

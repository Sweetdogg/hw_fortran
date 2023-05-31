program main
implicit none
interface 
 subroutine trans(a,n)
 implicit none
 integer :: a(:,:),n 
 end subroutine
end interface

integer :: a(2,2)
data a /1,2,3,4/
call tans(a,2)
print*,a

contains
subroutine tans(a,n)
implicit none
integer :: a(:,:),e(n,n),i,j,n
 do i=1,n
  do j=1,n
   e(i,j)=a(j,i)
  end do
 end do 
 a=e
end

end



program main
implicit none
integer :: i
do i=1,3
call opp()
enddo
end


subroutine opp()
implicit none 
integer :: i
real :: j
j=0
print*,j
do i=1,3
 j=2+j
enddo
print*,j
end

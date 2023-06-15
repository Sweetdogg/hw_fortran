program main
implicit none
integer :: n,i
real,allocatable :: array(:)
real :: ave,dev
print*,'please input the number of array'
read*,n
allocate(array(1:n))
print*,'input data'
read*,(array(i),i=1,n)
ave=sum(array)/real(n)
dev=(sum((ave-array)**2)/real(n))**0.5
print*,ave,dev,n
end
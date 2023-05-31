program main 
integer :: a(2,2)
read*,((a(j,i),i=1,2),j=1,2)
print*,a(1,2)
end

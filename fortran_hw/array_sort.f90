program main
integer :: a(10)=(/8,1,5,6,3,3,7,9,2,10/) , c

do i=1,10

 do j=i,10
   
   if(a(i)<a(j)) then
       c=a(j)
       a(j)=a(i)
       a(i)=c
   end if   

 end do 
end do

write(*,*) (a(i),i=1,10)

end

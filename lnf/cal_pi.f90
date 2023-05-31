program main
implicit none
real*8 :: x,y,pi
integer :: i,n,t
write(*,"('input n=',$)")
read*,n
call random_seed()
do i=1,n
 call random_number(x)
 call random_number(y)
   if(x**2+y**2<1) then
   	t=t+1
   end if
end do
pi=4.d0*real(t)/real(n)
write(*,"('pi=',f6.5,i6)")pi,real(n)
end

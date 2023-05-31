program main
implicit none
integer :: i
real*8 :: t=0.9,tf=0
do i=1,10
	tf=xn1(t)
	if(abs(tf-t)<0.0001) then
		exit
	else
		t=tf
	end if
	print*,tf
end do



contains
	function xn1(xn)
	implicit none
	real*8 xn1,xn
	xn1=4*0.8*xn**1.05*(1-xn)
	end function
end

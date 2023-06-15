program lea
 implicit none
 integer :: days,month
 logical :: leap
 write(*,"('if this year leap year =',$)")
 read*,leap
 print*,'input mouth = ';read*,month
 select case(month)
 	case(4,6,9,11)
 	 days=30
 	case(1,3,5,7,8,10,12)
 	 days=31
 	case(2)
	 	if (leap) then
	 	  days=29
	 	else
	 	   days=28
	 	end if 
 	case default
 	 days=-10
 end select
 if(days>0) then
  write(*,"('this month has ',i3,' days' )") days
  else
  write(*,"('this month has ',i3,' days' )") days
  end if
end program
   
   
 	

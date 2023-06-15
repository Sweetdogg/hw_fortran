program change
implicit none
real a,b,t
write(*,"('input a,b =',$)")
read*,a,b
if(a<b) then
	t=a
	a=b
	b=t
	write(*,"('a =',f12.6,' b =',f12.6)") a,b
	else
	write(*,"('a =',f12.6,' b =',f12.6)") a,b
end if
end
			

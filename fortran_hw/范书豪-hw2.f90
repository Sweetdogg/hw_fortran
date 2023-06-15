Program hw2
 implicit none
 real(8) a,b,c,x1,x2,delta
 print*,'input a,b,c from in.dat'
 open(1,file='in.dat',status='old')
 read(1,*) a,b,c
 close(1)
 delta=b**2.d0-4.d0*a*c
 if(delta<0.d0) then
 	print*,'no real root'
 else
 	x1=(-b+sqrt(delta))/(2.d0*a)
 	x2=(-b-sqrt(delta))/(2.d0*a)
 	open(2,file='out.dat',status='replace')
	write(2,200) x1,x2
 	close(2)
200 format('x1=',f6.3,'x2=',f6.3)
 endif
End 

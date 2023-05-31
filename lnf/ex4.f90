program statistic
real a(6)
write(*,"('input:',$)")
read(*,*) a(1),a(2),a(3),a(4),a(5),a(6)
s1=0;s2=1
do i=1,6
	s1=s1+a(i)
	s2=s2*a(i)
enddo
open(1,file='out.dat',status='replace')
write(1,"('sum=',f8.3,2x,'c=',f8.3)") s1,s2
close(1)
end

program ex
	implicit none
	integer :: m,n
	open(1,file='in.dat',status='old')
	read(1,*) m,n
	close(1)
	open(2,file='out.dat',status='replace')
	write(2,200) m+n,m-n
	close(2)
	200 format('m+n+',i6,',m-n=',i6)
end
	

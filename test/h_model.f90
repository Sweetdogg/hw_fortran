module system
 implicit none

 integer :: l                       
 integer :: n,step1,step2,bins,step3
 integer, allocatable :: nbor(:,:)                         
 real(8), allocatable :: spin(:,:)
 real(8) :: temp,arm, asm, ave, ase
 real(8),parameter :: pi=acos(-1.d0)
 
end module system

program main
use system
implicit none
 integer :: i,j,k
  call initran(1)
  open(1,file='read.in',status='old')
  read(1,*)l,temp,step1,step2,bins

  call initial()

   do i=1,step1
     call wolff()
 enddo
 do i=1,step2
        call cleardata         
        do j=1,bins           
	  call wolff()
          call sample   
	enddo                                                             
        call writers
 enddo
 deallocate (spin)
 deallocate (nbor)

endprogram

subroutine initial()
	use system
	implicit none
	integer i,x,y
	real(8), allocatable :: r(:)
	  n=l**2
          allocate (nbor(0:n-1,4))
          allocate (r(0:n-1))
          allocate (spin(0:n-1,3))
          call random_number(spin)
          spin=2.d0*spin-1
	  r=sqrt(spin(:,1)**2+spin(:,2)**2+spin(:,3)**2)
	  do i=0,n-1
	  	spin(i,:)=spin(i,:)/r(i)	  	
	  	x=mod(i,l); y=i/l          
                nbor(i,1)=mod(x+1,l)+y*l     
                nbor(i,2)=x+mod(y+1,l)*l    
                nbor(i,3)=mod(x-1+l,l)+y*l  
                nbor(i,4)=x+mod(y-1+l,l)*l  
	  enddo          
	  deallocate (r)
end subroutine

subroutine writers
        use system
        implicit none
        open(2,file='out.dat',position='append')    
        arm=arm/(dble(bins)*dble(n))
        asm=asm/(dble(bins)*dble(n)**2)
        ave=ave/(dble(bins)*dble(n))
        ase=ase/(dble(bins)*dble(n)**2)
        write(2,1)temp,arm,asm,ave,ase,l !!!!
        1 format(f5.3,4f18.12,i3)
        close(2)
end subroutine writers

subroutine sample
use system
implicit none
 	real(8) :: e,mx,my,mz,m
        integer :: i,x,y
        e=0
        mx=0
        my=0
        mz=0
        do i=0,n-1
        x=mod(i,l); y=i/l 
	mx=mx+spin(i,1)
	my=my+spin(i,2)
	mz=mz+spin(i,3)
        enddo
        m=sqrt(mx**2+my**2+mz**2)
        arm=arm+m  !!
        asm=asm+m**2 
        ave=ave+e 
        ase=ase+e**2
end subroutine


subroutine wolff()

 use system
 implicit none
 
 integer :: i,j,k,cluster=0,sc
 real(8) :: rn,vect(3),l_vect,k_n,k_c
 integer,allocatable :: clusters(:)
 

 call random_number(vect)
 
 i=int(rn()*l*l)
 vect=2.d0*vect-1
 l_vect=sqrt(vect(1)**2+vect(2)**2+vect(3)**2)
 
 k_c=0
 do k=1,3	 	 
	 vect(k)=vect(k)/l_vect
	 k_c=spin(i,k)*vect(k)+k_c
 enddo
  
 do k=1,3
	 spin(i,k)=spin(i,k)-2.d0*k_c*vect(k)
 enddo
 
 allocate(clusters(4*l*l))
 clusters=0
 sc=i
 
do 
do j=1,4
k_n=spin(nbor(sc,j),1)*vect(1)+spin(nbor(sc,j),2)*vect(2)+spin(nbor(sc,j),3)*vect(3)
k_c=spin(sc,1)*vect(1)+spin(sc,2)*vect(2)+spin(sc,3)*vect(3)
 if (rn().lt.(1-exp(2.d0*k_c*k_n/temp))) then
	 do k=1,3
	 spin(nbor(sc,j),k)=spin(nbor(sc,j),k)-2.d0*k_n*vect(k)
	 enddo 
 	 cluster=cluster+1
 	 clusters(cluster)=nbor(sc,j)
 endif
enddo
 sc=clusters(cluster)
  if(cluster==0) exit
 cluster=cluster-1
enddo

deallocate (clusters)
endsubroutine


subroutine cleardata
      use system
      arm=0              ! average relative magnetization
      asm=0              ! average square magnetization
      ave=0              ! average relative energy
      ase=0              ! average square energy
endsubroutine cleardata

real(8) function rn()
!-----------------------------------------------------!
! 64-bit linear congruental random number generator   !
! iran64=oran64*2862933555777941757+1013904243        !
!-----------------------------------------------------!
 implicit none

 real(8)    :: dmu64
 integer(8) :: ran64,mul64,add64
 common/bran64/dmu64,ran64,mul64,add64

 ran64=ran64*mul64+add64
 rn=0.5d0+dmu64*dble(ran64)

 end function rn

subroutine initran(w)

implicit none

 integer(8) :: irmax
 integer(4) :: w,nb,b

 real(8)    :: dmu64
 integer(8) :: ran64,mul64,add64
 common/bran64/dmu64,ran64,mul64,add64
      
 irmax=2_8**31
 irmax=2*(irmax**2-1)+1
 mul64=2862933555777941757_8
 add64=1013904243
 dmu64=0.5d0/dble(irmax)

 open(10,file='seed.in',status='old')
 read(10,*)ran64
 close(10)
 if (w.ne.0) then
    open(10,file='seed.in',status='unknown')
    write(10,*)abs((ran64*mul64)/5+5265361)
    close(10)
 endif

 end subroutine initran


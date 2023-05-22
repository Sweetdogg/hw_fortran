program main
real :: vect(3),l_vect


 call random_number(vect)
 print*,vect
 l_vect=sqrt(vect(1)**2+vect(2)**2+vect(3)**2)
 vect(1)=vect(1)/l_vect;vect(2)=vect(2)/l_vect;vect(3)=vect(3)/l_vect
print*,vect(1)**2+vect(2)**2+vect(3)**2
end

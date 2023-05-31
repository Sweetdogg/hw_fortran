program landau_ginzburg
  implicit none
  integer :: i, N
  real :: dx, dt, alpha, beta, gamma
  real :: phi(N), dphi(N), ddphi(N), f(N)
  ! Define the parameters of the model
  N = 100 ! Number of grid points
  dx = 0.01 ! Grid spacing
  dt = 0.001 ! Time step
  alpha = -1.0 ! Coefficient of linear term in free energy
  beta = -1.0 ! Coefficient of quadratic term in free energy
  gamma = -1.0 ! Coefficient of quartic term in free energy

  ! Initialize the field phi with random values between -1 and +1
  call random_seed()
  call random_number(phi)
  phi = (2*phi-1)

  ! Loop over time steps until convergence is reached (optional)
  
   do 
    ! Calculate the first and second derivatives of phi using finite difference method with periodic boundary conditions
    
     do i = 1,N 
      dphi(i) = (phi(mod(i,N)+1) - phi(mod(i-2,N)+1))/(2*dx) 
      ddphi(i) = (phi(mod(i,N)+1) + phi(mod(i-2,N)+1) -2*phi(i))/(dx**2) 
     end do

    ! Calculate the free energy density f as a function of phi and its derivatives
    
     do i = 1,N 
      f(i) = alpha*phi(i)**2 + beta*phi(i)*dphi(i) + gamma*phi(i)**4 + ddphi(i)**2/4 
     end do

    ! Update phi by using gradient descent method
    
     do i = 1,N 
      phi(i) = phi(i) - dt*(alpha*2*phi(i) + beta*(dphi(mod(i+1,N)) - dphi(mod(i-1,N)))/(2*dx) + gamma*4*phi(i)**3 + ddphi(mod(i+1,N)) + ddphi(mod(i-1,N)) -2*ddphi)/dx**2/4 ) 
     end do

    ! Print some intermediate results for debugging purposes (optional)
    
     print *, 'sum(f)=', sum(f), 'max(phi)=', maxval(phi), 'min(phi)=', minval(phi)

    ! Check for convergence criterion (optional)
    
     if (abs(sum(f)) < epsilon(0.0) ) exit 

   end do

   ! Print the final result and plot it using gnuplot (optional)
   
    print *, 'final sum(f)=', sum(f), 'final max(phi)=', maxval(phi), 'final min(phi)=', minval(phi)

   open(unit=10,file='output.dat')
   write(10,'(F8.3,F8.3,F8.3,F8.3,F8.3,F8.3,F8.3,F8.3,F8.3,F8.3)')
   close(unit=10)

   open(unit=11,file='plot.gp')
   write(11,'(A)')
   close(unit=11)

   call system('gnuplot plot.gp')

end program landau_ginzburg 

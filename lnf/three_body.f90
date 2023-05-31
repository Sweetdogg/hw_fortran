PROGRAM THREE_BODY

  IMPLICIT NONE

  INTEGER :: i, j, k, n
  REAL :: dt, tmax, theta, r12, r23, r31
  REAL, DIMENSION(3,3) :: pos, vel, accel
  REAL, PARAMETER :: G = 6.6743E-11

  vel=0.0;vel(1,2)=0.1;vel(2,2)=-0.1;vel(3,2)=0.6
  pos=0.0;pos(1,1)=-0.5;pos(2,1)=0.5;pos(3,2)=1.0;pos(3,2)=3

  n = 10000
  dt = 0.001
  tmax = n * dt
  open(unit=1, file='three_dody.dat', status='unknown')
  ! Run the simulation
  DO i = 1, n
    DO j = 1, 3
      ! Compute the acceleration for the j-th body
      accel(j,:) = 0.0
      DO k = 1, 3
        IF (j .NE. k) THEN
          r12 = SQRT((pos(j,1) - pos(k,1))**2 + (pos(j,2) - pos(k,2))**2 + (pos(j,3) - pos(k,3))**2)
          theta = ATAN2(pos(j,2) - pos(k,2), pos(j,1) - pos(k,1))
          r23 = SQRT(pos(k,1)**2 + pos(k,2)**2 + pos(k,3)**2)
          r31 = SQRT(pos(j,1)**2 + pos(j,2)**2 + pos(j,3)**2)
          accel(j,1) = accel(j,1) - G * (pos(k,1) - pos(j,1)) / r12**3 + G * (pos(j,1) - pos(k,1)) / r23**3
          accel(j,2) = accel(j,2) - G * (pos(k,2) - pos(j,2)) / r12**3 + G * (pos(j,2) - pos(k,2)) / r23**3
          accel(j,3) = accel(j,3) - G * (pos(k,3) - pos(j,3)) / r12**3 + G * (pos(j,3) - pos(k,3)) / r23**3
        END IF
      END DO

      ! Update the velocity and position for the j-th body
      vel(j,:) = vel(j,:) + dt * accel(j,:)
      pos(j,:) = pos(j,:) + dt * vel(j,:)
    END DO

    ! Print the positions of the three bodies at this timestep
     write(1,*) pos

    ! Stop the simulation if the bodies collide
    IF (SQRT((pos(1,1) - pos(2,1))**2 + (pos(1,2) - pos(2,2))**2 + (pos(1,3) - pos(2,3))**2) < 0.1) THEN
      PRINT *, 'The three bodies have collided. Simulation stopped at time:', i * dt
EXIT
END IF
END DO
close(unit=1)

END PROGRAM THREE_BODY

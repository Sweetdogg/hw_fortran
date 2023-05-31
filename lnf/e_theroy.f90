program po

    implicit none

    ! Define constants
    real*8, parameter :: G = 6.67430d-11   ! Gravitational constant
    real*8, parameter :: M = 1.98847d30    ! Mass of the Sun
    real*8, parameter :: c = 299792458    ! Speed of light
    real*8, parameter :: a = 0.3871d12     ! Semimajor axis of Mercury's orbit
    real*8, parameter :: e = 0.2056d0      ! Eccentricity of Mercury's orbit
    real*8, parameter :: pi = 3.14159265359

    ! Define variables
    real*8 :: t, r, phi,dt
    real*8 :: r_prev, r_dot_prev, phi_prev, phi_dot_prev
    real*8 :: r_dot_dot, phi_dot_dot
    real*8 :: precession

    ! Set initial conditions
    dt=100d0
    t = 0
    r = a * (1 - e)
    phi = 0
    r_prev = r
    r_dot_prev = 0
    phi_prev = phi
    phi_dot_prev = (c/a) * sqrt((1 + e)/(1 - e))
    open(unit=1, file='mercury_orbit.dat', status='unknown')
    ! Integrate the geodesic equation
    do while (t < 2*pi*a/(c*sqrt(1 - e**2)))

        ! Calculate the acceleration
        r_dot_dot = -G*M/(r**2 * sqrt(1 - (r_dot_prev/c)**2 - (r*phi_dot_prev/c)**2))
        phi_dot_dot = -2 * r_dot_prev * phi_dot_prev / r

        ! Update the velocities and positions
        r = r_prev + r_dot_prev*dt + 0.5*r_dot_dot*dt**2
        phi = phi_prev + phi_dot_prev*dt + 0.5*phi_dot_dot*dt**2
        r_dot_prev = r_dot_prev + r_dot_dot*dt
        phi_dot_prev = phi_dot_prev + phi_dot_dot*dt

        ! Store the previous values
        r_prev = r
        phi_prev = phi

        ! Update the time
        t = t + dt

        write(1,*) r*cos(phi), r*sin(phi)
    end do

    close(unit=1)

    
    read*

end program po

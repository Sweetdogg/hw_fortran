program euler

    
    implicit none
    real , parameter :: pi=3.1415926
    ! Declare variables
    real :: t, x, y, vx, vy, c, g, dt
    integer :: i, max_steps
    character(len=256) :: filename
    logical :: file_exists
    logical, parameter :: overwrite = .true.
    real, dimension(:,:), allocatable :: trajectory

    ! Set initial values
    t = 0.0
    x = 0.0
    y = 0.0
    vx = 10.0 * cos(0.25 * pi)
    vy = 10.0 * sin(0.25 * pi)
    c = 0.0
    g = 9.8
    dt = 0.01
    max_steps = 1000

    ! Get output filename and check if file already exists
    filename = 'out.dat'
    inquire(file=filename, exist=file_exists)
    if (file_exists .and. overwrite) then
        call system('rm '//trim(filename))
    end if
    
    ! Allocate memory for trajectory array
    allocate(trajectory(2, max_steps))

    ! Loop over time steps
    do i = 1, max_steps
        
        ! Update position and velocity using Euler method
        call euler2d(x, y, vx, vy, t, c, g, dt)
        
        ! Check if particle has hit the ground
        if (y <= 0.0) then
            print *, 'Particle has hit the ground.'
            exit
        end if
        
        ! Store current position in trajectory array
        trajectory(1,i) = x
        trajectory(2,i) = y
        
        ! Increment time
        t = t + dt
        
    end do
    
    ! Write trajectory to output file
    open(unit=1, file=filename, status='replace')
    do i = 1, max_steps
        write(1,*) trajectory(1,i), trajectory(2,i)
    end do
    close(1)
    
contains

    subroutine euler2d(x, y, vx, vy, t, c, g, dt)
    
        ! Declare input/output variables
        real, intent(inout) :: x, y, vx, vy, t, c, g, dt
        real :: ax, ay, v
        
        ! Calculate acceleration and update velocity and position
        v = sqrt(vx**2 + vy**2)
        ax = -c * v * vx
        ay = -g - c * v * vy
        vx = vx + ax * dt
        vy = vy + ay * dt
        x = x + vx * dt
        y = y + vy * dt
        
    end subroutine euler2d

end program euler


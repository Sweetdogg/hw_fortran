module harmonic_oscillator
    implicit none
    
    private
    
    real*8 :: position=0.0, velocity=1.0, time=0.0, damping=0.1, frequency=400.0, time_step=0.01
    
    public :: solve_oscillator
    
contains
    
    subroutine solve_oscillator()
        real*8 :: x, v
        integer :: ios
        
        open(1, file='osc.dat', status='replace', iostat=ios)
        if (ios /= 0) stop 'Error: could not open file "osc.dat"'
        
        do while(time < 1.0)
            call euler_osc(position, velocity, time, time_step, damping, frequency)
            x = position
            v = velocity
            write(1, *) x, t
        end do
        
        close(1, iostat=ios)
        if (ios /= 0) stop 'Error: could not close file'
    end subroutine
    
    subroutine euler_osc(position, velocity, time, time_step, damping, frequency)
        real*8, intent(inout) :: position, velocity, time, time_step, damping, frequency
        
        position = position + time_step * velocity
        velocity = velocity - 2.0 * damping * velocity * time_step - frequency**2 * position * time_step
        time = time + time_step
    end subroutine
end module

program main
    use harmonic_oscillator
    implicit none
    
    call solve_oscillator()
end program


program projectile_motion
  implicit none
  real :: x0, y0, v0, theta, g, t, dt, x, y, vx, vy
  real, parameter:: pi=3.1415926
  ! 初始位置和速度
  x0 = 0.0
  y0 = 0.0
  v0 = 10.0
  theta = 45.0
  ! 重力加速度
  g = 9.8
  ! 时间步长和初始时间
  dt = 0.01
  t = 0.0
  ! 初始速度分量
  vx = v0 * cos(theta * pi / 180.0)
  vy = v0 * sin(theta * pi / 180.0)
  ! 计算抛体运动轨迹
  do while (y >= 0.0)
    x = x0 + vx * t
    y = y0 + vy * t - 0.5 * g * t**2
    ! 更新速度分量
    vx = vx
    vy = vy - g * dt
    ! 更新位置和时间
    x0 = x
    y0 = y
    t = t + dt
    ! 输出结果
    print *, x, y
  end do
end program projectile_motion


Program TB
   implicit none
   real(8) :: X1_0,X2_0,X3_0,Y1_0,Y2_0,Y3_0
   real(8) :: V1X_0,V1Y_0,V2X_0,V2Y_0,V3X_0,V3Y_0
   real(8),parameter :: G = 6.6726E-11,M1 = 1.9891E30,m2=1.9891E29,m3=1.9891E29
   real(8),parameter :: yr = 3.15E7,Au = 1.496E11
   real(8) :: t,h !时间步长
   real(8) :: time !总时间
   real(8) :: a12,a13,a1_X,a1_Y
   real(8) :: a21,a23,a2_X,a2_Y
   real(8) :: a31,a32,a3_X,a3_Y
   real(8) :: distance12,distance23,distance13
   
   t = 0.0d0
   h = 3600.0d0   !1h为步长
   time = 3600.0d0 * 24.0d0 * 365.0d0  !一年的时间
    
   open(101,file = "TB.dat")
   !设置初始距离和初始速度
   X1_0 = 0.0d0; Y1_0 = 0.0d0; V1x_0 = 0.0d0; V1y_0 = 0.0d0  !太阳初始位置
   X2_0 = Au; Y2_0 = 0.0d0; V2x_0 = 3.0E4; V2y_0 = 0.0d0 !1号行星位置
   X3_0 = 2*Au; Y3_0 = 0.0; V3X_0 = 3.0E4 ; V3Y_0 = 0.0 !2号行星位置
   
   do 
        distance12 = sqrt((x1_0 - X2_0)**2 + (Y1_0 - Y2_0)**2)
        distance23 = sqrt((x2_0 - X3_0)**2 + (Y2_0 - Y3_0)**2)
        distance13 = sqrt((x1_0 - X3_0)**2 + (Y1_0 - Y3_0)**2)

        !对物体1的计算
        a12 = G*m2/(distance12**2)
        a13 = G*m3/(distance13**2)
        a1_X = a12*(X2_0 - X1_0)/distance12 + a13*(X3_0 - X1_0)/distance13
        a1_Y = a12*(Y2_0 - Y1_0)/distance12 + a13*(Y3_0 - Y1_0)/distance13
        V1X_0 = V1X_0 + a1_X * h 
        V1Y_0 = V1Y_0 + a1_Y * h
        X1_0 = X1_0 + V1X_0 * h
        Y1_0 = Y1_0 + V1Y_0 * h

        !对物体2的计算
        a21 = G*m1/(distance12**2)
        a23 = G*m3/(distance23**2)
        a2_X = a21*(X1_0 - X2_0)/distance12 + a23*(X3_0 - X2_0)/distance23
        a2_Y = a12*(Y1_0 - Y2_0)/distance12 + a23*(Y3_0 - Y2_0)/distance23
        V2X_0 = V2X_0 + a2_X * h 
        V2Y_0 = V2Y_0 + a2_Y * h
        X2_0 = X2_0 + V2X_0 * h
        Y2_0 = Y2_0 + V2Y_0 * h

        !对物体3的计算
        a31 = G*m1/(distance13**2)
        a32 = G*m2/(distance23**2)
        a3_X = a31*(X1_0 - X3_0)/distance13 + a32*(X2_0 - X3_0)/distance23
        a3_Y = a32*(Y1_0 - Y3_0)/distance13 + a32*(Y2_0 - Y3_0)/distance23
        V3X_0 = V3X_0 + a3_X * h 
        V3Y_0 = V3Y_0 + a3_Y * h
        X3_0 = X3_0 + V3X_0 * h
        Y3_0 = Y3_0 + V3Y_0 * h
        
        write(101,"(6E24.7)") X1_0,Y1_0,X2_0,Y2_0,X3_0,Y3_0

        t = t + h        
        if(t > time) exit
    end do
    close(101)
end

  

module system
    implicit none
    integer, parameter:: maxl=1024,maxsize=maxl*maxl*2   !定义数组大小
    real(8):: isp(maxsize),isf(maxsize),msign!bp表示磁场强度
    integer :: nbor(5,maxsize),n1,nsq
    double precision,parameter :: j1=-1.0d0
    double precision,parameter :: j2=-1.0d0
    double precision,parameter :: kn=1.0d0
    ! isp，(一个整数数组)，用于表示自旋的状态
    ! nbor，一个整数二维数组，用于存储相邻的自旋的位置
    ! n1，一个整数，用于表示长宽
    ! nsq，一个整数，用于表示二维格子的大小（总数）
    ! msign，（一个整数），用于存储自旋的符号
endmodule system

module global
    implicit none
    double precision,parameter :: pi = acos(-1.0d0) !定义pi为pi
end module

module data
    real(8):: armt,arm1,arm2,asmt,asm1,asm2,afmt,afm1,afm2,ave,ase,sgm,ssg,fsg
    ! arm，用于表示平均磁矩
    ! asm，用于表示平均自旋
    ! ave，用于表示平均能量
    ! ase，用于表示平均熵
endmodule data

program ising
    use system; use data
    implicit none
    integer:: iseed,msample,nbin,neq,nm,i,ib,ip,ntemp,step_count,step_msample
    real(8):: temp
    save
    OPEN(1,file='in8.dat',status='old')
    read (1,*) n1,neq,iseed,msample,nbin,nm
    close(1)
    if ((n1.eq.0).or.(n1.gt.maxl).or.(iseed.eq.0)) stop
    print *, 'size=',n1
! initialize, equilibrate 初始化，平衡
    do ntemp = 0, 37
        temp = 0.3d0 + ntemp*0.1d0
        call initial(temp,iseed)
        do i=1,neq
            call simul(temp,nm)
        end do
        do ib=1,nbin
          call cleardata
!       simulate and sample 模拟和取样
          step_count = 0 ! 初始化计数器
          step_msample=0
            do ip=1,msample
            call simul(temp,nm)
            call sample
            step_count = step_count + 1
            if (step_count == 500) then ! 每隔500个时间步取样
                step_msample=step_msample+1
                call writesgm(step_msample,ib,temp,msample)
                step_count=0
            end if
            end do
            call writeres(msample,temp)
        end do
        call debug(isp,n1,temp)
    end do
end

    subroutine writeres(msample,temp)
    use data; use system
    implicit none
    integer:: msample
    real(8):: cv,qmt,qm1,qm2,qs,ksit,ksi1,ksi2,umt,um1,um2,us
    double precision,intent(in)::temp
! normalize, analyze, and print output
    armt=armt/msample
    arm1=arm1/msample
    arm2=arm2/msample

    asmt=asmt/msample
    asm1=asm1/msample
    asm2=asm2/msample

    ave=ave/msample
    ase=ase/msample
    sgm=sgm/msample
    ssg=ssg/msample
    fsg=fsg/msample

    afmt=afmt/msample
    afm1=afm1/msample
    afm2=afm2/msample

    sgm=abs(sgm)
    cv=(ase-(ave*ave))/(temp*temp)*dble(nsq)
    qmt=afmt/(asmt*asmt)
    qm1=afm1/(asm1*asm1)
    qm2=afm2/(asm2*asm2)
    qs=fsg/(ssg*ssg)
    umt=2.0d0*(1.0d0-0.5d0*qmt)
    um1=2.0d0*(1.0d0-0.5d0*qm1)
    um2=2.0d0*(1.0d0-0.5d0*qm2)
    us=1.5d0*(1.0d0-(1.0d0/3.0d0)*qs)
    ksit=(asmt-(armt*armt))/temp*dble(nsq)
    ksi1=(asm1-(arm1*arm1))/temp*dble(nsq)
    ksi2=(asm2-(arm2*arm2))/temp*dble(nsq)
    write(*,9) temp, armt,arm1,arm2, asmt,asm1,asm2, ave, cv, qmt,qm1,qm2, qs, &
    ksit,ksi1,ksi2,sgm, umt, um1, um2,us
    9 format(F10.4,F10.4,F10.4,F10.4,F10.4,F10.4,F10.4,F10.4,F10.4,F10.4,&
    F10.4,F10.4,F10.4,F10.4,F10.4,F10.4,F10.4,F10.4,F10.4,F10.4,F10.4)
    open(7, file='bin8.dat',access='append')
    write(7, *)temp, armt,arm1,arm2, asmt,asm1,asm2, ave, cv, qmt,qm1,qm2, qs ,&
    ksit,ksi1,ksi2,sgm, umt, um1, um2,us
    close(7)

    endsubroutine writeres

    subroutine writesgm(m,nb,temp,msp)
        use data; use system
        implicit none
        integer:: m,nb,msp
        double precision,intent(in)::temp
        real(8)::sgm2,tp,ms
        sgm2=sgm/(500*m)
        ms=500*m+nb*msp
        tp=temp
        if (tp.eq.0.4d0) then
            open(104, file='binsgm04.dat',access='append')
            write(104, *)temp,ms,sgm2
            close(104)
        end if
        if (tp.eq.0.5d0) then
            open(105, file='binsgm05.dat',access='append')
            write(105, *)temp,ms,sgm2
            close(105)
        end if
        if (tp.eq.0.6d0) then
            open(106, file='binsgm06.dat',access='append')
            write(106, *)temp,ms,sgm2
            close(106)
        end if
        if (tp.eq.0.7d0) then
            open(107, file='binsgm07.dat',access='append')
            write(107, *)temp,ms,sgm2
            close(107)
        end if
        if (tp.eq.0.8d0) then
            open(108, file='binsgm08.dat',access='append')
            write(108, *)temp,ms,sgm2
            close(108)
        end if
        if (tp.eq.0.9d0) then
            open(109, file='binsgm09.dat',access='append')
            write(109, *)temp,ms,sgm2
            close(109)
        end if
        if (tp.eq.1.0d0) then
            open(110, file='binsgm10.dat',access='append')
            write(110, *)temp,ms,sgm2
            close(110)
        end if
        if (tp.eq.1.1d0) then
            open(111, file='binsgm11.dat',access='append')
            write(111, *)temp,ms,sgm2
            close(111)
        end if
        if (tp.eq.2.0d0) then
            open(120, file='binsgm20.dat',access='append')
            write(120, *)temp,ms,sgm2
            close(120)
        end if
    end

    subroutine cleardata
    use data
    arm=0              ! average relative magnetization**
    asm=0              ! average square magnetization
    ave=0              ! average relative energy
    ase=0              ! average square energy
    endsubroutine cleardata

    subroutine initial(temp,iseed)
!     initialize for Ising mc
    use system; use global
    implicit real*8(a-h,o-z)
    real(8):: rn
    nsq=n1*n1*2
    print * , 'temp=',temp
! define neighbors of each spin
    do ispin=1,nsq
    iz=(ispin+n1**2-1)/n1**2
    iy=((ispin-(iz-1)*n1**2)-1)/n1+1
    ix=(ispin-(iz-1)*n1**2)-(iy-1)*n1
    ixp=ix+1-(ix/n1)*n1
    iyp=iy+1-(iy/n1)*n1
    ixm=ix-1+((n1-ix+1)/n1)*n1
    iym=iy-1+((n1-iy+1)/n1)*n1
    nbor(1,ispin)=(iy -1)*n1+ixm+(iz-1)*n1**2 !左
    nbor(4,ispin)=(iy -1)*n1+ixp+(iz-1)*n1**2 !右
    nbor(2,ispin)=(iym-1)*n1+ix+(iz-1)*n1**2  !前
    nbor(3,ispin)=(iyp-1)*n1+ix+(iz-1)*n1**2  !后
    if(ispin>nsq/2) then
    nbor(5,ispin)=((iy-1)*n1+ix)      !下
    else
    nbor(5,ispin)=((iy-1)*n1+ix)+(iz)*n1**2      !上
    end if
  end do
! initialize random generator
    call ransi(iabs(iseed)+1)
! initialize spin config 初始化自旋配置
    do  ns=1,nsq
    isp(ns)=2.0d0 * pi * rn()
    end do
    return
    end


    subroutine simul(temp,nmsteps)
    implicit none
    integer::nmsteps
    double precision,intent(in)::temp
!     execute mc steps
    call mcm(temp,nmsteps)   !metropolis sweeps
    return
    end

    subroutine mcm(temp,nsteps)
! subroutine for metropolis sweeps
    use system ;use global
    implicit none
    integer::nsteps,isteps,ispin,ns
    real(8)::rn,it_1,it_2
  double precision,intent(in)::temp
    do  isteps=1,nsteps
    do  ispin=1,nsq
    ns=int(rn()*nsq)+1          !自旋位置
    if (ns>nsq/2) then
    it_1=(j2*(cos(isp(ns)-isp(nbor(1,ns)))+cos(isp(ns)-isp(nbor(2,ns))) &
        +cos(isp(ns)-isp(nbor(3,ns)))+cos(isp(ns)-isp(nbor(4,ns))))&
        +kn*cos(2.0d0*(isp(ns)-isp(nbor(5,ns)))))
        else
    it_1=(j1*(cos(isp(ns)-isp(nbor(1,ns)))+cos(isp(ns)-isp(nbor(2,ns))) &
        +cos(isp(ns)-isp(nbor(3,ns)))+cos(isp(ns)-isp(nbor(4,ns))))&
        +kn*cos(2.0d0*(isp(ns)-isp(nbor(5,ns)))))
    end if
    isf(ns)=2.0d0*pi*rn()
    if (ns>nsq/2) then
    it_2=(j2*(cos(isf(ns)-isp(nbor(1,ns)))+cos(isf(ns)-isp(nbor(2,ns))) &
        +cos(isf(ns)-isp(nbor(3,ns)))+cos(isf(ns)-isp(nbor(4,ns))))&
        +kn*cos(2.0d0*(isf(ns)-isp(nbor(5,ns)))))
        else
    it_2=(j1*(cos(isf(ns)-isp(nbor(1,ns)))+cos(isf(ns)-isp(nbor(2,ns))) &
        +cos(isf(ns)-isp(nbor(3,ns)))+cos(isf(ns)-isp(nbor(4,ns))))&
        +kn*cos(2.0d0*(isf(ns)-isp(nbor(5,ns)))))
    end if
    if (rn().lt.exp(-(it_2-it_1)/temp)) then
       isp(ns)=isf(ns)
    end if
    end do
    end do
    return
    end

    subroutine debug(s,n,temp)
        use system
        implicit none
        real(8):: s(maxsize),temp
        integer ::n
        integer :: n3
        integer ::z, y, x,a
        n3=2
        write (*, '(a, F6.4)') "temp=", temp
        do z = 1, n3
            write (*, '(a, i0)') "Z=", z
            do y = 1, n
                do x = 1, n
                    write (*, '(3(F4.1, x))', advance='no') s((z-1)*n*n+(y-1)*n+x)
                    a=(z-1)*n*n+(y-1)*n+x
                end do
                write (*, *)
            end do
            write (*, *)
        end do
        write (*, *)

        open(11, file='isp.dat',access='append')
        write (11, '(a, F6.4)') "temp=", temp
        do z = 1, n3
            write (11, '(a, i0)') "Z=", z
            do y = 1, n
                do x = 1, n
                    write (11, '(3(F4.1, x))', advance='no') s((z-1)*n*n+(y-1)*n+x)
                    a=(z-1)*n*n+(y-1)*n+x
                end do
                write (11, *)
            end do
            write (11, *)
        end do
        write (11, *)
        close(11)
    end

    subroutine sample
!     compute observables from given spin configuration
    use system;use data
    implicit none
    integer::ns
    real(8)::enr,remt,rem1,rem2,sqmt,sqm1,sqm2,mt,m1,m2,mx1,my1,mx2,my2,&
    nbs,ds,dsgm,fqmt,fqm1,fqm2,ddsgm
    save
!     sample magnetization
    m1=0.0d0
    m2=0.0d0
    mx1=0.0d0
    my1=0.0d0
    mx2=0.0d0
    my2=0.0d0
    mt=0.0d0
    do ns=1,nsq
        if (ns>nsq/2) then
        mx2=mx2+cos(isp(ns))
        my2=my2+sin(isp(ns))
        else
        mx1=mx1+cos(isp(ns))
        my1=my1+sin(isp(ns))
        end if
    end do
    m1=sqrt(mx1*mx1+my1*my1)
    m2=sqrt(mx2*mx2+my2*my2)
    mt=sqrt((mx1+mx2)*(mx1+mx2)+(my1+my2)*(my1+my2))
    ! sin(theta-eta)
    dsgm=0.0d0
    do ns=1,nsq/2
        dsgm=dsgm+(sin(isp(ns)-isp(nbor(5,ns))))
    end do
!     sample nearest-neighbour sum
    nbs=0.0d0
    do ns=1,nsq
    if(ns>nsq/2)   then
        ds=(j2*(cos(isp(ns)-isp(nbor(1,ns)))+cos(isp(ns)-isp(nbor(2,ns)))))
    else
        ds=(j1*(cos(isp(ns)-isp(nbor(1,ns)))+cos(isp(ns)-isp(nbor(2,ns))))&
        +kn*cos(2.0d0*(isp(ns)-isp(nbor(5,ns)))))
    end if
    nbs=nbs+ds
    end do
    enr=(nbs)/dble(nsq)
    remt=abs(mt/dble(nsq))
    rem1=abs(m1/dble(nsq/2.0d0))
    rem2=abs(m2/dble(nsq/2.0d0))
    ddsgm=dsgm/dble(nsq/2.0d0)
    sgm=sgm+ddsgm !sin(sgm)
    ssg=ssg+ddsgm*ddsgm !sin**2
    fsg=fsg+ddsgm*ddsgm*ddsgm*ddsgm
!     accumulate results
    sqmt=remt*remt            ! calculate square magnetization
    sqm1=rem1*rem1
    sqm2=rem2*rem2

    armt=armt+remt            ! accumulate relative magnetization
    arm1=arm1+rem1
    arm2=arm2+rem2

    fqmt=remt*remt*remt*remt
    fqm1=rem1*rem1*rem1*rem1
    fqm2=rem2*rem2*rem2*rem2

    asmt=asmt+sqmt            ! accumulate square magnetization
    asm1=asm1+sqm1
    asm2=asm2+sqm2

    afmt=afmt+fqmt
    afm1=afm1+fqm1
    afm2=afm2+fqm2
    ave=ave+enr            ! accumulate energy density
    ase=ase+enr*enr        ! accumulate square energy
    return
    end

    subroutine ransi(iseed)
! initialize shift register random generator
    implicit real*8(a-h,o-z)
    save
    parameter (mult=32781,lenr=9689,ifdb=471)
    common/ransrb/ irs(lenr),next(lenr),ipoint,ipoinf
    k=3**18+2*iseed
    do 100 i=1,lenr
    k=k*mult
    irs(i)=k+i/3+i/17
100  continue
    do i=1,lenr
    next(i)=i+1
    next(lenr)=1
    ipoint=1
    ipoinf=ifdb+1
    end do
    return
    end

    function rn()
!     calculate random number
    implicit real*8(a-h,o-z)
    save
    parameter (tm32=2.d0**(-32),lenr=9689,ifdb=471)
    common/ransrb/ irs(lenr),next(lenr),ipoint,ipoinf
    irn=ieor(irs(ipoint),irs(ipoinf))
    irs(ipoint)=irn
    rn=irn*tm32+0.5d0
    ipoint=next(ipoint)
    ipoinf=next(ipoinf)
    return
    end

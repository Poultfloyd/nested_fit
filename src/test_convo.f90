PROGRAM TEST_CONVO
IMPLICIT NONE 
CHARACTER*64 funcname,funcname2
INTEGER*4 n,i,npoints,j
REAL*8 yfit,xfit, tot
REAL*8 DEXP
REAL*8,dimension(:),allocatable :: w, x, total
REAL*8, dimension(:),allocatable :: func,t,interp,lorentz
REAL*8 val(16), val2(3), USERFCN, LORE
INTEGER*4 npar, k, nn_1, nn_2, nest
PARAMETER (nest=1000)
REAL a,b,c
REAL*8 t_1(nest), c_1(nest), t_2(nest), c_2(nest)
COMMON /two_interp/ t_1, t_2, c_1, c_2, k, nn_1, nn_2
n=1100
npar=16
npoints=3000
allocate(w(n))
allocate(x(n))
allocate(func(npoints))
allocate(t(npoints))
allocate(interp(n))
allocate(lorentz(n))
allocate(total(n))
funcname = "TWO_INTERP_VOIGT_POLY"
call INIT_TWO_INTERP(1000,1000)

val(1)=1
val(2)=0
val(3)=0
val(4)=0
val(5)=0
val(6)=0
val(7)=1
val(8)=1
val(9)=0
val(10)=0
val(11)=0
val(12)=0
val(13)=0
val(14)=0
val(15)=1000
val(16)=1000


val2(1)=0
val2(2)=100000
val2(3)=0.0008

do i=1,npoints
t(i)=32.63+0.4*(REAL(i)-1)/REAL(npoints)
enddo
funcname2="LORENORM"
CALL CPU_TIME(a)
CALL GAULEG(t_2(1),t_2(nn_2),x,w,n)
CALL CPU_TIME(b)
do i=1,npoints
do j=1,n

total(j)=w(j)*USERFCN(t(i)-x(j),3,val2,funcname2)*USERFCN(x(j),npar,val,funcname)
! +w(j)*USERFCN(-x(j),3,val2,funcname2)*USERFCN(t(i)-x(j),npar,val,funcname)*DEXP(x(j))
! WRITE(*,*) t(i),w(j),total(j)
enddo


func(i)= sum(total)

enddo

DO i=1,npoints
WRITE(*,*) t(i), func(i),USERFCN(t(i),npar,val,funcname)
enddo

CALL CPU_TIME(c)
! WRITE(*,*) x,w
WRITE(*,*) a,b,c
END PROGRAM TEST_CONVO
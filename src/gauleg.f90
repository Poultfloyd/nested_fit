SUBROUTINE GAULEG(x1,x2,x,w,n)
INTEGER*4 n,i,j,m
REAL*8 x1,x2,x(n),w(n)
REAL (8), PARAMETER :: eps_gleg = 1.d-10
REAL (16), PARAMETER :: pi=3.141592654d0
REAL*8 p1, p2, p3, pp, xl, xm, z, z1
m=(n+1)/2
xm=0.5d0*(x2+x1)
xl=0.5d0*(x2-x1)
DO i=1,m
    z=cos(pi*(i-0.25d0)/(n+0.25d0))
1     CONTINUE 
      P1=1.Q0 
      P2=0.Q0 
      DO  J=1,N 
         P3=P2 
         P2=P1 
         P1=((2*j-1)*z*p2-(J-1)*p3)/j 
      ENDDO
      pp=n*(z*p1-p2)/(z**2-1.d0) 
      z1=z
      z=z1-p1/pp 
      IF(ABS(z-z1).GT.eps_gleg) GO TO 1 
      x(i)=xm-xl*z
      x(n+1-i)=xm+xl*z
      w(i)=2.d0*xl/((1.d0-z**2)*pp**2)
      w(n+1-i)=w(i) 
ENDDO
RETURN
END SUBROUTINE gauleg
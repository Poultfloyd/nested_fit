SUBROUTINE GAULEG(N,X,W) 
! Automatic Time-stamp: <2018-07-23 16:22:26 paulinde>                  
   !   ***************************************************************     
   !   COMPUTES GAUSS-LEGENDRE POINTS AND WEIGHTS                          
   !   FROM "NUMERICAL RECIPES" P. 125, METHOD DUE TO G. RYBICKI           
   !   TAKES 0.034 SEC TO CALCULATE 60 ORDER POINTS AND WEIGHTS ON 205     
   !   TESTED ON 205 FOR N = 1,100                                         
   !   ***************************************************************     
   IMPLICIT NONE
   ! External variables
   REAL (8), INTENT (OUT), DIMENSION (n) :: x, w
   INTEGER (4), INTENT (IN) :: n
   REAL (16) :: z, z1, p1, p2, p3, pp
   REAL (16), PARAMETER :: eps_gleg = 1.Q-16
   INTEGER (4) :: m, i, j
   M=(N+1)/2 
   DO I=1,M 
      Z=COS(3.141592654Q0*(I-0.25Q0)/(N+0.5Q0)) 
1     CONTINUE 
      P1=1.Q0 
      P2=0.Q0 
      DO  J=1,N 
         P3=P2 
         P2=P1 
         P1=((2*J-1)*Z*P2-(J-1)*P3)/J 
      END DO
      PP=N*(Z*P1-P2)/(Z**2-1.Q0) 
      Z1=Z 
      Z=Z1-P1/PP 
      IF(ABS(Z-Z1).GT.eps_gleg) GO TO 1 
      X(I)=0.5Q0*(1.Q0-Z) 
      X(N+1-I)=0.5Q0*(1.Q0+Z) 
      W(I)=1.D0/((1.D0-Z**2)*PP**2) 
      W(N+1-I)=W(I) 
   END DO
   RETURN 
END SUBROUTINE GAULEG

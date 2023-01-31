! Automatic Time-stamp: <2018-07-23 16:29:10 paulinde>
SUBROUTINE GAULAG(N,X,W) 
   !   ***************************************************************     
   !   PETER J. MOHR, 4/20/90                                              
   !   COMPUTES GAUSS-LAGUERRE POINTS AND WEIGHTS                          
   !   ASYMPTOTIC INITIAL GUESS FOR POINTS WITH CORRECTIONS BASED          
   !   ON CALCULATED POINTS                                                
   !   TAKES 0.099 SEC TO CALCULATE 60 ORDER POINTS AND WEIGHTS ON 205     
   !   TESTED ON 205 FOR N=1,95; FAILS TO GET ROOTS FOR N > 96             
   !   ***************************************************************     
   IMPLICIT NONE
   ! External variables
   REAL (8), INTENT (OUT), DIMENSION (n) :: x, w
   INTEGER (4), INTENT (IN) :: n
   ! Internal variables
   REAL (16) :: z, z1, p1, p2, p3, pp,d, xp, xpp
   REAL (16), PARAMETER :: eps = 1.Q-16
   INTEGER (4) :: i, j, its
   !U    USES gammln                                                       
   REAL (16) :: ai
   D=1.D0/(4*N+2) 
   X(1)=5.55165247D0*D 
   DO  I=2,N 
      X(I)=X(I-1)+19.739209D0*(I-0.75D0)*D 
   END DO
   X(N)=1.01D0*X(N) 
   XP=X(2) 
   XPP=X(1) 
   DO I=1,N 
      IF(I.EQ.1) Z=X(1) 
      IF(I.EQ.2) Z=X(1)+(XP-XPP) 
      IF(I.GT.2) THEN 
         Z=X(I-1)+(X(I)-XP)*(X(I-1)-X(I-2))/(XP-XPP) 
         XPP=XP 
         XP=X(I) 
      ENDIF
1     CONTINUE 
      P1=1.Q0 
      P2=0.Q0 
      DO  J=1,N 
         P3=P2 
         P2=P1 
         P1=((2*J-1-Z)*P2-(J-1)*P3)/J 
      END DO
      PP=N*(P1-P2)/Z 
      Z1=Z 
      Z=Z1-P1/PP 
      IF(ABS(Z-Z1).GT.EPS) GO TO 1 
      X(I)=Z 
      W(I)=-1.Q0/(N*PP*P2) 
   END DO
   RETURN 
END SUBROUTINE GAULAG

c     Automatic Time-stamp: <Last changed by martino on Tuesday 02 April 2019 at CEST 17:19:46>
c################################### USERFCN DEFINITION #####################################

      FUNCTION SELECT_USERFCN_SET(funcname)
      IMPLICIT NONE
      CHARACTER*64 funcname
      INTEGER*4 SELECT_USERFCN_SET

 
c     Choose your model (see below for definition)
      IF(funcname.EQ.'GAUSS_BG_SET') THEN
            SELECT_USERFCN_SET = 0
      ELSE IF(funcname.EQ.'DOUBLE_EXPSIMP') THEN
            SELECT_USERFCN_SET = 1
      ELSE IF(funcname.EQ.'DOUBLE_EXPSIN') THEN
            SELECT_USERFCN_SET = 2
      ELSE IF(funcname.EQ.'DOUBLE_EXPSIN_BIS') THEN
            SELECT_USERFCN_SET = 3
      ELSE IF(funcname.EQ.'DOUBLE_TWO_EXPSIN') THEN
            SELECT_USERFCN_SET = 4
      ELSE IF(funcname.EQ.'TRIPLE_EXPSIMP') THEN
            SELECT_USERFCN_SET = 5
      ELSE IF(funcname.EQ.'TRIPLE_EXPSIN') THEN
            SELECT_USERFCN_SET = 6
      ELSE IF(funcname.EQ.'TRIPLE_EXPSIN_BIS') THEN
            SELECT_USERFCN_SET = 7
      ELSE IF(funcname.EQ.'TRIPLE_EXPSIN_TRIS') THEN
            SELECT_USERFCN_SET = 8
      ELSE IF(funcname.EQ.'WEIBULL_EL_LASER') THEN
            SELECT_USERFCN_SET = 9
      ELSE IF(funcname.EQ.'SIX_GAUSS_ERF_FREESIG_POLY_SET') THEN
            SELECT_USERFCN_SET = 10
      ELSE IF(funcname.EQ.'SIX_GAUSS_ERF_FREESIG_POLY_SET2') THEN
            SELECT_USERFCN_SET = 11
      ELSE IF(funcname.EQ.'SIX_GAUSS_ERF_FREESIG_POLY_SET3') THEN
            SELECT_USERFCN_SET = 12
      ELSE IF(funcname.EQ.'FOUR_GAUSS_ERF_TWO_GAUSS_SET') THEN
            SELECT_USERFCN_SET = 13
      ELSE IF(funcname.EQ.'FOUR_GAUSS_ERF_TWO_GAUSS_STAN_SET') THEN
            SELECT_USERFCN_SET = 14
      ELSE IF(funcname.EQ.'FOUR_GAUSS_ERF_TWO_GAUSS_STAN2_SET') THEN
            SELECT_USERFCN_SET = 15
      ELSE IF(funcname.EQ.'FOUR_GAUSS_ERF_TWO_GAUSS_STAN3_SET') THEN
            SELECT_USERFCN_SET = 16
      ELSE IF(funcname.EQ.'ROCKING_CURVE_SET') THEN
            SELECT_USERFCN_SET = 17
      ELSE IF(funcname.EQ.'DCS_EIGHT_VOIGT_POLYBG_X0_SET') THEN
            SELECT_USERFCN_SET = 18
      ELSE IF(funcname.EQ.'DCS_EIGHT_VOIGT_SET_NEIGHBOUR') THEN
            SELECT_USERFCN_SET = 19
      ELSE IF(funcname.EQ.'TWO_INTERP_THREE_VOIGT_POLY') THEN
            SELECT_USERFCN_SET = 20
      ELSE IF(funcname.EQ.'THREE_INTERP_POLY_N_SET') THEN
            SELECT_USERFCN_SET = 21

            
      ELSE
         WRITE(*,*) 'Selected function:', funcname
         WRITE(*,*) 'Error in the function name def. in USERFCN_SET'
         WRITE(*,*) 'Check in the manual and in the input.dat file'
         STOP
      END IF
      RETURN
      END

      FUNCTION USERFCN_SET(x,npar,val,funcid,j)
      IMPLICIT NONE
      INTEGER*4 npar, j,funcid
      REAL*8 val(npar)
      REAL*8 x, USERFCN_SET, WEIBULL_EL_LASER, GAUSS_BG_SET
      REAL*8 DOUBLE_EXPSIN, DOUBLE_EXPSIN_BIS, DOUBLE_EXPSIMP
      REAL*8 DOUBLE_TWO_EXPSIN
      REAL*8 TRIPLE_EXPSIN,TRIPLE_EXPSIN_BIS,TRIPLE_EXPSIN_TRIS
      REAL*8 TRIPLE_EXPSIMP
      REAL*8 SIX_GAUSS_ERF_FREESIG_POLY_SET
      REAL*8 SIX_GAUSS_ERF_FREESIG_POLY_SET2
      REAL*8 SIX_GAUSS_ERF_FREESIG_POLY_SET3
      REAL*8 FOUR_GAUSS_ERF_TWO_GAUSS_SET
      REAL*8 FOUR_GAUSS_ERF_TWO_GAUSS_STAN_SET
      REAL*8 FOUR_GAUSS_ERF_TWO_GAUSS_STAN2_SET
      REAL*8 FOUR_GAUSS_ERF_TWO_GAUSS_STAN3_SET
      REAL*8 ROCKING_CURVE_SET
      REAL*8 DCS_EIGHT_VOIGT_POLYBG_X0_SET
      REAL*8 DCS_EIGHT_VOIGT_SET_NEIGHBOUR
      REAL*8 THREE_INTERP_POLY_N_SET
      REAL*8 TWO_INTERP_THREE_VOIGT_POLY

 
c     Choose your model (see below for definition)
      SELECT CASE (funcid)
      CASE(0)
         USERFCN_SET = GAUSS_BG_SET(x,npar,val,j)
      CASE(1)
         USERFCN_SET = DOUBLE_EXPSIMP(x,npar,val,j)
      CASE(2)
         USERFCN_SET = DOUBLE_EXPSIN(x,npar,val,j)
      CASE(3)
         USERFCN_SET = DOUBLE_EXPSIN_BIS(x,npar,val,j)
      CASE(4)
         USERFCN_SET = DOUBLE_TWO_EXPSIN(x,npar,val,j)
      CASE(5)
         USERFCN_SET = TRIPLE_EXPSIMP(x,npar,val,j)
      CASE(6)
         USERFCN_SET = TRIPLE_EXPSIN(x,npar,val,j)
      CASE(7)
         USERFCN_SET = TRIPLE_EXPSIN_BIS(x,npar,val,j)
      CASE(8)
         USERFCN_SET = TRIPLE_EXPSIN_TRIS(x,npar,val,j)
      CASE(9)
         USERFCN_SET = WEIBULL_EL_LASER(x,npar,val,j)
      CASE(10)
         USERFCN_SET = SIX_GAUSS_ERF_FREESIG_POLY_SET(x,npar,val,j)
      CASE(11)
         USERFCN_SET = SIX_GAUSS_ERF_FREESIG_POLY_SET2(x,npar,val,j)
      CASE(12)
         USERFCN_SET = SIX_GAUSS_ERF_FREESIG_POLY_SET3(x,npar,val,j)
      CASE(13)
         USERFCN_SET = FOUR_GAUSS_ERF_TWO_GAUSS_SET(x,npar,val,j)
      CASE(14)
         USERFCN_SET = FOUR_GAUSS_ERF_TWO_GAUSS_STAN_SET(x,npar,val,j)
      CASE(15)
         USERFCN_SET = FOUR_GAUSS_ERF_TWO_GAUSS_STAN2_SET(x,npar,val,j)
      CASE(16)
         USERFCN_SET = FOUR_GAUSS_ERF_TWO_GAUSS_STAN3_SET(x,npar,val,j)
      CASE(17)
         USERFCN_SET = ROCKING_CURVE_SET(x,npar,val,j)
      CASE(18)
         USERFCN_SET = DCS_EIGHT_VOIGT_POLYBG_X0_SET(x,npar,val,j)
      CASE(19)
         USERFCN_SET = DCS_EIGHT_VOIGT_SET_NEIGHBOUR(x,npar,val,j)
      CASE(20)
            USERFCN_SET = TWO_INTERP_THREE_VOIGT_POLY(x,npar,val,j)
      CASE(21)
            USERFCN_SET = THREE_INTERP_POLY_N_SET(x,npar,val,j) 
            
      
      END SELECT
      RETURN
      END

c______________________________________________________________________________________________

      FUNCTION GAUSS_BG_SET(x,npar,val,j)
c     Double gaussian with background with common sigma
      IMPLICIT NONE
      INTEGER*4 npar, j
      REAL*8 val(npar), val1(7), val2(4)
      REAL*8 GAUSS_BG_SET, GAUSS_BG, x
      REAL*8 x01, delta, amp1, amp2, sigma, bg1, bg2

      bg1   = val(1)
      x01   = val(2)
      amp1  = val(3)
      sigma = val(4)
      bg2   = val(5)
      delta = val(6)
      amp2  = val(7)

c     first gauss peak
      val1(1) = bg1
      val1(2) = x01
      val1(3) = amp1
      val1(4) = sigma


c     second gauss peak
      val2(1) = bg2
      val2(2) = x01 + delta
      val2(3) = amp2
      val2(4) = sigma

      IF (j.EQ.1) THEN
         GAUSS_BG_SET = GAUSS_BG(x,4,val1)
      ELSEIF (j.EQ.2) THEN
         GAUSS_BG_SET = GAUSS_BG(x,4,val2)
      ELSE
         WRITE(*,*) 'Too many spectra! Check your input files'
      END IF

      RETURN
      END

c______________________________________________________________________________________________

      FUNCTION DOUBLE_EXPSIMP(x,npar,val,j)
c     Double exponential decay with common decay constant
      IMPLICIT NONE
      INTEGER*4 npar, j
      REAL*8 val(npar), val1(2), val2(2)
      REAL*8 DOUBLE_EXPSIMP, EXPSIMP, x
      REAL*8 N01, N02, tau


      N01   = val(1)
      N02   = val(2)
      tau   = val(3)

c     first exponential decay
      val1(1) = N01
      val1(2) = tau


c     second exponential decay
      val2(1) = N02
      val2(2) = tau

      IF (j.EQ.1) THEN
         DOUBLE_EXPSIMP = EXPSIMP(x,2,val1)
      ELSEIF (j.EQ.2) THEN
         DOUBLE_EXPSIMP = EXPSIMP(x,2,val2)
      ELSE
         WRITE(*,*) 'Too many spectra! Check your input files'
      END IF

      RETURN
      END

c______________________________________________________________________________________________

      FUNCTION DOUBLE_EXPSIN(x,npar,val,j)
c     Double exponential decay with modulation with common decay constant and frequency (almost)
      IMPLICIT NONE
      INTEGER*4 npar, j
      REAL*8 val(npar), val1(5), val2(5)
      REAL*8 DOUBLE_EXPSIN, EXPSIN, x
      REAL*8 N01, N02, tau, amp1, amp2, omega, domega, phi1, phi2


      N01    = val(1)
      N02    = val(2)
      tau    = val(3)
      amp1   = val(4)
      amp2   = val(5)
      omega  = val(6)
      domega = val(7)
      phi1   = val(8)
      phi2   = val(9)


c     first exponential decay
      val1(1) = N01
      val1(2) = tau
      val1(3) = amp1
      val1(4) = omega
      val1(5) = phi1


c     second exponential decay
      val2(1) = N02
      val2(2) = tau
      val2(3) = amp2
      val2(4) = omega + domega
      val2(5) = phi2

      IF (j.EQ.1) THEN
         DOUBLE_EXPSIN = EXPSIN(x,5,val1)
      ELSEIF (j.EQ.2) THEN
         DOUBLE_EXPSIN = EXPSIN(x,5,val2)
      ELSE
         WRITE(*,*) 'Too many spectra! Check your input files'
      END IF

      RETURN
      END
c______________________________________________________________________________________________

      FUNCTION DOUBLE_EXPSIN_BIS(x,npar,val,j)
c     Double exponential decay with modulation with common decay constant and frequency (almost)
      IMPLICIT NONE
      INTEGER*4 npar, j
      REAL*8 val(npar), val1(5), val2(5)
      REAL*8 DOUBLE_EXPSIN_BIS, EXPSIN, x
      REAL*8 N01, N02, tau, amp1, damp2, omega, domega, phi1, dphi2


      N01    = val(1)
      N02    = val(2)
      tau    = val(3)
      amp1   = val(4)
      damp2   = val(5)
      omega  = val(6)
      domega = val(7)
      phi1   = val(8)
      dphi2   = val(9)


c     first exponential decay
      val1(1) = N01
      val1(2) = tau
      val1(3) = amp1
      val1(4) = omega
      val1(5) = phi1


c     second exponential decay
      val2(1) = N02
      val2(2) = tau
      val2(3) = amp1*damp2
      val2(4) = omega + domega
      val2(5) = phi1 + dphi2

      IF (j.EQ.1) THEN
         DOUBLE_EXPSIN_BIS = EXPSIN(x,5,val1)
      ELSEIF (j.EQ.2) THEN
         DOUBLE_EXPSIN_BIS = EXPSIN(x,5,val2)
      ELSE
         WRITE(*,*) 'Too many spectra! Check your input files'
      END IF

      RETURN
      END

c______________________________________________________________________________________________

      FUNCTION DOUBLE_TWO_EXPSIN(x,npar,val,j)
c     Double exponential decay with modulation with common decay constant and frequency (almost)
      IMPLICIT NONE
      INTEGER*4 npar, j
      REAL*8 val(npar), val1(8), val2(8)
      REAL*8 DOUBLE_TWO_EXPSIN, TWO_EXPSIN, x
      REAL*8 N01, N02, tau, amp11, amp12, damp2
      REAL*8 omega1, omega2, phi1, dphi2


      N01    = val(1)
      N02    = val(2)
      tau    = val(3)
      amp11  = val(4)
      amp12  = val(5)
      damp2  = val(6)
      omega1 = val(7)
      omega2 = val(8)
      phi1   = val(9)
      dphi2  = val(10)


c     first exponential decay
      val1(1) = N01
      val1(2) = tau
      val1(3) = amp11
      val1(4) = amp12
      val1(5) = omega1
      val1(6) = omega2
      val1(7) = phi1
      val1(8) = dphi2


c     second exponential decay
      val2(1) = N02
      val2(2) = tau
      val2(3) = amp11*damp2
      val2(4) = amp12*damp2
      val2(5) = omega1
      val2(6) = omega2
      val2(7) = phi1
      val2(8) = dphi2

      IF (j.EQ.1) THEN
         DOUBLE_TWO_EXPSIN = TWO_EXPSIN(x,8,val1)
      ELSEIF (j.EQ.2) THEN
         DOUBLE_TWO_EXPSIN = TWO_EXPSIN(x,8,val2)
      ELSE
         WRITE(*,*) 'Too many spectra! Check your input files'
      END IF

      RETURN
      END


c______________________________________________________________________________________________

      FUNCTION TRIPLE_EXPSIMP(x,npar,val,j)
c     Triple exponential decay with common decay constant
      IMPLICIT NONE
      INTEGER*4 npar, j
      REAL*8 val(npar), val1(2), val2(2), val3(2)
      REAL*8 TRIPLE_EXPSIMP, EXPSIMP, x
      REAL*8 N01, N02, N03,tau


      N01   = val(1)
      N02   = val(2)
      N03   = val(3)
      tau   = val(4)

c     first exponential decay
      val1(1) = N01
      val1(2) = tau


c     second exponential decay
      val2(1) = N02
      val2(2) = tau


c     third exponential decay
      val3(1) = N03
      val3(2) = tau

      IF (j.EQ.1) THEN
         TRIPLE_EXPSIMP = EXPSIMP(x,2,val1)
      ELSEIF (j.EQ.2) THEN
         TRIPLE_EXPSIMP = EXPSIMP(x,2,val2)
      ELSEIF (j.EQ.3) THEN
         TRIPLE_EXPSIMP = EXPSIMP(x,2,val3)
      ELSE
         WRITE(*,*) 'Too many spectra! Check your input files'
      END IF

      RETURN
      END

c______________________________________________________________________________________________

      FUNCTION TRIPLE_EXPSIN(x,npar,val,j)
c     Double exponential decay with modulation with common decay constant and frequency (almost)
      IMPLICIT NONE
      INTEGER*4 npar, j
      REAL*8 val(npar), val1(5), val2(5), val3(5)
      REAL*8 TRIPLE_EXPSIN, EXPSIN, x
      REAL*8 N01, N02,  N03, tau, amp1, amp2, amp3
      REAL*8  omega, domega1, domega2, phi1, phi2, phi3


      N01    = val(1)
      N02    = val(2)
      N03    = val(3)
      tau    = val(4)
      amp1   = val(5)
      amp2   = val(6)
      amp3   = val(7)
      omega  = val(8)
      domega1= val(9)
      domega2= val(10)
      phi1   = val(11)
      phi2   = val(12)
      phi3   = val(13)


c     first exponential decay
      val1(1) = N01
      val1(2) = tau
      val1(3) = amp1
      val1(4) = omega
      val1(5) = phi1


c     second exponential decay
      val2(1) = N02
      val2(2) = tau
      val2(3) = amp2
      val2(4) = omega + domega1
      val2(5) = phi2


c     third exponential decay
      val3(1) = N03
      val3(2) = tau
      val3(3) = amp3
      val3(4) = omega + domega2
      val3(5) = phi3

      IF (j.EQ.1) THEN
         TRIPLE_EXPSIN = EXPSIN(x,5,val1)
      ELSEIF (j.EQ.2) THEN
         TRIPLE_EXPSIN = EXPSIN(x,5,val2)
      ELSEIF (j.EQ.3) THEN
         TRIPLE_EXPSIN = EXPSIN(x,5,val3)
      ELSE
         WRITE(*,*) 'Too many spectra! Check your input files'
      END IF

      RETURN
      END
c______________________________________________________________________________________________

      FUNCTION TRIPLE_EXPSIN_BIS(x,npar,val,j)
c     Double exponential decay with modulation with common decay constant and frequency (almost)
      IMPLICIT NONE
      INTEGER*4 npar, j
      REAL*8 val(npar), val1(5), val2(5), val3(5)
      REAL*8 TRIPLE_EXPSIN_BIS, EXPSIN, x
      REAL*8 N01, N02,  N03, tau, amp1, damp2, damp3
      REAL*8  omega, domega1, domega2, phi1, dphi2, dphi3


      N01    = val(1)
      N02    = val(2)
      N03    = val(3)
      tau    = val(4)
      amp1   = val(5)
      damp2   = val(6)
      damp3   = val(7)
      omega  = val(8)
      domega1= val(9)
      domega2= val(10)
      phi1   = val(11)
      dphi2   = val(12)
      dphi3   = val(13)


c     first exponential decay
      val1(1) = N01
      val1(2) = tau
      val1(3) = amp1
      val1(4) = omega
      val1(5) = phi1


c     second exponential decay
      val2(1) = N02
      val2(2) = tau
      val2(3) = amp1*damp2
      val2(4) = omega + domega1
      val2(5) = phi1+dphi2


c     third exponential decay
      val3(1) = N03
      val3(2) = tau
      val3(3) = amp1*damp2*damp3
      val3(4) = omega + domega2
      val3(5) = phi1+dphi2+dphi3

      IF (j.EQ.1) THEN
         TRIPLE_EXPSIN_BIS = EXPSIN(x,5,val1)
      ELSEIF (j.EQ.2) THEN
         TRIPLE_EXPSIN_BIS = EXPSIN(x,5,val2)
      ELSEIF (j.EQ.3) THEN
         TRIPLE_EXPSIN_BIS = EXPSIN(x,5,val3)
      ELSE
         WRITE(*,*) 'Too many spectra! Check your input files'
      END IF

      RETURN
      END
c______________________________________________________________________________________________

      FUNCTION TRIPLE_EXPSIN_TRIS(x,npar,val,j)
c     Double exponential decay with modulation with common decay constant and frequency (almost)
      IMPLICIT NONE
      INTEGER*4 npar, j
      REAL*8 val(npar), val1(5), val2(5), val3(5)
      REAL*8 TRIPLE_EXPSIN_TRIS, EXPSIN, x
      REAL*8 N01, N02,  N03, tau, amp1, amp2, damp3
      REAL*8  omega, domega1, domega2, phi1, phi2, dphi3


      N01    = val(1)
      N02    = val(2)
      N03    = val(3)
      tau    = val(4)
      amp1   = val(5)
      amp2   = val(6)
      damp3   = val(7)
      omega  = val(8)
      domega1= val(9)
      domega2= val(10)
      phi1   = val(11)
      phi2   = val(12)
      dphi3   = val(13)


c     first exponential decay
      val1(1) = N01
      val1(2) = tau
      val1(3) = amp1
      val1(4) = omega
      val1(5) = phi1


c     second exponential decay
      val2(1) = N02
      val2(2) = tau
      val2(3) = amp2
      val2(4) = omega + domega1
      val2(5) = phi2


c     third exponential decay
      val3(1) = N03
      val3(2) = tau
      val3(3) = amp2*damp3
      val3(4) = omega + domega2
      val3(5) = phi2+dphi3

      IF (j.EQ.1) THEN
         TRIPLE_EXPSIN_TRIS = EXPSIN(x,5,val1)
      ELSEIF (j.EQ.2) THEN
         TRIPLE_EXPSIN_TRIS = EXPSIN(x,5,val2)
      ELSEIF (j.EQ.3) THEN
         TRIPLE_EXPSIN_TRIS = EXPSIN(x,5,val3)
      ELSE
         WRITE(*,*) 'Too many spectra! Check your input files'
      END IF

      RETURN
      END
c______________________________________________________________________________________________

      FUNCTION WEIBULL_EL_LASER(X,npar,val,j)
c     Double fit with normalized Weibull function with "erf" background
c     for the first spectrum (cluster-electron type) and
c     normalized Weibull for the second spectrum (cluster-laser type)
      IMPLICIT NONE
      INTEGER*4 npar, j
      REAL*8 val(npar), val_el(8), val_laser(4)
      REAL*8 WEIBULL,WEIBULL_EL_LASER, WEIBULL_ERFBG, x
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 x0_el, amp_el, lambda, kappa
      REAL*8 dx0_erf, amp_erf,dsigma_erf, bg_el
      REAL*8 dx0_laser, amp_laser
      CHARACTER*1 lr
      COMMON /func_exp/ lr

      x0_el      = val(1)
      amp_el     = val(2)
      lambda     = val(3)
      kappa      = val(4)
      dx0_erf    = val(5)
      amp_erf    = val(6)
      dsigma_erf = val(7)
      bg_el      = val(8)
      dx0_laser  = val(9)
      amp_laser  = val(10)

c     Parameters for the Weibull distribution with erf background for cluster-electron interaction
      val_el(1) = x0_el
      val_el(2) = amp_el
      val_el(3) = lambda
      val_el(4) = kappa
      val_el(5) = dx0_erf
      val_el(6) = amp_erf
      val_el(7) = dsigma_erf
      val_el(8) = bg_el
c     Parameters for the Weibull distribution for cluster-electron interaction
      val_laser(1) = x0_el + dx0_laser
      val_laser(2) = amp_laser
      val_laser(3) = lambda
      val_laser(4) = kappa


      IF(j.EQ.1) THEN
         WEIBULL_EL_LASER = WEIBULL_ERFBG(X,8,val_el)
      ELSE IF(j.EQ.2) THEN
         WEIBULL_EL_LASER = WEIBULL(X,4,val_laser)
      ELSE
         WRITE(*,*) 'Too many spectra! Check your input files'
      END IF


      RETURN
      END

c     _______________________________________________________________________________________________

      FUNCTION SIX_GAUSS_ERF_FREESIG_POLY_SET(X,npar,val,j)
c     two doublet of lines at different relative position with polynomial background (order 4)
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      INTEGER*4 j
      REAL*8 SIX_GAUSS_ERF_FREESIG_POLY_SET, GAUSS_ERF, POLY, x
      REAL*8 GAUSS, ERFFCN
      REAL*8 valp_1(8), val1_1g(3),val2_1g(3),val3_1g(3)
      REAL*8 val4_1g(3),val5_1g(3),val6_1g(3)
      REAL*8 val1_1(4),val2_1(4),val3_1(4)
      REAL*8 val4_1(4),val5_1(4),val6_1(4)
      REAL*8 val1_1e(3),val2_1e(3),val3_1e(3)
      REAL*8 val4_1e(3),val5_1e(3),val6_1e(3)
      REAL*8 valp_2(8), val1_2g(3),val2_2g(3),val3_2g(3)
      REAL*8 val4_2g(3),val5_2g(3),val6_2g(3)
      REAL*8 val1_2(4),val2_2(4),val3_2(4)
      REAL*8 val4_2(4),val5_2(4),val6_2(4)
      REAL*8 val1_2e(3),val2_2e(3),val3_2e(3)
      REAL*8 val4_2e(3),val5_2e(3),val6_2e(3)
      REAL*8 valp_3(8), val1_3g(3),val2_3g(3),val3_3g(3)
      REAL*8 val4_3g(3),val5_3g(3),val6_3g(3)
      REAL*8 val1_3(4),val2_3(4),val3_3(4)
      REAL*8 val4_3(4),val5_3(4),val6_3(4)
      REAL*8 val1_3e(3),val2_3e(3),val3_3e(3)
      REAL*8 val4_3e(3),val5_3e(3),val6_3e(3)
      REAL*8 x1_1, x1_2, x1_3, dx2, dx3, dx4, dx5, dx6
      REAL*8 amp1_1, amp2_1, amp3_1, amp4_1, amp5_1, amp6_1, damperf_1
      REAL*8 amp1_2, amp2_2, amp3_2, amp4_2, amp5_2, amp6_2, damperf_2
      REAL*8 amp1_3, amp2_3, amp3_3, amp4_3, amp5_3, amp6_3, damperf_3
      REAL*8 sigma1_1, sigma1_2, sigma1_3
      REAL*8 dsigma2, dsigma3, dsigma4, dsigma5, dsigma6
      REAL*8 a_1, b_1, c_1, a_2, b_2, c_2, a_3, b_3, c_3
      CHARACTER*1 lr
      COMMON /func_exp/ lr
      ! To plot the different components
      LOGICAL plot
      COMMON /func_plot/ plot

      x1_1       = val(1)
      dx2        = val(2)
      dx3        = val(3)
      dx4        = val(4)
      dx5        = val(5)
      dx6        = val(6)
      amp1_1     = val(7)
      amp2_1     = val(8)
      amp3_1     = val(9)
      amp4_1     = val(10)
      amp5_1     = val(11)
      amp6_1     = val(12)
      sigma1_1   = val(13)
      dsigma2    = val(14)
      dsigma3    = val(15)
      dsigma4    = val(16)
      dsigma5    = val(17)
      dsigma6    = val(18)
      damperf_1  = val(19)
      a_1        = val(20)
      b_1        = val(21)
      c_1        = val(22)
      x1_2       = val(23)
      amp1_2     = val(24)
      amp2_2     = val(25)
      amp3_2     = val(26)
      amp4_2     = val(27)
      amp5_2     = val(28)
      amp6_2     = val(29)
      sigma1_2   = val(30)
      damperf_2  = val(31)
      a_2        = val(32)
      b_2        = val(33)
      c_2        = val(34)
      x1_3       = val(35)
      amp1_3     = val(36)
      amp2_3     = val(37)
      amp3_3     = val(38)
      amp4_3     = val(39)
      amp5_3     = val(40)
      amp6_3     = val(41)
      sigma1_3   = val(42)
      damperf_3  = val(43)
      a_3        = val(44)
      b_3        = val(45)
      c_3        = val(46)


c     FIRST SPECTRUM

c     First (main) peak
      val1_1(1) = x1_1
      val1_1(2) = amp1_1
      val1_1(3) = sigma1_1
      val1_1(4) = damperf_1

c     Second peak
      val2_1(1) = x1_1 + dx2
      val2_1(2) = amp2_1
      val2_1(3) = sigma1_1*dsigma2
      val2_1(4) = damperf_1

c     Third peak
      val3_1(1) = x1_1 + dx3
      val3_1(2) = amp3_1
      val3_1(3) = sigma1_1*dsigma3
      val3_1(4) = damperf_1

c     Fourth peak
      val4_1(1) = x1_1 + dx4
      val4_1(2) = amp4_1
      val4_1(3) = sigma1_1*dsigma4
      val4_1(4) = damperf_1

c     Fifth peak
      val5_1(1) = x1_1 + dx5
      val5_1(2) = amp5_1
      val5_1(3) = sigma1_1*dsigma5
      val5_1(4) = damperf_1

c     Sixth peak
      val6_1(1) = x1_1 + dx6
      val6_1(2) = amp6_1
      val6_1(3) = sigma1_1*dsigma6
      val6_1(4) = damperf_1

c     SECOND SPECTRUM

c     First (main) peak
      val1_2(1) = x1_2
      val1_2(2) = amp1_2
      val1_2(3) = sigma1_2
      val1_2(4) = damperf_2

c     Second peak
      val2_2(1) = x1_2 + dx2
      val2_2(2) = amp2_2
      val2_2(3) = sigma1_2*dsigma2
      val2_2(4) = damperf_2

c     Third peak
      val3_2(1) = x1_2 + dx3
      val3_2(2) = amp3_2
      val3_2(3) = sigma1_2*dsigma3
      val3_2(4) = damperf_2

c     Fourth peak
      val4_2(1) = x1_2 + dx4
      val4_2(2) = amp4_2
      val4_2(3) = sigma1_2*dsigma4
      val4_2(4) = damperf_2

c     Fifth peak
      val5_2(1) = x1_2 + dx5
      val5_2(2) = amp5_2
      val5_2(3) = sigma1_2*dsigma5
      val5_2(4) = damperf_2

c     Sixth peak
      val6_2(1) = x1_2 + dx6
      val6_2(2) = amp6_2
      val6_2(3) = sigma1_2*dsigma6
      val6_2(4) = damperf_2

c     THIRD SPECTRUM

c     First (main) peak
      val1_3(1) = x1_3
      val1_3(2) = amp1_3
      val1_3(3) = sigma1_3
      val1_3(4) = damperf_3

c     Second peak
      val2_3(1) = x1_3 + dx2
      val2_3(2) = amp2_3
      val2_3(3) = sigma1_3*dsigma2
      val2_3(4) = damperf_3

c     Third peak
      val3_3(1) = x1_3 + dx3
      val3_3(2) = amp3_3
      val3_3(3) = sigma1_3*dsigma3
      val3_3(4) = damperf_3

c     Fourth peak
      val4_3(1) = x1_3 + dx4
      val4_3(2) = amp4_3
      val4_3(3) = sigma1_3*dsigma4
      val4_3(4) = damperf_3

c     Fifth peak
      val5_3(1) = x1_3 + dx5
      val5_3(2) = amp5_3
      val5_3(3) = sigma1_3*dsigma5
      val5_3(4) = damperf_3

c     Sixth peak
      val6_3(1) = x1_3 + dx6
      val6_3(2) = amp6_3
      val6_3(3) = sigma1_3*dsigma6
      val6_3(4) = damperf_3

c     FIRST SPECTRUM

c     First (main) peak
      val1_1g(1) = x1_1
      val1_1g(2) = amp1_1
      val1_1g(3) = sigma1_1
      val1_1e(1) = x1_1
      val1_1e(2) = amp1_1*damperf_1
      val1_1e(3) = sigma1_1

c     Second peak
      val2_1g(1) = x1_1 + dx2
      val2_1g(2) = amp2_1
      val2_1g(3) = sigma1_1*dsigma2
      val2_1e(1) = x1_1 + dx2
      val2_1e(2) = amp2_1*damperf_1
      val2_1e(3) = sigma1_1*dsigma2

c     Third peak
      val3_1g(1) = x1_1 + dx3
      val3_1g(2) = amp3_1
      val3_1g(3) = sigma1_1*dsigma3
      val3_1e(1) = x1_1 + dx3
      val3_1e(2) = amp3_1*damperf_1
      val3_1e(3) = sigma1_1*dsigma3

c     Fourth peak
      val4_1g(1) = x1_1 + dx4
      val4_1g(2) = amp4_1
      val4_1g(3) = sigma1_1*dsigma4
      val4_1e(1) = x1_1 + dx4
      val4_1e(2) = amp4_1*damperf_1
      val4_1e(3) = sigma1_1*dsigma4

c     Fifth peak
      val5_1g(1) = x1_1 + dx5
      val5_1g(2) = amp5_1
      val5_1g(3) = sigma1_1*dsigma5
      val5_1e(1) = x1_1 + dx5
      val5_1e(2) = amp5_1*damperf_1
      val5_1e(3) = sigma1_1*dsigma5

c     Sixth peak
      val6_1g(1) = x1_1 + dx6
      val6_1g(2) = amp6_1
      val6_1g(3) = sigma1_1*dsigma6
      val6_1e(1) = x1_1 + dx6
      val6_1e(2) = amp6_1*damperf_1
      val6_1e(3) = sigma1_1*dsigma6

c     Polynomial background
      valp_1(1) = 0.
      valp_1(2) = a_1
      valp_1(3) = b_1
      valp_1(4) = c_1
      valp_1(5) = 0.
      valp_1(6) = 0.
      valp_1(7) = 0.
      valp_1(8) = 0.

c     SECOND SPECTRUM

c     First (main) peak
      val1_2g(1) = x1_2
      val1_2g(2) = amp1_2
      val1_2g(3) = sigma1_2
      val1_2e(1) = x1_2
      val1_2e(2) = amp1_2*damperf_2
      val1_2e(3) = sigma1_2

c     Second peak
      val2_2g(1) = x1_2 + dx2
      val2_2g(2) = amp2_2
      val2_2g(3) = sigma1_2*dsigma2
      val2_2e(1) = x1_2 + dx2
      val2_2e(2) = amp2_2*damperf_2
      val2_2e(3) = sigma1_2*dsigma2

c     Third peak
      val3_2g(1) = x1_2 + dx3
      val3_2g(2) = amp3_2
      val3_2g(3) = sigma1_2*dsigma3
      val3_2e(1) = x1_2 + dx3
      val3_2e(2) = amp3_2*damperf_2
      val3_2e(3) = sigma1_2*dsigma3

c     Fourth peak
      val4_2g(1) = x1_2 + dx4
      val4_2g(2) = amp4_2
      val4_2g(3) = sigma1_2*dsigma4
      val4_2e(1) = x1_2 + dx4
      val4_2e(2) = amp4_2*damperf_2
      val4_2e(3) = sigma1_2*dsigma4

c     Fifth peak
      val5_2g(1) = x1_2 + dx5
      val5_2g(2) = amp5_2
      val5_2g(3) = sigma1_2*dsigma5
      val5_2e(1) = x1_2 + dx5
      val5_2e(2) = amp5_2*damperf_2
      val5_2e(3) = sigma1_2*dsigma5

c     Sixth peak
      val6_2g(1) = x1_2 + dx6
      val6_2g(2) = amp6_2
      val6_2g(3) = sigma1_2*dsigma6
      val6_2e(1) = x1_2 + dx6
      val6_2e(2) = amp6_2*damperf_2
      val6_2e(3) = sigma1_2*dsigma6

c     Polynomial background
      valp_2(1) = 0.
      valp_2(2) = a_2
      valp_2(3) = b_2
      valp_2(4) = c_2
      valp_2(5) = 0.
      valp_2(6) = 0.
      valp_2(7) = 0.
      valp_2(8) = 0.

c     THIRD SPECTRUM

c     First (main) peak
      val1_3g(1) = x1_3
      val1_3g(2) = amp1_3
      val1_3g(3) = sigma1_3
      val1_3e(1) = x1_3
      val1_3e(2) = amp1_3*damperf_3
      val1_3e(3) = sigma1_3

c     Second peak
      val2_3g(1) = x1_3 + dx2
      val2_3g(2) = amp2_3
      val2_3g(3) = sigma1_3*dsigma2
      val2_3e(1) = x1_3 + dx2
      val2_3e(2) = amp2_3*damperf_3
      val2_3e(3) = sigma1_3*dsigma2

c     Third peak
      val3_3g(1) = x1_3 + dx3
      val3_3g(2) = amp3_3
      val3_3g(3) = sigma1_3*dsigma3
      val3_3e(1) = x1_3 + dx3
      val3_3e(2) = amp3_3*damperf_3
      val3_3e(3) = sigma1_3*dsigma3

c     Fourth peak
      val4_3g(1) = x1_3 + dx4
      val4_3g(2) = amp4_3
      val4_3g(3) = sigma1_3*dsigma4
      val4_3e(1) = x1_3 + dx4
      val4_3e(2) = amp4_3*damperf_3
      val4_3e(3) = sigma1_3*dsigma4

c     Fifth peak
      val5_3g(1) = x1_3 + dx5
      val5_3g(2) = amp5_3
      val5_3g(3) = sigma1_3*dsigma5
      val5_3e(1) = x1_3 + dx5
      val5_3e(2) = amp5_3*damperf_3
      val5_3e(3) = sigma1_3*dsigma5

c     Sixth peak
      val6_3g(1) = x1_3 + dx6
      val6_3g(2) = amp6_3
      val6_3g(3) = sigma1_3*dsigma6
      val6_3e(1) = x1_3 + dx6
      val6_3e(2) = amp6_3*damperf_3
      val6_3e(3) = sigma1_3*dsigma6

c     Polynomial background
      valp_3(1) = 0.
      valp_3(2) = a_3
      valp_3(3) = b_3
      valp_3(4) = c_3
      valp_3(5) = 0.
      valp_3(6) = 0.
      valp_3(7) = 0.
      valp_3(8) = 0.

      IF (j.EQ.1) THEN
         SIX_GAUSS_ERF_FREESIG_POLY_SET = GAUSS_ERF(x,4,val1_1) +
     +        GAUSS_ERF(x,4,val2_1) + GAUSS_ERF(x,4,val3_1) +
     +        GAUSS_ERF(x,4,val4_1) + GAUSS_ERF(x,4,val5_1) +
     +        GAUSS_ERF(x,4,val6_1) + POLY(x,8,valp_1)
      ELSEIF (j.EQ.2) THEN
         SIX_GAUSS_ERF_FREESIG_POLY_SET = GAUSS_ERF(x,4,val1_2) +
     +        GAUSS_ERF(x,4,val2_2) + GAUSS_ERF(x,4,val3_2) +
     +        GAUSS_ERF(x,4,val4_2) + GAUSS_ERF(x,4,val5_2) +
     +        GAUSS_ERF(x,4,val6_2) + POLY(x,8,valp_2)
      ELSEIF (j.EQ.3) THEN
         SIX_GAUSS_ERF_FREESIG_POLY_SET = GAUSS_ERF(x,4,val1_3) +
     +        GAUSS_ERF(x,4,val2_3) + GAUSS_ERF(x,4,val3_3) +
     +        GAUSS_ERF(x,4,val4_3) + GAUSS_ERF(x,4,val5_3) +
     +        GAUSS_ERF(x,4,val6_3) + POLY(x,8,valp_3)
      ELSE
         WRITE(*,*) 'Too many spectra! Check your input files'
      END IF

c     Save the different components
      IF(plot) THEN
         IF (j.EQ.1) THEN
           WRITE(30,*) x, GAUSS(x,4,val1_1g),
     +           GAUSS(x,3,val2_1g),  GAUSS(x,3,val3_1g),
     +           GAUSS(x,3,val4_1g),  GAUSS(x,3,val5_1g),
     +           GAUSS(x,3,val6_1g),  POLY(x,8,valp_1),
     +           ERFFCN(x,3,val1_1e), ERFFCN(x,3,val2_1e),
     +			 ERFFCN(x,3,val3_1e), ERFFCN(x,3,val4_1e),
     +			 ERFFCN(x,3,val5_1e), ERFFCN(x,3,val6_1e)
         ELSEIF (j.EQ.2) THEN
            WRITE(30,*) x, GAUSS(x,3,val1_2g),
     +           GAUSS(x,3,val2_2g),  GAUSS(x,3,val3_2g),
     +           GAUSS(x,3,val4_2g),  GAUSS(x,3,val5_2g),
     +           GAUSS(x,3,val6_2g),  POLY(x,8,valp_2),
     +           ERFFCN(x,3,val1_2e), ERFFCN(x,3,val2_2e),
     +		 	 ERFFCN(x,3,val3_2e), ERFFCN(x,3,val4_2e),
     +			 ERFFCN(x,3,val5_2e), ERFFCN(x,3,val6_2e)
         ELSEIF (j.EQ.3) THEN
            WRITE(30,*) x, GAUSS(x,3,val1_3g),
     +           GAUSS(x,3,val2_3g),  GAUSS(x,3,val3_3g),
     +           GAUSS(x,3,val4_3g),  GAUSS(x,3,val5_3g),
     +           GAUSS(x,3,val6_3g),  POLY(x,8,valp_3),
     +			 ERFFCN(x,3,val1_3e), ERFFCN(x,3,val2_3e),
     +			 ERFFCN(x,3,val3_3e), ERFFCN(x,3,val4_3e),
     +			 ERFFCN(x,3,val5_3e), ERFFCN(x,3,val6_3e)
      ELSE
         WRITE(*,*) 'Too many spectra! Check your input files'
      END IF
      ENDIF


      RETURN
      END

c     _______________________________________________________________________________________________


      FUNCTION SIX_GAUSS_ERF_FREESIG_POLY_SET2(X,npar,val,j)
c     two doublet of lines at different relative position with polynomial background (order 4)
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      INTEGER*4 j
      REAL*8 SIX_GAUSS_ERF_FREESIG_POLY_SET2, GAUSS_ERF, POLY, x
      REAL*8 GAUSS, ERFFCN
      REAL*8 valp_1(8), val1_1g(3),val2_1g(3),val3_1g(3)
      REAL*8 val4_1g(3),val5_1g(3),val6_1g(3)
      REAL*8 val1_1(4),val2_1(4),val3_1(4)
      REAL*8 val4_1(4),val5_1(4),val6_1(4)
      REAL*8 val1_1e(3),val2_1e(3),val3_1e(3)
      REAL*8 val4_1e(3),val5_1e(3),val6_1e(3)
      REAL*8 valp_2(8), val1_2g(3),val2_2g(3),val3_2g(3)
      REAL*8 val4_2g(3),val5_2g(3),val6_2g(3)
      REAL*8 val1_2(4),val2_2(4),val3_2(4)
      REAL*8 val4_2(4),val5_2(4),val6_2(4)
      REAL*8 val1_2e(3),val2_2e(3),val3_2e(3)
      REAL*8 val4_2e(3),val5_2e(3),val6_2e(3)
      REAL*8 valp_3(8), val1_3g(3),val2_3g(3),val3_3g(3)
      REAL*8 val4_3g(3),val5_3g(3),val6_3g(3)
      REAL*8 val1_3(4),val2_3(4),val3_3(4)
      REAL*8 val4_3(4),val5_3(4),val6_3(4)
      REAL*8 val1_3e(3),val2_3e(3),val3_3e(3)
      REAL*8 val4_3e(3),val5_3e(3),val6_3e(3)
      REAL*8 x1_1, x1_2, x1_3, dx2, dx3, dx4, dx5, dx6
      REAL*8 amp1_1, amp2_1, amp3_1, amp4_1, damp45, amp6_1, damperf_1
      REAL*8 amp1_2, amp2_2, amp3_2, amp4_2, amp6_2, damperf_2
      REAL*8 amp1_3, amp2_3, amp3_3, amp4_3, amp6_3, damperf_3
      REAL*8 sigma1_1, sigma1_2, sigma1_3
      REAL*8 sigma4_1, sigma4_2, sigma4_3
      REAL*8 dsigma21, dsigma31, dsigma54, dsigma61
      REAL*8 a_1, b_1, c_1, a_2, b_2, c_2, a_3, b_3, c_3
      CHARACTER*1 lr
      COMMON /func_exp/ lr
      ! To plot the different components
      LOGICAL plot
      COMMON /func_plot/ plot

      x1_1       = val(1)
      dx2        = val(2)
      dx3        = val(3)
      dx4        = val(4)
      dx5        = val(5)
      dx6        = val(6)
      amp1_1     = val(7)
      amp2_1     = val(8)
      amp3_1     = val(9)
      amp4_1     = val(10)
      damp45     = val(11)
      amp6_1     = val(12)
      sigma1_1   = val(13)
      dsigma21   = val(14)
      dsigma31   = val(15)
      sigma4_1   = val(16)
      dsigma54   = val(17)
      dsigma61   = val(18)
      damperf_1  = val(19)
      a_1        = val(20)
      b_1        = val(21)
      c_1        = val(22)
      x1_2       = val(23)
      amp1_2     = val(24)
      amp2_2     = val(25)
      amp3_2     = val(26)
      amp4_2     = val(27)
      amp6_2     = val(28)
      sigma1_2   = val(29)
      sigma4_2   = val(30)
      damperf_2  = val(31)
      a_2        = val(32)
      b_2        = val(33)
      c_2        = val(34)
      x1_3       = val(35)
      amp1_3     = val(36)
      amp2_3     = val(37)
      amp3_3     = val(38)
      amp4_3     = val(39)
      amp6_3     = val(40)
      sigma1_3   = val(41)
      sigma4_3   = val(42)
      damperf_3  = val(43)
      a_3        = val(44)
      b_3        = val(45)
      c_3        = val(46)


c     FIRST SPECTRUM

c     First (main) peak
      val1_1(1) = x1_1
      val1_1(2) = amp1_1
      val1_1(3) = sigma1_1
      val1_1(4) = damperf_1

c     Second peak
      val2_1(1) = x1_1 + dx2
      val2_1(2) = amp2_1
      val2_1(3) = sigma1_1*dsigma21
      val2_1(4) = damperf_1

c     Third peak
      val3_1(1) = x1_1 + dx3
      val3_1(2) = amp3_1
      val3_1(3) = sigma1_1*dsigma31
      val3_1(4) = damperf_1

c     Fourth peak
      val4_1(1) = x1_1 + dx4
      val4_1(2) = amp4_1
      val4_1(3) = sigma4_1
      val4_1(4) = damperf_1

c     Fifth peak
      val5_1(1) = x1_1 + dx5
      val5_1(2) = amp4_1*damp45
      val5_1(3) = sigma4_1*dsigma54
      val5_1(4) = damperf_1

c     Sixth peak
      val6_1(1) = x1_1 + dx6
      val6_1(2) = amp6_1
      val6_1(3) = sigma1_1*dsigma61
      val6_1(4) = damperf_1

c     SECOND SPECTRUM

c     First (main) peak
      val1_2(1) = x1_2
      val1_2(2) = amp1_2
      val1_2(3) = sigma1_2
      val1_2(4) = damperf_2

c     Second peak
      val2_2(1) = x1_2 + dx2
      val2_2(2) = amp2_2
      val2_2(3) = sigma1_2*dsigma21
      val2_2(4) = damperf_2

c     Third peak
      val3_2(1) = x1_2 + dx3
      val3_2(2) = amp3_2
      val3_2(3) = sigma1_2*dsigma31
      val3_2(4) = damperf_2

c     Fourth peak
      val4_2(1) = x1_2 + dx4
      val4_2(2) = amp4_2
      val4_2(3) = sigma4_2
      val4_2(4) = damperf_2

c     Fifth peak
      val5_2(1) = x1_2 + dx5
      val5_2(2) = amp4_2*damp45
      val5_2(3) = sigma4_2*dsigma54
      val5_2(4) = damperf_2

c     Sixth peak
      val6_2(1) = x1_2 + dx6
      val6_2(2) = amp6_2
      val6_2(3) = sigma1_2*dsigma61
      val6_2(4) = damperf_2

c     THIRD SPECTRUM

c     First (main) peak
      val1_3(1) = x1_3
      val1_3(2) = amp1_3
      val1_3(3) = sigma1_3
      val1_3(4) = damperf_3

c     Second peak
      val2_3(1) = x1_3 + dx2
      val2_3(2) = amp2_3
      val2_3(3) = sigma1_3*dsigma21
      val2_3(4) = damperf_3

c     Third peak
      val3_3(1) = x1_3 + dx3
      val3_3(2) = amp3_3
      val3_3(3) = sigma1_3*dsigma31
      val3_3(4) = damperf_3

c     Fourth peak
      val4_3(1) = x1_3 + dx4
      val4_3(2) = amp4_3
      val4_3(3) = sigma4_3
      val4_3(4) = damperf_3

c     Fifth peak
      val5_3(1) = x1_3 + dx5
      val5_3(2) = amp4_3*damp45
      val5_3(3) = sigma4_3*dsigma54
      val5_3(4) = damperf_3

c     Sixth peak
      val6_3(1) = x1_3 + dx6
      val6_3(2) = amp6_3
      val6_3(3) = sigma1_3*dsigma61
      val6_3(4) = damperf_3

c     FOR SEPARATE COMPONENTS FOR PLOTS cccccccccccccccccccccc

c     FIRST SPECTRUM

c     First (main) peak
      val1_1g(1) = x1_1
      val1_1g(2) = amp1_1
      val1_1g(3) = sigma1_1
      val1_1e(1) = x1_1
      val1_1e(2) = amp1_1*damperf_1
      val1_1e(3) = sigma1_1

c     Second peak
      val2_1g(1) = x1_1 + dx2
      val2_1g(2) = amp2_1
      val2_1g(3) = sigma1_1*dsigma21
      val2_1e(1) = x1_1 + dx2
      val2_1e(2) = amp2_1*damperf_1
      val2_1e(3) = sigma1_1*dsigma21

c     Third peak
      val3_1g(1) = x1_1 + dx3
      val3_1g(2) = amp3_1
      val3_1g(3) = sigma1_1*dsigma31
      val3_1e(1) = x1_1 + dx3
      val3_1e(2) = amp3_1*damperf_1
      val3_1e(3) = sigma1_1*dsigma31

c     Fourth peak
      val4_1g(1) = x1_1 + dx4
      val4_1g(2) = amp4_1
      val4_1g(3) = sigma4_1
      val4_1e(1) = x1_1 + dx4
      val4_1e(2) = amp4_1*damperf_1
      val4_1e(3) = sigma4_1

c     Fifth peak
      val5_1g(1) = x1_1 + dx5
      val5_1g(2) = amp4_1*damp45
      val5_1g(3) = sigma4_1*dsigma54
      val5_1e(1) = x1_1 + dx5
      val5_1e(2) = amp4_1*damp45*damperf_1
      val5_1e(3) = sigma4_1*dsigma54

c     Sixth peak
      val6_1g(1) = x1_1 + dx6
      val6_1g(2) = amp6_1
      val6_1g(3) = sigma1_1*dsigma61
      val6_1e(1) = x1_1 + dx6
      val6_1e(2) = amp6_1*damperf_1
      val6_1e(3) = sigma1_1*dsigma61

c     Polynomial background
      valp_1(1) = 0.
      valp_1(2) = a_1
      valp_1(3) = b_1
      valp_1(4) = c_1
      valp_1(5) = 0.
      valp_1(6) = 0.
      valp_1(7) = 0.
      valp_1(8) = 0.

c     SECOND SPECTRUM

c     First (main) peak
      val1_2g(1) = x1_2
      val1_2g(2) = amp1_2
      val1_2g(3) = sigma1_2
      val1_2e(1) = x1_2
      val1_2e(2) = amp1_2*damperf_2
      val1_2e(3) = sigma1_2

c     Second peak
      val2_2g(1) = x1_2 + dx2
      val2_2g(2) = amp2_2
      val2_2g(3) = sigma1_2*dsigma21
      val2_2e(1) = x1_2 + dx2
      val2_2e(2) = amp2_2*damperf_2
      val2_2e(3) = sigma1_2*dsigma21

c     Third peak
      val3_2g(1) = x1_2 + dx3
      val3_2g(2) = amp3_2
      val3_2g(3) = sigma1_2*dsigma31
      val3_2e(1) = x1_2 + dx3
      val3_2e(2) = amp3_2*damperf_2
      val3_2e(3) = sigma1_2*dsigma31

c     Fourth peak
      val4_2g(1) = x1_2 + dx4
      val4_2g(2) = amp4_2
      val4_2g(3) = sigma4_2
      val4_2e(1) = x1_2 + dx4
      val4_2e(2) = amp4_2*damperf_2
      val4_2e(3) = sigma4_2

c     Fifth peak
      val5_2g(1) = x1_2 + dx5
      val5_2g(2) = amp4_2*damp45
      val5_2g(3) = sigma4_2*dsigma54
      val5_2e(1) = x1_2 + dx5
      val5_2e(2) = amp4_2*damp45*damperf_2
      val5_2e(3) = sigma4_2*dsigma54

c     Sixth peak
      val6_2g(1) = x1_2 + dx6
      val6_2g(2) = amp6_2
      val6_2g(3) = sigma1_2*dsigma61
      val6_2e(1) = x1_2 + dx6
      val6_2e(2) = amp6_2*damperf_2
      val6_2e(3) = sigma1_2*dsigma61

c     Polynomial background
      valp_2(1) = 0.
      valp_2(2) = a_2
      valp_2(3) = b_2
      valp_2(4) = c_2
      valp_2(5) = 0.
      valp_2(6) = 0.
      valp_2(7) = 0.
      valp_2(8) = 0.

c     THIRD SPECTRUM

c     First (main) peak
      val1_3g(1) = x1_3
      val1_3g(2) = amp1_3
      val1_3g(3) = sigma1_3
      val1_3e(1) = x1_3
      val1_3e(2) = amp1_3*damperf_3
      val1_3e(3) = sigma1_3

c     Second peak
      val2_3g(1) = x1_3 + dx2
      val2_3g(2) = amp2_3
      val2_3g(3) = sigma1_3*dsigma21
      val2_3e(1) = x1_3 + dx2
      val2_3e(2) = amp2_3*damperf_3
      val2_3e(3) = sigma1_3*dsigma21

c     Third peak
      val3_3g(1) = x1_3 + dx3
      val3_3g(2) = amp3_3
      val3_3g(3) = sigma1_3*dsigma31
      val3_3e(1) = x1_3 + dx3
      val3_3e(2) = amp3_3*damperf_3
      val3_3e(3) = sigma1_3*dsigma31

c     Fourth peak
      val4_3g(1) = x1_3 + dx4
      val4_3g(2) = amp4_3
      val4_3g(3) = sigma4_3
      val4_3e(1) = x1_3 + dx4
      val4_3e(2) = amp4_3*damperf_3
      val4_3e(3) = sigma4_3

c     Fifth peak
      val5_3g(1) = x1_3 + dx5
      val5_3g(2) = amp4_3*damp45
      val5_3g(3) = sigma4_3*dsigma54
      val5_3e(1) = x1_3 + dx5
      val5_3e(2) = amp4_3*damp45*damperf_3
      val5_3e(3) = sigma4_3*dsigma54

c     Sixth peak
      val6_3g(1) = x1_3 + dx6
      val6_3g(2) = amp6_3
      val6_3g(3) = sigma1_3*dsigma61
      val6_3e(1) = x1_3 + dx6
      val6_3e(2) = amp6_3*damperf_3
      val6_3e(3) = sigma1_3*dsigma61

c     Polynomial background
      valp_3(1) = 0.
      valp_3(2) = a_3
      valp_3(3) = b_3
      valp_3(4) = c_3
      valp_3(5) = 0.
      valp_3(6) = 0.
      valp_3(7) = 0.
      valp_3(8) = 0.

      IF (j.EQ.1) THEN
         SIX_GAUSS_ERF_FREESIG_POLY_SET2 = GAUSS_ERF(x,4,val1_1) +
     +        GAUSS_ERF(x,4,val2_1) + GAUSS_ERF(x,4,val3_1) +
     +        GAUSS_ERF(x,4,val4_1) + GAUSS_ERF(x,4,val5_1) +
     +        GAUSS_ERF(x,4,val6_1) + POLY(x,8,valp_1)
      ELSEIF (j.EQ.2) THEN
         SIX_GAUSS_ERF_FREESIG_POLY_SET2 = GAUSS_ERF(x,4,val1_2) +
     +        GAUSS_ERF(x,4,val2_2) + GAUSS_ERF(x,4,val3_2) +
     +        GAUSS_ERF(x,4,val4_2) + GAUSS_ERF(x,4,val5_2) +
     +        GAUSS_ERF(x,4,val6_2) + POLY(x,8,valp_2)
      ELSEIF (j.EQ.3) THEN
         SIX_GAUSS_ERF_FREESIG_POLY_SET2 = GAUSS_ERF(x,4,val1_3) +
     +        GAUSS_ERF(x,4,val2_3) + GAUSS_ERF(x,4,val3_3) +
     +        GAUSS_ERF(x,4,val4_3) + GAUSS_ERF(x,4,val5_3) +
     +        GAUSS_ERF(x,4,val6_3) + POLY(x,8,valp_3)
      ELSE
         WRITE(*,*) 'Too many spectra! Check your input files'
      END IF

c     Save the different components
      IF(plot) THEN
         IF (j.EQ.1) THEN
           WRITE(30,*) x, GAUSS(x,4,val1_1g),
     +           GAUSS(x,3,val2_1g),  GAUSS(x,3,val3_1g),
     +           GAUSS(x,3,val4_1g),  GAUSS(x,3,val5_1g),
     +           GAUSS(x,3,val6_1g),  POLY(x,8,valp_1),
     +           ERFFCN(x,3,val1_1e), ERFFCN(x,3,val2_1e),
     +			 ERFFCN(x,3,val3_1e), ERFFCN(x,3,val4_1e),
     +			 ERFFCN(x,3,val5_1e), ERFFCN(x,3,val6_1e)
         ELSEIF (j.EQ.2) THEN
            WRITE(30,*) x, GAUSS(x,3,val1_2g),
     +           GAUSS(x,3,val2_2g),  GAUSS(x,3,val3_2g),
     +           GAUSS(x,3,val4_2g),  GAUSS(x,3,val5_2g),
     +           GAUSS(x,3,val6_2g),  POLY(x,8,valp_2),
     +           ERFFCN(x,3,val1_2e), ERFFCN(x,3,val2_2e),
     +		 	 ERFFCN(x,3,val3_2e), ERFFCN(x,3,val4_2e),
     +			 ERFFCN(x,3,val5_2e), ERFFCN(x,3,val6_2e)
         ELSEIF (j.EQ.3) THEN
            WRITE(30,*) x, GAUSS(x,3,val1_3g),
     +           GAUSS(x,3,val2_3g),  GAUSS(x,3,val3_3g),
     +           GAUSS(x,3,val4_3g),  GAUSS(x,3,val5_3g),
     +           GAUSS(x,3,val6_3g),  POLY(x,8,valp_3),
     +			 ERFFCN(x,3,val1_3e), ERFFCN(x,3,val2_3e),
     +			 ERFFCN(x,3,val3_3e), ERFFCN(x,3,val4_3e),
     +			 ERFFCN(x,3,val5_3e), ERFFCN(x,3,val6_3e)
      ELSE
         WRITE(*,*) 'Too many spectra! Check your input files'
      END IF
      ENDIF

	  RETURN
	  END


      FUNCTION SIX_GAUSS_ERF_FREESIG_POLY_SET3(X,npar,val,j)
c     two doublet of lines at different relative position with polynomial background (order 4)
      IMPLICIT NONE
      INTEGER*4 npar
      REAL*8 val(npar)
      INTEGER*4 j
      REAL*8 SIX_GAUSS_ERF_FREESIG_POLY_SET3, GAUSS_ERF, POLY, x
      REAL*8 GAUSS, ERFFCN
      REAL*8 valp_1(8), val1_1g(3),val2_1g(3),val3_1g(3)
      REAL*8 val4_1g(3),val5_1g(3),val6_1g(3)
      REAL*8 val1_1(4),val2_1(4),val3_1(4)
      REAL*8 val4_1(4),val5_1(4),val6_1(4)
      REAL*8 val1_1e(3),val2_1e(3),val3_1e(3)
      REAL*8 val4_1e(3),val5_1e(3),val6_1e(3)
      REAL*8 valp_2(8), val1_2g(3),val2_2g(3),val3_2g(3)
      REAL*8 val4_2g(3),val5_2g(3),val6_2g(3)
      REAL*8 val1_2(4),val2_2(4),val3_2(4)
      REAL*8 val4_2(4),val5_2(4),val6_2(4)
      REAL*8 val1_2e(3),val2_2e(3),val3_2e(3)
      REAL*8 val4_2e(3),val5_2e(3),val6_2e(3)
      REAL*8 valp_3(8), val1_3g(3),val2_3g(3),val3_3g(3)
      REAL*8 val4_3g(3),val5_3g(3),val6_3g(3)
      REAL*8 val1_3(4),val2_3(4),val3_3(4)
      REAL*8 val4_3(4),val5_3(4),val6_3(4)
      REAL*8 val1_3e(3),val2_3e(3),val3_3e(3)
      REAL*8 val4_3e(3),val5_3e(3),val6_3e(3)
      REAL*8 x1_1, x1_2, x1_3, dx2, dx3, dx4, dx5, dx6
      REAL*8 amp1_1, amp2_1, amp3_1, amp4_1, damp45, amp6_1, damperf_1
      REAL*8 amp1_2, amp2_2, amp3_2, amp4_2, amp6_2, damperf_2
      REAL*8 amp1_3, amp2_3, amp3_3, amp4_3, amp6_3, damperf_3
      REAL*8 sigma1_1, sigma1_2, sigma1_3
      REAL*8 sigma3_1, sigma3_2, sigma3_3
      REAL*8 sigma4_1, sigma4_2, sigma4_3
      REAL*8 dsigma21, dsigma54, dsigma61
      REAL*8 a_1, b_1, c_1, a_2, b_2, c_2, a_3, b_3, c_3
      CHARACTER*1 lr
      COMMON /func_exp/ lr
      ! To plot the different components
      LOGICAL plot
      COMMON /func_plot/ plot

      x1_1       = val(1)
      dx2        = val(2)
      dx3        = val(3)
      dx4        = val(4)
      dx5        = val(5)
      dx6        = val(6)
      amp1_1     = val(7)
      amp2_1     = val(8)
      amp3_1     = val(9)
      amp4_1     = val(10)
      damp45     = val(11)
      amp6_1     = val(12)
      sigma1_1   = val(13)
      dsigma21   = val(14)
      sigma3_1   = val(15)
      sigma4_1   = val(16)
      dsigma54   = val(17)
      dsigma61   = val(18)
      damperf_1  = val(19)
      a_1        = val(20)
      b_1        = val(21)
      c_1        = val(22)
      x1_2       = val(23)
      amp1_2     = val(24)
      amp2_2     = val(25)
      amp3_2     = val(26)
      amp4_2     = val(27)
      amp6_2     = val(28)
      sigma1_2   = val(29)
      sigma3_2   = val(30)
      sigma4_2   = val(31)
      damperf_2  = val(32)
      a_2        = val(33)
      b_2        = val(34)
      c_2        = val(35)
      x1_3       = val(36)
      amp1_3     = val(37)
      amp2_3     = val(38)
      amp3_3     = val(39)
      amp4_3     = val(40)
      amp6_3     = val(41)
      sigma1_3   = val(42)
      sigma3_3   = val(43)
      sigma4_3   = val(44)
      damperf_3  = val(45)
      a_3        = val(46)
      b_3        = val(47)
      c_3        = val(48)


c     FIRST SPECTRUM

c     First (main) peak
      val1_1(1) = x1_1
      val1_1(2) = amp1_1
      val1_1(3) = sigma1_1
      val1_1(4) = damperf_1

c     Second peak
      val2_1(1) = x1_1 + dx2
      val2_1(2) = amp2_1
      val2_1(3) = sigma1_1*dsigma21
      val2_1(4) = damperf_1

c     Third peak
      val3_1(1) = x1_1 + dx3
      val3_1(2) = amp3_1
      val3_1(3) = sigma3_1
      val3_1(4) = damperf_1

c     Fourth peak
      val4_1(1) = x1_1 + dx4
      val4_1(2) = amp4_1
      val4_1(3) = sigma4_1
      val4_1(4) = damperf_1

c     Fifth peak
      val5_1(1) = x1_1 + dx5
      val5_1(2) = amp4_1*damp45
      val5_1(3) = sigma4_1*dsigma54
      val5_1(4) = damperf_1

c     Sixth peak
      val6_1(1) = x1_1 + dx6
      val6_1(2) = amp6_1
      val6_1(3) = sigma1_1*dsigma61
      val6_1(4) = damperf_1

c     SECOND SPECTRUM

c     First (main) peak
      val1_2(1) = x1_2
      val1_2(2) = amp1_2
      val1_2(3) = sigma1_2
      val1_2(4) = damperf_2

c     Second peak
      val2_2(1) = x1_2 + dx2
      val2_2(2) = amp2_2
      val2_2(3) = sigma1_2*dsigma21
      val2_2(4) = damperf_2

c     Third peak
      val3_2(1) = x1_2 + dx3
      val3_2(2) = amp3_2
      val3_2(3) = sigma3_2
      val3_2(4) = damperf_2

c     Fourth peak
      val4_2(1) = x1_2 + dx4
      val4_2(2) = amp4_2
      val4_2(3) = sigma4_2
      val4_2(4) = damperf_2

c     Fifth peak
      val5_2(1) = x1_2 + dx5
      val5_2(2) = amp4_2*damp45
      val5_2(3) = sigma4_2*dsigma54
      val5_2(4) = damperf_2

c     Sixth peak
      val6_2(1) = x1_2 + dx6
      val6_2(2) = amp6_2
      val6_2(3) = sigma1_2*dsigma61
      val6_2(4) = damperf_2

c     THIRD SPECTRUM

c     First (main) peak
      val1_3(1) = x1_3
      val1_3(2) = amp1_3
      val1_3(3) = sigma1_3
      val1_3(4) = damperf_3

c     Second peak
      val2_3(1) = x1_3 + dx2
      val2_3(2) = amp2_3
      val2_3(3) = sigma1_3*dsigma21
      val2_3(4) = damperf_3

c     Third peak
      val3_3(1) = x1_3 + dx3
      val3_3(2) = amp3_3
      val3_3(3) = sigma3_3
      val3_3(4) = damperf_3

c     Fourth peak
      val4_3(1) = x1_3 + dx4
      val4_3(2) = amp4_3
      val4_3(3) = sigma4_3
      val4_3(4) = damperf_3

c     Fifth peak
      val5_3(1) = x1_3 + dx5
      val5_3(2) = amp4_3*damp45
      val5_3(3) = sigma4_3*dsigma54
      val5_3(4) = damperf_3

c     Sixth peak
      val6_3(1) = x1_3 + dx6
      val6_3(2) = amp6_3
      val6_3(3) = sigma1_3*dsigma61
      val6_3(4) = damperf_3

c     FOR SEPARATE COMPONENTS FOR PLOTS cccccccccccccccccccccc

c     FIRST SPECTRUM

c     First (main) peak
      val1_1g(1) = x1_1
      val1_1g(2) = amp1_1
      val1_1g(3) = sigma1_1
      val1_1e(1) = x1_1
      val1_1e(2) = amp1_1*damperf_1
      val1_1e(3) = sigma1_1

c     Second peak
      val2_1g(1) = x1_1 + dx2
      val2_1g(2) = amp2_1
      val2_1g(3) = sigma1_1*dsigma21
      val2_1e(1) = x1_1 + dx2
      val2_1e(2) = amp2_1*damperf_1
      val2_1e(3) = sigma1_1*dsigma21

c     Third peak
      val3_1g(1) = x1_1 + dx3
      val3_1g(2) = amp3_1
      val3_1g(3) = sigma3_1
      val3_1e(1) = x1_1 + dx3
      val3_1e(2) = amp3_1*damperf_1
      val3_1e(3) = sigma3_1

c     Fourth peak
      val4_1g(1) = x1_1 + dx4
      val4_1g(2) = amp4_1
      val4_1g(3) = sigma4_1
      val4_1e(1) = x1_1 + dx4
      val4_1e(2) = amp4_1*damperf_1
      val4_1e(3) = sigma4_1

c     Fifth peak
      val5_1g(1) = x1_1 + dx5
      val5_1g(2) = amp4_1*damp45
      val5_1g(3) = sigma4_1*dsigma54
      val5_1e(1) = x1_1 + dx5
      val5_1e(2) = amp4_1*damp45*damperf_1
      val5_1e(3) = sigma4_1*dsigma54

c     Sixth peak
      val6_1g(1) = x1_1 + dx6
      val6_1g(2) = amp6_1
      val6_1g(3) = sigma1_1*dsigma61
      val6_1e(1) = x1_1 + dx6
      val6_1e(2) = amp6_1*damperf_1
      val6_1e(3) = sigma1_1*dsigma61

c     Polynomial background
      valp_1(1) = 0.
      valp_1(2) = a_1
      valp_1(3) = b_1
      valp_1(4) = c_1
      valp_1(5) = 0.
      valp_1(6) = 0.
      valp_1(7) = 0.
      valp_1(8) = 0.

c     SECOND SPECTRUM

c     First (main) peak
      val1_2g(1) = x1_2
      val1_2g(2) = amp1_2
      val1_2g(3) = sigma1_2
      val1_2e(1) = x1_2
      val1_2e(2) = amp1_2*damperf_2
      val1_2e(3) = sigma1_2

c     Second peak
      val2_2g(1) = x1_2 + dx2
      val2_2g(2) = amp2_2
      val2_2g(3) = sigma1_2*dsigma21
      val2_2e(1) = x1_2 + dx2
      val2_2e(2) = amp2_2*damperf_2
      val2_2e(3) = sigma1_2*dsigma21

c     Third peak
      val3_2g(1) = x1_2 + dx3
      val3_2g(2) = amp3_2
      val3_2g(3) = sigma3_2
      val3_2e(1) = x1_2 + dx3
      val3_2e(2) = amp3_2*damperf_2
      val3_2e(3) = sigma3_2

c     Fourth peak
      val4_2g(1) = x1_2 + dx4
      val4_2g(2) = amp4_2
      val4_2g(3) = sigma4_2
      val4_2e(1) = x1_2 + dx4
      val4_2e(2) = amp4_2*damperf_2
      val4_2e(3) = sigma4_2

c     Fifth peak
      val5_2g(1) = x1_2 + dx5
      val5_2g(2) = amp4_2*damp45
      val5_2g(3) = sigma4_2*dsigma54
      val5_2e(1) = x1_2 + dx5
      val5_2e(2) = amp4_2*damp45*damperf_2
      val5_2e(3) = sigma4_2*dsigma54

c     Sixth peak
      val6_2g(1) = x1_2 + dx6
      val6_2g(2) = amp6_2
      val6_2g(3) = sigma1_2*dsigma61
      val6_2e(1) = x1_2 + dx6
      val6_2e(2) = amp6_2*damperf_2
      val6_2e(3) = sigma1_2*dsigma61

c     Polynomial background
      valp_2(1) = 0.
      valp_2(2) = a_2
      valp_2(3) = b_2
      valp_2(4) = c_2
      valp_2(5) = 0.
      valp_2(6) = 0.
      valp_2(7) = 0.
      valp_2(8) = 0.

c     THIRD SPECTRUM

c     First (main) peak
      val1_3g(1) = x1_3
      val1_3g(2) = amp1_3
      val1_3g(3) = sigma1_3
      val1_3e(1) = x1_3
      val1_3e(2) = amp1_3*damperf_3
      val1_3e(3) = sigma1_3

c     Second peak
      val2_3g(1) = x1_3 + dx2
      val2_3g(2) = amp2_3
      val2_3g(3) = sigma1_3*dsigma21
      val2_3e(1) = x1_3 + dx2
      val2_3e(2) = amp2_3*damperf_3
      val2_3e(3) = sigma1_3*dsigma21

c     Third peak
      val3_3g(1) = x1_3 + dx3
      val3_3g(2) = amp3_3
      val3_3g(3) = sigma3_3
      val3_3e(1) = x1_3 + dx3
      val3_3e(2) = amp3_3*damperf_3
      val3_3e(3) = sigma3_3

c     Fourth peak
      val4_3g(1) = x1_3 + dx4
      val4_3g(2) = amp4_3
      val4_3g(3) = sigma4_3
      val4_3e(1) = x1_3 + dx4
      val4_3e(2) = amp4_3*damperf_3
      val4_3e(3) = sigma4_3

c     Fifth peak
      val5_3g(1) = x1_3 + dx5
      val5_3g(2) = amp4_3*damp45
      val5_3g(3) = sigma4_3*dsigma54
      val5_3e(1) = x1_3 + dx5
      val5_3e(2) = amp4_3*damp45*damperf_3
      val5_3e(3) = sigma4_3*dsigma54

c     Sixth peak
      val6_3g(1) = x1_3 + dx6
      val6_3g(2) = amp6_3
      val6_3g(3) = sigma1_3*dsigma61
      val6_3e(1) = x1_3 + dx6
      val6_3e(2) = amp6_3*damperf_3
      val6_3e(3) = sigma1_3*dsigma61

c     Polynomial background
      valp_3(1) = 0.
      valp_3(2) = a_3
      valp_3(3) = b_3
      valp_3(4) = c_3
      valp_3(5) = 0.
      valp_3(6) = 0.
      valp_3(7) = 0.
      valp_3(8) = 0.

      IF (j.EQ.1) THEN
         SIX_GAUSS_ERF_FREESIG_POLY_SET3 = GAUSS_ERF(x,4,val1_1) +
     +        GAUSS_ERF(x,4,val2_1) + GAUSS_ERF(x,4,val3_1) +
     +        GAUSS_ERF(x,4,val4_1) + GAUSS_ERF(x,4,val5_1) +
     +        GAUSS_ERF(x,4,val6_1) + POLY(x,8,valp_1)
      ELSEIF (j.EQ.2) THEN
         SIX_GAUSS_ERF_FREESIG_POLY_SET3 = GAUSS_ERF(x,4,val1_2) +
     +        GAUSS_ERF(x,4,val2_2) + GAUSS_ERF(x,4,val3_2) +
     +        GAUSS_ERF(x,4,val4_2) + GAUSS_ERF(x,4,val5_2) +
     +        GAUSS_ERF(x,4,val6_2) + POLY(x,8,valp_2)
      ELSEIF (j.EQ.3) THEN
         SIX_GAUSS_ERF_FREESIG_POLY_SET3 = GAUSS_ERF(x,4,val1_3) +
     +        GAUSS_ERF(x,4,val2_3) + GAUSS_ERF(x,4,val3_3) +
     +        GAUSS_ERF(x,4,val4_3) + GAUSS_ERF(x,4,val5_3) +
     +        GAUSS_ERF(x,4,val6_3) + POLY(x,8,valp_3)
      ELSE
         WRITE(*,*) 'Too many spectra! Check your input files'
      END IF

c     Save the different components
      IF(plot) THEN
         IF (j.EQ.1) THEN
           WRITE(30,*) x, GAUSS(x,4,val1_1g),
     +           GAUSS(x,3,val2_1g),  GAUSS(x,3,val3_1g),
     +           GAUSS(x,3,val4_1g),  GAUSS(x,3,val5_1g),
     +           GAUSS(x,3,val6_1g),  POLY(x,8,valp_1),
     +           ERFFCN(x,3,val1_1e), ERFFCN(x,3,val2_1e),
     +			 ERFFCN(x,3,val3_1e), ERFFCN(x,3,val4_1e),
     +			 ERFFCN(x,3,val5_1e), ERFFCN(x,3,val6_1e)
         ELSEIF (j.EQ.2) THEN
            WRITE(30,*) x, GAUSS(x,3,val1_2g),
     +           GAUSS(x,3,val2_2g),  GAUSS(x,3,val3_2g),
     +           GAUSS(x,3,val4_2g),  GAUSS(x,3,val5_2g),
     +           GAUSS(x,3,val6_2g),  POLY(x,8,valp_2),
     +           ERFFCN(x,3,val1_2e), ERFFCN(x,3,val2_2e),
     +		 	 ERFFCN(x,3,val3_2e), ERFFCN(x,3,val4_2e),
     +			 ERFFCN(x,3,val5_2e), ERFFCN(x,3,val6_2e)
         ELSEIF (j.EQ.3) THEN
            WRITE(30,*) x, GAUSS(x,3,val1_3g),
     +           GAUSS(x,3,val2_3g),  GAUSS(x,3,val3_3g),
     +           GAUSS(x,3,val4_3g),  GAUSS(x,3,val5_3g),
     +           GAUSS(x,3,val6_3g),  POLY(x,8,valp_3),
     +			 ERFFCN(x,3,val1_3e), ERFFCN(x,3,val2_3e),
     +			 ERFFCN(x,3,val3_3e), ERFFCN(x,3,val4_3e),
     +			 ERFFCN(x,3,val5_3e), ERFFCN(x,3,val6_3e)
      ELSE
         WRITE(*,*) 'Too many spectra! Check your input files'
      END IF
      ENDIF

      RETURN
      END

	  FUNCTION FOUR_GAUSS_ERF_TWO_GAUSS_SET(x,npar,val,j)
c     two doublet of lines at different relative position with polynomial background (order 4)
      IMPLICIT NONE
	  INTEGER*4 j
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 FOUR_GAUSS_ERF_TWO_GAUSS_SET, GAUSS_ERF, POLY, x
      REAL*8 GAUSS, ERFFCN
      REAL*8 valp_1(8), val1_1g(3),val2_1g(3),val3_1g(3)
      REAL*8 val4_1g(3),val5_1g(3),val6_1g(3)
      REAL*8 val1_1(4),val2_1(4),val3_1(4)
      REAL*8 val4_1(3),val5_1(3),val6_1(4)
      REAL*8 val1_1e(3),val2_1e(3),val3_1e(3)
      REAL*8 val4_1e(3),val5_1e(3),val6_1e(3)
      REAL*8 valp_2(8), val1_2g(3),val2_2g(3),val3_2g(3)
      REAL*8 val4_2g(3),val5_2g(3),val6_2g(3)
      REAL*8 val1_2(4),val2_2(4),val3_2(4)
      REAL*8 val4_2(3),val5_2(3),val6_2(4)
      REAL*8 val1_2e(3),val2_2e(3),val3_2e(3)
      REAL*8 val4_2e(3),val5_2e(3),val6_2e(3)
	  REAL*8 x1, dx21, dx31, x4, dx54, dx61
      REAL*8 amp1_1, damp21, damp31, amp4_1, damp45, damp61, damperf_1
      REAL*8 amp1_2, amp4_2, damperf_2
      REAL*8 sigma1
      REAL*8 sigma4
      REAL*8 dsigma21, dsigma31, dsigma54, dsigma61
      REAL*8 a_1
      REAL*8 a_2
      CHARACTER*1 lr
      COMMON /func_exp/ lr
      ! To plot the different components
      LOGICAL plot
      COMMON /func_plot/ plot

      x1         = val(1)
      dx21       = val(2)
      dx31       = val(3)
      x4         = val(4)
      dx54       = val(5)
      dx61       = val(6)
      amp1_1     = val(7)
      damp21     = val(8)
      damp31     = val(9)
      amp4_1     = val(10)
      damp45     = val(11)
      damp61     = val(12)
      sigma1     = val(13)
      dsigma21   = val(14)
      dsigma31   = val(15)
      sigma4     = val(16)
      dsigma54   = val(17)
      dsigma61   = val(18)
      damperf_1  = val(19)
      a_1        = val(20)
      amp1_2     = val(21)
      amp4_2     = val(22)
      damperf_2  = val(23)
      a_2        = val(24)

c     FIRST SPECTRUM

c     First (main) peak solid
      val1_1(1) = x1
      val1_1(2) = amp1_1
      val1_1(3) = sigma1
      val1_1(4) = damperf_1

c     Second peak solid
      val2_1(1) = x1 + dx21
      val2_1(2) = amp1_1*damp21
      val2_1(3) = sigma1*dsigma21
      val2_1(4) = damperf_1

c     Third peak solid
      val3_1(1) = x1 + dx31
      val3_1(2) = amp1_1*damp31
      val3_1(3) = sigma1*dsigma31
      val3_1(4) = damperf_1

c     Fourth peak gas
      val4_1(1) = x4
      val4_1(2) = amp4_1
      val4_1(3) = sigma4

c     Fifth peak gas
      val5_1(1) = x4 + dx54
      val5_1(2) = amp4_1*damp45
      val5_1(3) = sigma4*dsigma54

c     Sixth peak solid
      val6_1(1) = x1 + dx61
      val6_1(2) = amp1_1*damp61
      val6_1(3) = sigma1*dsigma61
      val6_1(4) = damperf_1

c     SECOND SPECTRUM

c     First (main) peak solid
      val1_2(1) = x1
      val1_2(2) = amp1_2
      val1_2(3) = sigma1
      val1_2(4) = damperf_2

c     Second peak solid
      val2_2(1) = x1 + dx21
      val2_2(2) = amp1_2*damp21
      val2_2(3) = sigma1*dsigma21
      val2_2(4) = damperf_2

c     Third peak solid
      val3_2(1) = x1 + dx31
      val3_2(2) = amp1_2*damp31
      val3_2(3) = sigma1*dsigma31
      val3_2(4) = damperf_2

c     Fourth peak gas
      val4_2(1) = x4
      val4_2(2) = amp4_2
      val4_2(3) = sigma4

c     Fifth peak gas
      val5_2(1) = x4 + dx54
      val5_2(2) = amp4_2*damp45
      val5_2(3) = sigma4*dsigma54

c     Sixth peak solid
      val6_2(1) = x1 + dx61
      val6_2(2) = amp1_2*damp61
      val6_2(3) = sigma1*dsigma61
      val6_2(4) = damperf_2

c     FOR SEPARATE COMPONENTS FOR PLOTS cccccccccccccccccccccc

c     FIRST SPECTRUM

c     First (main) peak solid
      val1_1g(1) = x1
      val1_1g(2) = amp1_1
      val1_1g(3) = sigma1
      val1_1e(1) = x1
      val1_1e(2) = amp1_1*damperf_1
      val1_1e(3) = sigma1

c     Second peak solid
      val2_1g(1) = x1 + dx21
      val2_1g(2) = amp1_1*damp21
      val2_1g(3) = sigma1*dsigma21
      val2_1e(1) = x1 + dx21
      val2_1e(2) = amp1_1*damp21*damperf_1
      val2_1e(3) = sigma1*dsigma21

c     Third peak solid
      val3_1g(1) = x1 + dx31
      val3_1g(2) = amp1_1*damp31
      val3_1g(3) = sigma1*dsigma31
      val3_1e(1) = x1 + dx31
      val3_1e(2) = amp1_1*damp31*damperf_1
      val3_1e(3) = sigma1*dsigma31

c     Fourth peak gas
      val4_1g(1) = x4
      val4_1g(2) = amp4_1
      val4_1g(3) = sigma4

c     Fifth peak gas
      val5_1g(1) = x4 + dx54
      val5_1g(2) = amp4_1*damp45
      val5_1g(3) = sigma4*dsigma54

c     Sixth peak solid
      val6_1g(1) = x1 + dx61
      val6_1g(2) = amp1_1*damp61
      val6_1g(3) = sigma1*dsigma61
      val6_1e(1) = x1 + dx61
      val6_1e(2) = amp1_1*damp61*damperf_1
      val6_1e(3) = sigma1*dsigma61

c     Polynomial background
      valp_1(1) = 0.
      valp_1(2) = a_1
      valp_1(3) = 0.
      valp_1(4) = 0.
      valp_1(5) = 0.
      valp_1(6) = 0.
      valp_1(7) = 0.
      valp_1(8) = 0.

c     SECOND SPECTRUM

c     First (main) peak solid
      val1_2g(1) = x1
      val1_2g(2) = amp1_2
      val1_2g(3) = sigma1
      val1_2e(1) = x1
      val1_2e(2) = amp1_2*damperf_2
      val1_2e(3) = sigma1

c     Second peak solid
      val2_2g(1) = x1 + dx21
      val2_2g(2) = amp1_2*damp21
      val2_2g(3) = sigma1*dsigma21
      val2_2e(1) = x1 + dx21
      val2_2e(2) = amp1_2*damp21*damperf_2
      val2_2e(3) = sigma1*dsigma21

c     Third peak solid
      val3_2g(1) = x1 + dx31
      val3_2g(2) = amp1_2*damp31
      val3_2g(3) = sigma1*dsigma31
      val3_2e(1) = x1 + dx31
      val3_2e(2) = amp1_2*damp31*damperf_2
      val3_2e(3) = sigma1*dsigma31

c     Fourth peak gas
      val4_2g(1) = x4
      val4_2g(2) = amp4_2
      val4_2g(3) = sigma4

c     Fifth peak gas
      val5_2g(1) = x4 + dx54
      val5_2g(2) = amp4_2*damp45
      val5_2g(3) = sigma4*dsigma54

c     Sixth peak solid
      val6_2g(1) = x1 + dx61
      val6_2g(2) = amp1_2*damp61
      val6_2g(3) = sigma1*dsigma61
      val6_2e(1) = x1 + dx61
      val6_2e(2) = amp1_2*damp61*damperf_2
      val6_2e(3) = sigma1*dsigma61

c     Polynomial background
      valp_2(1) = 0.
      valp_2(2) = a_2
      valp_2(3) = 0.
      valp_2(4) = 0.
      valp_2(5) = 0.
      valp_2(6) = 0.
      valp_2(7) = 0.
      valp_2(8) = 0.

      IF (j.EQ.1) THEN
			FOUR_GAUSS_ERF_TWO_GAUSS_SET = GAUSS_ERF(x,4,val1_1) +
     +      GAUSS_ERF(x,4,val2_1) + GAUSS_ERF(x,4,val3_1) +
     +      GAUSS(x,3,val4_1) + GAUSS(x,3,val5_1) +
     +      GAUSS_ERF(x,4,val6_1) + POLY(x,8,valp_1)
      ELSEIF (j.EQ.2) THEN
			FOUR_GAUSS_ERF_TWO_GAUSS_SET = GAUSS_ERF(x,4,val1_2) +
     +      GAUSS_ERF(x,4,val2_2) + GAUSS_ERF(x,4,val3_2) +
     +      GAUSS(x,3,val4_2) + GAUSS(x,3,val5_2) +
     +      GAUSS_ERF(x,4,val6_2) + POLY(x,8,valp_2)

	  ELSE
         WRITE(*,*) 'Too many spectra !! Check your input files'
         WRITE(*,*) j
      END IF

c     Save the different components
      IF(plot) THEN
           IF (j.EQ.1) THEN
			WRITE(30,*) x, GAUSS(x,4,val1_1g),
     +           GAUSS(x,3,val2_1g),  GAUSS(x,3,val3_1g),
     +           GAUSS(x,3,val4_1g),  GAUSS(x,3,val5_1g),
     +           GAUSS(x,3,val6_1g),  POLY(x,8,valp_1),
     +           ERFFCN(x,3,val1_1e), ERFFCN(x,3,val2_1e),
     +			 ERFFCN(x,3,val3_1e), ERFFCN(x,3,val6_1e)
	       ELSEIF (j.EQ.2) THEN
			WRITE(30,*) x, GAUSS(x,4,val1_2g),
     +           GAUSS(x,3,val2_2g),  GAUSS(x,3,val3_2g),
     +           GAUSS(x,3,val4_2g),  GAUSS(x,3,val5_2g),
     +           GAUSS(x,3,val6_2g),  POLY(x,8,valp_2),
     +           ERFFCN(x,3,val1_2e), ERFFCN(x,3,val2_2e),
     +			 ERFFCN(x,3,val3_2e), ERFFCN(x,3,val6_2e)
		   ELSE
			WRITE(*,*) 'Too many spectra! Check your input files'
		   END IF
      ENDIF

	  RETURN
	  END


  	  FUNCTION FOUR_GAUSS_ERF_TWO_GAUSS_STAN_SET(x,npar,val,j)
c     two doublet of lines at different relative position with polynomial background (order 4)
      IMPLICIT NONE
	  INTEGER*4 j
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 FOUR_GAUSS_ERF_TWO_GAUSS_STAN_SET, GAUSS_ERF, POLY, x
      REAL*8 GAUSS, ERFFCN
      REAL*8 valp_1(8), val1_1g(3),val2_1g(3),val3_1g(3)
      REAL*8 val4_1g(3),val5_1g(3),val6_1g(3)
      REAL*8 val1_1(4),val2_1(4),val3_1(4)
      REAL*8 val4_1(3),val5_1(4),val6_1(4)
      REAL*8 val1_1e(3),val2_1e(3),val3_1e(3)
      REAL*8 val4_1e(3),val5_1e(3),val6_1e(3)
      REAL*8 valp_2(8), val1_2g(3),val2_2g(3),val3_2g(3)
      REAL*8 val4_2g(3),val5_2g(3),val6_2g(3)
      REAL*8 val1_2(4),val2_2(4),val3_2(4)
      REAL*8 val4_2(3),val5_2(4),val6_2(4)
      REAL*8 val1_2e(3),val2_2e(3),val3_2e(3)
      REAL*8 val4_2e(3),val5_2e(3),val6_2e(3)
	  REAL*8 x1, dx21, dx31, x4, dx51, dx61
      REAL*8 amp1_1, damp21, damp31, amp4_1, damp51, damp61, damperf_1
      REAL*8 amp1_2, amp4_2, damperf_2
      REAL*8 sigma1
      REAL*8 sigma4
      REAL*8 dsigma21, dsigma31, dsigma51, dsigma61
      REAL*8 a_1
      REAL*8 a_2
      CHARACTER*1 lr
      COMMON /func_exp/ lr
      ! To plot the different components
      LOGICAL plot
      COMMON /func_plot/ plot

      x1         = val(1)
      dx21       = val(2)
      dx31       = val(3)
      x4         = val(4)
      dx51       = val(5)
      dx61       = val(6)
      amp1_1     = val(7)
      damp21     = val(8)
      damp31     = val(9)
      amp4_1     = val(10)
      damp51     = val(11)
      damp61     = val(12)
      sigma1     = val(13)
      dsigma21   = val(14)
      dsigma31   = val(15)
      sigma4     = val(16)
      dsigma51   = val(17)
      dsigma61   = val(18)
      damperf_1  = val(19)
      a_1        = val(20)
      amp1_2     = val(21)
      amp4_2     = val(22)
      damperf_2  = val(23)
      a_2        = val(24)

c     FIRST SPECTRUM

c     First (main) peak solid
      val1_1(1) = x1
      val1_1(2) = amp1_1
      val1_1(3) = sigma1
      val1_1(4) = damperf_1

c     Second peak solid
      val2_1(1) = x1 + dx21
      val2_1(2) = amp1_1*damp21
      val2_1(3) = sigma1*dsigma21
      val2_1(4) = damperf_1

c     Third peak solid
      val3_1(1) = x1 + dx31
      val3_1(2) = amp1_1*damp31
      val3_1(3) = sigma1*dsigma31
      val3_1(4) = damperf_1

c     Fourth peak gas
      val4_1(1) = x4
      val4_1(2) = amp4_1
      val4_1(3) = sigma4

c     Fifth peak gas
      val5_1(1) = x1 + dx51
      val5_1(2) = amp1_1*damp51
      val5_1(3) = sigma1*dsigma51
      val5_1(4) = damperf_1

c     Sixth peak solid
      val6_1(1) = x1 + dx61
      val6_1(2) = amp1_1*damp61
      val6_1(3) = sigma1*dsigma61
      val6_1(4) = damperf_1

c     SECOND SPECTRUM

c     First (main) peak solid
      val1_2(1) = x1
      val1_2(2) = amp1_2
      val1_2(3) = sigma1
      val1_2(4) = damperf_2

c     Second peak solid
      val2_2(1) = x1 + dx21
      val2_2(2) = amp1_2*damp21
      val2_2(3) = sigma1*dsigma21
      val2_2(4) = damperf_2

c     Third peak solid
      val3_2(1) = x1 + dx31
      val3_2(2) = amp1_2*damp31
      val3_2(3) = sigma1*dsigma31
      val3_2(4) = damperf_2

c     Fourth peak gas
      val4_2(1) = x4
      val4_2(2) = amp4_2
      val4_2(3) = sigma4

c     Fifth peak gas
      val5_2(1) = x1 + dx51
      val5_2(2) = amp1_2*damp51
      val5_2(3) = sigma1*dsigma51
      val5_2(4) = damperf_2

c     Sixth peak solid
      val6_2(1) = x1 + dx61
      val6_2(2) = amp1_2*damp61
      val6_2(3) = sigma1*dsigma61
      val6_2(4) = damperf_2

c     FOR SEPARATE COMPONENTS FOR PLOTS cccccccccccccccccccccc

c     FIRST SPECTRUM

c     First (main) peak solid
      val1_1g(1) = x1
      val1_1g(2) = amp1_1
      val1_1g(3) = sigma1
      val1_1e(1) = x1
      val1_1e(2) = amp1_1*damperf_1
      val1_1e(3) = sigma1

c     Second peak solid
      val2_1g(1) = x1 + dx21
      val2_1g(2) = amp1_1*damp21
      val2_1g(3) = sigma1*dsigma21
      val2_1e(1) = x1 + dx21
      val2_1e(2) = amp1_1*damp21*damperf_1
      val2_1e(3) = sigma1*dsigma21

c     Third peak solid
      val3_1g(1) = x1 + dx31
      val3_1g(2) = amp1_1*damp31
      val3_1g(3) = sigma1*dsigma31
      val3_1e(1) = x1 + dx31
      val3_1e(2) = amp1_1*damp31*damperf_1
      val3_1e(3) = sigma1*dsigma31

c     Fourth peak gas
      val4_1g(1) = x4
      val4_1g(2) = amp4_1
      val4_1g(3) = sigma4

c     Fifth peak gas
      val5_1g(1) = x1 + dx51
      val5_1g(2) = amp1_1*damp51
      val5_1g(3) = sigma1*dsigma51
      val5_1e(1) = x1 + dx51
      val5_1e(2) = amp1_1*damp51*damperf_1
      val5_1e(3) = sigma1*dsigma51

c     Sixth peak solid
      val6_1g(1) = x1 + dx61
      val6_1g(2) = amp1_1*damp61
      val6_1g(3) = sigma1*dsigma61
      val6_1e(1) = x1 + dx61
      val6_1e(2) = amp1_1*damp61*damperf_1
      val6_1e(3) = sigma1*dsigma61

c     Polynomial background
      valp_1(1) = 0.
      valp_1(2) = a_1
      valp_1(3) = 0.
      valp_1(4) = 0.
      valp_1(5) = 0.
      valp_1(6) = 0.
      valp_1(7) = 0.
      valp_1(8) = 0.

c     SECOND SPECTRUM

c     First (main) peak solid
      val1_2g(1) = x1
      val1_2g(2) = amp1_2
      val1_2g(3) = sigma1
      val1_2e(1) = x1
      val1_2e(2) = amp1_2*damperf_2
      val1_2e(3) = sigma1

c     Second peak solid
      val2_2g(1) = x1 + dx21
      val2_2g(2) = amp1_2*damp21
      val2_2g(3) = sigma1*dsigma21
      val2_2e(1) = x1 + dx21
      val2_2e(2) = amp1_2*damp21*damperf_2
      val2_2e(3) = sigma1*dsigma21

c     Third peak solid
      val3_2g(1) = x1 + dx31
      val3_2g(2) = amp1_2*damp31
      val3_2g(3) = sigma1*dsigma31
      val3_2e(1) = x1 + dx31
      val3_2e(2) = amp1_2*damp31*damperf_2
      val3_2e(3) = sigma1*dsigma31

c     Fourth peak gas
      val4_2g(1) = x4
      val4_2g(2) = amp4_2
      val4_2g(3) = sigma4

c     Fifth peak gas
      val5_2g(1) = x1 + dx51
      val5_2g(2) = amp1_2*damp51
      val5_2g(3) = sigma1*dsigma51
      val5_2e(1) = x1 + dx51
      val5_2e(2) = amp1_2*damp51*damperf_2
      val5_2e(3) = sigma1*dsigma51

c     Sixth peak solid
      val6_2g(1) = x1 + dx61
      val6_2g(2) = amp1_2*damp61
      val6_2g(3) = sigma1*dsigma61
      val6_2e(1) = x1 + dx61
      val6_2e(2) = amp1_2*damp61*damperf_2
      val6_2e(3) = sigma1*dsigma61

c     Polynomial background
      valp_2(1) = 0.
      valp_2(2) = a_2
      valp_2(3) = 0.
      valp_2(4) = 0.
      valp_2(5) = 0.
      valp_2(6) = 0.
      valp_2(7) = 0.
      valp_2(8) = 0.

      IF (j.EQ.1) THEN
			FOUR_GAUSS_ERF_TWO_GAUSS_STAN_SET = GAUSS_ERF(x,4,val1_1) +
     +      GAUSS_ERF(x,4,val2_1) + GAUSS_ERF(x,4,val3_1) +
     +      GAUSS(x,3,val4_1) + GAUSS_ERF(x,3,val5_1) +
     +      GAUSS_ERF(x,4,val6_1) + POLY(x,8,valp_1)
      ELSEIF (j.EQ.2) THEN
			FOUR_GAUSS_ERF_TWO_GAUSS_STAN_SET = GAUSS_ERF(x,4,val1_2) +
     +      GAUSS_ERF(x,4,val2_2) + GAUSS_ERF(x,4,val3_2) +
     +      GAUSS(x,3,val4_2) + GAUSS_ERF(x,3,val5_2) +
     +      GAUSS_ERF(x,4,val6_2) + POLY(x,8,valp_2)

	  ELSE
         WRITE(*,*) 'Too many spectra !! Check your input files'
         WRITE(*,*) j
      END IF

c     Save the different components
      IF(plot) THEN
           IF (j.EQ.1) THEN
			WRITE(30,*) x, GAUSS(x,4,val1_1g),
     +           GAUSS(x,3,val2_1g),  GAUSS(x,3,val3_1g),
     +           GAUSS(x,3,val4_1g),  GAUSS(x,3,val5_1g),
     +           GAUSS(x,3,val6_1g),  POLY(x,8,valp_1),
     +           ERFFCN(x,3,val1_1e), ERFFCN(x,3,val2_1e),
     +			 ERFFCN(x,3,val3_1e), ERFFCN(x,3,val5_1e), ERFFCN(x,3,val6_1e)
	       ELSEIF (j.EQ.2) THEN
			WRITE(30,*) x, GAUSS(x,4,val1_2g),
     +           GAUSS(x,3,val2_2g),  GAUSS(x,3,val3_2g),
     +           GAUSS(x,3,val4_2g),  GAUSS(x,3,val5_2g),
     +           GAUSS(x,3,val6_2g),  POLY(x,8,valp_2),
     +           ERFFCN(x,3,val1_2e), ERFFCN(x,3,val2_2e),
     +			 ERFFCN(x,3,val3_2e), ERFFCN(x,3,val5_2e), ERFFCN(x,3,val6_2e)
		   ELSE
			WRITE(*,*) 'Too many spectra! Check your input files'
		   END IF
      ENDIF

	  RETURN
	  END


      FUNCTION FOUR_GAUSS_ERF_TWO_GAUSS_STAN2_SET(x,npar,val,j)
c     two doublet of lines at different relative position with polynomial background (order 4)
      IMPLICIT NONE
	  INTEGER*4 j
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 FOUR_GAUSS_ERF_TWO_GAUSS_STAN2_SET, GAUSS_ERF, POLY, x
      REAL*8 GAUSS, ERFFCN
      REAL*8 valp_1(8), val1_1g(3),val2_1g(3),val3_1g(3)
      REAL*8 val4_1g(3),val5_1g(3),val6_1g(3),val7_1g(3)
      REAL*8 val1_1(4),val2_1(4),val3_1(4)
      REAL*8 val4_1(3),val5_1(4),val6_1(4), val7_1(4)
      REAL*8 val1_1e(3),val2_1e(3),val3_1e(3)
      REAL*8 val4_1e(3),val5_1e(3),val6_1e(3),val7_1e(3)
      REAL*8 valp_2(8), val1_2g(3),val2_2g(3),val3_2g(3)
      REAL*8 val4_2g(3),val5_2g(3),val6_2g(3),val7_2g(3)
      REAL*8 val1_2(4),val2_2(4),val3_2(4)
      REAL*8 val4_2(3),val5_2(4),val6_2(4),val7_2(4)
      REAL*8 val1_2e(3),val2_2e(3),val3_2e(3)
      REAL*8 val4_2e(3),val5_2e(3),val6_2e(3),val7_2e(3)
	  REAL*8 x1, dx21, dx31, x4, dx51, dx61, dx71
      REAL*8 amp1_1, damp21, damp31, amp4_1, damp51, damp61, damp71
	  REAL*8 damperf_1
      REAL*8 amp1_2, amp4_2, damperf_2
      REAL*8 sigma1
      REAL*8 sigma4, sigma7
      REAL*8 dsigma21, dsigma31, dsigma51, dsigma61
      REAL*8 a_1
      REAL*8 a_2
      CHARACTER*1 lr
      COMMON /func_exp/ lr
      ! To plot the different components
      LOGICAL plot
      COMMON /func_plot/ plot

      x1         = val(1)
      dx21       = val(2)
      dx31       = val(3)
      x4         = val(4)
      dx51       = val(5)
      dx61       = val(6)
      dx71       = val(7)
      amp1_1     = val(8)
      damp21     = val(9)
      damp31     = val(10)
      amp4_1     = val(11)
      damp51     = val(12)
      damp61     = val(13)
      damp71     = val(14)
      sigma1     = val(15)
      dsigma21   = val(16)
      dsigma31   = val(17)
      sigma4     = val(18)
      dsigma51   = val(19)
      dsigma61   = val(20)
      sigma7     = val(21)
      damperf_1  = val(22)
      a_1        = val(23)
      amp1_2     = val(24)
      amp4_2     = val(25)
      damperf_2  = val(26)
      a_2        = val(27)

c     FIRST SPECTRUM

c     First (main) peak solid
      val1_1(1) = x1
      val1_1(2) = amp1_1
      val1_1(3) = sigma1
      val1_1(4) = damperf_1

c     Second peak solid
      val2_1(1) = x1 + dx21
      val2_1(2) = amp1_1*damp21
      val2_1(3) = sigma1*dsigma21
      val2_1(4) = damperf_1

c     Third peak solid
      val3_1(1) = x1 + dx31
      val3_1(2) = amp1_1*damp31
      val3_1(3) = sigma1*dsigma31
      val3_1(4) = damperf_1

c     Fourth peak gas
      val4_1(1) = x4
      val4_1(2) = amp4_1
      val4_1(3) = sigma4

c     Fifth peak gas
      val5_1(1) = x1 + dx51
      val5_1(2) = amp1_1*damp51
      val5_1(3) = sigma1*dsigma51
      val5_1(4) = damperf_1

c     Sixth peak solid
      val6_1(1) = x1 + dx61
      val6_1(2) = amp1_1*damp61
      val6_1(3) = sigma1*dsigma61
      val6_1(4) = damperf_1

c     Seventh peak solid
      val7_1(1) = x1 + dx71
      val7_1(2) = amp1_1*damp71
      val7_1(3) = sigma7
      val7_1(4) = damperf_1

c     SECOND SPECTRUM

c     First (main) peak solid
      val1_2(1) = x1
      val1_2(2) = amp1_2
      val1_2(3) = sigma1
      val1_2(4) = damperf_2

c     Second peak solid
      val2_2(1) = x1 + dx21
      val2_2(2) = amp1_2*damp21
      val2_2(3) = sigma1*dsigma21
      val2_2(4) = damperf_2

c     Third peak solid
      val3_2(1) = x1 + dx31
      val3_2(2) = amp1_2*damp31
      val3_2(3) = sigma1*dsigma31
      val3_2(4) = damperf_2

c     Fourth peak gas
      val4_2(1) = x4
      val4_2(2) = amp4_2
      val4_2(3) = sigma4

c     Fifth peak gas
      val5_2(1) = x1 + dx51
      val5_2(2) = amp1_2*damp51
      val5_2(3) = sigma1*dsigma51
      val5_2(4) = damperf_2

c     Sixth peak solid
      val6_2(1) = x1 + dx61
      val6_2(2) = amp1_2*damp61
      val6_2(3) = sigma1*dsigma61
      val6_2(4) = damperf_2

c     Seventh peak solid
      val7_2(1) = x1 + dx71
      val7_2(2) = amp1_2*damp71
      val7_2(3) = sigma7
      val7_2(4) = damperf_2

c     FOR SEPARATE COMPONENTS FOR PLOTS cccccccccccccccccccccc

c     FIRST SPECTRUM

c     First (main) peak solid
      val1_1g(1) = x1
      val1_1g(2) = amp1_1
      val1_1g(3) = sigma1
      val1_1e(1) = x1
      val1_1e(2) = amp1_1*damperf_1
      val1_1e(3) = sigma1

c     Second peak solid
      val2_1g(1) = x1 + dx21
      val2_1g(2) = amp1_1*damp21
      val2_1g(3) = sigma1*dsigma21
      val2_1e(1) = x1 + dx21
      val2_1e(2) = amp1_1*damp21*damperf_1
      val2_1e(3) = sigma1*dsigma21

c     Third peak solid
      val3_1g(1) = x1 + dx31
      val3_1g(2) = amp1_1*damp31
      val3_1g(3) = sigma1*dsigma31
      val3_1e(1) = x1 + dx31
      val3_1e(2) = amp1_1*damp31*damperf_1
      val3_1e(3) = sigma1*dsigma31

c     Fourth peak gas
      val4_1g(1) = x4
      val4_1g(2) = amp4_1
      val4_1g(3) = sigma4

c     Fifth peak gas
      val5_1g(1) = x1 + dx51
      val5_1g(2) = amp1_1*damp51
      val5_1g(3) = sigma1*dsigma51
      val5_1e(1) = x1 + dx51
      val5_1e(2) = amp1_1*damp51*damperf_1
      val5_1e(3) = sigma1*dsigma51

c     Sixth peak solid
      val6_1g(1) = x1 + dx61
      val6_1g(2) = amp1_1*damp61
      val6_1g(3) = sigma1*dsigma61
      val6_1e(1) = x1 + dx61
      val6_1e(2) = amp1_1*damp61*damperf_1
      val6_1e(3) = sigma1*dsigma61

c     Seventh peak solid
      val7_1g(1) = x1 + dx71
      val7_1g(2) = amp1_1*damp71
      val7_1g(3) = sigma7
      val7_1e(1) = x1 + dx71
      val7_1e(2) = amp1_1*damp71*damperf_1
      val7_1e(3) = sigma7

c     Polynomial background
      valp_1(1) = 0.
      valp_1(2) = a_1
      valp_1(3) = 0.
      valp_1(4) = 0.
      valp_1(5) = 0.
      valp_1(6) = 0.
      valp_1(7) = 0.
      valp_1(8) = 0.

c     SECOND SPECTRUM

c     First (main) peak solid
      val1_2g(1) = x1
      val1_2g(2) = amp1_2
      val1_2g(3) = sigma1
      val1_2e(1) = x1
      val1_2e(2) = amp1_2*damperf_2
      val1_2e(3) = sigma1

c     Second peak solid
      val2_2g(1) = x1 + dx21
      val2_2g(2) = amp1_2*damp21
      val2_2g(3) = sigma1*dsigma21
      val2_2e(1) = x1 + dx21
      val2_2e(2) = amp1_2*damp21*damperf_2
      val2_2e(3) = sigma1*dsigma21

c     Third peak solid
      val3_2g(1) = x1 + dx31
      val3_2g(2) = amp1_2*damp31
      val3_2g(3) = sigma1*dsigma31
      val3_2e(1) = x1 + dx31
      val3_2e(2) = amp1_2*damp31*damperf_2
      val3_2e(3) = sigma1*dsigma31

c     Fourth peak gas
      val4_2g(1) = x4
      val4_2g(2) = amp4_2
      val4_2g(3) = sigma4

c     Fifth peak gas
      val5_2g(1) = x1 + dx51
      val5_2g(2) = amp1_2*damp51
      val5_2g(3) = sigma1*dsigma51
      val5_2e(1) = x1 + dx51
      val5_2e(2) = amp1_2*damp51*damperf_2
      val5_2e(3) = sigma1*dsigma51

c     Sixth peak solid
      val6_2g(1) = x1 + dx61
      val6_2g(2) = amp1_2*damp61
      val6_2g(3) = sigma1*dsigma61
      val6_2e(1) = x1 + dx61
      val6_2e(2) = amp1_2*damp61*damperf_2
      val6_2e(3) = sigma1*dsigma61

c     Seventh peak solid
      val7_2g(1) = x1 + dx71
      val7_2g(2) = amp1_2*damp71
      val7_2g(3) = sigma7
      val7_2e(1) = x1 + dx71
      val7_2e(2) = amp1_2*damp71*damperf_2
      val7_2e(3) = sigma7

c     Polynomial background
      valp_2(1) = 0.
      valp_2(2) = a_2
      valp_2(3) = 0.
      valp_2(4) = 0.
      valp_2(5) = 0.
      valp_2(6) = 0.
      valp_2(7) = 0.
      valp_2(8) = 0.

      IF (j.EQ.1) THEN
			FOUR_GAUSS_ERF_TWO_GAUSS_STAN2_SET = GAUSS_ERF(x,4,val1_1) +
     +      GAUSS_ERF(x,4,val2_1) + GAUSS_ERF(x,4,val3_1) +
     +      GAUSS(x,3,val4_1) + GAUSS_ERF(x,4,val5_1) +
     +      GAUSS_ERF(x,4,val6_1) + GAUSS_ERF(x,4,val7_1) +
     +      POLY(x,8,valp_1)
      ELSEIF (j.EQ.2) THEN
			FOUR_GAUSS_ERF_TWO_GAUSS_STAN2_SET = GAUSS_ERF(x,4,val1_2) +
     +      GAUSS_ERF(x,4,val2_2) + GAUSS_ERF(x,4,val3_2) +
     +      GAUSS(x,3,val4_2) + GAUSS_ERF(x,4,val5_2) +
     +      GAUSS_ERF(x,4,val6_2) + GAUSS_ERF(x,4,val7_2) +
     +      POLY(x,8,valp_2)
      ELSE
         WRITE(*,*) 'Too many spectra !! Check your input files'
         WRITE(*,*) j
      END IF

c     Save the different components
      IF(plot) THEN
         IF (j.EQ.1) THEN
            WRITE(30,*) x, GAUSS(x,4,val1_1g),
     +           GAUSS(x,3,val2_1g),  GAUSS(x,3,val3_1g),
     +           GAUSS(x,3,val4_1g),  GAUSS(x,3,val5_1g),
     +           GAUSS(x,3,val6_1g),GAUSS(x,3,val7_1g),
     +           POLY(x,8,valp_1), ERFFCN(x,3,val1_1e),
     +           ERFFCN(x,3,val2_1e), ERFFCN(x,3,val3_1e),
     +           ERFFCN(x,3,val5_1e), ERFFCN(x,3,val6_1e),
     +           ERFFCN(x,3,val7_1e)
         ELSEIF (j.EQ.2) THEN
            WRITE(30,*) x, GAUSS(x,4,val1_2g),
     +           GAUSS(x,3,val2_2g),  GAUSS(x,3,val3_2g),
     +           GAUSS(x,3,val4_2g),  GAUSS(x,3,val5_2g),
     +           GAUSS(x,3,val6_2g),  GAUSS(x,3,val7_2g),
     +           POLY(x,8,valp_2), ERFFCN(x,3,val1_2e),
     +           ERFFCN(x,3,val2_2e), ERFFCN(x,3,val3_2e),
     +           ERFFCN(x,3,val5_2e), ERFFCN(x,3,val6_2e),
     +           ERFFCN(x,3,val7_2e)
         ELSE
            WRITE(*,*) 'Too many spectra! Check your input files'
         END IF
      ENDIF

      RETURN
      END
      FUNCTION FOUR_GAUSS_ERF_TWO_GAUSS_STAN3_SET(x,npar,val,j)
c     two doublet of lines at different relative position with polynomial background (order 4)
      IMPLICIT NONE
	  INTEGER*4 j
      INTEGER*4 npar
      REAL*8 val(npar)
      REAL*8 FOUR_GAUSS_ERF_TWO_GAUSS_STAN3_SET, GAUSS_ERF, POLY, x
      REAL*8 GAUSS, ERFFCN
      REAL*8 valp_1(8), val1_1g(3),val2_1g(3),val3_1g(3)
      REAL*8 val4_1g(3),val5_1g(3),val6_1g(3),val7_1g(3)
      REAL*8 val1_1(4),val2_1(4),val3_1(4)
      REAL*8 val4_1(3),val5_1(3),val6_1(4), val7_1(4)
      REAL*8 val1_1e(3),val2_1e(3),val3_1e(3)
      REAL*8 val6_1e(3),val7_1e(3)
      REAL*8 valp_2(8), val1_2g(3),val2_2g(3),val3_2g(3)
      REAL*8 val4_2g(3),val5_2g(3),val6_2g(3),val7_2g(3)
      REAL*8 val1_2(4),val2_2(4),val3_2(4)
      REAL*8 val4_2(3),val5_2(3),val6_2(4),val7_2(4)
      REAL*8 val1_2e(3),val2_2e(3),val3_2e(3)
      REAL*8 val6_2e(3),val7_2e(3)
	  REAL*8 x1, dx21, dx31, x4, dx54, dx61, dx71
      REAL*8 amp1_1, damp21, damp31, amp4_1, damp54, damp61, damp71
	  REAL*8 damperf_1
      REAL*8 amp1_2, amp4_2, damperf_2
      REAL*8 sigma1
      REAL*8 sigma4, sigma7
      REAL*8 dsigma21, dsigma31, dsigma54, dsigma61
      REAL*8 a_1
      REAL*8 a_2
      CHARACTER*1 lr
      COMMON /func_exp/ lr
      ! To plot the different components
      LOGICAL plot
      COMMON /func_plot/ plot

      x1         = val(1)
      dx21       = val(2)
      dx31       = val(3)
      x4         = val(4)
      dx54       = val(5)
      dx61       = val(6)
      dx71       = val(7)
      amp1_1     = val(8)
      damp21     = val(9)
      damp31     = val(10)
      amp4_1     = val(11)
      damp54    = val(12)
      damp61     = val(13)
      damp71     = val(14)
      sigma1     = val(15)
      dsigma21   = val(16)
      dsigma31   = val(17)
      sigma4     = val(18)
      dsigma54   = val(19)
      dsigma61   = val(20)
      sigma7     = val(21)
      damperf_1  = val(22)
      a_1        = val(23)
      amp1_2     = val(24)
      amp4_2     = val(25)
      damperf_2  = val(26)
      a_2        = val(27)

c     FIRST SPECTRUM

c     First (main) peak solid
      val1_1(1) = x1
      val1_1(2) = amp1_1
      val1_1(3) = sigma1
      val1_1(4) = damperf_1

c     Second peak solid
      val2_1(1) = x1 + dx21
      val2_1(2) = amp1_1*damp21
      val2_1(3) = sigma1*dsigma21
      val2_1(4) = damperf_1

c     Third peak solid
      val3_1(1) = x1 + dx31
      val3_1(2) = amp1_1*damp31
      val3_1(3) = sigma1*dsigma31
      val3_1(4) = damperf_1

c     Fourth peak gas
      val4_1(1) = x4
      val4_1(2) = amp4_1
      val4_1(3) = sigma4

c     Fifth peak gas
      val5_1(1) = x4 + dx54
      val5_1(2) = amp4_1*damp54
      val5_1(3) = sigma4*dsigma54

c     Sixth peak solid
      val6_1(1) = x1 + dx61
      val6_1(2) = amp1_1*damp61
      val6_1(3) = sigma1*dsigma61
      val6_1(4) = damperf_1

c     Seventh peak solid
      val7_1(1) = x1 + dx71
      val7_1(2) = amp1_1*damp71
      val7_1(3) = sigma7
      val7_1(4) = damperf_1

c     SECOND SPECTRUM

c     First (main) peak solid
      val1_2(1) = x1
      val1_2(2) = amp1_2
      val1_2(3) = sigma1
      val1_2(4) = damperf_2

c     Second peak solid
      val2_2(1) = x1 + dx21
      val2_2(2) = amp1_2*damp21
      val2_2(3) = sigma1*dsigma21
      val2_2(4) = damperf_2

c     Third peak solid
      val3_2(1) = x1 + dx31
      val3_2(2) = amp1_2*damp31
      val3_2(3) = sigma1*dsigma31
      val3_2(4) = damperf_2

c     Fourth peak gas
      val4_2(1) = x4
      val4_2(2) = amp4_2
      val4_2(3) = sigma4

c     Fifth peak gas
      val5_2(1) = x4 + dx54
      val5_2(2) = amp4_2*damp54
      val5_2(3) = sigma4*dsigma54

c     Sixth peak solid
      val6_2(1) = x1 + dx61
      val6_2(2) = amp1_2*damp61
      val6_2(3) = sigma1*dsigma61
      val6_2(4) = damperf_2

c     Seventh peak solid
      val7_2(1) = x1 + dx71
      val7_2(2) = amp1_2*damp71
      val7_2(3) = sigma7
      val7_2(4) = damperf_2

c     FOR SEPARATE COMPONENTS FOR PLOTS cccccccccccccccccccccc

c     FIRST SPECTRUM

c     First (main) peak solid
      val1_1g(1) = x1
      val1_1g(2) = amp1_1
      val1_1g(3) = sigma1
      val1_1e(1) = x1
      val1_1e(2) = amp1_1*damperf_1
      val1_1e(3) = sigma1

c     Second peak solid
      val2_1g(1) = x1 + dx21
      val2_1g(2) = amp1_1*damp21
      val2_1g(3) = sigma1*dsigma21
      val2_1e(1) = x1 + dx21
      val2_1e(2) = amp1_1*damp21*damperf_1
      val2_1e(3) = sigma1*dsigma21

c     Third peak solid
      val3_1g(1) = x1 + dx31
      val3_1g(2) = amp1_1*damp31
      val3_1g(3) = sigma1*dsigma31
      val3_1e(1) = x1 + dx31
      val3_1e(2) = amp1_1*damp31*damperf_1
      val3_1e(3) = sigma1*dsigma31

c     Fourth peak gas
      val4_1g(1) = x4
      val4_1g(2) = amp4_1
      val4_1g(3) = sigma4

c     Fifth peak gas
      val5_1g(1) = x4 + dx54
      val5_1g(2) = amp4_1*damp54
      val5_1g(3) = sigma4*dsigma54

c     Sixth peak solid
      val6_1g(1) = x1 + dx61
      val6_1g(2) = amp1_1*damp61
      val6_1g(3) = sigma1*dsigma61
      val6_1e(1) = x1 + dx61
      val6_1e(2) = amp1_1*damp61*damperf_1
      val6_1e(3) = sigma1*dsigma61

c     Seventh peak solid
      val7_1g(1) = x1 + dx71
      val7_1g(2) = amp1_1*damp71
      val7_1g(3) = sigma7
      val7_1e(1) = x1 + dx71
      val7_1e(2) = amp1_1*damp71*damperf_1
      val7_1e(3) = sigma7

c     Polynomial background
      valp_1(1) = 0.
      valp_1(2) = a_1
      valp_1(3) = 0.
      valp_1(4) = 0.
      valp_1(5) = 0.
      valp_1(6) = 0.
      valp_1(7) = 0.
      valp_1(8) = 0.

c     SECOND SPECTRUM

c     First (main) peak solid
      val1_2g(1) = x1
      val1_2g(2) = amp1_2
      val1_2g(3) = sigma1
      val1_2e(1) = x1
      val1_2e(2) = amp1_2*damperf_2
      val1_2e(3) = sigma1

c     Second peak solid
      val2_2g(1) = x1 + dx21
      val2_2g(2) = amp1_2*damp21
      val2_2g(3) = sigma1*dsigma21
      val2_2e(1) = x1 + dx21
      val2_2e(2) = amp1_2*damp21*damperf_2
      val2_2e(3) = sigma1*dsigma21

c     Third peak solid
      val3_2g(1) = x1 + dx31
      val3_2g(2) = amp1_2*damp31
      val3_2g(3) = sigma1*dsigma31
      val3_2e(1) = x1 + dx31
      val3_2e(2) = amp1_2*damp31*damperf_2
      val3_2e(3) = sigma1*dsigma31

c     Fourth peak gas
      val4_2g(1) = x4
      val4_2g(2) = amp4_2
      val4_2g(3) = sigma4

c     Fifth peak gas
      val5_2g(1) = x4 + dx54
      val5_2g(2) = amp4_2*damp54
      val5_2g(3) = sigma4*dsigma54

c     Sixth peak solid
      val6_2g(1) = x1 + dx61
      val6_2g(2) = amp1_2*damp61
      val6_2g(3) = sigma1*dsigma61
      val6_2e(1) = x1 + dx61
      val6_2e(2) = amp1_2*damp61*damperf_2
      val6_2e(3) = sigma1*dsigma61

c     Seventh peak solid
      val7_2g(1) = x1 + dx71
      val7_2g(2) = amp1_2*damp71
      val7_2g(3) = sigma7
      val7_2e(1) = x1 + dx71
      val7_2e(2) = amp1_2*damp71*damperf_2
      val7_2e(3) = sigma7

c     Polynomial background
      valp_2(1) = 0.
      valp_2(2) = a_2
      valp_2(3) = 0.
      valp_2(4) = 0.
      valp_2(5) = 0.
      valp_2(6) = 0.
      valp_2(7) = 0.
      valp_2(8) = 0.

      IF (j.EQ.1) THEN
			FOUR_GAUSS_ERF_TWO_GAUSS_STAN3_SET = GAUSS_ERF(x,4,val1_1) +
     +      GAUSS_ERF(x,4,val2_1) + GAUSS_ERF(x,4,val3_1) +
     +      GAUSS(x,3,val4_1) + GAUSS(x,3,val5_1) +
     +      GAUSS_ERF(x,4,val6_1) + GAUSS_ERF(x,4,val7_1) +
     +      POLY(x,8,valp_1)
      ELSEIF (j.EQ.2) THEN
			FOUR_GAUSS_ERF_TWO_GAUSS_STAN3_SET = GAUSS_ERF(x,4,val1_2) +
     +      GAUSS_ERF(x,4,val2_2) + GAUSS_ERF(x,4,val3_2) +
     +      GAUSS(x,3,val4_2) + GAUSS(x,3,val5_2) +
     +      GAUSS_ERF(x,4,val6_2) + GAUSS_ERF(x,4,val7_2) +
     +      POLY(x,8,valp_2)
      ELSE
         WRITE(*,*) 'Too many spectra !! Check your input files'
         WRITE(*,*) j
      END IF

c     Save the different components
      IF(plot) THEN
         IF (j.EQ.1) THEN
            WRITE(30,*) x, GAUSS(x,4,val1_1g),
     +           GAUSS(x,3,val2_1g),  GAUSS(x,3,val3_1g),
     +           GAUSS(x,3,val4_1g),  GAUSS(x,3,val5_1g),
     +           GAUSS(x,3,val6_1g),GAUSS(x,3,val7_1g),
     +           POLY(x,8,valp_1), ERFFCN(x,3,val1_1e),
     +           ERFFCN(x,3,val2_1e), ERFFCN(x,3,val3_1e),
     +           ERFFCN(x,3,val6_1e), ERFFCN(x,3,val7_1e)
         ELSEIF (j.EQ.2) THEN
            WRITE(30,*) x, GAUSS(x,4,val1_2g),
     +           GAUSS(x,3,val2_2g),  GAUSS(x,3,val3_2g),
     +           GAUSS(x,3,val4_2g),  GAUSS(x,3,val5_2g),
     +           GAUSS(x,3,val6_2g),  GAUSS(x,3,val7_2g),
     +           POLY(x,8,valp_2), ERFFCN(x,3,val1_2e),
     +           ERFFCN(x,3,val2_2e), ERFFCN(x,3,val3_2e),
     +           ERFFCN(x,3,val6_2e), ERFFCN(x,3,val7_2e)
         ELSE
            WRITE(*,*) 'Too many spectra! Check your input files'
         END IF
      ENDIF

      RETURN
      END


c _______________________________________________________________________________________________


      FUNCTION ROCKING_CURVE_SET(X,npar,val,j)
c     Rocking curve with s and p polarization extracted from simulations
      IMPLICIT NONE
      INTEGER*4 npar, j
      REAL*8 val(npar)
      REAL*8 ROCKING_CURVE_SET, x, y_s_par, y_p_par,  y_s_apar, y_p_apar
      REAL*8 amp_par, amp_apar, damp_sp, x0_s_par, x0_s_apar
      REAL*8 dx0_sp, bg_par, bg_apar
c     Interpolation variables
      INTEGER*4 k, nn_s_par, nn_p_par, nn_s_apar, nn_p_apar
      INTEGER*4 ier_s,ier_p, nest
      PARAMETER (nest=1000)
      REAL*8 t_s_par(nest),c_s_par(nest),t_p_par(nest),c_p_par(nest)
      REAL*8 t_s_apar(nest),c_s_apar(nest),t_p_apar(nest),c_p_apar(nest)
      COMMON /rocking_set/ t_s_par, t_p_par, t_s_apar, t_p_apar,
     +     c_s_par, c_p_par, c_s_apar, c_p_apar,k,
     +     nn_s_par, nn_p_par, nn_s_apar, nn_p_apar
      ! To plot the different components
      LOGICAL plot
      COMMON /func_plot/ plot


c     Parallel spectrum
      amp_par  = val(1)
      damp_sp  = val(3)
      x0_s_par = val(4)
      dx0_sp   = val(6)
      bg_par   = val(7)

c     Antiparallel spectrum
      amp_apar  = val(2)
      damp_sp   = val(3)
      x0_s_apar = val(5)
      dx0_sp    = val(6)
      bg_apar   = val(8)

      IF (j.EQ.1) THEN
c     Parallel component
c     s polarization contribution
         CALL SPLEV(t_s_par,nn_s_par,c_s_par,k,
     +        x-x0_s_par,y_s_par,1,1,ier_s)

c     p polarization contribution
         CALL SPLEV(t_p_par,nn_p_par,c_p_par,k,
     +        x-(x0_s_par+dx0_sp),y_p_par,1,1,ier_p)

         ROCKING_CURVE_SET =
     +        amp_par*((1-damp_sp)*y_s_par+damp_sp*y_p_par) + bg_par

      ELSEIF (j.EQ.2) THEN
c     Antiparallel component
c     s polarization contribution
         CALL SPLEV(t_s_apar,nn_s_apar,c_s_apar,k,
     +        x-x0_s_apar,y_s_apar,1,1,ier_s)

c     p polarization contribution
         CALL SPLEV(t_p_apar,nn_p_apar,c_p_apar,k,
     +        x-(x0_s_apar+dx0_sp),y_p_apar,1,1,ier_p)

         ROCKING_CURVE_SET =
     +        amp_apar*((1-damp_sp)*y_s_apar+damp_sp*y_p_apar) + bg_apar
      ENDIF

c     Save the different components
      IF(plot) THEN
         IF (j.EQ.1) THEN
            ROCKING_CURVE_SET =
     +           amp_par*((1-damp_sp)*y_s_par+damp_sp*y_p_par) + bg_par
            WRITE(30,*) x, ROCKING_CURVE_SET,
     +           amp_par*((1-damp_sp)*y_s_par),
     +           amp_par*(damp_sp*y_p_par), bg_par

         ELSEIF (j.EQ.2) THEN
            ROCKING_CURVE_SET =
     +        amp_apar*((1-damp_sp)*y_s_apar+damp_sp*y_p_apar) + bg_apar
            WRITE(30,*) x, ROCKING_CURVE_SET,
     +           amp_apar*((1-damp_sp)*y_s_apar),
     +           amp_apar*(damp_sp*y_p_apar), bg_apar
         END IF
      ENDIF

      RETURN
      END

c ##############################################################################################



      FUNCTION DCS_EIGHT_VOIGT_POLYBG_X0_SET(X,npar,val,j) 
c     8 Normalized Voigt distribution plus exponential background
c     The value of 'amp' is the value of the surface below the curve
      IMPLICIT NONE
      INTEGER*4 npar,j
      REAL*8 val(npar)
      REAL*8 valv1_1(4), valv1_2(4), valv1_3(4), valv1_4(4), valv1_5(4)  
      REAL*8 valv1_6(4), valv1_7(4), valv1_8(4), valv1_9(4)
      REAL*8 valv2_1(4), valv2_2(4), valv2_3(4), valv2_4(4), valv2_5(4)  
      REAL*8 valv2_6(4), valv2_7(4), valv2_8(4), valv2_9(4)
      REAL*8 valv3_1(4), valv3_2(4), valv3_3(4), valv3_4(4), valv3_5(4)  
      REAL*8 valv3_6(4), valv3_7(4), valv3_8(4), valv3_9(4)
      REAL*8 valv4_1(4), valv4_2(4), valv4_3(4), valv4_4(4), valv4_5(4)  
      REAL*8 valv4_6(4), valv4_7(4), valv4_8(4), valv4_9(4)
      REAL*8 valv5_1(4), valv5_2(4), valv5_3(4), valv5_4(4), valv5_5(4)  
      REAL*8 valv5_6(4), valv5_7(4), valv5_8(4), valv5_9(4)
      REAL*8 valv6_1(4), valv6_2(4), valv6_3(4), valv6_4(4), valv6_5(4)  
      REAL*8 valv6_6(4), valv6_7(4), valv6_8(4), valv6_9(4)
      REAL*8 valv7_1(4), valv7_2(4), valv7_3(4), valv7_4(4), valv7_5(4)  
      REAL*8 valv7_6(4), valv7_7(4), valv7_8(4), valv7_9(4)
      REAL*8 valv8_1(4), valv8_2(4), valv8_3(4), valv8_4(4), valv8_5(4)  
      REAL*8 valv8_6(4), valv8_7(4), valv8_8(4), valv8_9(4)
      REAL*8 DCS_EIGHT_VOIGT_POLYBG_X0_SET, VOIGT, x
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 dx1, dx2, dx4, dx5, dx6, dx7, dx8
      REAL*8 x3_1, x3_2, x3_3, x3_4, x3_5, x3_6, x3_7, x3_8, x3_9
      REAL*8 damp1, damp2, damp4, damp5, damp6, damp7, damp8
      REAL*8 amp3_1,amp3_2,amp3_3, amp3_4, amp3_5, amp3_6
      REAL*8 amp3_7, amp3_8, amp3_9
      REAL*8 sigma
      REAL*8 gamma1,gamma2,gamma3,gamma4,gamma5,gamma6,gamma7,gamma8
      REAL*8 a_1, a_2, a_3, a_4, a_5, a_6, a_7, a_8, a_9

      CHARACTER*1 lr
      COMMON /func_exp/ lr
      ! To plot the different components
      LOGICAL plot
      COMMON /func_plot/ plot

      dx1    = val(1)
      dx2    = val(2)
      x3_1   = val(3)
      x3_2   = val(4)
      x3_3   = val(5)
      x3_4   = val(6)
      x3_5   = val(7)
      x3_6   = val(8)
      x3_7   = val(9)
      x3_8   = val(10)
      x3_9   = val(11)
      dx4    = val(12)
      dx5    = val(13)
      dx6    = val(14)
      dx7    = val(15)
      dx8    = val(16)
      sigma  = val(17)
      damp1  = val(18)
      damp2  = val(19)
      damp4  = val(20)
      damp5  = val(21)
      damp6  = val(22)
      damp7  = val(23)
      damp8  = val(24)
      amp3_1 = val(25)        
      amp3_2 = val(26)
      amp3_3 = val(27)
      amp3_4 = val(28)
      amp3_5 = val(29)
      amp3_6 = val(30)
      amp3_7 = val(31)
      amp3_8 = val(32)
      amp3_9 = val(33)
      gamma1 = val(34)
      gamma2 = val(35)
      gamma3 = val(36)
      gamma4 = val(37)
      gamma5 = val(38)
      gamma6 = val(39)
      gamma7 = val(40)
      gamma8 = val(41)
      a_1    = val(42)
      a_2    = val(43)
      a_3    = val(44)
      a_4    = val(45)
      a_5    = val(46)
      a_6    = val(47)
      a_7    = val(48)
      a_8    = val(49)
      a_9    = val(50)


c     first voigt peak

      valv1_1(1) = x3_1+dx1
      valv1_1(2) = damp1*amp3_1
      valv1_1(3) = sigma
      valv1_1(4) = gamma1

      valv1_2(1) = x3_2+dx1
      valv1_2(2) = damp1*amp3_2
      valv1_2(3) = sigma
      valv1_2(4) = gamma1

      valv1_3(1) = x3_3+dx1
      valv1_3(2) = damp1*amp3_3
      valv1_3(3) = sigma
      valv1_3(4) = gamma1

      valv1_4(1) = x3_4+dx1
      valv1_4(2) = damp1*amp3_4
      valv1_4(3) = sigma
      valv1_4(4) = gamma1

      valv1_5(1) = x3_5+dx1
      valv1_5(2) = damp1*amp3_5
      valv1_5(3) = sigma
      valv1_5(4) = gamma1

      valv1_6(1) = x3_6+dx1
      valv1_6(2) = damp1*amp3_6
      valv1_6(3) = sigma
      valv1_6(4) = gamma1

      valv1_7(1) = x3_7+dx1
      valv1_7(2) = damp1*amp3_7
      valv1_7(3) = sigma
      valv1_7(4) = gamma1

      valv1_8(1) = x3_8+dx1
      valv1_8(2) = damp1*amp3_8
      valv1_8(3) = sigma
      valv1_8(4) = gamma1

      valv1_9(1) = x3_9+dx1
      valv1_9(2) = damp1*amp3_9
      valv1_9(3) = sigma
      valv1_9(4) = gamma1

c     second voigt peak

      valv2_1(1) = x3_1+dx2
      valv2_1(2) = damp2*amp3_1
      valv2_1(3) = sigma
      valv2_1(4) = gamma2

      valv2_2(1) = x3_2+dx2
      valv2_2(2) = damp2*amp3_2
      valv2_2(3) = sigma
      valv2_2(4) = gamma2

      valv2_3(1) = x3_3+dx2
      valv2_3(2) = damp2*amp3_3
      valv2_3(3) = sigma
      valv2_3(4) = gamma2

      valv2_4(1) = x3_4+dx2
      valv2_4(2) = damp2*amp3_4
      valv2_4(3) = sigma
      valv2_4(4) = gamma2

      valv2_5(1) = x3_5+dx2
      valv2_5(2) = damp2*amp3_5
      valv2_5(3) = sigma
      valv2_5(4) = gamma2

      valv2_6(1) = x3_6+dx2
      valv2_6(2) = damp2*amp3_6
      valv2_6(3) = sigma
      valv2_6(4) = gamma2

      valv2_7(1) = x3_7+dx2
      valv2_7(2) = damp2*amp3_7
      valv2_7(3) = sigma
      valv2_7(4) = gamma2

      valv2_8(1) = x3_8+dx2
      valv2_8(2) = damp2*amp3_8
      valv2_8(3) = sigma
      valv2_8(4) = gamma2

      valv2_9(1) = x3_9+dx2
      valv2_9(2) = damp2*amp3_9
      valv2_9(3) = sigma
      valv2_9(4) = gamma2


c     third voigt peak

      valv3_1(1) = x3_1
      valv3_1(2) = amp3_1
      valv3_1(3) = sigma
      valv3_1(4) = gamma3

      valv3_2(1) = x3_2
      valv3_2(2) = amp3_2
      valv3_2(3) = sigma
      valv3_2(4) = gamma3

      valv3_3(1) = x3_3
      valv3_3(2) = amp3_3
      valv3_3(3) = sigma
      valv3_3(4) = gamma3

      valv3_4(1) = x3_4
      valv3_4(2) = amp3_4
      valv3_4(3) = sigma
      valv3_4(4) = gamma3

      valv3_5(1) = x3_5
      valv3_5(2) = amp3_5
      valv3_5(3) = sigma
      valv3_5(4) = gamma3

      valv3_6(1) = x3_6
      valv3_6(2) = amp3_6
      valv3_6(3) = sigma
      valv3_6(4) = gamma3

      valv3_7(1) = x3_7
      valv3_7(2) = amp3_7
      valv3_7(3) = sigma
      valv3_7(4) = gamma3

      valv3_8(1) = x3_8
      valv3_8(2) = amp3_8
      valv3_8(3) = sigma
      valv3_8(4) = gamma3

      valv3_9(1) = x3_9
      valv3_9(2) = amp3_9
      valv3_9(3) = sigma
      valv3_9(4) = gamma3

c     fourth voigt peak

      valv4_1(1) = x3_1+dx4
      valv4_1(2) = damp4*amp3_1
      valv4_1(3) = sigma
      valv4_1(4) = gamma4

      valv4_2(1) = x3_2+dx4
      valv4_2(2) = damp4*amp3_2
      valv4_2(3) = sigma
      valv4_2(4) = gamma4

      valv4_3(1) = x3_3+dx4
      valv4_3(2) = damp4*amp3_3
      valv4_3(3) = sigma
      valv4_3(4) = gamma4

      valv4_4(1) = x3_4+dx4
      valv4_4(2) = damp4*amp3_4
      valv4_4(3) = sigma
      valv4_4(4) = gamma4

      valv4_5(1) = x3_5+dx4
      valv4_5(2) = damp4*amp3_5
      valv4_5(3) = sigma
      valv4_5(4) = gamma4

      valv4_6(1) = x3_6+dx4
      valv4_6(2) = damp4*amp3_6
      valv4_6(3) = sigma
      valv4_6(4) = gamma4

      valv4_7(1) = x3_7+dx4
      valv4_7(2) = damp4*amp3_7
      valv4_7(3) = sigma
      valv4_7(4) = gamma4

      valv4_8(1) = x3_8+dx4
      valv4_8(2) = damp4*amp3_8
      valv4_8(3) = sigma
      valv4_8(4) = gamma4

      valv4_9(1) = x3_9+dx4
      valv4_9(2) = damp4*amp3_9
      valv4_9(3) = sigma
      valv4_9(4) = gamma4




c     fifth voigt peak

      valv5_1(1) = x3_1+dx5
      valv5_1(2) = damp5*amp3_1
      valv5_1(3) = sigma
      valv5_1(4) = gamma5

      valv5_2(1) = x3_2+dx5
      valv5_2(2) = damp5*amp3_2
      valv5_2(3) = sigma
      valv5_2(4) = gamma5

      valv5_3(1) = x3_3+dx5
      valv5_3(2) = damp5*amp3_3
      valv5_3(3) = sigma
      valv5_3(4) = gamma5

      valv5_4(1) = x3_4+dx5
      valv5_4(2) = damp5*amp3_4
      valv5_4(3) = sigma
      valv5_4(4) = gamma5

      valv5_5(1) = x3_5+dx5
      valv5_5(2) = damp5*amp3_5
      valv5_5(3) = sigma
      valv5_5(4) = gamma5

      valv5_6(1) = x3_6+dx5
      valv5_6(2) = damp5*amp3_6
      valv5_6(3) = sigma
      valv5_6(4) = gamma5

      valv5_7(1) = x3_7+dx5
      valv5_7(2) = damp5*amp3_7
      valv5_7(3) = sigma
      valv5_7(4) = gamma5

      valv5_8(1) = x3_8+dx5
      valv5_8(2) = damp5*amp3_8
      valv5_8(3) = sigma
      valv5_8(4) = gamma5

      valv5_9(1) = x3_9+dx5
      valv5_9(2) = damp5*amp3_9
      valv5_9(3) = sigma
      valv5_9(4) = gamma5

c     sixth voigt peak
      valv6_1(1) = x3_1+dx6
      valv6_1(2) = damp6*amp3_1
      valv6_1(3) = sigma
      valv6_1(4) = gamma6

      valv6_2(1) = x3_2+dx6
      valv6_2(2) = damp6*amp3_2
      valv6_2(3) = sigma
      valv6_2(4) = gamma6

      valv6_3(1) = x3_3+dx6
      valv6_3(2) = damp6*amp3_3
      valv6_3(3) = sigma
      valv6_3(4) = gamma6

      valv6_4(1) = x3_4+dx6
      valv6_4(2) = damp6*amp3_4
      valv6_4(3) = sigma
      valv6_4(4) = gamma6

      valv6_5(1) = x3_5+dx6
      valv6_5(2) = damp6*amp3_5
      valv6_5(3) = sigma
      valv6_5(4) = gamma6

      valv6_6(1) = x3_6+dx6
      valv6_6(2) = damp4*amp3_6
      valv6_6(3) = sigma
      valv6_6(4) = gamma4

      valv6_7(1) = x3_7+dx6
      valv6_7(2) = damp4*amp3_7
      valv6_7(3) = sigma
      valv6_7(4) = gamma4

      valv6_8(1) = x3_8+dx6
      valv6_8(2) = damp6*amp3_8
      valv6_8(3) = sigma
      valv6_8(4) = gamma6

      valv6_9(1) = x3_9+dx6
      valv6_9(2) = damp6*amp3_9
      valv6_9(3) = sigma
      valv6_9(4) = gamma6

c     seventh voigt peak

      valv7_1(1) = x3_1+dx7
      valv7_1(2) = damp7*amp3_1
      valv7_1(3) = sigma
      valv7_1(4) = gamma7

      valv7_2(1) = x3_2+dx7
      valv7_2(2) = damp7*amp3_2
      valv7_2(3) = sigma
      valv7_2(4) = gamma7

      valv7_3(1) = x3_3+dx7
      valv7_3(2) = damp7*amp3_3
      valv7_3(3) = sigma
      valv7_3(4) = gamma7

      valv7_4(1) = x3_4+dx7
      valv7_4(2) = damp7*amp3_4
      valv7_4(3) = sigma
      valv7_4(4) = gamma7

      valv7_5(1) = x3_5+dx7
      valv7_5(2) = damp7*amp3_5
      valv7_5(3) = sigma
      valv7_5(4) = gamma7

      valv7_6(1) = x3_6+dx7
      valv7_6(2) = damp7*amp3_6
      valv7_6(3) = sigma
      valv7_6(4) = gamma7

      valv7_7(1) = x3_7+dx7
      valv7_7(2) = damp7*amp3_7
      valv7_7(3) = sigma
      valv7_7(4) = gamma7

      valv7_8(1) = x3_8+dx7
      valv7_8(2) = damp7*amp3_8
      valv7_8(3) = sigma
      valv7_8(4) = gamma7

      valv7_9(1) = x3_9+dx7
      valv7_9(2) = damp7*amp3_9
      valv7_9(3) = sigma
      valv7_9(4) = gamma7



c     eighth voigt peak
      valv8_1(1) = x3_1+dx8
      valv8_1(2) = damp8*amp3_1
      valv8_1(3) = sigma
      valv8_1(4) = gamma8

      valv8_2(1) = x3_2+dx8
      valv8_2(2) = damp8*amp3_2
      valv8_2(3) = sigma
      valv8_2(4) = gamma8

      valv8_3(1) = x3_3+dx8
      valv8_3(2) = damp8*amp3_3
      valv8_3(3) = sigma
      valv8_3(4) = gamma8

      valv8_4(1) = x3_4+dx8
      valv8_4(2) = damp8*amp3_4
      valv8_4(3) = sigma
      valv8_4(4) = gamma8

      valv8_5(1) = x3_5+dx8
      valv8_5(2) = damp8*amp3_5
      valv8_5(3) = sigma
      valv8_5(4) = gamma8

      valv8_6(1) = x3_6+dx8
      valv8_6(2) = damp8*amp3_6
      valv8_6(3) = sigma
      valv8_6(4) = gamma8

      valv8_7(1) = x3_7+dx8
      valv8_7(2) = damp8*amp3_7
      valv8_7(3) = sigma
      valv8_7(4) = gamma8

      valv8_8(1) = x3_8+dx8
      valv8_8(2) = damp8*amp3_8
      valv8_8(3) = sigma
      valv8_8(4) = gamma8

      valv7_9(1) = x3_9+dx7
      valv7_9(2) = damp7*amp3_9
      valv7_9(3) = sigma
      valv7_9(4) = gamma7


c Values for each spectrum
      IF (j.EQ.1) THEN
            DCS_EIGHT_VOIGT_POLYBG_X0_SET = VOIGT(x,4,valv1_1) 
     +     + VOIGT(x,4,valv2_1) + VOIGT(x,4,valv3_1) 
     +     + VOIGT(x,4,valv4_1) + VOIGT(x,4,valv5_1)
     +     + VOIGT(x,4,valv6_1) + VOIGT(x,4,valv7_1)
     +     + VOIGT(x,4,valv8_1) + a_1
     

      ELSEIF (j.EQ.2) THEN
            DCS_EIGHT_VOIGT_POLYBG_X0_SET = VOIGT(x,4,valv1_2) 
     +     + VOIGT(x,4,valv2_2) + VOIGT(x,4,valv3_2) 
     +     + VOIGT(x,4,valv4_2) + VOIGT(x,4,valv5_2)
     +     + VOIGT(x,4,valv6_2) + VOIGT(x,4,valv7_2)
     +     + VOIGT(x,4,valv8_2) + a_2
     

      ELSEIF (j.EQ.3) THEN
            DCS_EIGHT_VOIGT_POLYBG_X0_SET = VOIGT(x,4,valv1_3)  
     +     + VOIGT(x,4,valv2_3) + VOIGT(x,4,valv3_3) 
     +     + VOIGT(x,4,valv4_3) + VOIGT(x,4,valv5_3)
     +     + VOIGT(x,4,valv6_3) + VOIGT(x,4,valv7_3)
     +     + VOIGT(x,4,valv8_3) + a_3

      ELSEIF (j.EQ.4) THEN
            DCS_EIGHT_VOIGT_POLYBG_X0_SET = VOIGT(x,4,valv1_4) 
     +     + VOIGT(x,4,valv2_4) + VOIGT(x,4,valv3_4) 
     +     + VOIGT(x,4,valv4_4) + VOIGT(x,4,valv5_4)
     +     + VOIGT(x,4,valv6_4) + VOIGT(x,4,valv7_4)
     +     + VOIGT(x,4,valv8_4) + a_4

      ELSEIF (j.EQ.5) THEN
            DCS_EIGHT_VOIGT_POLYBG_X0_SET = VOIGT(x,4,valv1_5) 
     +     + VOIGT(x,4,valv2_5) + VOIGT(x,4,valv3_5) 
     +     + VOIGT(x,4,valv4_5) + VOIGT(x,4,valv5_5)
     +     + VOIGT(x,4,valv6_5) + VOIGT(x,4,valv7_5)
     +     + VOIGT(x,4,valv8_5) + a_5

      ELSEIF (j.EQ.6) THEN
            DCS_EIGHT_VOIGT_POLYBG_X0_SET = VOIGT(x,4,valv1_6) 
     +     + VOIGT(x,4,valv2_6) + VOIGT(x,4,valv3_6) 
     +     + VOIGT(x,4,valv4_6) + VOIGT(x,4,valv5_6)
     +     + VOIGT(x,4,valv6_6) + VOIGT(x,4,valv7_6)
     +     + VOIGT(x,4,valv8_6) + a_6

      ELSEIF (j.EQ.7) THEN
            DCS_EIGHT_VOIGT_POLYBG_X0_SET = VOIGT(x,4,valv1_7) 
     +     + VOIGT(x,4,valv2_7) + VOIGT(x,4,valv3_7) 
     +     + VOIGT(x,4,valv4_7) + VOIGT(x,4,valv5_7)
     +     + VOIGT(x,4,valv6_7) + VOIGT(x,4,valv7_7)
     +     + VOIGT(x,4,valv8_7) + a_7

      ELSEIF (j.EQ.8) THEN
            DCS_EIGHT_VOIGT_POLYBG_X0_SET = VOIGT(x,4,valv1_8) 
     +     + VOIGT(x,4,valv2_8) + VOIGT(x,4,valv3_8) 
     +     + VOIGT(x,4,valv4_8) + VOIGT(x,4,valv5_8)
     +     + VOIGT(x,4,valv6_8) + VOIGT(x,4,valv7_8) 
     +     + VOIGT(x,4,valv8_8) + a_8

      ELSEIF (j.EQ.9) THEN
            DCS_EIGHT_VOIGT_POLYBG_X0_SET = VOIGT(x,4,valv1_9) 
     +     + VOIGT(x,4,valv2_9) + VOIGT(x,4,valv3_9)
     +     +  VOIGT(x,4,valv4_9) + VOIGT(x,4,valv5_9)
     +     + VOIGT(x,4,valv6_9) + VOIGT(x,4,valv7_9)+ VOIGT(x,4,valv8_9) 
     +     + a_9

      ELSE
         WRITE(*,*) 'Too many spectra !! Check your input files, CALCUL'
         WRITE(*,*) j
      END IF




c     Save the different components
      IF(plot) THEN
         IF (j.EQ.1) THEN
            WRITE(40,*) x, DCS_EIGHT_VOIGT_POLYBG_X0_SET, 
     +        VOIGT(x,4,valv1_1), VOIGT(x,4,valv2_1),
     +        VOIGT(x,4,valv3_1), VOIGT(x,4,valv4_1),
     +        VOIGT(x,4,valv5_1), VOIGT(x,4,valv6_1),
     +        VOIGT(x,4,valv7_1), VOIGT(x,4,valv8_1), a_1
         ELSEIF (j.EQ.2) THEN
            WRITE(40,*) x, DCS_EIGHT_VOIGT_POLYBG_X0_SET, 
     +        VOIGT(x,4,valv1_2), VOIGT(x,4,valv2_2),
     +        VOIGT(x,4,valv3_2), VOIGT(x,4,valv4_2),
     +        VOIGT(x,4,valv5_2), VOIGT(x,4,valv6_2),
     +        VOIGT(x,4,valv7_2), VOIGT(x,4,valv8_2), a_2
         ELSEIF (j.EQ.3) THEN
            WRITE(40,*) x, DCS_EIGHT_VOIGT_POLYBG_X0_SET, 
     +        VOIGT(x,4,valv1_3), VOIGT(x,4,valv2_3),
     +        VOIGT(x,4,valv3_3), VOIGT(x,4,valv4_3),
     +        VOIGT(x,4,valv5_3), VOIGT(x,4,valv6_3),
     +        VOIGT(x,4,valv7_3), VOIGT(x,4,valv8_3), a_3
         ELSEIF (j.EQ.4) THEN
            WRITE(40,*) x, DCS_EIGHT_VOIGT_POLYBG_X0_SET, 
     +        VOIGT(x,4,valv1_4), VOIGT(x,4,valv2_4),
     +        VOIGT(x,4,valv3_4), VOIGT(x,4,valv4_4),
     +        VOIGT(x,4,valv5_4), VOIGT(x,4,valv6_4),
     +        VOIGT(x,4,valv7_4), VOIGT(x,4,valv8_4), a_4
         ELSEIF (j.EQ.5) THEN
            WRITE(40,*) x, DCS_EIGHT_VOIGT_POLYBG_X0_SET, 
     +        VOIGT(x,4,valv1_5), VOIGT(x,4,valv2_5),
     +        VOIGT(x,4,valv3_5), VOIGT(x,4,valv4_5),
     +        VOIGT(x,4,valv5_5), VOIGT(x,4,valv6_5),
     +        VOIGT(x,4,valv7_5), VOIGT(x,4,valv8_5), a_5
         ELSEIF (j.EQ.6) THEN
            WRITE(40,*) x, DCS_EIGHT_VOIGT_POLYBG_X0_SET, 
     +        VOIGT(x,4,valv1_6), VOIGT(x,4,valv2_6),
     +        VOIGT(x,4,valv3_6), VOIGT(x,4,valv4_6),
     +        VOIGT(x,4,valv5_6), VOIGT(x,4,valv6_6),
     +        VOIGT(x,4,valv7_6), VOIGT(x,4,valv8_6), a_6
         ELSEIF (j.EQ.7) THEN
            WRITE(40,*) x, DCS_EIGHT_VOIGT_POLYBG_X0_SET, 
     +        VOIGT(x,4,valv1_7), VOIGT(x,4,valv2_7),
     +        VOIGT(x,4,valv3_7), VOIGT(x,4,valv4_7),
     +        VOIGT(x,4,valv5_7), VOIGT(x,4,valv6_7),
     +        VOIGT(x,4,valv7_7), VOIGT(x,4,valv8_7), a_7
         ELSEIF (j.EQ.8) THEN
            WRITE(40,*) x, DCS_EIGHT_VOIGT_POLYBG_X0_SET, 
     +        VOIGT(x,4,valv1_8), VOIGT(x,4,valv2_8),
     +        VOIGT(x,4,valv3_8), VOIGT(x,4,valv4_8),
     +        VOIGT(x,4,valv5_8), VOIGT(x,4,valv6_8),
     +        VOIGT(x,4,valv7_8), VOIGT(x,4,valv8_8), a_8
         ELSEIF (j.EQ.9) THEN
            WRITE(40,*) x, DCS_EIGHT_VOIGT_POLYBG_X0_SET, 
     +        VOIGT(x,4,valv1_9), VOIGT(x,4,valv2_9),
     +        VOIGT(x,4,valv3_9), VOIGT(x,4,valv4_9),
     +        VOIGT(x,4,valv5_9), VOIGT(x,4,valv6_9),
     +        VOIGT(x,4,valv7_9), VOIGT(x,4,valv8_9), a_9
         ELSE
            WRITE(*,*) j
            WRITE(*,*) 'Too many spectra! Check your input files, AFF'
         ENDIF
      ENDIF



      RETURN
      END

      FUNCTION DCS_EIGHT_VOIGT_SET_NEIGHBOUR(X,npar,val,j) 
c     8 Normalized Voigt distribution plus exponential background
c     The value of 'amp' is the value of the surface below the curve
      IMPLICIT NONE
      INTEGER*4 npar,j
      REAL*8 val(npar)
      REAL*8 valv1_1(4), valv1_2(4), valv1_3(4), valv1_4(4), valv1_5(4)  
      REAL*8 valv1_6(4), valv1_7(4), valv1_8(4), valv1_9(4)
      REAL*8 valv2_1(4), valv2_2(4), valv2_3(4), valv2_4(4), valv2_5(4)  
      REAL*8 valv2_6(4), valv2_7(4), valv2_8(4), valv2_9(4)
      REAL*8 valv3_1(4), valv3_2(4), valv3_3(4), valv3_4(4), valv3_5(4)  
      REAL*8 valv3_6(4), valv3_7(4), valv3_8(4), valv3_9(4)
      REAL*8 valv4_1(4), valv4_2(4), valv4_3(4), valv4_4(4), valv4_5(4)  
      REAL*8 valv4_6(4), valv4_7(4), valv4_8(4), valv4_9(4)
      REAL*8 valv5_1(4), valv5_2(4), valv5_3(4), valv5_4(4), valv5_5(4)  
      REAL*8 valv5_6(4), valv5_7(4), valv5_8(4), valv5_9(4)
      REAL*8 valv6_1(4), valv6_2(4), valv6_3(4), valv6_4(4), valv6_5(4)  
      REAL*8 valv6_6(4), valv6_7(4), valv6_8(4), valv6_9(4)
      REAL*8 valv7_1(4), valv7_2(4), valv7_3(4), valv7_4(4), valv7_5(4)  
      REAL*8 valv7_6(4), valv7_7(4), valv7_8(4), valv7_9(4)
      REAL*8 valv8_1(4), valv8_2(4), valv8_3(4), valv8_4(4), valv8_5(4)
      REAL*8 valv8_6(4), valv8_7(4), valv8_8(4), valv8_9(4)
      REAL*8 valvn1_1(4), valvn1_2(4), valvn1_3(4), valvn1_4(4)
      REAL*8 valvn1_5(4)
      REAL*8 valvn1_6(4), valvn1_7(4), valvn1_8(4), valvn1_9(4)
      REAL*8 valvn2_1(4), valvn2_2(4), valvn2_3(4), valvn2_4(4) 
      REAL*8 valvn2_5(4)
      REAL*8 valvn2_6(4), valvn2_7(4), valvn2_8(4), valvn2_9(4)



      REAL*8 DCS_EIGHT_VOIGT_SET_NEIGHBOUR, VOIGT, x
      REAL*8 pi
      PARAMETER(pi=3.141592653589793d0)
      REAL*8 dx3, dx2, dx4, dx5, dx6, dx7, dx8
      REAL*8 x1_1, x1_2, x1_3, x1_4, x1_5, x1_6, x1_7, x1_8, x1_9
      REAL*8 damp3, damp2, damp4, damp5, damp6, damp7, damp8
      REAL*8 amp1_1,amp1_2,amp1_3, amp1_4, amp1_5, amp1_6
      REAL*8 amp1_7, amp1_8, amp1_9
      REAL*8 sigma
      REAL*8 gamma1,gamma2,gamma3,gamma4,gamma5,gamma6,gamma7,gamma8
      REAL*8 a_1, a_2, a_3, a_4, a_5, a_6, a_7, a_8, a_9
      REAL*8 b_1, b_2, b_3, b_4, b_5, b_6, b_7, b_8, b_9

      REAL*8 xn1_1, xn1_2, xn1_3, xn1_4, xn1_5, xn1_6, xn1_7, xn1_8
      REAL*8 xn2_1, xn2_2, xn2_3, xn2_4, xn2_5, xn2_6, xn2_7, xn2_8
      REAL*8 xn1_9, xn2_9
      REAL*8 ampn1_1, ampn1_2, ampn1_3, ampn1_4, ampn1_5, ampn1_6
      REAL*8 ampn1_7, ampn1_8, ampn1_9
      REAL*8 ampn2_1, ampn2_2, ampn2_3, ampn2_4, ampn2_5, ampn2_6
      REAL*8 ampn2_7, ampn2_8, ampn2_9
      REAL*8 sigman1_1, sigman1_2, sigman1_3, sigman1_4,sigman1_5
      REAL*8 sigman1_6, sigman1_7, sigman1_8, sigman1_9
      REAL*8 sigman2_1, sigman2_2, sigman2_3, sigman2_4,sigman2_5
      REAL*8 sigman2_6, sigman2_7, sigman2_8, sigman2_9
      REAL*8 gamman1_1, gamman1_2, gamman1_3, gamman1_4,gamman1_5
      REAL*8 gamman1_6, gamman1_7, gamman1_8, gamman1_9
      REAL*8 gamman2_1, gamman2_2, gamman2_3, gamman2_4,gamman2_5
      REAL*8 gamman2_6, gamman2_7, gamman2_8, gamman2_9

      CHARACTER*1 lr
      COMMON /func_exp/ lr
      ! To plot the different components
      LOGICAL plot
      COMMON /func_plot/ plot

c Parameter initialisation      
      x1_1   = val(1)
      x1_2   = val(2)
      x1_3   = val(3)
      x1_4   = val(4)
      x1_5   = val(5)
      x1_6   = val(6)
      x1_7   = val(7)
      x1_8   = val(8)
      x1_9   = val(9)
      dx2    = val(10)
      dx3    = val(11)
      dx4    = val(12)
      dx5    = val(13)
      dx6    = val(14)
      dx7    = val(15)
      dx8    = val(16)
      sigma  = val(17)
      amp1_1 = val(18) 
      amp1_2 = val(19)
      amp1_3 = val(20)
      amp1_4 = val(21)
      amp1_5 = val(22)
      amp1_6 = val(23)
      amp1_7 = val(24)
      amp1_8 = val(25)
      amp1_9 = val(26)
      damp2  = val(27)
      damp3  = val(28)
      damp4  = val(29)
      damp5  = val(30)
      damp6  = val(31)
      damp7  = val(32)
      damp8  = val(33)
      gamma1 = val(34)
      gamma2 = val(35)
      gamma3 = val(36)
      gamma4 = val(37)
      gamma5 = val(38)
      gamma6 = val(39)
      gamma7 = val(40)
      gamma8 = val(41)
      a_1 = val(42)
      a_2 = val(43)
      a_3 = val(44)
      a_4 = val(45)
      a_5 = val(46)
      a_6 = val(47)
      a_7 = val(48)
      a_8 = val(49)
      a_9 = val(50)
      b_1 = val(51)
      b_2 = val(52)
      b_3 = val(53)
      b_4 = val(54)
      b_5 = val(55)
      b_6 = val(56)
      b_7 = val(57)
      b_8 = val(58)
      b_9 = val(59)
      xn1_1 = val(60)
      xn1_2 = val(61)
      xn1_3 = val(62)
      xn1_4 = val(63)
      xn1_5 = val(64)
      xn1_6 = val(65)
      xn1_7 = val(66)
      xn1_8 = val(67)
      xn1_9 = val(68)
      ampn1_1 = val(69)        
      ampn1_2 = val(70)
      ampn1_3 = val(71)
      ampn1_4 = val(72)
      ampn1_5 = val(73)
      ampn1_6 = val(74)
      ampn1_7 = val(75)
      ampn1_8 = val(76)
      ampn1_9 = val(77)
      sigman1_1 = val(78)
      sigman1_2 = val(79)
      sigman1_3 = val(80)
      sigman1_4 = val(81)
      sigman1_5 = val(82)
      sigman1_6 = val(83)
      sigman1_7 = val(84)
      sigman1_8 = val(85)
      sigman1_9 = val(86)
      gamman1_1 = val(87)
      gamman1_2 = val(88)
      gamman1_3 = val(89)
      gamman1_4 = val(90)
      gamman1_5 = val(91)
      gamman1_6 = val(92)
      gamman1_7 = val(93)
      gamman1_8 = val(94)
      gamman1_9 = val(95)
      xn2_1 = val(96)
      xn2_2 = val(97)
      xn2_3 = val(98)
      xn2_4 = val(99)
      xn2_5 = val(100)
      xn2_6 = val(101)
      xn2_7 = val(102)
      xn2_8 = val(103)
      xn2_9 = val(104)
      ampn2_1 = val(105)        
      ampn2_2 = val(106)
      ampn2_3 = val(107)
      ampn2_4 = val(108)
      ampn2_5 = val(109)
      ampn2_6 = val(110)
      ampn2_7 = val(111)
      ampn2_8 = val(112)
      ampn2_9 = val(113)
      sigman2_1 = val(114)
      sigman2_2 = val(115)
      sigman2_3 = val(116)
      sigman2_4 = val(117)
      sigman2_5 = val(118)
      sigman2_6 = val(119)
      sigman2_7 = val(120)
      sigman2_8 = val(121)
      sigman2_9 = val(122)
      gamman2_1 = val(123)
      gamman2_2 = val(124)
      gamman2_3 = val(125)
      gamman2_4 = val(126)
      gamman2_5 = val(127)
      gamman2_6 = val(128)
      gamman2_7 = val(129)
      gamman2_8 = val(130)
      gamman2_9 = val(131)

      valv1_1(1) = x1_1
      valv1_1(2) = amp1_1
      valv1_1(3) = sigma
      valv1_1(4) = gamma1

      valv2_1(1) = x1_1+dx2
      valv2_1(2) = damp2*amp1_1
      valv2_1(3) = sigma
      valv2_1(4) = gamma2

      valv3_1(1) = x1_1+dx3
      valv3_1(2) = amp1_1*damp3
      valv3_1(3) = sigma
      valv3_1(4) = gamma3

      valv4_1(1) = x1_1+dx4
      valv4_1(2) = damp4*amp1_1
      valv4_1(3) = sigma
      valv4_1(4) = gamma4

      valv5_1(1) = x1_1+dx5
      valv5_1(2) = damp5*amp1_1
      valv5_1(3) = sigma
      valv5_1(4) = gamma5

      valv6_1(1) = x1_1+dx6
      valv6_1(2) = damp6*amp1_1
      valv6_1(3) = sigma
      valv6_1(4) = gamma6


      valv7_1(1) = x1_1+dx7
      valv7_1(2) = damp7*amp1_1
      valv7_1(3) = sigma
      valv7_1(4) = gamma7

      valv8_1(1) = x1_1+dx8
      valv8_1(2) = damp8*amp1_1
      valv8_1(3) = sigma
      valv8_1(4) = gamma8

      valvn1_1(1)= xn1_1
      valvn1_1(2)= ampn1_1
      valvn1_1(3)= sigman1_1
      valvn1_1(4)= gamman1_1

      valvn2_1(1)= xn2_1
      valvn2_1(2)= ampn2_1
      valvn2_1(3)= sigman2_1
      valvn2_1(4)= gamman2_1

      valv1_2(1) = x1_2
      valv1_2(2) = amp1_2
      valv1_2(3) = sigma
      valv1_2(4) = gamma1

      valv2_2(1) = x1_2+dx2
      valv2_2(2) = damp2*amp1_2
      valv2_2(3) = sigma
      valv2_2(4) = gamma2

      valv3_2(1) = x1_2+dx3
      valv3_2(2) = amp1_2*damp3
      valv3_2(3) = sigma
      valv3_2(4) = gamma3

      valv4_2(1) = x1_2+dx4
      valv4_2(2) = damp4*amp1_2
      valv4_2(3) = sigma
      valv4_2(4) = gamma4

      valv5_2(1) = x1_2+dx5
      valv5_2(2) = damp5*amp1_2
      valv5_2(3) = sigma
      valv5_2(4) = gamma5   


      valv6_2(1) = x1_2+dx6
      valv6_2(2) = damp6*amp1_2
      valv6_2(3) = sigma
      valv6_2(4) = gamma6

      valv7_2(1) = x1_2+dx7
      valv7_2(2) = damp7*amp1_2
      valv7_2(3) = sigma
      valv7_2(4) = gamma7


      valv8_2(1) = x1_2+dx8
      valv8_2(2) = damp8*amp1_2
      valv8_2(3) = sigma
      valv8_2(4) = gamma8

      valvn1_2(1)= xn1_2
      valvn1_2(2)= ampn1_2
      valvn1_2(3)= sigman1_2
      valvn1_2(4)= gamman1_2

      valvn2_2(1)= xn2_2
      valvn2_2(2)= ampn2_2
      valvn2_2(3)= sigman2_2
      valvn2_2(4)= gamman2_2

      valv1_3(1) = x1_3
      valv1_3(2) = amp1_3
      valv1_3(3) = sigma
      valv1_3(4) = gamma1

      valv2_3(1) = x1_3+dx2
      valv2_3(2) = damp2*amp1_3
      valv2_3(3) = sigma
      valv2_3(4) = gamma2

      valv3_3(1) = x1_3+dx3
      valv3_3(2) = amp1_3*damp3
      valv3_3(3) = sigma
      valv3_3(4) = gamma3

      valv4_3(1) = x1_3+dx4
      valv4_3(2) = damp4*amp1_3
      valv4_3(3) = sigma
      valv4_3(4) = gamma4

      valv5_3(1) = x1_3+dx5
      valv5_3(2) = damp5*amp1_3
      valv5_3(3) = sigma
      valv5_3(4) = gamma5

      valv6_3(1) = x1_3+dx6
      valv6_3(2) = damp6*amp1_3
      valv6_3(3) = sigma
      valv6_3(4) = gamma6

      valv7_3(1) = x1_3+dx7
      valv7_3(2) = damp7*amp1_3
      valv7_3(3) = sigma
      valv7_3(4) = gamma7

      valv8_3(1) = x1_3+dx8
      valv8_3(2) = damp8*amp1_3
      valv8_3(3) = sigma
      valv8_3(4) = gamma8

      valvn1_3(1)= xn1_1
      valvn1_3(2)= ampn1_3
      valvn1_3(3)= sigman1_3
      valvn1_3(4)= gamman1_3

      valvn2_3(1)= xn2_1
      valvn2_3(2)= ampn2_3
      valvn2_3(3)= sigman2_3
      valvn2_3(4)= gamman2_3


      valv1_4(1) = x1_4
      valv1_4(2) = amp1_4
      valv1_4(3) = sigma
      valv1_4(4) = gamma1


      valv2_4(1) = x1_4+dx2
      valv2_4(2) = damp2*amp1_4
      valv2_4(3) = sigma
      valv2_4(4) = gamma2

      valv3_4(1) = x1_4+dx3
      valv3_4(2) = amp1_4*damp3
      valv3_4(3) = sigma
      valv3_4(4) = gamma3


      valv4_4(1) = x1_4+dx4
      valv4_4(2) = damp4*amp1_4
      valv4_4(3) = sigma
      valv4_4(4) = gamma4


      valv5_4(1) = x1_4+dx5
      valv5_4(2) = damp5*amp1_4
      valv5_4(3) = sigma
      valv5_4(4) = gamma5

      valv6_4(1) = x1_4+dx6
      valv6_4(2) = damp6*amp1_4
      valv6_4(3) = sigma
      valv6_4(4) = gamma6


      valv7_4(1) = x1_4+dx7
      valv7_4(2) = damp7*amp1_4
      valv7_4(3) = sigma
      valv7_4(4) = gamma7


      valv8_4(1) = x1_4+dx8
      valv8_4(2) = damp8*amp1_4
      valv8_4(3) = sigma
      valv8_4(4) = gamma8


      valvn1_4(1)= xn1_1
      valvn1_4(2)= ampn1_4
      valvn1_4(3)= sigman1_4
      valvn1_4(4)= gamman1_4


      valvn2_4(1)= xn2_1
      valvn2_4(2)= ampn2_4
      valvn2_4(3)= sigman2_4
      valvn2_4(4)= gamman2_4


      valv1_5(1) = x1_5
      valv1_5(2) = amp1_5
      valv1_5(3) = sigma
      valv1_5(4) = gamma1

      valv2_5(1) = x1_5+dx2
      valv2_5(2) = damp2*amp1_5
      valv2_5(3) = sigma
      valv2_5(4) = gamma2


      valv3_5(1) = x1_5+dx3
      valv3_5(2) = amp1_5*damp3
      valv3_5(3) = sigma
      valv3_5(4) = gamma3

      valv4_5(1) = x1_5+dx4
      valv4_5(2) = damp4*amp1_5
      valv4_5(3) = sigma
      valv4_5(4) = gamma4

      valv5_5(1) = x1_5+dx5
      valv5_5(2) = damp5*amp1_5
      valv5_5(3) = sigma
      valv5_5(4) = gamma5

      valv6_5(1) = x1_5+dx6
      valv6_5(2) = damp6*amp1_5
      valv6_5(3) = sigma
      valv6_5(4) = gamma6

      valv7_5(1) = x1_5+dx7
      valv7_5(2) = damp7*amp1_5
      valv7_5(3) = sigma
      valv7_5(4) = gamma7

      valv8_5(1) = x1_5+dx8
      valv8_5(2) = damp8*amp1_5
      valv8_5(3) = sigma
      valv8_5(4) = gamma8

      valvn1_5(1)= xn1_5
      valvn1_5(2)= ampn1_5
      valvn1_5(3)= sigman1_5
      valvn1_5(4)= gamman1_5


      valvn2_5(1)= xn2_5
      valvn2_5(2)= ampn2_5
      valvn2_5(3)= sigman2_5
      valvn2_5(4)= gamman2_5


      valv1_6(1) = x1_6
      valv1_6(2) = amp1_6
      valv1_6(3) = sigma
      valv1_6(4) = gamma1


      valv2_6(1) = x1_6+dx2
      valv2_6(2) = damp2*amp1_6
      valv2_6(3) = sigma
      valv2_6(4) = gamma2


      valv3_6(1) = x1_6+dx3
      valv3_6(2) = amp1_6*damp3
      valv3_6(3) = sigma
      valv3_6(4) = gamma3


      valv4_6(1) = x1_6+dx4
      valv4_6(2) = damp4*amp1_6
      valv4_6(3) = sigma
      valv4_6(4) = gamma4

      valv5_6(1) = x1_6+dx5
      valv5_6(2) = damp5*amp1_6
      valv5_6(3) = sigma
      valv5_6(4) = gamma5


      valv6_6(1) = x1_6+dx6
      valv6_6(2) = damp4*amp1_6
      valv6_6(3) = sigma
      valv6_6(4) = gamma4


      valv7_6(1) = x1_6+dx7
      valv7_6(2) = damp7*amp1_6
      valv7_6(3) = sigma
      valv7_6(4) = gamma7


      valv8_6(1) = x1_6+dx8
      valv8_6(2) = damp8*amp1_6
      valv8_6(3) = sigma
      valv8_6(4) = gamma8


      valvn1_6(1)= xn1_6
      valvn1_6(2)= ampn1_6
      valvn1_6(3)= sigman1_6
      valvn1_6(4)= gamman1_6

      valvn2_6(1)= xn2_7
      valvn2_6(2)= ampn2_6
      valvn2_6(3)= sigman2_6
      valvn2_6(4)= gamman2_6

      valv1_7(1) = x1_7
      valv1_7(2) = amp1_7
      valv1_7(3) = sigma
      valv1_7(4) = gamma1

      valv2_7(1) = x1_7+dx2
      valv2_7(2) = damp2*amp1_7
      valv2_7(3) = sigma
      valv2_7(4) = gamma2

      valv3_7(1) = x1_7+dx3
      valv3_7(2) = amp1_7*damp3
      valv3_7(3) = sigma
      valv3_7(4) = gamma3

      valv4_7(1) = x1_7+dx4
      valv4_7(2) = damp4*amp1_7
      valv4_7(3) = sigma
      valv4_7(4) = gamma4

      valv5_7(1) = x1_7+dx5
      valv5_7(2) = damp5*amp1_7
      valv5_7(3) = sigma
      valv5_7(4) = gamma5
      
      valv6_7(1) = x1_7+dx6
      valv6_7(2) = damp4*amp1_7
      valv6_7(3) = sigma
      valv6_7(4) = gamma4

      valv7_7(1) = x1_7+dx7
      valv7_7(2) = damp7*amp1_7
      valv7_7(3) = sigma
      valv7_7(4) = gamma7
      valv8_7(1) = x1_7+dx8
      valv8_7(2) = damp8*amp1_7
      valv8_7(3) = sigma
      valv8_7(4) = gamma8

      valvn1_7(1)= xn1_1
      valvn1_7(2)= ampn1_7
      valvn1_7(3)= sigman1_7
      valvn1_7(4)= gamman1_7
      
      valvn2_7(1)= xn2_1
      valvn2_7(2)= ampn2_7
      valvn2_7(3)= sigman2_7
      valvn2_7(4)= gamman2_7


      valv1_8(1) = x1_8
      valv1_8(2) = amp1_8
      valv1_8(3) = sigma
      valv1_8(4) = gamma1

      valv2_8(1) = x1_8+dx2
      valv2_8(2) = damp2*amp1_8
      valv2_8(3) = sigma
      valv2_8(4) = gamma2

      valv3_8(1) = x1_8+dx3
      valv3_8(2) = amp1_8*damp3
      valv3_8(3) = sigma
      valv3_8(4) = gamma3


      valv4_8(1) = x1_8+dx4
      valv4_8(2) = damp4*amp1_8
      valv4_8(3) = sigma
      valv4_8(4) = gamma4


      valv5_8(1) = x1_8+dx5
      valv5_8(2) = damp5*amp1_8
      valv5_8(3) = sigma
      valv5_8(4) = gamma5

      valv6_8(1) = x1_8+dx6
      valv6_8(2) = damp6*amp1_8
      valv6_8(3) = sigma
      valv6_8(4) = gamma6

      valv7_8(1) = x1_8+dx7
      valv7_8(2) = damp7*amp1_8
      valv7_8(3) = sigma
      valv7_8(4) = gamma7

      valv8_8(1) = x1_8+dx8
      valv8_8(2) = damp8*amp1_8
      valv8_8(3) = sigma
      valv8_8(4) = gamma8

      valvn1_8(1)= xn1_9
      valvn1_8(2)= ampn1_8
      valvn1_8(3)= sigman1_8
      valvn1_8(4)= gamman1_8

      valvn2_8(1)= xn2_9
      valvn2_8(2)= ampn2_8
      valvn2_8(3)= sigman2_8
      valvn2_8(4)= gamman2_8

      valv1_9(1) = x1_9
      valv1_9(2) = amp1_9
      valv1_9(3) = sigma
      valv1_9(4) = gamma1

      valv2_9(1) = x1_9+dx2
      valv2_9(2) = damp2*amp1_9
      valv2_9(3) = sigma
      valv2_9(4) = gamma2


      valv3_9(1) = x1_9+dx3
      valv3_9(2) = amp1_9*damp3
      valv3_9(3) = sigma
      valv3_9(4) = gamma3


      valv4_9(1) = x1_9+dx4
      valv4_9(2) = damp4*amp1_9
      valv4_9(3) = sigma
      valv4_9(4) = gamma4

      valv5_9(1) = x1_9+dx5
      valv5_9(2) = damp5*amp1_9
      valv5_9(3) = sigma
      valv5_9(4) = gamma5


      valv6_9(1) = x1_9+dx6
      valv6_9(2) = damp6*amp1_9
      valv6_9(3) = sigma
      valv6_9(4) = gamma6


      valv7_9(1) = x1_9+dx7
      valv7_9(2) = damp7*amp1_9
      valv7_9(3) = sigma
      valv7_9(4) = gamma7


      valv8_9(1) = x1_9+dx8
      valv8_9(2) = damp8*amp1_9
      valv8_9(3) = sigma
      valv8_9(4) = gamma8


      valvn1_9(1)= xn1_9
      valvn1_9(2)= ampn1_9
      valvn1_9(3)= sigman1_9
      valvn1_9(4)= gamman1_9


      valvn2_9(1)= xn2_9
      valvn2_9(2)= ampn2_9
      valvn2_9(3)= sigman2_9
      valvn2_9(4)= gamman2_9



c Values for each spectrum




c sp 1
      IF (j.EQ.1) THEN

            DCS_EIGHT_VOIGT_SET_NEIGHBOUR = VOIGT(x,4,valv1_1) 
     +     + VOIGT(x,4,valv2_1) + VOIGT(x,4,valv3_1) 
     +     + VOIGT(x,4,valv4_1) + VOIGT(x,4,valv5_1)
     +     + VOIGT(x,4,valv6_1) + VOIGT(x,4,valv7_1)
     +     + VOIGT(x,4,valv8_1) + a_1
     +     + VOIGT(x,4,valvn1_1) + VOIGT(x,4,valvn2_1)


c sp2     

      ELSEIF (j.EQ.2) THEN

            DCS_EIGHT_VOIGT_SET_NEIGHBOUR = VOIGT(x,4,valv1_2) 
     +     + VOIGT(x,4,valv2_2) + VOIGT(x,4,valv3_2) 
     +     + VOIGT(x,4,valv4_2) + VOIGT(x,4,valv5_2)
     +     + VOIGT(x,4,valv6_2) + VOIGT(x,4,valv7_2)
     +     + VOIGT(x,4,valv8_2) + a_2
     +     + VOIGT(x,4,valvn1_2) + VOIGT(x,4,valvn2_2)


c sp 3     
      ELSEIF (j.EQ.3) THEN


            DCS_EIGHT_VOIGT_SET_NEIGHBOUR = VOIGT(x,4,valv1_3)  
     +     + VOIGT(x,4,valv2_3) + VOIGT(x,4,valv3_3) 
     +     + VOIGT(x,4,valv4_3) + VOIGT(x,4,valv5_3)
     +     + VOIGT(x,4,valv6_3) + VOIGT(x,4,valv7_3)
     +     + VOIGT(x,4,valv8_3) + a_3
     +     + VOIGT(x,4,valvn1_3) + VOIGT(x,4,valvn2_3)

c sp 4
      ELSEIF (j.EQ.4) THEN

            DCS_EIGHT_VOIGT_SET_NEIGHBOUR = VOIGT(x,4,valv1_4) 
     +     + VOIGT(x,4,valv2_4) + VOIGT(x,4,valv3_4) 
     +     + VOIGT(x,4,valv4_4) + VOIGT(x,4,valv5_4)
     +     + VOIGT(x,4,valv6_4) + VOIGT(x,4,valv7_4)
     +     + VOIGT(x,4,valv8_4) + a_4
     +     + VOIGT(x,4,valvn1_4) + VOIGT(x,4,valvn2_4)



c sp 5
      ELSEIF (j.EQ.5) THEN

            DCS_EIGHT_VOIGT_SET_NEIGHBOUR = VOIGT(x,4,valv1_5) 
     +     + VOIGT(x,4,valv2_5) + VOIGT(x,4,valv3_5) 
     +     + VOIGT(x,4,valv4_5) + VOIGT(x,4,valv5_5)
     +     + VOIGT(x,4,valv6_5) + VOIGT(x,4,valv7_5)
     +     + VOIGT(x,4,valv8_5) + a_5
     +     + VOIGT(x,4,valvn1_5) + VOIGT(x,4,valvn2_5)

c sp 6
      ELSEIF (j.EQ.6) THEN

            DCS_EIGHT_VOIGT_SET_NEIGHBOUR = VOIGT(x,4,valv1_6) 
     +     + VOIGT(x,4,valv2_6) + VOIGT(x,4,valv3_6) 
     +     + VOIGT(x,4,valv4_6) + VOIGT(x,4,valv5_6)
     +     + VOIGT(x,4,valv6_6) + VOIGT(x,4,valv7_6)
     +     + VOIGT(x,4,valv8_6) + a_6
     +     + VOIGT(x,4,valvn1_6) + VOIGT(x,4,valvn2_6)
c sp 7
      ELSEIF (j.EQ.7) THEN

            DCS_EIGHT_VOIGT_SET_NEIGHBOUR = VOIGT(x,4,valv1_7) 
     +     + VOIGT(x,4,valv2_7) + VOIGT(x,4,valv3_7) 
     +     + VOIGT(x,4,valv4_7) + VOIGT(x,4,valv5_7)
     +     + VOIGT(x,4,valv6_7) + VOIGT(x,4,valv7_7)
     +     + VOIGT(x,4,valv8_7) + a_7
     +     + VOIGT(x,4,valvn1_7) + VOIGT(x,4,valvn2_7)


c sp 8
      ELSEIF (j.EQ.8) THEN

            DCS_EIGHT_VOIGT_SET_NEIGHBOUR = VOIGT(x,4,valv1_8) 
     +     + VOIGT(x,4,valv2_8) + VOIGT(x,4,valv3_8) 
     +     + VOIGT(x,4,valv4_8) + VOIGT(x,4,valv5_8)
     +     + VOIGT(x,4,valv6_8) + VOIGT(x,4,valv7_8) 
     +     + VOIGT(x,4,valv8_8) + a_8
     +     + VOIGT(x,4,valvn1_8) + VOIGT(x,4,valvn2_8)


c sp 9
      ELSEIF (j.EQ.9) THEN


            DCS_EIGHT_VOIGT_SET_NEIGHBOUR = VOIGT(x,4,valv1_9) 
     +     + VOIGT(x,4,valv2_9) + VOIGT(x,4,valv3_9)
     +     + VOIGT(x,4,valv4_9) + VOIGT(x,4,valv5_9)
     +     + VOIGT(x,4,valv6_9) + VOIGT(x,4,valv7_9) 
     +     + VOIGT(x,4,valv8_9) + a_9 
     +     + VOIGT(x,4,valvn1_9) + VOIGT(x,4,valvn2_9)

      ELSE
         WRITE(*,*) 'Too many spectra !! Check your input files, CALCUL'
         WRITE(*,*) j
      END IF



c     Save the different components
      IF(plot) THEN
         IF (j.EQ.1) THEN
            WRITE(40,*) x, DCS_EIGHT_VOIGT_SET_NEIGHBOUR, 
     +        VOIGT(x,4,valv1_1), VOIGT(x,4,valv2_1),
     +        VOIGT(x,4,valv3_1), VOIGT(x,4,valv4_1),
     +        VOIGT(x,4,valv5_1), VOIGT(x,4,valv6_1),
     +        VOIGT(x,4,valv7_1), VOIGT(x,4,valv8_1), a_1,
     +        VOIGT(x,4,valvn1_1), VOIGT(x,4,valvn2_1)

         ELSEIF (j.EQ.2) THEN
            WRITE(40,*) x, DCS_EIGHT_VOIGT_SET_NEIGHBOUR, 
     +        VOIGT(x,4,valv1_2), VOIGT(x,4,valv2_2),
     +        VOIGT(x,4,valv3_2), VOIGT(x,4,valv4_2),
     +        VOIGT(x,4,valv5_2), VOIGT(x,4,valv6_2),
     +        VOIGT(x,4,valv7_2), VOIGT(x,4,valv8_2), a_2,
     +        VOIGT(x,4,valvn1_2), VOIGT(x,4,valvn2_2)
         ELSEIF (j.EQ.3) THEN
            WRITE(40,*) x, DCS_EIGHT_VOIGT_SET_NEIGHBOUR, 
     +        VOIGT(x,4,valv1_3), VOIGT(x,4,valv2_3),
     +        VOIGT(x,4,valv3_3), VOIGT(x,4,valv4_3),
     +        VOIGT(x,4,valv5_3), VOIGT(x,4,valv6_3),
     +        VOIGT(x,4,valv7_3), VOIGT(x,4,valv8_3), a_3,
     +        VOIGT(x,4,valvn1_3), VOIGT(x,4,valvn2_3)
         ELSEIF (j.EQ.4) THEN
            WRITE(40,*) x, DCS_EIGHT_VOIGT_SET_NEIGHBOUR, 
     +        VOIGT(x,4,valv1_4), VOIGT(x,4,valv2_4),
     +        VOIGT(x,4,valv3_4), VOIGT(x,4,valv4_4),
     +        VOIGT(x,4,valv5_4), VOIGT(x,4,valv6_4),
     +        VOIGT(x,4,valv7_4), VOIGT(x,4,valv8_4), a_4,
     +        VOIGT(x,4,valvn1_4), VOIGT(x,4,valvn2_4)
         ELSEIF (j.EQ.5) THEN
            WRITE(40,*) x, DCS_EIGHT_VOIGT_SET_NEIGHBOUR, 
     +        VOIGT(x,4,valv1_5), VOIGT(x,4,valv2_5),
     +        VOIGT(x,4,valv3_5), VOIGT(x,4,valv4_5),
     +        VOIGT(x,4,valv5_5), VOIGT(x,4,valv6_5),
     +        VOIGT(x,4,valv7_5), VOIGT(x,4,valv8_5), a_5,
     +        VOIGT(x,4,valvn1_5), VOIGT(x,4,valvn2_5)
         ELSEIF (j.EQ.6) THEN
            WRITE(40,*) x, DCS_EIGHT_VOIGT_SET_NEIGHBOUR, 
     +        VOIGT(x,4,valv1_6), VOIGT(x,4,valv2_6),
     +        VOIGT(x,4,valv3_6), VOIGT(x,4,valv4_6),
     +        VOIGT(x,4,valv5_6), VOIGT(x,4,valv6_6),
     +        VOIGT(x,4,valv7_6), VOIGT(x,4,valv8_6), a_6,
     +        VOIGT(x,4,valvn1_6), VOIGT(x,4,valvn2_6)
         ELSEIF (j.EQ.7) THEN
            WRITE(40,*) x, DCS_EIGHT_VOIGT_SET_NEIGHBOUR, 
     +        VOIGT(x,4,valv1_7), VOIGT(x,4,valv2_7),
     +        VOIGT(x,4,valv3_7), VOIGT(x,4,valv4_7),
     +        VOIGT(x,4,valv5_7), VOIGT(x,4,valv6_7),
     +        VOIGT(x,4,valv7_7), VOIGT(x,4,valv8_7), a_7,
     +        VOIGT(x,4,valvn1_7), VOIGT(x,4,valvn2_7)
         ELSEIF (j.EQ.8) THEN
            WRITE(40,*) x, DCS_EIGHT_VOIGT_SET_NEIGHBOUR, 
     +        VOIGT(x,4,valv1_8), VOIGT(x,4,valv2_8),
     +        VOIGT(x,4,valv3_8), VOIGT(x,4,valv4_8),
     +        VOIGT(x,4,valv5_8), VOIGT(x,4,valv6_8),
     +        VOIGT(x,4,valv7_8), VOIGT(x,4,valv8_8), a_8,
     +        VOIGT(x,4,valvn1_8), VOIGT(x,4,valvn2_8)
         ELSEIF (j.EQ.9) THEN
            WRITE(40,*) x, DCS_EIGHT_VOIGT_SET_NEIGHBOUR, 
     +        VOIGT(x,4,valv1_9), VOIGT(x,4,valv2_9),
     +        VOIGT(x,4,valv3_9), VOIGT(x,4,valv4_9),
     +        VOIGT(x,4,valv5_9), VOIGT(x,4,valv6_9),
     +        VOIGT(x,4,valv7_9), VOIGT(x,4,valv8_9), a_9,
     +        VOIGT(x,4,valvn1_9), VOIGT(x,4,valvn2_9)
         ELSE
            WRITE(*,*) j
            WRITE(*,*) 'Too many spectra! Check your input files, AFF'
         ENDIF
      ENDIF
      RETURN
      END

      







      FUNCTION TWO_INTERP_THREE_VOIGT_POLY(X,npar,val,j)
c     Rocking curve with s and p polarization extracted from simulations
      IMPLICIT NONE
      INTEGER*4 npar,j
      REAL*8 val(npar), valv(4)
      REAL*8 TWO_INTERP_THREE_VOIGT_POLY, VOIGT, x, y_1, y_2
      REAL*8 sigma, gamma
      REAL*8 amp1_1, amp1_2, amp1_3, amp1_4, amp1_5, amp1_6, amp1_7
      REAL*8 amp1_8, amp1_9
      REAL*8 amp2_1, amp2_2, amp2_3, amp2_4, amp2_5, amp2_6, amp2_7
      REAL*8 amp2_8, amp2_9
      REAL*8 amp3_1, amp3_2, amp3_3, amp3_4, amp3_5, amp3_6, amp3_7
      REAL*8 amp3_8, amp3_9
      REAL*8 x1_1, x1_2, x1_3, x1_4
      REAL*8 x1_5, x1_6, x1_7, x1_8, x1_9
      REAL*8 x2_1, x2_2, x2_3, x2_4
      REAL*8 x2_5, x2_6, x2_7, x2_8, x2_9
      REAL*8 x3_1, x3_2, x3_3, x3_4
      REAL*8 x3_5, x3_6, x3_7, x3_8, x3_9
      REAL*8 a_1, a_2, a_3, a_4, a_5, a_6, a_7, a_8, a_9

c     Interpolation variables
      INTEGER*4 k, nn_1, nn_2, ier_1,ier_2, nest
      PARAMETER (nest=1000)
      REAL*8 t_1(nest), c_1(nest), t_2(nest), c_2(nest)
      COMMON /two_interp/ t_1, t_2, c_1, c_2, k, nn_1, nn_2
      ! To plot the different components
      LOGICAL plot
      COMMON /func_plot/ plot

      amp1_1 = val(1)
      amp1_2 = val(2)
      amp1_3 = val(3)
      amp1_4 = val(4)
      amp1_5 = val(5)
      amp1_6 = val(6)
      amp1_7 = val(7)
      amp1_8 = val(8)
      amp1_9 = val(9)
      amp2_1 = val(10)
      amp2_2 = val(11)
      amp2_3 = val(12)
      amp2_4 = val(13)
      amp2_5 = val(14)
      amp2_6 = val(15)
      amp2_7 = val(16)
      amp2_8 = val(17)
      amp2_9 = val(18)
      amp3_1 = val(19)
      amp3_2 = val(20)
      amp3_3 = val(21)
      amp3_4 = val(22)
      amp3_5 = val(23)
      amp3_6 = val(24)
      amp3_7 = val(25)
      amp3_8 = val(26)
      amp3_9 = val(27)
      x1_1   = val(28)
      x1_2   = val(29)
      x1_3   = val(30)
      x1_4   = val(31)
      x1_5   = val(32)
      x1_6   = val(33)
      x1_7   = val(34)
      x1_8   = val(35)
      x1_9   = val(36)
      x2_1   = val(37)
      x2_2   = val(38)
      x2_3   = val(39)
      x2_4   = val(40)
      x2_5   = val(41)
      x2_6   = val(42)
      x2_7   = val(43)
      x2_8   = val(44)
      x2_9   = val(45)
      x3_1   = val(46)
      x3_2   = val(47)
      x3_3   = val(48)
      x3_4   = val(49)
      x3_5   = val(50)
      x3_6   = val(51)
      x3_7   = val(52)
      x3_8   = val(53)
      x3_9   = val(54)
      sigma = val(55)
      gamma = val(56)
      a_1     = val(57)
      a_2     = val(58)
      a_3     = val(59)
      a_4     = val(60)
      a_5     = val(61)
      a_6     = val(62)
      a_7     = val(63)
      a_8     = val(64)
      a_9     = val(65)



      IF (j.EQ.1) THEN

      CALL SPLEV(t_1,nn_1,c_1,k,x-x1_1,y_1,1,1,ier_1)
      CALL SPLEV(t_2,nn_2,c_2,k,x-x2_1,y_2,1,1,ier_2)
      valv(1)= x3_1
      valv(2)= amp3_1
      valv(3)= sigma
      valv(4)= gamma

            TWO_INTERP_THREE_VOIGT_POLY =  
     +     amp1_1*y_1 +amp2_1*y_2
     +     + VOIGT(x,4,valv) + a_1



c sp2     

      ELSEIF (j.EQ.2) THEN

            CALL SPLEV(t_1,nn_1,c_1,k,x-x1_2,y_1,1,1,ier_1)
            CALL SPLEV(t_2,nn_2,c_2,k,x-x2_2,y_2,1,1,ier_2)
            valv(1)= x3_2
            valv(2)= amp3_2
            valv(3)= sigma
            valv(4)= gamma
      
                  TWO_INTERP_THREE_VOIGT_POLY =  
     +     amp1_2*y_1 +amp2_2*y_2
     +     + VOIGT(x,4,valv) + a_2
      


c sp 3     
      ELSEIF (j.EQ.3) THEN

            CALL SPLEV(t_1,nn_1,c_1,k,x-x1_3,y_1,1,1,ier_1)
            CALL SPLEV(t_2,nn_2,c_2,k,x-x2_3,y_2,1,1,ier_2)
            valv(1)= x3_3
            valv(2)= amp3_3
            valv(3)= sigma
            valv(4)= gamma
      
                  TWO_INTERP_THREE_VOIGT_POLY =  
     +     amp1_3*y_1 +amp2_3*y_2
     +     + VOIGT(x,4,valv) + a_3

c sp 4
      ELSEIF (j.EQ.4) THEN

            CALL SPLEV(t_1,nn_1,c_1,k,x-x1_4,y_1,1,1,ier_1)
            CALL SPLEV(t_2,nn_2,c_2,k,x-x2_4,y_2,1,1,ier_2)
            valv(1)= x3_4
            valv(2)= amp3_4
            valv(3)= sigma
            valv(4)= gamma
      
                  TWO_INTERP_THREE_VOIGT_POLY =  
     +     amp1_4*y_1 +amp2_4*y_2
     +     + VOIGT(x,4,valv) + a_4



c sp 5
      ELSEIF (j.EQ.5) THEN

            CALL SPLEV(t_1,nn_1,c_1,k,x-x1_5,y_1,1,1,ier_1)
            CALL SPLEV(t_2,nn_2,c_2,k,x-x2_5,y_2,1,1,ier_2)
            valv(1)= x3_5
            valv(2)= amp3_5
            valv(3)= sigma
            valv(4)= gamma
      
                  TWO_INTERP_THREE_VOIGT_POLY =  
     +     amp1_5*y_1 +amp2_5*y_2
     +     + VOIGT(x,4,valv) + a_5
c sp 6
      ELSEIF (j.EQ.6) THEN
            CALL SPLEV(t_1,nn_1,c_1,k,x-x1_6,y_1,1,1,ier_1)
            CALL SPLEV(t_2,nn_2,c_2,k,x-x2_6,y_2,1,1,ier_2)
            valv(1)= x3_6
            valv(2)= amp3_6
            valv(3)= sigma
            valv(4)= gamma
      
                  TWO_INTERP_THREE_VOIGT_POLY =  
     +     amp1_6*y_1 +amp2_6*y_2
     +     + VOIGT(x,4,valv) + a_6

c sp 7
      ELSEIF (j.EQ.7) THEN

            CALL SPLEV(t_1,nn_1,c_1,k,x-x1_7,y_1,1,1,ier_1)
            CALL SPLEV(t_2,nn_2,c_2,k,x-x2_7,y_2,1,1,ier_2)
            valv(1)= x3_7
            valv(2)= amp3_7
            valv(3)= sigma
            valv(4)= gamma
      
                  TWO_INTERP_THREE_VOIGT_POLY =  
     +     amp1_7*y_1 +amp2_7*y_2
     +     + VOIGT(x,4,valv) + a_7

c sp 8
      ELSEIF (j.EQ.8) THEN

            CALL SPLEV(t_1,nn_1,c_1,k,x-x1_8,y_1,1,1,ier_1)
            CALL SPLEV(t_2,nn_2,c_2,k,x-x2_8,y_2,1,1,ier_2)
            valv(1)= x3_8
            valv(2)= amp3_8
            valv(3)= sigma
            valv(4)= gamma
      
                  TWO_INTERP_THREE_VOIGT_POLY =  
     +     amp1_8*y_1 +amp2_8*y_2
     +     + VOIGT(x,4,valv) + a_8


c sp 9
      ELSEIF (j.EQ.9) THEN
            CALL SPLEV(t_1,nn_1,c_1,k,x-x1_9,y_1,1,1,ier_1)
            CALL SPLEV(t_2,nn_2,c_2,k,x-x2_9,y_2,1,1,ier_2)
            valv(1)= x3_9
            valv(2)= amp3_9
            valv(3)= sigma
            valv(4)= gamma
      
                  TWO_INTERP_THREE_VOIGT_POLY =  
     +     amp1_9*y_1 +amp2_9*y_2
     +     + VOIGT(x,4,valv) + a_9

      ELSE
         WRITE(*,*) 'Too many spectra !! Check your input files, CALCUL'
         WRITE(*,*) j
      END IF
      


c     Save different components
      IF(plot) THEN
            IF(j.EQ.1) THEN
            WRITE(40,*) x, TWO_INTERP_THREE_VOIGT_POLY, amp1_1*y_1,
     +        amp2_1*y_2, VOIGT(x,4,valv), a_1

            ELSEIF(j.EQ.2) THEN
            WRITE(40,*) x, TWO_INTERP_THREE_VOIGT_POLY, amp1_2*y_1, 
     +        amp2_2*y_2, VOIGT(x,4,valv), a_2

            ELSEIF(j.EQ.3) THEN
            WRITE(40,*) x, TWO_INTERP_THREE_VOIGT_POLY, amp1_3*y_1,
     +        amp2_3*y_2, VOIGT(x,4,valv), a_3

            ELSEIF(j.EQ.4) THEN
            WRITE(40,*) x, TWO_INTERP_THREE_VOIGT_POLY, amp1_4*y_1,
     +        amp2_4*y_2, VOIGT(x,4,valv), a_4

            ELSEIF(j.EQ.5) THEN
            WRITE(40,*) x, TWO_INTERP_THREE_VOIGT_POLY, amp1_5*y_1, 
     +        amp2_5*y_2, VOIGT(x,4,valv), a_5

            ELSEIF(j.EQ.6) THEN
            WRITE(40,*) x, TWO_INTERP_THREE_VOIGT_POLY, amp1_6*y_1, 
     +        amp2_6*y_2, VOIGT(x,4,valv), a_6

            ELSEIF(j.EQ.7) THEN
            WRITE(40,*) x, TWO_INTERP_THREE_VOIGT_POLY, amp1_7*y_1,
     +        amp2_7*y_2, VOIGT(x,4,valv), a_7

            ELSEIF(j.EQ.8) THEN
            WRITE(40,*) x, TWO_INTERP_THREE_VOIGT_POLY, amp1_8*y_1,
     +        amp2_8*y_2, VOIGT(x,4,valv), a_8

            ELSEIF(j.EQ.9) THEN
            WRITE(40,*) x, TWO_INTERP_THREE_VOIGT_POLY, amp1_9*y_1, 
     +        amp2_9*y_2, VOIGT(x,4,valv), a_9




            ENDIF
      ENDIF

      RETURN
      END






      FUNCTION THREE_INTERP_POLY_N_SET(X,npar,val,j)
c     Rocking curve with s and p polarization extracted from simulations
      IMPLICIT NONE
      INTEGER*4 npar,j
      REAL*8 val(npar), valv(4)
      REAL*8 THREE_INTERP_POLY_N_SET, x, y_1, y_2, y_3, VOIGT
      REAL*8 sigma, gamma
      REAL*8 amp1_1, amp1_2, amp1_3, amp1_4, amp1_5, amp1_6, amp1_7
      REAL*8 amp1_8, amp1_9
      REAL*8 damp2, damp3
      REAL*8 x1_1, x1_2, x1_3, x1_4
      REAL*8 x1_5, x1_6, x1_7, x1_8, x1_9
      REAL*8 dx2, dx3
      REAL*8 a_1, a_2, a_3, a_4, a_5, a_6, a_7, a_8, a_9
      
      REAL*8 xn1_1, xn1_2, xn1_3, xn1_4, xn1_5, xn1_6, xn1_7, xn1_8
      REAL*8 xn2_1, xn2_2, xn2_3, xn2_4, xn2_5, xn2_6, xn2_7, xn2_8
      REAL*8 xn1_9, xn2_9
      REAL*8 ampn1_1, ampn1_2, ampn1_3, ampn1_4, ampn1_5, ampn1_6
      REAL*8 ampn1_7, ampn1_8, ampn1_9
      REAL*8 ampn2_1, ampn2_2, ampn2_3, ampn2_4, ampn2_5, ampn2_6
      REAL*8 ampn2_7, ampn2_8, ampn2_9
      REAL*8 sigman1_1, sigman1_2, sigman1_3, sigman1_4,sigman1_5
      REAL*8 sigman1_6, sigman1_7, sigman1_8, sigman1_9
      REAL*8 sigman2_1, sigman2_2, sigman2_3, sigman2_4,sigman2_5
      REAL*8 sigman2_6, sigman2_7, sigman2_8, sigman2_9
      REAL*8 gamman1_1, gamman1_2, gamman1_3, gamman1_4,gamman1_5
      REAL*8 gamman1_6, gamman1_7, gamman1_8, gamman1_9
      REAL*8 gamman2_1, gamman2_2, gamman2_3, gamman2_4,gamman2_5
      REAL*8 gamman2_6, gamman2_7, gamman2_8, gamman2_9

      REAL*8 valvn1_1(4), valvn1_2(4), valvn1_3(4), valvn1_4(4)
      REAL*8 valvn1_5(4)
      REAL*8 valvn1_6(4), valvn1_7(4), valvn1_8(4), valvn1_9(4)
      REAL*8 valvn2_1(4), valvn2_2(4), valvn2_3(4), valvn2_4(4) 
      REAL*8 valvn2_5(4)
      REAL*8 valvn2_6(4), valvn2_7(4), valvn2_8(4), valvn2_9(4)


c     Interpolation variables
      INTEGER*4 k, nn_1, nn_2, ier_1,ier_2, nest, nn_3, ier_3
      PARAMETER (nest=1000)
      REAL*8 t_1(nest),c_1(nest),t_2(nest),c_2(nest)
      REAL*8 t_3(nest),c_3(nest)
      COMMON /three_interp/ t_1,t_2,t_3,c_1,c_2,c_3,k,nn_1,nn_2,nn_3
      ! To plot the different components
      LOGICAL plot
      COMMON /func_plot/ plot

      amp1_1 = val(1)
      amp1_2 = val(2)
      amp1_3 = val(3)
      amp1_4 = val(4)
      amp1_5 = val(5)
      amp1_6 = val(6)
      amp1_7 = val(7)
      amp1_8 = val(8)
      amp1_9 = val(9)
      x1_1   = val(10)
      x1_2   = val(11)
      x1_3   = val(12)
      x1_4   = val(13)
      x1_5   = val(14)
      x1_6   = val(15)
      x1_7   = val(16)
      x1_8   = val(17)
      x1_9   = val(18)
      damp2  = val(19)
      dx2    = val(20)
      damp3  = val(21)
      dx3    = val(22)
      a_1     = val(23)
      a_2     = val(24)
      a_3     = val(25)
      a_4     = val(26)
      a_5     = val(27)
      a_6     = val(28)
      a_7     = val(29)
      a_8     = val(30)
      a_9     = val(31)
      xn1_1 = val(32)
      xn1_2 = val(33)
      xn1_3 = val(34)
      xn1_4 = val(35)
      xn1_5 = val(36)
      xn1_6 = val(37)
      xn1_7 = val(38)
      xn1_8 = val(39)
      xn1_9 = val(40)
      ampn1_1 = val(41)        
      ampn1_2 = val(42)
      ampn1_3 = val(43)
      ampn1_4 = val(44)
      ampn1_5 = val(45)
      ampn1_6 = val(46)
      ampn1_7 = val(47)
      ampn1_8 = val(48)
      ampn1_9 = val(49)
      sigman1_1 = val(50)
      sigman1_2 = val(51)
      sigman1_3 = val(52)
      sigman1_4 = val(53)
      sigman1_5 = val(54)
      sigman1_6 = val(55)
      sigman1_7 = val(56)
      sigman1_8 = val(57)
      sigman1_9 = val(58)
      gamman1_1 = val(59)
      gamman1_2 = val(60)
      gamman1_3 = val(61)
      gamman1_4 = val(62)
      gamman1_5 = val(63)
      gamman1_6 = val(64)
      gamman1_7 = val(65)
      gamman1_8 = val(66)
      gamman1_9 = val(67)
      xn2_1 = val(68)
      xn2_2 = val(69)
      xn2_3 = val(70)
      xn2_4 = val(71)
      xn2_5 = val(72)
      xn2_6 = val(73)
      xn2_7 = val(74)
      xn2_8 = val(75)
      xn2_9 = val(76)
      ampn2_1 = val(77)        
      ampn2_2 = val(78)
      ampn2_3 = val(79)
      ampn2_4 = val(80)
      ampn2_5 = val(81)
      ampn2_6 = val(82)
      ampn2_7 = val(83)
      ampn2_8 = val(84)
      ampn2_9 = val(85)
      sigman2_1 = val(86)
      sigman2_2 = val(87)
      sigman2_3 = val(88)
      sigman2_4 = val(89)
      sigman2_5 = val(90)
      sigman2_6 = val(91)
      sigman2_7 = val(92)
      sigman2_8 = val(93)
      sigman2_9 = val(94)
      gamman2_1 = val(95)
      gamman2_2 = val(96)
      gamman2_3 = val(97)
      gamman2_4 = val(98)
      gamman2_5 = val(99)
      gamman2_6 = val(100)
      gamman2_7 = val(101)
      gamman2_8 = val(102)
      gamman2_9 = val(103)





      valvn1_1(1)= xn1_1
      valvn1_1(2)= ampn1_1
      valvn1_1(3)= sigman1_1
      valvn1_1(4)= gamman1_1

      valvn2_1(1)= xn2_1
      valvn2_1(2)= ampn2_1
      valvn2_1(3)= sigman2_1
      valvn2_1(4)= gamman2_1

      valvn1_2(1)= xn1_2
      valvn1_2(2)= ampn1_2
      valvn1_2(3)= sigman1_2
      valvn1_2(4)= gamman1_2

      valvn2_2(1)= xn2_2
      valvn2_2(2)= ampn2_2
      valvn2_2(3)= sigman2_2
      valvn2_2(4)= gamman2_2

      valvn1_3(1)= xn1_3
      valvn1_3(2)= ampn1_3
      valvn1_3(3)= sigman1_3
      valvn1_3(4)= gamman1_3

      valvn2_3(1)= xn2_3
      valvn2_3(2)= ampn2_3
      valvn2_3(3)= sigman2_3
      valvn2_3(4)= gamman2_3

      valvn1_4(1)= xn1_4
      valvn1_4(2)= ampn1_4
      valvn1_4(3)= sigman1_4
      valvn1_4(4)= gamman1_4

      valvn2_4(1)= xn2_4
      valvn2_4(2)= ampn2_4
      valvn2_4(3)= sigman2_4
      valvn2_4(4)= gamman2_4

      valvn1_5(1)= xn1_5
      valvn1_5(2)= ampn1_5
      valvn1_5(3)= sigman1_5
      valvn1_5(4)= gamman1_5

      valvn2_5(1)= xn2_5
      valvn2_5(2)= ampn2_5
      valvn2_5(3)= sigman2_5
      valvn2_5(4)= gamman2_5

      valvn1_6(1)= xn1_6
      valvn1_6(2)= ampn1_6
      valvn1_6(3)= sigman1_6
      valvn1_6(4)= gamman1_6

      valvn2_6(1)= xn2_6
      valvn2_6(2)= ampn2_6
      valvn2_6(3)= sigman2_6
      valvn2_6(4)= gamman2_6

      valvn1_7(1)= xn1_7
      valvn1_7(2)= ampn1_7
      valvn1_7(3)= sigman1_7
      valvn1_7(4)= gamman1_7

      valvn2_7(1)= xn2_7
      valvn2_7(2)= ampn2_7
      valvn2_7(3)= sigman2_7
      valvn2_7(4)= gamman2_7

      valvn1_8(1)= xn1_8
      valvn1_8(2)= ampn1_8
      valvn1_8(3)= sigman1_8
      valvn1_8(4)= gamman1_8

      valvn2_8(1)= xn2_8
      valvn2_8(2)= ampn2_8
      valvn2_8(3)= sigman2_8
      valvn2_8(4)= gamman2_8

      valvn1_9(1)= xn1_9
      valvn1_9(2)= ampn1_9
      valvn1_9(3)= sigman1_9
      valvn1_9(4)= gamman1_9

      valvn2_9(1)= xn2_9
      valvn2_9(2)= ampn2_9
      valvn2_9(3)= sigman2_9
      valvn2_9(4)= gamman2_9


      IF (j.EQ.1) THEN

      CALL SPLEV(t_1,nn_1,c_1,k,x-x1_1,y_1,1,1,ier_1)
      CALL SPLEV(t_2,nn_2,c_2,k,x-x1_1-dx2,y_2,1,1,ier_2)
      CALL SPLEV(t_3,nn_3,c_2,k,x-x1_1-dx3,y_3,1,1,ier_3)


      THREE_INTERP_POLY_N_SET =  amp1_1*y_1 +damp2*amp1_1*y_2
     +     + damp3*amp1_1*y_3 + VOIGT(x,4,valvn1_1) 
     +     + VOIGT(x,4,valvn2_1) + a_1



c sp2     

      ELSEIF (j.EQ.2) THEN
      CALL SPLEV(t_1,nn_1,c_1,k,x-x1_2,y_1,1,1,ier_1)
      CALL SPLEV(t_2,nn_2,c_2,k,x-x1_2-dx2,y_2,1,1,ier_2)
      CALL SPLEV(t_3,nn_3,c_2,k,x-x1_2-dx3,y_3,1,1,ier_3)
      
      
      THREE_INTERP_POLY_N_SET =  amp1_2*y_1 +damp2*amp1_2*y_2
     +     + damp3*amp1_2*y_3 + VOIGT(x,4,valvn1_2) 
     +     + VOIGT(x,4,valvn2_2) + a_2

c sp 3     
      ELSEIF (j.EQ.3) THEN

      CALL SPLEV(t_1,nn_1,c_1,k,x-x1_3,y_1,1,1,ier_1)
      CALL SPLEV(t_2,nn_2,c_2,k,x-x1_3-dx2,y_2,1,1,ier_2)
      CALL SPLEV(t_3,nn_3,c_2,k,x-x1_3-dx3,y_3,1,1,ier_3)
      
      
      THREE_INTERP_POLY_N_SET =  amp1_3*y_1 +damp2*amp1_3*y_2
     +     + damp3*amp1_3*y_3 + VOIGT(x,4,valvn1_3) 
     +     + VOIGT(x,4,valvn2_3) + a_3

c sp 4
      ELSEIF (j.EQ.4) THEN

      CALL SPLEV(t_1,nn_1,c_1,k,x-x1_4,y_1,1,1,ier_1)
      CALL SPLEV(t_2,nn_2,c_2,k,x-x1_4-dx2,y_2,1,1,ier_2)
      CALL SPLEV(t_3,nn_3,c_2,k,x-x1_4-dx3,y_3,1,1,ier_3)
      
      
      THREE_INTERP_POLY_N_SET =  amp1_4*y_1 +damp2*amp1_4*y_2
     +     + damp3*amp1_4*y_3 + VOIGT(x,4,valvn1_4) 
     +     + VOIGT(x,4,valvn2_4) + a_4



c sp 5
      ELSEIF (j.EQ.5) THEN

      CALL SPLEV(t_1,nn_1,c_1,k,x-x1_5,y_1,1,1,ier_1)
      CALL SPLEV(t_2,nn_2,c_2,k,x-x1_5-dx2,y_2,1,1,ier_2)
      CALL SPLEV(t_3,nn_3,c_2,k,x-x1_5-dx3,y_3,1,1,ier_3)
            

      THREE_INTERP_POLY_N_SET =  amp1_5*y_1 +damp2*amp1_5*y_2
     +     + damp3*amp1_5*y_3 + VOIGT(x,4,valvn1_5) 
     +     + VOIGT(x,4,valvn2_5) + a_5

c sp 6
      ELSEIF (j.EQ.6) THEN

      CALL SPLEV(t_1,nn_1,c_1,k,x-x1_6,y_1,1,1,ier_1)
      CALL SPLEV(t_2,nn_2,c_2,k,x-x1_6-dx2,y_2,1,1,ier_2)
      CALL SPLEV(t_3,nn_3,c_2,k,x-x1_6-dx3,y_3,1,1,ier_3)
            

      THREE_INTERP_POLY_N_SET =  amp1_6*y_1 +damp2*amp1_6*y_2
     +     + damp3*amp1_6*y_3 + VOIGT(x,4,valvn1_6) 
     +     + VOIGT(x,4,valvn2_6) + a_6

c sp 7
      ELSEIF (j.EQ.7) THEN

      CALL SPLEV(t_1,nn_1,c_1,k,x-x1_7,y_1,1,1,ier_1)
      CALL SPLEV(t_2,nn_2,c_2,k,x-x1_7-dx2,y_2,1,1,ier_2)
      CALL SPLEV(t_3,nn_3,c_2,k,x-x1_7-dx3,y_3,1,1,ier_3)
            

      THREE_INTERP_POLY_N_SET =  amp1_7*y_1 +damp2*amp1_7*y_2
     +     + damp3*amp1_7*y_3 + VOIGT(x,4,valvn1_7) 
     +     + VOIGT(x,4,valvn2_7) + a_7

c sp 8
      ELSEIF (j.EQ.8) THEN

      CALL SPLEV(t_1,nn_1,c_1,k,x-x1_8,y_1,1,1,ier_1)
      CALL SPLEV(t_2,nn_2,c_2,k,x-x1_8-dx2,y_2,1,1,ier_2)
      CALL SPLEV(t_3,nn_3,c_2,k,x-x1_8-dx3,y_3,1,1,ier_3)
            

      THREE_INTERP_POLY_N_SET =  amp1_8*y_1 +damp2*amp1_8*y_2
     +     + damp3*amp1_8*y_3 + VOIGT(x,4,valvn1_8) 
     +     + VOIGT(x,4,valvn2_8) + a_8


c sp 9
      ELSEIF (j.EQ.9) THEN

      CALL SPLEV(t_1,nn_1,c_1,k,x-x1_9,y_1,1,1,ier_1)
      CALL SPLEV(t_2,nn_2,c_2,k,x-x1_9-dx2,y_2,1,1,ier_2)
      CALL SPLEV(t_3,nn_3,c_2,k,x-x1_9-dx3,y_3,1,1,ier_3)
            

      THREE_INTERP_POLY_N_SET =  amp1_9*y_1 +damp2*amp1_9*y_2
     +     + damp3*amp1_9*y_3 + VOIGT(x,4,valvn1_9) 
     +     + VOIGT(x,4,valvn2_9) + a_9

      ELSE
      WRITE(*,*) 'Too many spectra !! Check your input files, CALCUL'
      WRITE(*,*) j
      END IF
      


c     Save different components
      IF(plot) THEN
            IF(j.EQ.1) THEN
            WRITE(40,*) x, THREE_INTERP_POLY_N_SET, amp1_1*y_1,
     +      damp2*amp1_1*y_2, damp3*amp1_1*y_3, 
     +      VOIGT(x,4,valvn1_1), VOIGT(x,4,valvn2_1), a_1


            ELSEIF(j.EQ.2) THEN
            WRITE(40,*) x, THREE_INTERP_POLY_N_SET, amp1_2*y_1,
     +      damp2*amp1_2*y_2, damp3*amp1_2*y_3, 
     +      VOIGT(x,4,valvn1_2), VOIGT(x,4,valvn2_2), a_2

            ELSEIF(j.EQ.3) THEN
            WRITE(40,*) x, THREE_INTERP_POLY_N_SET, amp1_3*y_1,
     +      damp2*amp1_3*y_2, damp3*amp1_3*y_3, 
     +      VOIGT(x,4,valvn1_3), VOIGT(x,4,valvn2_3), a_3

            ELSEIF(j.EQ.4) THEN
            WRITE(40,*) x, THREE_INTERP_POLY_N_SET, amp1_4*y_1,
     +      damp2*amp1_4*y_2, damp3*amp1_4*y_3, 
     +      VOIGT(x,4,valvn1_4), VOIGT(x,4,valvn2_4), a_4

            ELSEIF(j.EQ.5) THEN
            WRITE(40,*) x, THREE_INTERP_POLY_N_SET, amp1_5*y_1,
     +      damp2*amp1_5*y_2, damp3*amp1_5*y_3, 
     +      VOIGT(x,4,valvn1_5), VOIGT(x,4,valvn2_5), a_5

            ELSEIF(j.EQ.6) THEN
            WRITE(40,*) x, THREE_INTERP_POLY_N_SET, amp1_6*y_1,
     +      damp2*amp1_6*y_2, damp3*amp1_6*y_3, 
     +      VOIGT(x,4,valvn1_6), VOIGT(x,4,valvn2_6), a_6

            ELSEIF(j.EQ.7) THEN
            WRITE(40,*) x, THREE_INTERP_POLY_N_SET, amp1_7*y_1,
     +      damp2*amp1_7*y_2, damp3*amp1_7*y_3, 
     +      VOIGT(x,4,valvn1_7), VOIGT(x,4,valvn2_7), a_7

            ELSEIF(j.EQ.8) THEN
            WRITE(40,*) x, THREE_INTERP_POLY_N_SET, amp1_8*y_1,
     +      damp2*amp1_8*y_2, damp3*amp1_8*y_3, 
     +      VOIGT(x,4,valvn1_8), VOIGT(x,4,valvn2_8), a_8

            ELSEIF(j.EQ.9) THEN
            WRITE(40,*) x, THREE_INTERP_POLY_N_SET, amp1_9*y_1,
     +      damp2*amp1_9*y_2, damp3*amp1_9*y_3, 
     +      VOIGT(x,4,valvn1_9), VOIGT(x,4,valvn2_9), a_9

            ENDIF
      ENDIF

      RETURN
      END
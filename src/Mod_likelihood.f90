MODULE MOD_LIKELIHOOD
  ! Automatic Time-stamp: <Last changed by martino on Wednesday 31 March 2021 at CEST 00:13:00>
  ! Module of the likelihood function for data analysis


  !#####################################################################################################################

  ! IMPORTANT: to switch between likelihood types for test and others,
  ! change the name of the file to compile in the Makefile,
  ! Mod_likeihood.f90 or Mod_likeihood_tests.f90

  !#####################################################################################################################


  ! Module for the input parameter definition
  USE MOD_PARAMETERS

  IMPLICIT NONE

  ! Data variables
  INTEGER(4) :: ndata
  INTEGER(4), DIMENSION(nsetmax) :: ndata_set=0
  REAL(8), ALLOCATABLE, DIMENSION(:,:) :: x, nc, nc_err
  ! Data variable for 2D images
  INTEGER(4) :: nx=0, ny=0
  REAL(8), ALLOCATABLE, DIMENSION(:,:) :: adata
  ! Likelihood variables
  REAL(8) :: const_ll = 0.

CONTAINS

  SUBROUTINE INIT_LIKELIHOOD()
    ! Initialize the normal likelihood with data files and special function


    ! Read data ------------------------------------------------------------------------------------------------------------------------
    CALL READ_DATA()

    ! Initialize functions
    CALL INIT_FUNCTIONS()

  END SUBROUTINE INIT_LIKELIHOOD


  !#####################################################################################################################

  SUBROUTINE READ_DATA()
    ! Subroutine to read data files
    INTEGER(4) :: k=0
    REAL(8), DIMENSION(maxdata,nsetmax) :: x_tmp=0, y_tmp=0, nc_tmp=0, nc_err_tmp=0

    ! READ DATA, calculate the constants for the likelihood function
    ! Initialize
    ndata = 0
    ndata_set = 0
    const_ll = 0.

    IF (data_type.EQ.'1c') THEN
       ! Case 1D with counts ----------------------------------------------------------------
       DO k=1,nset
          CALL READ_FILE_COUNTS(filename(k),xmin(k),xmax(k),ndata_set(k), &
               x_tmp(:,k),nc_tmp(:,k))
          WRITE(*,*) 'Number of file = ', k, ' of ', nset
          WRITE(*,*) 'Read data file ', TRIM(filename(k))
          WRITE(*,*) 'ndata = ', ndata_set(k)
          WRITE(*,*) 'constant in evidence calc. = ', const_ll
       END DO
       ! Allocate set of data
       ndata = MAXVAL(ndata_set)
       ALLOCATE(x(ndata,nset),nc(ndata,nset))
       x = 0.
       nc = 0.
       DO k=1,nset
          x(1:ndata_set(k),k)  = x_tmp(1:ndata_set(k),k)
          nc(1:ndata_set(k),k) = nc_tmp(1:ndata_set(k),k)
       END DO
    ELSE IF (data_type.EQ.'1e') THEN
       ! Case 1D with errorbars -------------------------------------------------------------------
       DO k=1,nset
          CALL READ_FILE_ERRORBARS(filename(k),xmin(k),xmax(k),ndata_set(k), &
               x_tmp(:,k),nc_tmp(:,k),nc_err_tmp(:,k))
          WRITE(*,*) 'Number of file = ', k, ' of ', nset
          WRITE(*,*) 'Data file', filename(k), ' read'
          WRITE(*,*) 'ndata = ', ndata
          WRITE(*,*) 'constant in evidence calc. = ', const_ll
       END DO
       ! Allocate set of data
       ndata = MAXVAL(ndata_set)
       ALLOCATE(x(ndata,nset),nc(ndata,nset),nc_err(ndata,nset))
       x = 0.
       nc = 0.
       nc_err = 0.
       DO k=1,nset
          x(1:ndata_set(k),k)  = x_tmp(1:ndata_set(k),k)
          nc(1:ndata_set(k),k) = nc_tmp(1:ndata_set(k),k)
          nc_err(1:ndata_set(k),k) = nc_err_tmp(1:ndata_set(k),k)
       END DO
    ELSE IF (data_type.EQ.'2c') THEN
       ! Case 2D with counts in a matrix -------------------------------------------------------------------
       DO k=1,nset
          CALL READ_FILE_COUNTS_2D(filename(k),xmin(k),xmax(k),ymin(k),ymax(k),ndata_set(k))
          WRITE(*,*) 'Number of file = ', k, ' of ', nset
          WRITE(*,*) 'Read data file ', TRIM(filename(k))
          WRITE(*,*) 'ndata = ', ndata_set(k)
          WRITE(*,*) 'constant in evidence calc. = ', const_ll
       END DO
    ELSE
       WRITE(*,*) 'Data type ', data_type, ' not available. Please change your input file'
       STOP
    END IF


  END SUBROUTINE READ_DATA

  !#####################################################################################################################

  SUBROUTINE READ_FILE_COUNTS(namefile,minx,maxx,datan,x_tmp,nc_tmp)
    ! Read one file of data
    CHARACTER, INTENT(IN) :: namefile*64
    REAL(8), INTENT(IN) :: minx, maxx
    INTEGER(4), INTENT(OUT) :: datan
    REAL(8), DIMENSION(maxdata), INTENT(OUT) :: x_tmp, nc_tmp
    !
    INTEGER(4) :: i=0, nd=0
    REAL(8), DIMENSION(maxdata) :: x_raw=0, nc_raw=0
    REAL(8) :: DLOG_FAC

    ! Initialize
    x_tmp = 0.
    nc_tmp = 0.
    datan = 0
    nd=0

    ! Open file and read
    OPEN(10,file=namefile,status='old')
    DO i=1, maxdata
       READ(10,*,END=20) x_raw(i), nc_raw(i)
       ! Make test for integer numbers
       IF (ABS(nc_raw(i)-INT(nc_raw(i))).GT.1E-5) THEN
          WRITE(*,*) 'Attention, input numbers are not counts and you are using Poisson statistic (no error bar)'
          WRITE(*,*) 'n. counts = ', nc_raw(i)
          WRITE(*,*) 'Change something!'
          STOP
       END IF
       ! Select the data
       IF(x_raw(i).GE.minx.AND.x_raw(i).LE.maxx) THEN
          nd = nd + 1
          x_tmp(nd) = x_raw(i)
          nc_tmp(nd) = nc_raw(i)
          ! Calculation of the constant part of the likelihood with Poisson distribution
          !IF (nc_tmp(nd).GT.0.) const_ll = const_ll - DFAC_LN(INT(nc_tmp(nd)))
          ! Uses of the gamma function gamma(n) = (n-1)!
          IF (nc_tmp(nd).GT.0.) const_ll = const_ll - DLOG_FAC(INT(nc_tmp(nd)))
          !write(*,*) nc_tmp(nd), const_ll
       END IF
    ENDDO

20  CONTINUE
    CLOSE(10)
    datan = nd

  END SUBROUTINE READ_FILE_COUNTS

  !#####################################################################################################################

  SUBROUTINE READ_FILE_ERRORBARS(namefile,minx,maxx,datan,x_tmp,nc_tmp,nc_err_tmp)
    ! Read one file of data
    CHARACTER, INTENT(IN) :: namefile*64
    REAL(8), INTENT(IN) :: minx, maxx
    INTEGER(4), INTENT(OUT) :: datan
    REAL(8), DIMENSION(maxdata), INTENT(OUT) :: x_tmp, nc_tmp, nc_err_tmp
    !
    INTEGER(4) :: i=0, nd = 0
    REAL(8), DIMENSION(maxdata) :: x_raw=0, nc_raw=0, nc_err_raw=0
    REAL(8), PARAMETER :: pi=3.141592653589793d0


    ! Initialize
    x_tmp = 0.
    nc_tmp = 0.
    nc_err_tmp = 0.
    nd = 0

    ! Open file and read
    OPEN(10,file=namefile,status='old')
    DO i=1, maxdata
       READ(10,*,END=20) x_raw(i), nc_raw(i), nc_err_raw(i)
       ! Select the data
       IF(x_raw(i).GE.minx.AND.x_raw(i).LE.maxx) THEN
          nd = nd + 1
          x_tmp(nd) = x_raw(i)
          nc_tmp(nd) = nc_raw(i)
          nc_err_tmp(nd) = nc_err_raw(i)
          IF (nc_err_tmp(nd).LE.0.) THEN
             WRITE(*,*) 'Errorbars with a value equal to 0 or negative'
             WRITE(*,*) 'Check our data or do not use errorbars'
             STOP
          END IF
          ! Calculation of the constant part of the likelihood with Gaussian distribution
          const_ll = const_ll -DLOG(nc_err_raw(i))
       END IF
    ENDDO

20  CONTINUE
    CLOSE(10)
    ! Calculation of the constant part of the likelihood with Gaussian distribution
    const_ll = -nd*DLOG(2*pi)/2
    datan = nd

  END SUBROUTINE READ_FILE_ERRORBARS

    !#####################################################################################################################

  SUBROUTINE READ_FILE_COUNTS_2D(namefile,minx,maxx,miny,maxy,datan)
    ! Read one file of data
    
    USE, INTRINSIC :: IEEE_ARITHMETIC
    
    CHARACTER, INTENT(IN) :: namefile*64
    REAL(8), INTENT(IN) :: minx, maxx, miny, maxy
    INTEGER(4), INTENT(OUT) :: datan
    !
    REAL(8) :: DLOG_FAC, nan
    INTEGER(4) :: i, j
    REAL(8), ALLOCATABLE, DIMENSION(:,:) :: adata_tmp

    ! Initialize
    datan = 0
    nan = IEEE_VALUE(nan, IEEE_QUIET_NAN)

    ! Open file and read
    OPEN(10,file=namefile,status='old')
    READ(10,*) nx, ny
    ALLOCATE(adata(nx,ny))
    ALLOCATE(adata_tmp(ny,nx))
    READ(10,*) adata_tmp
    CLOSE(10)
    adata = TRANSPOSE(adata_tmp)
    DEALLOCATE(adata_tmp)

    
    !write(*,*) adata(1,:)
    !write(*,*) adata(2,:)
    !write(*,*) adata(3,:)
    !pause

    ! Substitute infinite values with NaNs
    WHERE (.NOT.ieee_is_finite(adata))
       adata = nan
    END WHERE



    ! Count the available data
    !datan = COUNT(ieee_is_finite(adata)) 

    ! Calculation of the constant part of the likelihood with Poisson distribution
    DO i=1,nx
       DO j=1,ny
          IF (.NOT.isnan(adata(i,j))) THEN
             datan = datan + 1
             const_ll = const_ll - DLOG_FAC(INT(adata(i,j)))
          END IF
       END DO
    END DO
  
  END SUBROUTINE READ_FILE_COUNTS_2D

  !#####################################################################################################################

  

  SUBROUTINE INIT_FUNCTIONS()
    ! Subroutine to initialize the user functions if needed

    ! Initialise functions if needed
    IF (set_yn.EQ.'n'.OR.set_yn.EQ.'N') THEN
       IF(funcname.EQ.'ROCKING_CURVE') THEN
          ! Passing as argument the smoothing factors to be adjusted case by case
          ! Suggestion for the values: s between m-sqrt(2*m),m+sqrt(2*m)
          ! with m the number of points
          CALL INIT_ROCKING(par_in(6),par_in(7))
       ELSE IF(funcname.EQ.'TWO_INTERP_VOIGT_POLY'&
            .OR.funcname.EQ.'TWO_INTERP_VOIGT_POLY_X0') THEN
          ! Passing as argument the smoothing factors to be adjusted case by case
          ! Suggestion for the values: s between m-sqrt(2*m),m+sqrt(2*m)
          ! with m the number of points
          CALL INIT_TWO_INTERP(par_in(14),par_in(15))
       ELSE IF(funcname.EQ.'THREE_INTERP_VOIGT_POLY') THEN
          ! Passing as argument the smoothing factors to be adjusted case by case
          ! Suggestion for the values: s between m-sqrt(2*m),m+sqrt(2*m)
          ! with m the number of points
          CALL INIT_THREE_INTERP(par_in(12),par_in(13),par_in(14))
       ELSE IF(funcname.EQ.'SIX_GAUSS_SHIRLEYBG'&
            .OR.funcname.EQ.'SIX_VOIGT_SHIRLEYBG'&
            .OR.funcname.EQ.'SIX_VOIGT_PARA_SHIRBG_SIG_PLEIADES'&
            .OR.funcname.EQ.'SIX_VOIGT_PARAMETER_SHIRLEYBG_PLEIADES') THEN
          CALL INIT_SHIRLEY(ndata,x(:,1),nc(:,1))
       END IF
    ELSE
       IF(funcname.EQ.'ROCKING_CURVE_SET') THEN
          ! Passing as argument the smoothing factors to be adjusted case by case
          ! Suggestion for the values: s between m-sqrt(2*m),m+sqrt(2*m)
          ! with m the number of points
          CALL INIT_ROCKING_SET(par_in(9),par_in(10),par_in(11),par_in(12))
       END IF
    END IF

  END SUBROUTINE INIT_FUNCTIONS
  
!#####################################################################################################################

  REAL(8) FUNCTION LOGLIKELIHOOD(par)
    ! Main likelihood function

    REAL(8), DIMENSION(npar), INTENT(IN) :: par

    IF (data_type(1:1).EQ.'1') THEN
       LOGLIKELIHOOD = LOGLIKELIHOOD_1D(par)
    ELSE IF (data_type(1:1).EQ.'2') THEN
       LOGLIKELIHOOD = LOGLIKELIHOOD_2D(par)
    END IF



  END FUNCTION LOGLIKELIHOOD

  !#####################################################################################################################

  REAL(8) FUNCTION LOGLIKELIHOOD_1D(par)

    ! Main likelihood function
    ! Type: Poisson , Gaussian , .... soon 2D I hope

    REAL(8), DIMENSION(npar), INTENT(IN) :: par
    !
    REAL(8) :: USERFCN, USERFCN_SET
    REAL(8) :: ll_tmp, enc
    INTEGER(4) :: i=0, k=0


    ! Calculate LIKELIHOOD
    ll_tmp = 0.

    IF (set_yn.EQ.'n'.OR.set_yn.EQ.'N') THEN
       ! No set --------------------------------------------------------------------------------------------------------
       k=1
       IF (data_type.EQ.'1c') THEN
          !$OMP PARALLEL DO PRIVATE(enc) REDUCTION(+:ll_tmp)
          DO i=1, ndata_set(k)
             ! Poisson distribution calculation --------------------------------------------------
             enc = USERFCN(x(i,k),npar,par,funcname)
             IF (nc(i,k).EQ.0..AND.enc.GT.0.) THEN
                ll_tmp = ll_tmp - enc
             ELSE IF(nc(i,k).GT.0..AND.enc.GT.0.) THEN
                !ll_tmp(i) = nc(i)*DLOG(enc) - enc - DFACTLN(INT(nc(i)))
                ll_tmp = ll_tmp + nc(i,k)*DLOG(enc) - enc
             ELSE IF(nc(i,k).GT.0..AND.enc.LE.0.) THEN
                WRITE(*,*) 'LIKELIHOOD ERROR: put a background in your function'
                WRITE(*,*) 'number of counts different from 0, model prediction equal 0 or less'
                STOP
             END IF
          END DO
          !$OMP END PARALLEL DO
       ELSE IF (data_type.EQ.'1c') THEN
          !$OMP PARALLEL DO PRIVATE(i,enc) REDUCTION(+:ll_tmp)
          DO i=1, ndata_set(k)
             ! Normal (Gaussian) distribution calculation --------------------------------------
             enc = USERFCN(x(i,k),npar,par,funcname)
             ll_tmp = ll_tmp - (nc(i,k) - enc)**2/(2*nc_err(i,k)**2)
          ENDDO
          !$OMP END PARALLEL DO
       END IF
    ELSE
       ! Set ----------------------------------------------------------------------------------------------------------
       DO k=1,nset
          IF (data_type.EQ.'1c') THEN
             !$OMP PARALLEL DO PRIVATE(i,enc) REDUCTION(+:ll_tmp)
             DO i=1, ndata_set(k)
                ! Poisson distribution calculation --------------------------------------------------
                enc = USERFCN_SET(x(i,k),npar,par,funcname,k)
                IF (nc(i,k).EQ.0..AND.enc.GT.0.) THEN
                   ll_tmp = ll_tmp - enc
                ELSE IF(nc(i,k).GT.0..AND.enc.GT.0.) THEN
                   !ll_tmp(i) = nc(i)*DLOG(enc) - enc - DFACTLN(INT(nc(i)))
                   ll_tmp = ll_tmp + nc(i,k)*DLOG(enc) - enc
                ELSE IF(nc(i,k).GT.0..AND.enc.LE.0.) THEN
                   WRITE(*,*) 'LIKELIHOOD ERROR: put a background in your function'
                   WRITE(*,*) 'number of counts different from 0, model prediction equal 0 or less'
                   STOP
                END IF
             END DO
             !$OMP END PARALLEL DO
          ELSE
             !$OMP PARALLEL DO PRIVATE(i,enc) REDUCTION(+:ll_tmp)
             DO i=1, ndata_set(k)
                ! Normal (Gaussian) distribution calculation --------------------------------------
                enc = USERFCN_SET(x(i,k),npar,par,funcname,k)
                ll_tmp = ll_tmp - (nc(i,k) - enc)**2/(2*nc_err(i,k)**2)

             ENDDO
             !$OMP END PARALLEL DO
          END IF
       END DO
    END IF
    !
    ! Sum all together
    LOGLIKELIHOOD_1D = ll_tmp + const_ll


  END FUNCTION LOGLIKELIHOOD_1D

  !###################################################################################################

  REAL(8) FUNCTION LOGLIKELIHOOD_2D(par)
    
    ! Main likelihood function for 2D data
    ! Type: Poisson only for the moment

    REAL(8), DIMENSION(npar), INTENT(IN) :: par
    !
    REAL(8) :: USERFCN_2D
    REAL(8) :: ll_tmp, enc, x, y
    INTEGER(4) :: i=0, j=0
    

    ! Calculate LIKELIHOOD
    ll_tmp = 0.

    !$OMP PARALLEL DO PRIVATE(enc) REDUCTION(+:ll_tmp)
    DO i=1, nx
       DO j=1, ny
          ! Poisson distribution calculation --------------------------------------------------
          x = i - 0.5 ! Real coordinates are given by the bins, the center of the bin.
          y = j - 0.5 
          enc = USERFCN_2D(x,y,npar,par,funcname)
          IF (isnan(adata(i,j))) THEN
             CYCLE
          ELSE IF (adata(i,j).EQ.0..AND.enc.GT.0.) THEN
             ll_tmp = ll_tmp - enc
          ELSE IF(adata(i,j).GT.0..AND.enc.GT.0.) THEN
             ll_tmp = ll_tmp + adata(i,j)*DLOG(enc) - enc
          ELSE IF(adata(i,j).GT.0..AND.enc.LE.0.) THEN
             WRITE(*,*) 'LIKELIHOOD ERROR: put a background in your function'
             WRITE(*,*) 'number of counts different from 0, model prediction equal 0 or less'
             WRITE(*,*) 'Function value = ', enc, ' n. counts = ', adata(i,j)
             STOP
          END IF
          
       END DO
    END DO
    !$OMP END PARALLEL DO
    
    ! Sum all together
    LOGLIKELIHOOD_2D = ll_tmp + const_ll

    !write(*,*)  LOGLIKELIHOOD_2D

  END FUNCTION LOGLIKELIHOOD_2D

  !#####################################################################################################################

  SUBROUTINE FINAL_LIKELIHOOD(live_max,par_mean,par_median_w)
    ! Final action for the likelihood function

    REAL(8), DIMENSION(npar), INTENT(IN) :: live_max, par_mean, par_median_w

    ! Write auxiliar files for plots
    IF (data_type(1:1).EQ.'1') THEN
       CALL WRITE_EXPECTED_VALUES(live_max,par_mean,par_median_w)
    ELSE IF (data_type(1:1).EQ.'2') THEN
       CALL WRITE_EXPECTED_VALUES_2D(live_max,par_mean,par_median_w)
    END IF

    ! Deallocate variables
    CALL DEALLOCATE_DATA()

  END SUBROUTINE FINAL_LIKELIHOOD


  !#####################################################################################################################
  SUBROUTINE WRITE_EXPECTED_VALUES(live_max,par_mean,par_median_w)
    ! Write all auxiliar files for plots and co.

    REAL(8), DIMENSION(npar), INTENT(IN) :: live_max, par_mean, par_median_w
    !
    INTEGER(8), PARAMETER :: maxfit = 10000
    REAL(8) :: USERFCN, USERFCN_SET
    REAL(8) :: minx=0., maxx=0., xfit=0., dx=0., enc = 0.
    INTEGER(4) :: i=0, k=0
    ! Stuff to save separate components
    LOGICAL :: plot = .false.
    REAL(8) :: yfit
    CHARACTER :: out_filename*64

    COMMON /func_plot/ plot

    !-----------------------Calculate the expected function values -------------------------

    ! Calculate the expected function value and residual for max likelihood
    IF (set_yn.EQ.'n'.OR.set_yn.EQ.'N') THEN
       k=1
       enc = 0.
       ! max likelihood values -------------------------------------------------------------
       OPEN (UNIT=20, FILE='nf_output_data_max.dat', STATUS='unknown')
       WRITE(20,*)'# x    y data    y theory      y diff    y err'
       DO i=1, ndata
          enc = USERFCN(x(i,k),npar,live_max,funcname)
          IF (data_type.EQ.'1e') THEN
             WRITE(20,*) x(i,k), ' ',nc(i,k), ' ',enc, ' ',nc(i,k)-enc, ' ', nc_err(i,k)
          ELSE IF (data_type.EQ.'1c') THEN
             WRITE(20,*) x(i,k), ' ',nc(i,k), ' ',enc, ' ',nc(i,k)-enc, ' ', sqrt(nc(i,k))
          END IF
       END DO
       CLOSE(20)
       ! mean values ------------------------------------------------------------------------
       OPEN (UNIT=20, FILE='nf_output_data_mean.dat', STATUS='unknown')
       WRITE(20,*)'# x    y data    y theory      y diff    y err'
       DO i=1, ndata
          enc = USERFCN(x(i,k),npar,par_mean,funcname)
          IF (data_type.EQ.'1e') THEN
             WRITE(20,*) x(i,k), ' ',nc(i,k), ' ',enc, ' ',nc(i,k)-enc, ' ', nc_err(i,k)
          ELSE IF (data_type.EQ.'1c') THEN
             WRITE(20,*) x(i,k), ' ',nc(i,k), ' ',enc, ' ',nc(i,k)-enc, ' ', sqrt(nc(i,k))
          END IF
       END DO
       CLOSE(20)
       ! median values ----------------------------------------------------------------------
       OPEN (UNIT=20, FILE='nf_output_data_median.dat', STATUS='unknown')
       WRITE(20,*)'# x    y data    y theory      y diff    y err'
       DO i=1, ndata
          enc = USERFCN(x(i,k),npar,par_median_w,funcname)
          IF (data_type.EQ.'1e') THEN
             WRITE(20,*) x(i,k), ' ',nc(i,k), ' ',enc, ' ',nc(i,k)-enc, ' ', nc_err(i,k)
          ELSE IF (data_type.EQ.'1c') THEN
             WRITE(20,*) x(i,k), ' ',nc(i,k), ' ',enc, ' ',nc(i,k)-enc, ' ', sqrt(nc(i,k))
          END IF
       END DO
       CLOSE(20)
       ! -------------------------------------------------------------------------------------
    ELSE
       DO k=1, nset
          enc = 0.
          ! max likelihood values ------------------------------------------------------------
          WRITE(out_filename,1000) 'nf_output_data_max_',k,'.dat'
          OPEN (UNIT=30, FILE=out_filename, STATUS='unknown')
          WRITE(30,*)'# x    y data    y theory      y diff    y err'
          DO i=1, ndata_set(k)
             enc = USERFCN_SET(x(i,k),npar,live_max,funcname,k)
             IF (data_type.EQ.'1e') THEN
                WRITE(30,*) x(i,k), ' ',nc(i,k), ' ',enc, ' ',nc(i,k)-enc, ' ',  nc_err(i,k)
             ELSE IF (data_type.EQ.'1c') THEN
                WRITE(30,*) x(i,k), ' ',nc(i,k), ' ',enc, ' ',nc(i,k)-enc, ' ', sqrt(nc(i,k))
             END IF
          END DO
          CLOSE(30)
          ! mean values ----------------------------------------------------------------------
          WRITE(out_filename,2000) 'nf_output_data_mean_',k,'.dat'
          OPEN (UNIT=30, FILE=out_filename, STATUS='unknown')
          WRITE(30,*)'# x    y data    y theory      y diff    y err'
          DO i=1, ndata_set(k)
             enc = USERFCN_SET(x(i,k),npar,live_max,funcname,k)
             IF (data_type.EQ.'1e') THEN
                WRITE(30,*) x(i,k), ' ',nc(i,k), ' ',enc, ' ',nc(i,k)-enc, ' ',  nc_err(i,k)
             ELSE IF (data_type.EQ.'1c') THEN
                WRITE(30,*) x(i,k), ' ',nc(i,k), ' ',enc, ' ',nc(i,k)-enc, ' ', sqrt(nc(i,k))
             END IF
          END DO
          CLOSE(30)
          ! median values ---------------------------------------------------------------------
          WRITE(out_filename,3000) 'nf_output_data_median_',k,'.dat'
          OPEN (UNIT=30, FILE=out_filename, STATUS='unknown')
          WRITE(30,*)'# x    y data    y theory      y diff    y err'
          DO i=1, ndata_set(k)
             enc = USERFCN_SET(x(i,k),npar,live_max,funcname,k)
             IF (data_type.EQ.'1e') THEN
                WRITE(30,*) x(i,k), ' ',nc(i,k), ' ',enc, ' ',nc(i,k)-enc, ' ',  nc_err(i,k)
             ELSE IF (data_type.EQ.'1c') THEN
                WRITE(30,*) x(i,k), ' ',nc(i,k), ' ',enc, ' ',nc(i,k)-enc, ' ', sqrt(nc(i,k))
             END IF
          END DO
          CLOSE(30)
          ! ----------------------------------------------------------------------------------
       END DO
    END IF

    ! Save in a file the different fit components !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! New from version 1.0: save three pairs of files with values from max likelihood, mean and median of the parameters
    IF (set_yn.EQ.'n'.OR.set_yn.EQ.'N') THEN
       maxx = xmax(1)
       minx = xmin(1)
       plot = .true.
       xfit = 0.
       dx=(maxx-minx)/(maxfit-1)

       ! max likelihood values
       OPEN (UNIT=40, FILE='nf_output_fit_max.dat', STATUS='unknown')
       WRITE(40,*)'# x    y fit'
       DO i=1, maxfit
          xfit = minx + (i-1)*dx
          yfit = USERFCN(xfit,npar,live_max,funcname)
       ENDDO
       CLOSE(40)

       ! mean values
       OPEN (UNIT=40, FILE='nf_output_fit_mean.dat', STATUS='unknown')
       WRITE(40,*)'# x    y fit'
       DO i=1, maxfit
          xfit = minx + (i-1)*dx
          yfit = USERFCN(xfit,npar,par_mean,funcname)
       ENDDO
       CLOSE(40)

       ! mean values
       OPEN (UNIT=40, FILE='nf_output_fit_median.dat', STATUS='unknown')
       WRITE(40,*)'# x    y fit'
       DO i=1, maxfit
          xfit = minx + (i-1)*dx
          yfit = USERFCN(xfit,npar,par_median_w,funcname)
       ENDDO
       CLOSE(40)
    ELSE
       DO k=1, nset
          maxx = xmax(k)
          minx = xmin(k)
          plot = .true.
          xfit = 0.
          dx=(maxx-minx)/(maxfit-1)

          ! max likelihood values
          WRITE(out_filename,1001) 'nf_output_fit_max_',k,'.dat'
          OPEN (UNIT=40, FILE=out_filename, STATUS='unknown')
          WRITE(40,*)'# x    y fit'
          DO i=1, maxfit
             xfit = minx + (i-1)*dx
             !WRITE(30, *) xfit, USERFCN_SET(xfit,npar,live_max,funcname,k)
             yfit = USERFCN_SET(xfit,npar,live_max,funcname,k)
          ENDDO
          CLOSE(40)

          WRITE(out_filename,2001) 'nf_output_fit_mean_',k,'.dat'
          OPEN (UNIT=40, FILE=out_filename, STATUS='unknown')
          WRITE(40,*)'# x    y fit'
          DO i=1, maxfit
             xfit = minx + (i-1)*dx
             !WRITE(30, *) xfit, USERFCN_SET(xfit,npar,live_max,funcname,k)
             yfit = USERFCN_SET(xfit,npar,par_mean,funcname,k)
          ENDDO
          CLOSE(40)

          WRITE(out_filename,3001) 'nf_output_fit_median_',k,'.dat'
          WRITE(40,*)'# x    y fit'
          OPEN (UNIT=40, FILE=out_filename, STATUS='unknown')
          DO i=1, maxfit
             xfit = minx + (i-1)*dx
             !WRITE(30, *) xfit, USERFCN_SET(xfit,npar,live_max,funcname,k)
             yfit = USERFCN_SET(xfit,npar,par_median_w,funcname,k)
          ENDDO
          CLOSE(40)
       END DO
    END IF


1000 FORMAT (A19,I1,A4)
1001 FORMAT (A18,I1,A4)
2000 FORMAT (A20,I1,A4)
2001 FORMAT (A19,I1,A4)
3000 FORMAT (A22,I1,A4)
3001 FORMAT (A21,I1,A4)

  END SUBROUTINE WRITE_EXPECTED_VALUES

    !#####################################################################################################################
  SUBROUTINE WRITE_EXPECTED_VALUES_2D(live_max,par_mean,par_median_w)
    ! Write all auxiliar files for plots and co. in 2D
    
    USE, INTRINSIC :: IEEE_ARITHMETIC

    REAL(8), DIMENSION(npar), INTENT(IN) :: live_max, par_mean, par_median_w
    !
    INTEGER(8), PARAMETER :: maxfit = 10000
    REAL(8) :: USERFCN_2D
    REAL(8) :: minx=0., maxx=0., enc = 0.
    INTEGER(4) :: i=0, j=0, k=0
    ! Stuff to save separate components
    LOGICAL :: plot = .false.
    CHARACTER :: out_filename*64
    REAL(8), DIMENSION(nx,ny) :: aenc, ares
    REAL(8) :: x, y, nan

    COMMON /func_plot/ plot

    nan = IEEE_VALUE(nan, IEEE_QUIET_NAN)
    aenc = 0.
    ares = 0.


    IF (set_yn.EQ.'n'.OR.set_yn.EQ.'N') THEN
       k=1
       ! Rewrite data only -------------------------------------------------------------
       OPEN (UNIT=20, FILE='nf_output_data_2D.dat', STATUS='unknown')
       WRITE(20,*)'# matrix dimensions: ', nx, ny
       IF (data_type.EQ.'2c') THEN
          DO i=1,nx
             WRITE(20,*) adata(i,:)
          END DO
       END IF
       CLOSE(20)
       ! Write function and residuals -------------------------------------------------------------
       DO i=1, nx
          DO j=1, ny
             IF (.NOT.isnan(adata(i,j))) THEN
                ! Poisson distribution calculation --------------------------------------------------
                x = i - 0.5 ! Real coordinates are given by the bins, the center of the bin.
                y = j - 0.5 
                aenc(i,j) = USERFCN_2D(x,y,npar,live_max,funcname)
                ares(i,j) = adata(i,j) - aenc(i,j)
             ELSE
                aenc(i,j) = nan
                ares(i,j) = nan
             END IF
          END DO
       END DO
       
       OPEN (UNIT=20, FILE='nf_output_fit_max_2D.dat', STATUS='unknown')
       OPEN (UNIT=30, FILE='nf_output_fitres_max_2D.dat', STATUS='unknown')
       WRITE(20,*)'# matrix dimensions: ', nx, ny
       IF (data_type.EQ.'2c') THEN
          DO i=1,nx
             WRITE(20,*) aenc(i,:)
             WRITE(30,*) ares(i,:)
          END DO
       END IF
       CLOSE(20)
       CLOSE(30)
    END IF

  END SUBROUTINE WRITE_EXPECTED_VALUES_2D


  !#####################################################################################################################

  SUBROUTINE DEALLOCATE_DATA()

    IF (data_type.EQ.'1c') THEN
       DEALLOCATE(x,nc)
    ELSE IF (data_type.EQ.'1e') THEN
       DEALLOCATE(x,nc,nc_err)
    ELSE IF (data_type.EQ.'2c') THEN
       DEALLOCATE(adata)
    END IF

  END SUBROUTINE DEALLOCATE_DATA

  !#####################################################################################################################



END MODULE MOD_LIKELIHOOD

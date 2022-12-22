SUBROUTINE NESTED_SAMPLING(itry,maxstep,nall,evsum_final,live_like_final,weight,&
     live_final,live_like_max,live_max,mpi_rank,mpi_cluster_size)
  ! Time-stamp: <Last changed by martino on Monday 03 May 2021 at CEST 11:56:05>
  ! For parallel tests only
  !SUBROUTINE NESTED_SAMPLING(irnmax,rng,itry,ndata,x,nc,funcname,&
  !   npar,par_fix,par_step,par_in,par_bnd1,par_bnd2,nlive,evaccuracy,sdfraction,&
  !   nall,evsum_final,live_like_final,weight,live_final,live_like_max,live_max)
  !!USE OMP_LIB
  !USE RNG

  USE MPI
  USE MOD_MPI

  ! Additional math module
  USE MOD_MATH

  ! Parameter module
  USE MOD_PARAMETERS, ONLY:  nlive, evaccuracy, search_par2, par_in, par_step, par_bnd1, par_bnd2, par_fix, search_method, ntry
  ! Module for likelihood
  USE MOD_LIKELIHOOD
  ! Module for searching new live points
  USE MOD_SEARCH_NEW_POINT
  ! Module for cluster analysis
  USE MOD_CLUSTER_ANALYSIS, ONLY: cluster_on
  ! Module for the metadata
  USE MOD_METADATA

  !
  IMPLICIT NONE
  INTEGER(4), INTENT(IN) :: itry, maxstep
  INTEGER(4), INTENT(OUT) :: nall
  REAL(8), INTENT(OUT) :: evsum_final, live_like_max
  REAL(8), INTENT(OUT), DIMENSION(npar) :: live_max
  REAL(8), INTENT(OUT), DIMENSION(maxstep) :: weight
  REAL(8), INTENT(OUT), DIMENSION(maxstep) :: live_like_final
  REAL(8), INTENT(OUT), DIMENSION(maxstep,npar) :: live_final
  INTEGER(4), INTENT(IN) :: mpi_rank, mpi_cluster_size
  ! Random number variables
  !INTEGER, INTENT(IN) :: irnmax
  !REAL(8), INTENT(IN), DIMENSION(irnmax) :: rng
  ! Prior variables
  REAL(8), DIMENSION(npar) :: par_prior
  ! Loop variables
  INTEGER(4) :: nstep = 2
  REAL(8), DIMENSION(maxstep) :: tstep, tlnmass, tlnrest, evstep
  REAL(8) :: live_like_new
  REAL(8), DIMENSION(npar) :: live_new
  REAL(8), DIMENSION(maxstep) :: live_like_old
  REAL(8), DIMENSION(maxstep,npar) :: live_old
  REAL(8) :: evsum = 0., evrestest = 0., evtotest = 0.
  ! Search variable
  INTEGER(4) :: icluster=0, icluster_old=0, ntries=0
  LOGICAL :: too_many_tries = .false.
  ! Live points variables
  REAL(8) :: min_live_like = 0.
  REAL(8), DIMENSION(nlive) :: live_like
  REAL(8), DIMENSION(nlive,npar) :: live
  ! Final calculations
  INTEGER(4) :: nstep_final
  REAL(8) :: last_likes, live_like_last, evrest_last, evlast
  ! Rest
  INTEGER(4) :: i,j, l, n, jlim
  REAL(8) :: ADDLOG, RANDN, rn
  CHARACTER :: out_filename*64
  INTEGER(4) :: n_call_cluster
  REAL(8) :: MOVING_AVG
  
  ! MPI Stuff
  REAL(8) :: moving_eff_avg = 0.
  INTEGER(4) :: processor_name_size
  INTEGER(4) :: mpi_ierror
  INTEGER(4) :: mpi_child_spawn_error(1)
  character(LEN=MPI_MAX_PROCESSOR_NAME) :: processor_name
  CHARACTER :: info_string*256
  CHARACTER :: lines*20
  
  EXTERNAL :: SORTN

  ! Initialize variables (with different seeds for different processors)
  par_prior = 0.
  live = 0.
  live_like = 0.
  live_new = 0.
  live_like_new = 0.
  live_old = 0.
  live_like_old = 0.
  live_final = 0.
  live_like_final = 0.
  tstep = 0.
  tlnmass = 0.
  tlnrest = 0.
  evstep = 0.
  nstep = maxstep - nlive + 1

  n_call_cluster=0

  ! ---------- Inintial live points sorting ------------------------------------------------
  WRITE(*,*) 'Sorting live points. N. of points = ', nlive
  DO j=1, nlive


     ! Sort point considering priors
     ! Generate random prior if not fixe
     DO l=1,npar
        IF(par_fix(l).NE.1) THEN
           ! Uniform prior inside the limits
           IF (par_step(l).LE.0.) THEN
801           CALL RANDOM_NUMBER(rn)
              par_prior(l) = par_bnd1(l) + (par_bnd2(l)-par_bnd1(l))*rn
              IF(par_prior(l).LT.par_bnd1(l).OR.par_prior(l).GT.par_bnd2(l)) GOTO 801
           ELSE
              ! Gaussian distrigution
700           par_prior(l) = par_in(l) + par_step(l)*RANDN
              IF(par_prior(l).LT.par_bnd1(l).OR.par_prior(l).GT.par_bnd2(l)) GOTO 700
           END IF
        ELSE
           ! If fixed, take the value indicated in the file
           par_prior(l) = par_in(l)
        END IF
     END DO
     ! If it is good, take it
     live(j,:) = par_prior(:)
     live_like(j) = LOGLIKELIHOOD_WITH_TEST(par_prior)
  END DO

  ! Calculate average and standard deviation of the parameters

  ! Order livepoints
  CALL SORTN(nlive,npar,live_like,live)
  ! Store in a file
  OPEN(11,FILE='nf_output_initial_live_points.dat',STATUS= 'UNKNOWN')
  WRITE(11,*) '# n     lnlikelihood     parameters'
  DO j=1,nlive
     WRITE(11,*) j, live_like(j), live(j,:)
  END DO
  CLOSE(11)


  ! --------- Set-up stuff before main loop ------------------------------------------------
  ! Calculate the intervarls (logarithmic) for the integration
  ! This is equivalent to the "crude" approximation from Skilling
  IF (maxstep/nlive.GT.700.AND.mpi_rank.EQ.0) THEN
     WRITE(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
     WRITE(*,*) '!!! Attention, too few nlive points for too many maxstep !!!'
     WRITE(*,*) '!!! Step volumes cannot be calculated correctly          !!!'
     WRITE(*,*) '!!! Change your input file                               !!!'
     WRITE(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
     IF(parallel_mpi_on) THEN
         CALL MPI_Abort(MPI_COMM_WORLD, 1, mpi_ierror)
     ENDIF
     STOP
  END IF
  DO l=1, maxstep
     tstep(l) = DEXP(-DFLOAT(l)/DFLOAT(nlive))
  ENDDO

  ! Calculate the first differential prior mass and the remaining prior mass
  tlnmass(1) = DLOG(-(tstep(1)-1))
  tlnrest(1) = DLOG(tstep(1))

  ! Actual minimum loglikelihood and correspondent live point
  live_like_old(1) = live_like(1)
  live_old(1,:) = live(1,:)
  ! First evidence (sum because we are working with logarithms)
  evstep(1) = live_like_old(1) + tlnmass(1)
  evsum = evstep(1)


  ! If MPI is active, spawn the writter process
  IF(parallel_mpi_on) THEN
      CALL MPI_Comm_spawn(nf_child_proc_name, MPI_ARGV_NULL, 1, MPI_INFO_NULL, 0, MPI_COMM_WORLD, mpi_child_writter_comm, mpi_child_spawn_error, mpi_ierror)
      IF(mpi_rank.EQ.0) THEN
            CALL MPI_SEND(mpi_cluster_size, 1, MPI_INT, 0, 0, mpi_child_writter_comm, mpi_ierror)
      ENDIF
      CALL MPI_Get_processor_name(processor_name, processor_name_size, mpi_ierror)
      processor_name = TRIM(ADJUSTL(processor_name))
      CALL MPI_BARRIER(MPI_COMM_WORLD, mpi_ierror)
  ENDIF


  ! ---------------------------------------------------------------------------------------!
  !                                                                                        !
  !                                 START THE MAIN LOOP                                    !
  !                                                                                        !
  !----------------------------------------------------------------------------------------!

  DO n=2, nstep
     ! trapezoidal rule applied here (Skilling Entropy 2006)
     tlnmass(n) = DLOG(-(tstep(n+1)-tstep(n-1))/2.d0)
     tlnrest(n) = DLOG((tstep(n+1)+tstep(n-1))/2.d0)

     ! Present minimal value of the likelihood
     min_live_like = live_like(1)

     ! ##########################################################################
     ! Find a new live point

     

500  CALL SEARCH_NEW_POINT(n,itry,min_live_like,live_like,live, &
          live_like_new,live_new,icluster,ntries,too_many_tries,n_call_cluster)
     IF (too_many_tries) THEN
        IF(parallel_mpi_on) THEN
           ! Signal final data MAXED_OUT
           CALL MPI_Send(info_string, 256, MPI_CHARACTER, 0, MPI_TAG_SEARCH_DONE_MANY_TRIES, mpi_child_writter_comm, mpi_ierror)
        ENDIF
        nstep_final = n - 1
        GOTO 601
     END IF
     ! ##########################################################################

     ! Reorder found point (no parallel here) and make the required calculation for the evidence
     ! Reorder point
     ! Order and exclude last point
     DO j=1,nlive-1
        IF (live_like_new.GT.live_like(j).AND.live_like_new.LT.live_like(j+1)) THEN
           jlim = j
        ELSE IF (live_like_new.GT.live_like(nlive)) THEN
           jlim = nlive
        END IF
     END DO

     ! Store old values
     live_like_old(n) = live_like(1)
     live_old(n,:) = live(1,:)

     IF (jlim.LT.1.OR.jlim.GT.nlive) THEN
        WRITE(*,*) 'Problem in the search method, or in the calculations'
        WRITE(*,*) 'No improvement in the likelihood value after finding the new point'
        WRITE(*,*) 'j = ', jlim, 'old min like = ', min_live_like, 'new min like = ', live_like_new
        STOP
     ELSE IF (jlim.EQ.1) THEN
        live_like(1) = live_like_new
        live(1,:) = live_new(:)
     ELSE
        ! Shift values
        live_like(1:jlim-1) =  live_like(2:jlim)
        live(1:jlim-1,:) =  live(2:jlim,:)
        ! Insert new value
        live_like(jlim) =  live_like_new
        live(jlim,:) =  live_new
        ! The rest stay as it is
     END IF


     ! Assign to the new point, the same cluster number of the start point
     IF (cluster_on) THEN
        ! Instert new point
        p_cluster(1:jlim-1) =  p_cluster(2:jlim)
        p_cluster(jlim) = icluster
        cluster_np(icluster) = cluster_np(icluster) + 1
        ! Take out old point
        icluster_old = p_cluster(1)
        cluster_np(icluster_old) = cluster_np(icluster_old) - 1
        ! Call cluster module to recalculate the std of the considered cluster and the cluster of the discarted point
        CALL REMAKE_CLUSTER_STD(live,icluster,icluster_old)
     END IF

     ! Calculate the evidence for this step
     evstep(n) = live_like_old(n) + tlnmass(n)

     ! Sum the evidences
     evsum = ADDLOG(evsum,evstep(n))

     ! Check if the estimate accuracy is reached
     evrestest = live_like(nlive) + tlnrest(n)
     evtotest = ADDLOG(evsum,evrestest)

     IF (evtotest-evsum.LT.evaccuracy) GOTO 301
     
     moving_eff_avg = MOVING_AVG(search_par2/ntries)
     IF (MOD(n,100).EQ.0) THEN
         IF(parallel_mpi_on) THEN
            WRITE(info_string,21) processor_name, itry+1, n, min_live_like, evsum, evstep(n), evtotest-evsum, &
                                  moving_eff_avg
21          FORMAT('| Machine: ', A10, ' | N. try: ', I2, ' | N. step: ', I10, ' | Min. loglike: ', F23.15, ' | Evidence: ', F23.15, &
                   ' | Ev. step: ', F23.15, ' | Ev. pres. acc.: ', ES13.7, ' | Avg eff.: ', F6.4, ' |')
            CALL MPI_Send(info_string, 256, MPI_CHARACTER, 0, MPI_TAG_SEARCH_STATUS, mpi_child_writter_comm, mpi_ierror)
         ELSE
            WRITE(info_string,23) itry, n, min_live_like, evsum, evstep(n), evtotest-evsum, search_par2/ntries
23          FORMAT('| N. try: ', I2, ' | N. step: ', I10, ' | Min. loglike: ', F23.15, ' | Evidence: ', F23.15, &
                   ' | Ev. step: ', F23.15, ' | Ev. pres. acc.: ', ES13.7, ' | Typical eff.: ', F6.4, ' |')
            WRITE(*,24) info_string
24          FORMAT(A220)
         ENDIF
     END IF
  END DO

  ! ---------------------------------------------------------------------------------------!
  !                                                                                        !
  !                                 STOP THE MAIN LOOP                                     !
  !                                                                                        !
  !----------------------------------------------------------------------------------------!
301 CONTINUE

  IF((evtotest-evsum).GE.evaccuracy) THEN
     IF(parallel_mpi_on) THEN
         ! Signal final data MAXED_OUT
         CALL MPI_Send(info_string, 256, MPI_CHARACTER, 0, MPI_TAG_SEARCH_ERROR_MAXED_OUT, mpi_child_writter_comm, mpi_ierror)
     ENDIF
  ELSE
      IF(parallel_mpi_on) THEN
         ! Signal final data OK
         CALL MPI_Send(info_string, 256, MPI_CHARACTER, 0, MPI_TAG_SEARCH_DONE_OK, mpi_child_writter_comm, mpi_ierror)
      ENDIF
  END IF

  ! Store the number of steps
  nstep_final = n

  ! Store steps in a file
  !OPEN(11,FILE='tsteps.dat',STATUS= 'UNKNOWN')
  !WRITE(11,*) '# tstep  tlnmass    tlnrest'
  !DO j=1, nstep_final
  !   WRITE(11,*) j, tstep(j), tlnmass(j), tlnrest(j)
  !END DO
  !CLOSE(11)

  ! Write status
  !WRITE(*,*) 'N step : ', n, 'Evidence at present : ', evsum

  ! Store actual live points
  !WRITE(out_filename,1000) 'live_points_',itry,'.dat'
  !OPEN(22,FILE=out_filename,STATUS= 'UNKNOWN')
  !WRITE(22,*) '# n     lnlikelihood     parameters'
  !DO l=1,nlive
  !   WRITE(22,*) l, live_like(l), live(l,:)
  !END DO
  !CLOSE(22)
  !
  !   ! Write temporal results in a file
  !WRITE(out_filename,2000) 'status_',itry,'.dat'
  !OPEN(33,FILE=out_filename,STATUS= 'UNKNOWN')
  !WRITE(33,*) 'New last live point : ', live_like(1), live(1,:)
  !WRITE(33,*) 'New first live point : ', live_like(nlive), live(nlive,:)
  !WRITE(33,*) 'N step : ', n, 'Evidence at present : ',evsum, &
  !     'Ev. of the step : ', evstep(n), 'Diff. with the estimate total evidence : ', evtotest-evsum
  !CLOSE(33)
  !

  ! Check the calculation of the evsteps
  !WRITE(out_filename,3000) 'evsteps_',th_num,'.dat'
  !OPEN(44,FILE=out_filename,STATUS= 'UNKNOWN')
  !WRITE(44,*) '# n   lnlikelihood     parameters'
  !DO l=1,nstep_final
  !   WRITE(44,*) l, evstep(l)
  !END DO
  !CLOSE(44)

  !------------ Calculate the total evidence with the last live points ---------------------
601 CONTINUE
  IF(parallel_mpi_on) THEN
      CALL MPI_BARRIER(MPI_COMM_WORLD, mpi_ierror)
  ENDIF

  IF(((evtotest-evsum).GE.evaccuracy).AND.(n.GE.nstep)) THEN
      IF(parallel_mpi_on) THEN
         CALL MPI_Abort(MPI_COMM_WORLD, 1, mpi_ierror)
      ENDIF
      STOP
  ENDIF

  ! Store the last live points
  OPEN(99,FILE='nf_output_last_live_points.dat',STATUS= 'UNKNOWN')
  WRITE(99,*) '# n step =',  n-1
  WRITE(99,*) '# Evidence of the step =', evstep(n-1)
  WRITE(99,*) '# Evidence accuracy =',  ADDLOG(evsum,live_like(nlive) + tlnrest(n-1)) - evsum
  WRITE(99,*) '# n     lnlikelihood     parameters'
  DO j=1,nlive
     WRITE(99,*) j, live_like(j), live(j,:)
  END DO
  CLOSE(99)

  ! Calculate the last evidence
  ! Sum the last loglikelihood  (considering that we are dealing with logs)
  last_likes = live_like(1)
  DO j=2,nlive
     last_likes = ADDLOG(last_likes,live_like(j))
  END DO
  ! Average value of the loglikelihood
  live_like_last = last_likes - DLOG(DFLOAT(nlive))

  ! Evidence of each last points assuming equal volume spacing (EXP(tlnrest)/nlive)
  evlast = live_like_last + tlnrest(nstep_final) - DLOG(DFLOAT(nlive))

  ! The final evidence !!!
  evrest_last = live_like_last + tlnrest(nstep_final)
  evsum_final = ADDLOG(evsum,evrest_last)

  ! Insert the last live points in the ensemble
  live_old(nstep_final+1:nstep_final+nlive,:) = live(:,:)
  live_like_old(nstep_final+1:nstep_final+nlive) = live_like_last
  evstep(nstep_final+1:nstep_final+nlive) = evlast

  ! Total output of the routine (plis evsum_final)
  live_final = live_old
  live_like_final = live_like_old

  nall = nstep_final + nlive


  ! Deallocate variables for cluster analysis
  IF (cluster_on) THEN
     ! Reset cluster analysis
     cluster_on = .FALSE.
     CALL DEALLOCATE_CLUSTER()
  END IF


  !------------ Calculate weights and parameter best values and distributions ----------------

  ! Save the point with the highest likelihood
  live_max = live(nlive,:)
  live_like_max = live_like(nlive)

  ! Calculate the weights with test of overflow
  weight = 0.d0
  DO n=1,nstep_final
     IF(DABS(evstep(n) - evsum_final).LT.700) weight(n) = DEXP(evstep(n) - evsum_final)
  END DO

!1000 FORMAT (A12,I1,A4)
!2000 FORMAT (A7,I1,A4)
!3000 FORMAT (A8,I1,A4)
!4000 FORMAT (A,I3,A,I3,A,ES12.6,A,ES12.6,A,ES12.6,A,ES12.6,A,ES10.2)

  RETURN

END SUBROUTINE NESTED_SAMPLING

!#############################################################################################


! ___________________________________________________________________________________________

FUNCTION ADDLOG(log1,log2)
  IMPLICIT NONE
  REAL(8) :: log1, log2, ADDLOG

  ADDLOG = 0.
  ! Sum of two numbers in logarithmic notation with check of over and underflow

  IF (log1.GT.log2) THEN
     IF((log1-log2).LT.700) THEN
        ADDLOG = log1 + DLOG(1.d0 + DEXP(log2-log1))
     ELSE
        ADDLOG = log1
     END IF
  ELSE
     IF((log2-log1).LT.700) THEN
        ADDLOG = log2 + DLOG(1.d0 + DEXP(log1-log2))
     ELSE
        ADDLOG = log2
     END IF
  END IF

END FUNCTION ADDLOG

! ___________________________________________________________________________________________

FUNCTION MOVING_AVG(val)
   USE MOD_MATH
   IMPLICIT NONE
   REAL(8) :: val, MOVING_AVG

   MOVING_AVG = 0.

   window_array = CSHIFT(window_array, 1)
   window_array(moving_avg_window) = val

   MOVING_AVG = SUM(window_array) / moving_avg_window
END FUNCTION MOVING_AVG
 
 ! ___________________________________________________________________________________________
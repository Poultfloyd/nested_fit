MODULE MOD_USERFCN_SET_F90
    contains
FUNCTION MULTIPLE_VOIGT_SET(x,npar,val,j)
    IMPLICIT NONE
    REAL*8 x,MULTIPLE_VOIGT_SET,VOIGT,POLY
    INTEGER*4 npar,j,nvoigt,nrun,i_var,i_voigt,i_run
    REAL*8 val(npar),valv(4),valp(8)
    REAL*8 xv,ampv,sigma,gammav,xp
    REAL*8, dimension(:),allocatable:: val_voigt
    REAL*8 pi
    PARAMETER(pi=3.141592653589793d0)
    CHARACTER*1 lr
    COMMON /func_exp/ lr
    ! To plot the different components
    LOGICAL plot
    COMMON /func_plot/ plot


    nrun=INT(val(1))
    nvoigt=INT(val(2))

    

    !Where the variables for the jth run are
    i_run=4+(j-1)*5

    !extract variables for the 1st voigt
    xv=val(i_run+1)
    ampv=val(i_run+2)
    sigma=val(3)
    gammav=val(4)

    !create the table for voigt function input
    valv(1)=xv
    valv(2)=ampv
    valv(3)=sigma
    valv(4)=gammav

    !create the table where voigt values are stored
    allocate(val_voigt(nvoigt))
    !Store the value of the first voigt
    val_voigt(1)=VOIGT(x,4,valv)

    !iterations over the number of voigts

    DO i_voigt= 1,nvoigt-1
        !where the variables for the voigts are
        i_var=(i_voigt-1)*3+4+5*nrun
        valv(1)=xv+val(i_var+1)
        valv(2)=ampv*val(i_var+2)
        valv(3)=sigma
        valv(4)=val(i_var+3)
        val_voigt(i_voigt+1)=VOIGT(x,4,valv)
    ENDDO

    !set up polynomial background
    xp=val(i_run+3)
    valp(1)=val(i_run+4)
    valp(2)=val(i_run+5)
    valp(3)=0
    valp(4)=0
    valp(5)=0
    valp(6)=0
    valp(7)=0
    valp(8)=0

    MULTIPLE_VOIGT_SET=SUM(val_voigt)+POLY(x-xp)

    IF(plot) THEN 
            WRITE(40,*) x, MULTIPLE_VOIGT_SET, (val_voigt(i_voigt),i_voigt=1,nvoigt)
    ENDIF


END FUNCTION MULTIPLE_VOIGT_SET
END MODULE  MOD_USERFCN_SET_F90
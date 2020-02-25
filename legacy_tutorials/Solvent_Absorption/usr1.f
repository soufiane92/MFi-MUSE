!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: USR1                                                   C
!  Purpose: This routine is called from the time loop and is           C
!           user-definable.  The user may insert code in this routine  C
!           or call appropriate user defined subroutines.  This        C
!           can be used for setting or checking errors in quantities   C
!           that vary with time.  This routine is not called from an   C
!           IJK loop, hence all indices are undefined.                 C
!                                                                      C
!  Author:                                            Date: dd-mmm-yy  C
!  Reviewer:                                          Date: dd-mmm-yy  C
!                                                                      C
!  Revision Number:                                                    C
!  Purpose:                                                            C
!  Author:                                            Date: dd-mmm-yy  C
!  Reviewer:                                          Date: dd-mmm-yy  C
!                                                                      C
!  Literature/Document References:                                     C
!                                                                      C
!  Variables referenced:                                               C
!  Variables modified:                                                 C
!                                                                      C
!  Local variables:                                                    C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C

      SUBROUTINE USR1

! Modules
!--------------------------------------------------------------------//
      use usr, only: ABSORPTION_CHEM_TYPE_ENUM, EQUILIBRIUM_COUPLED
      use machine, only: WALL_TIME
      use time_cpu, only: WALL_START
      use compar, only: MYPE,PE_IO
      use mpi_utility, only: global_max_0d
      IMPLICIT NONE
! Used for cpu timing
      DOUBLE PRECISION:: TICK, TOCK
! Cumulative cpu spent on chem solver
      DOUBLE PRECISION, SAVE:: CPU_CHEM=0.0
! Max cpu spent on chem solver (across all procs)
      DOUBLE PRECISION:: MAX_CPU_CHEM

! Local variables
!--------------------------------------------------------------------//

! Liquid chemistry equilibrium.
      IF(ABSORPTION_CHEM_TYPE_ENUM == EQUILIBRIUM_COUPLED) THEN

! Solve for equilibrium speciation
         TICK = WALL_TIME()
         CALL LIQUID_CHEM_EQ()
         TOCK = WALL_TIME()

! Update the cpu consumed by chem solver:
         CPU_CHEM = CPU_CHEM + ( TOCK - TICK)
         call global_max_0d(CPU_CHEM,MAX_CPU_CHEM,PE_IO)
         if(MYPE==PE_IO) then
            write(*,1101)'Equilibrium Chem. CPU [Sec, % of MFIX] = ['&
               ,MAX_CPU_CHEM,'s, '&
               ,MAX_CPU_CHEM/(TOCK-WALL_START)*100.0,'%]'

1101 FORMAT(a,es12.4,a,f6.2,a)
         endif

      ENDIF

      RETURN
      END SUBROUTINE USR1


!----------------------------------------------------------------------!
! This module contains an equilibrium chemistry solver for aqueous MEA
! solutions loaded with CO2.  According to most authors, the chemistry
! governing the equilibrium concentrations of 9 species can be reduced
! to the following 5 independent equilibrium reactions.
! 1.  MEACOO- + H20  <--> MEA + HCO3m     (Reversion of Carbamate)
! 2.  CO2 + 2H2O <--> HCO3m + H3Op        (Dissociation of CO2)
! 3.  HCO3m + H2O --> CO3m2 + H3Op        (Dissociation of Bicarbonate)
! 4.  MEAH + H2O <--> MEA + H3Op          (Dissociation of MEAH+)
! 5.  2H2O <--> H3Op + OHm                (Ionization of Water)
!
! It should be possible to use this same approach for different solvent
! chemistry (ie different solvent: MDEA, BEA, etc), with careful
! re-evaluation of the functional representation (function FEVAL),its
! Jacobian (FUNCTION DFEVAL), the stoichiometric coefficients, and the
! temperature dependent equilibrium constants.
!
! JRF:  10/22/2016
!----------------------------------------------------------------------!
      MODULE CO2_MEA_EQUILIBRIUM
      implicit none
      include 'species.inc'

! Number of Equilibrium Reactions
      INTEGER,PARAMETER:: NR = 5

! Number of Species
      INTEGER,PARAMETER:: NS = 9

! Stoiciometric coeff Matrix: Species j in reaction i.
! We don't know the ordering of the species in species.inc until
! runtime, which is why we don't compose the whole matrix here.
      DOUBLE PRECISION, PRIVATE:: STOICH_MX(1:NR,1:NS)
      DOUBLE PRECISION,PARAMETER,PRIVATE:: &
         STOICH_CO2(NR)    =  (/  0.0,-1.0, 0.0, 0.0, 0.0   /),& !CO2
         STOICH_MEA(NR)    =  (/  1.0, 0.0, 0.0, 1.0, 0.0   /),& !MEA
         STOICH_MEAH(NR)   =  (/  0.0, 0.0, 0.0,-1.0, 0.0   /),& !MEAH
         STOICH_HCO3(NR)   =  (/  1.0, 1.0,-1.0, 0.0, 0.0   /),& !HCO3
         STOICH_OH(NR)     =  (/  0.0, 0.0, 0.0, 0.0, 1.0   /),& !OH
         STOICH_CO3(NR)    =  (/  0.0, 0.0, 1.0, 0.0, 0.0   /),& !CO3
         STOICH_H3O(NR)    =  (/  0.0, 1.0, 1.0, 1.0, 1.0   /),& !H3O
         STOICH_MEACOO(NR) =  (/ -1.0, 0.0, 0.0, 0.0, 0.0   /),& !MEACOO
         STOICH_H2O(NR)    =  (/ -1.0,-2.0,-1.0,-1.0,-2.0   /)   !H2O

! Scale factor for evaluation of the functional values, which
! helps avoid problems with roundoff errors associated with small
! species concentrations. SF should be set to be approximately
! ||F|| for typical values, so that  F_hat = F / SF ~ O(1).
      DOUBLE PRECISION,PARAMETER, PRIVATE:: SF = 1e-6

! Minimum (scaled) Newton step.  Should be large enough to avoid
! issues with roundoff error.
      DOUBLE PRECISION, PARAMETER, PRIVATE:: TOL_STEP = 1e-20 !Tol on newton step

! Minimum value of (scaled) infinity norm of the Jacobian.  Should
! be large enough to avoid issues with roundoff error.
      DOUBLE PRECISION, PARAMETER, PRIVATE:: TOL_DF = 1e-20

! Convergence criteria for line search:  Smaller values should
! increase stability and convergencd of Newton-Raphson, but could
! make the iteration expensive
      DOUBLE PRECISION, PARAMETER, PRIVATE:: TOL_LS = 1e-2

! Max number of Newton-Raphson iterations
      INTEGER,PARAMETER,PRIVATE:: ITER_MAX = 1000

! Factor to convert units of concentration to mol/dm^3
! This is set in beginning of solver.
      DOUBLE PRECISION,SAVE,PRIVATE:: CFACTOR

! Errors corrsponding to error codes returned in SUCCESS var
      CHARACTER(LEN=50), PARAMETER:: CO2_MEA_ERRORS(5) = (/ &
      'ZERO JACOBIAN, FALSE CONVERGENCE                 ', &
      'NEWTON-RAPHSON STEP IS ZERO                      ', &
      'MAX ITERATIONS EXCEEDED                          ', &
      'FOUND NaN DURING SOLVE                           ', &
      'UNRECOGNIZED UNITS, MUST BE "CGS" OR "SI"        ' &
      /)

      contains

!----------------------------------------------------------------------!
! Solve the coupled nonlinear system for the reaction rates (change in
! concentration) that will bring the 5 CO2-MEA-H2O reactions into
! equilibrium.  A Newton-Raphson iteration with step size limiter is
! used used to converge the rates to a specified tolerance.
!----------------------------------------------------------------------!
      SUBROUTINE SOLVE_CO2_MEA_EQ(X,T,RO,MW,UNITS,TOL_F,VERBOSE,SUCCESS)

      IMPLICIT NONE
      DOUBLE PRECISION, INTENT(INOUT):: X(NS)  !Mass fractions
      DOUBLE PRECISION, INTENT(IN):: T !Temperature
      DOUBLE PRECISION, INTENT(IN):: RO !Density
      DOUBLE PRECISION, INTENT(IN):: MW(NS) !Molecular weights
      CHARACTER(LEN=*), INTENT(IN):: UNITS !Unit system 'CGS' or 'SI'
      DOUBLE PRECISION, INTENT(IN):: TOL_F !Soln Tolerance
      LOGICAL,INTENT(IN):: VERBOSE
      INTEGER, INTENT(OUT):: SUCCESS !Error indicator
      !-----
      DOUBLE PRECISION:: XEQ(NS)  !Mass fractions
      DOUBLE PRECISION:: C(NS) !Molar Concentrations [gmol/cm^3]
      DOUBLE PRECISION:: R(NR) !Change in conc. [gmol/cm^3]
      DOUBLE PRECISION:: K(NR) !Equilibrium constants
      DOUBLE PRECISION:: F(NR) !Value of EQ functions
      DOUBLE PRECISION:: DF(NR,NR) !Jacobian of EQ functions
      DOUBLE PRECISION:: S(NR) !Newton-Raphson update: sol'n of DF*s=-F
      DOUBLE PRECISION:: LAMBDA !Fractional Newton step param
      INTEGER:: NITER !Counter for Newton iterations
      integer :: indx(NR)  !For linear solver.
      double precision :: d  !For linear solver
      !-----

      !Set the stoichiometric matrix according to species.inc:
      STOICH_MX(:,lco2) = STOICH_CO2
      STOICH_MX(:,lrnh2) = STOICH_MEA
      STOICH_MX(:,rnh3p) = STOICH_MEAH
      STOICH_MX(:,hco3m) = STOICH_HCO3
      STOICH_MX(:,ohm) = STOICH_OH
      STOICH_MX(:,co3m2) = STOICH_CO3
      STOICH_MX(:,h3op) = STOICH_H3O
      STOICH_MX(:,rnhco2m) = STOICH_MEACOO
      STOICH_MX(:,lh2o) = STOICH_H2O

      !Conversion factor to convert concentrations to mol/dm^3
      select case(trim(UNITS))
      case ('CGS')
         CFACTOR = 1000.00
      case ('SI')
         CFACTOR = 0.001
      case default
         SUCCESS = 5
         RETURN
      end select

      !Convert to molar concentrations:
      !Should be in MOl/DM^3
      C  = RO*X/MW * CFACTOR

      ! Temperature dependent equilibrium constants, K [mol/dm^3]
      ! These are computed using the coeffients suggested by Aboudheir
      ! et al [CES Vol 58, pp 5195-5210, 2003]
      ! K(i) = exp( A + B/T + C*log(T) +D*T)
      K(1)  = exp(6.69425  -3090.83/T  +0.0*log(T)       +0.0*T)
      K(2)  = exp(235.482  -12092.1/T  -36.7816*log(T)   +0.0*T)
      K(3)  = exp(220.067  -12431.7/T  -35.4819*log(T)   +0.0*T)
      K(4)  = exp(-3.3636  -5851.11/T  +0.0*log(T)       +0.0*T)
      K(5)  = exp(140.932  -13455.9/T  -22.4773*log(T)   +0.0*T)

      ! Set Initial guess for change in concentration to zero.
      R(1:NR) = 0.0

      !Check if our initial guess is a solution (ie, X is at equilibrium)
      F = FEVAL(R,K,C,SF)
      if(maxval(abs(F)) < TOL_F) then
         SUCCESS = 0
         RETURN
      endif

      !-----
      ! Newton-Raphson iteration loop:
      !-----
      NITER = 0
      do
         NITER = NITER + 1

         ! Evaluate RHS, F
         F = FEVAL(R,K,C,SF)

         ! Evaluate Jacobian, DF/DR
         DF = DFEVAL(R,K,C,SF)

         ! Solve DF(R_n)*S = -F(R_n) for the full Newton step, S
         ! using LU decomposition / back substitution
         S = -F
         CALL LUDCMP(DF, NR, NR, indx, d, 'SOLVE_CO2_MEA_EQ')
         CALL LUBKSB(DF, NR, NR, indx, S)

         !Check if we can accept the full Newton step by examining the
         !value of the error (FMIN) at R+S. If this is greater than the
         !error at R, we will use a golden section search to determine
         !the value of LAMBDA that minimizes the error at the next step.
         if(FMIN(R+S,K,C,SF) < FMIN2(F)) then
            LAMBDA = 1.0
         else
            call GOLDEN_MIN(LAMBDA,R,S,K,C,SF,TOL_LS)
         endif

         ! Update rates:  R_np1 = R_n + LAMBDA*S
         R = R + LAMBDA*S

         !Test for convergence on F
         if(maxval(abs(F)) < TOL_F) then
            SUCCESS = 0
            exit
         endif

         !Test for Zero Jacobian (false solution):
         if(maxval(abs(DF)) < TOL_DF) then
            SUCCESS = 1
            return
         endif

         !Test for convergence of Newton Step
         if(maxval(abs(S)) < TOL_STEP) then
            SUCCESS = 2
            return
         endif

         ! Don't do more than ITER_MAX iterations.
         if(NITER == ITER_MAX) then
            SUCCESS = 3
            return
         endif

         !Check for any NaN's
         if(any(isnan(S))) then
            SUCCESS = 4
            return
         endif

      enddo

      !-----
      !Check the solver finished ok:
      !-----
      if(SUCCESS==0) then
         if(VERBOSE) then
            write(*,'(a,i3,a,ES14.7)') 'Convergence in '&
               ,NITER, ' iterations.  Residual = ',MAXVAL(ABS(F))
         endif

         !Compute new mass fractions:
         call UPDATE_SPECIES_CO2_MEA(XEQ,X,MW,RO,R)

         !Report convergence and balances to stdout...
         if(VERBOSE) then
            CALL REPORT_EQ_STATUS(XEQ,X,MW,RO,K,R)
         endif

         !Return the equilibrium mass fractions in X
         X = XEQ
      endif

      END SUBROUTINE SOLVE_CO2_MEA_EQ

!----------------------------------------------------------------------!
! Functional representation, F(i) =0:
! These can be derived from the equilibrium expression for each
! reaction.
!----------------------------------------------------------------------!
      FUNCTION FEVAL(R,K,C,SF)
      IMPLICIT NONE
      DOUBLE PRECISION:: FEVAL(NR)
      DOUBLE PRECISION,INTENT(IN):: R(NR)
      DOUBLE PRECISION,INTENT(IN):: K(NR)
      DOUBLE PRECISION,INTENT(IN):: C(NS)
      DOUBLE PRECISION,INTENT(IN):: SF
      DOUBLE PRECISION:: F(NR)
      !Some aliases for species concentrations
      DOUBLE PRECISION:: CO2, MEA, MEAH, HCO, OH, CO3, H3O, MEACOO, H2O

      ! Alias the concentrations...
      CO2 = C(lCO2)
      MEA = C(lRNH2)
      MEAH = C(rnh3p)
      HCO = C(hco3m)
      OH = c(ohm)
      CO3 = C(co3m2)
      H3O = C(h3op)
      MEACOO = C(rnhco2m)
      H2O = C(lH2O)

      !Evaluate the function for each Equilibrium reaction
      FEVAL(1) =  K(1)*(MEACOO-R(1))-&
         (HCO+R(1)+R(2)-R(3))*(MEA+R(1)+R(4))
      FEVAL(2) =  K(2)*(CO2-R(2))-&
         (HCO+R(1)+R(2)-R(3))*(H3O+R(2)+R(3)+R(4)+R(5))
      FEVAL(3) =  K(3)*(HCO+R(1)+R(2)-R(3))-&
         (CO3+R(3))*(H3O+R(2)+R(3)+R(4)+R(5))
      FEVAL(4) =  K(4)*(MEAH-R(4))-&
         (MEA+R(1)+R(4))*(H3O+R(2)+R(3)+R(4)+R(5))
      FEVAL(5) =  K(5)-(OH+R(5))*(H3O+R(2)+R(3)+R(4)+R(5))

      !Scale according to scale factor, SF
      FEVAL = FEVAL/SF

      return
      END FUNCTION FEVAL

!----------------------------------------------------------------------!
! Jacobian, DF/DR:
! Terms computed/checked symbolically using Mathematica.
!----------------------------------------------------------------------!
      FUNCTION DFEVAL(R,K,C,SF)
      IMPLICIT NONE
      DOUBLE PRECISION:: DFEVAL(NR,NR)
      DOUBLE PRECISION,INTENT(IN):: R(NR)
      DOUBLE PRECISION,INTENT(IN):: K(NR)
      DOUBLE PRECISION,INTENT(IN):: C(NS)
      DOUBLE PRECISION,INTENT(IN):: SF
      DOUBLE PRECISION:: F(NR)
      !Some aliases for species concentrations
      DOUBLE PRECISION:: CO2, MEA, MEAH, HCO, OH, CO3, H3O, MEACOO, H2O

      ! Alias the concentrations...
      CO2 = C(lCO2)
      MEA = C(lRNH2)
      MEAH = C(rnh3p)
      HCO = C(hco3m)
      OH = c(ohm)
      CO3 = C(co3m2)
      H3O = C(h3op)
      MEACOO = C(rnhco2m)
      H2O = C(lH2O)

      !Evaluate the Jacobian...
      DFEVAL(1,1) = -HCO-K(1)-MEA-2.0*R(1)-R(2)+R(3)-R(4)
      DFEVAL(2,1) = -H3O-R(2)-R(3)-R(4)-R(5)
      DFEVAL(3,1) = K(3)
      DFEVAL(4,1) = -H3O-R(2)-R(3)-R(4)-R(5)
      DFEVAL(5,1) = 0.0
      DFEVAL(1,2) = -MEA-R(1)-R(4)
      DFEVAL(2,2) = -H3O-HCO-K(2)-R(1)-2.0*R(2)-R(4)-R(5)
      DFEVAL(3,2) = -CO3+K(3)-R(3)
      DFEVAL(4,2) = -MEA-R(1)-R(4)
      DFEVAL(5,2) = -OH-R(5)
      DFEVAL(1,3) = MEA+R(1)+R(4)
      DFEVAL(2,3) = H3O-HCO-R(1)+2.0*R(3)+R(4)+R(5)
      DFEVAL(3,3) = -CO3-H3O-K(3)-R(2)-2.0*R(3)-R(4)-R(5)
      DFEVAL(4,3) = -MEA-R(1)-R(4)
      DFEVAL(5,3) = -OH-R(5)
      DFEVAL(1,4) = -HCO-R(1)-R(2)+R(3)
      DFEVAL(2,4) = -HCO-R(1)-R(2)+R(3)
      DFEVAL(3,4) = -CO3-R(3)
      DFEVAL(4,4) = -H3O-K(4)-MEA-R(1)-R(2)-R(3)-2.0*R(4)-R(5)
      DFEVAL(5,4) = -OH-R(5)
      DFEVAL(1,5) = 0.0
      DFEVAL(2,5) = -HCO-R(1)-R(2)+R(3)
      DFEVAL(3,5) = -CO3-R(3)
      DFEVAL(4,5) = -MEA-R(1)-R(4)
      DFEVAL(5,5) = -H3O-OH-R(2)-R(3)-R(4)-2.0*R(5)

      !Scale according to scale factor, SF
      DFEVAL = DFEVAL/SF

      return
      END FUNCTION DFEVAL

!----------------------------------------------------------------------!
! Returns value of f = 0.5 F*F, the objective function to be minimized
! during convergence.
! Assumes that F=F(R,K,C) has not been calculated
!----------------------------------------------------------------------!
      DOUBLE PRECISION FUNCTION FMIN(R,K,C,SF)
      IMPLICIT NONE
      DOUBLE PRECISION,INTENT(IN):: R(NR)
      DOUBLE PRECISION,INTENT(IN):: K(NR)
      DOUBLE PRECISION,INTENT(IN):: C(NS)
      DOUBLE PRECISION,INTENT(IN):: SF
      DOUBLE PRECISION:: F(NR)
      INTEGER:: ir
      F = FEVAL(R,K,C,SF)
      FMIN = 0.0
      do ir = 1,NR
         FMIN = FMIN + F(ir)*F(ir)
      enddo
      FMIN = 0.5*FMIN
      return
      END FUNCTION FMIN

!----------------------------------------------------------------------!
! Returns value of f = 0.5 F*F, the objective function to be minimized
! during convergence.
! Similar to routine above, but assumes that F=F(R,K,C) has already
! been calculated.
!----------------------------------------------------------------------!
      DOUBLE PRECISION FUNCTION FMIN2(F)
      IMPLICIT NONE
      DOUBLE PRECISION,INTENT(IN):: F(NR)
      INTEGER:: ir
      FMIN2 = 0.0
      do ir = 1,NR
         FMIN2 = FMIN2 + F(ir)*F(ir)
      enddo
      FMIN2 = 0.5*FMIN2
      return
      END FUNCTION FMIN2

!----------------------------------------------------------------------!
! Subroutine:  GOLDEN_MIN(LAMBDA,R,S,K,C,SF,TOL)
! Find the value of Lambda where the function f(R,K,C) is minimized on the
! interval [R, R+Lambda*S], where 0 < Lambda < 1
!----------------------------------------------------------------------!
      SUBROUTINE GOLDEN_MIN(LAMBDA,R,S,K,C,SF,TOL)
      IMPLICIT NONE
      DOUBLE PRECISION, INTENT(OUT):: LAMBDA
      DOUBLE PRECISION, INTENT(IN):: R(NR)
      DOUBLE PRECISION, INTENT(IN):: S(NR)
      DOUBLE PRECISION,INTENT(IN):: K(NR)
      DOUBLE PRECISION,INTENT(IN):: C(NS)
      DOUBLE PRECISION,INTENT(IN):: SF
      DOUBLE PRECISION, INTENT(IN):: TOL
      !-----
      DOUBLE PRECISION,PARAMETER:: G = 0.618033988 ! 1.0 - Golden ratio
      INTEGER,PARAMETER:: MAX_ITER = 100
      DOUBLE PRECISION:: a(NR), b(NR), x1(NR), x2(NR)
      DOUBLE PRECISION:: f1,f2
      DOUBLE PRECISION:: resid,delx(NR),smag
      integer:: i
      !-----

      !Initial bounds
      a = R
      b = R+S

      !Magnitude of the S Vector
      smag = sqrt(dot_product(S,S))

      !Bracket the minimum function value:
      DO i = 1,MAX_ITER
         x1 = a + G*(b-a)
         x2 = b - G*(b-a)

         f1=FMIN(x1,K,C,SF)
         f2=FMIN(x2,K,C,SF)

         IF ( f1 < f2) THEN
            a = x2 ; x2 = x1 ; f2 = f1
            x1 = a + G*(b-a) ; f1 = FMIN(x1,K,C,SF)
         ELSE
            b = x1 ; x1 = x2 ; f1 = f2
            x2 = b - G*(b-a) ; f2 = FMIN(x2,K,C,SF)
         ENDIF

         !Check convergence:
         delx = x1-x2
         resid = sqrt(dot_product(delx,delx))/smag
         if(resid < TOL) then
            exit
         endif

      ENDDO

      !Compute Lambda as middle of interval...
      delx = 0.5*(x1+x2)-R
      LAMBDA = sqrt(dot_product(delx,delx))/smag

      !Ensure that Lambda is not too small or large
      LAMBDA = max(LAMBDA,0.1)
      LAMBDA = min(LAMBDA,0.9)

      END SUBROUTINE GOLDEN_MIN

!---------------------------------------------------------------------
! Report Equilibrium Status to stdout.
!---------------------------------------------------------------------
      SUBROUTINE REPORT_EQ_STATUS(X_N,X_NM1,MW,RO,K,R)
      IMPLICIT NONE
      !Current and old mass fractions:
      DOUBLE PRECISION, INTENT(IN):: X_N(NS), X_NM1(NS)
      !Molecular weights of species
      DOUBLE PRECISION, INTENT(IN):: MW(NS)
      !Liquid Density
      DOUBLE PRECISION, INTENT(IN):: RO
      !Reaction eq. constants:
      DOUBLE PRECISION, INTENT(IN):: K(NR)
      !Net reaction rates:
      DOUBLE PRECISION, INTENT(IN):: R(NR)
      !-----
      !Current and old concentrations:
      DOUBLE PRECISION:: C_N(NS), C_NM1(NS)
      ! Reaction quotients
      DOUBLE PRECISION:: Q(NR)
      ! Indices
      INTEGER:: ir,is
      ! Balances: Species, mass, charge, etc.  Should remain 0 in a
      ! closed system
      DOUBLE PRECISION:: BALANCE(4)
      !Set a string name for each equilibrium reaction, for reporting
      !only
      CHARACTER(len=22),PARAMETER:: REACTION_NAME(NR) = (/ &
         '  Carbamate Reversion',&
         '     Dissociation CO2',&
         '    Dissociation HCO3',&
         '    Dissociation MEAH',&
         '       Ionization H2O' &
         /)
      !-----

! Convert from mass frac to conc.
      C_N   = X_N*RO/MW * CFACTOR
      C_NM1 = X_NM1*RO/MW * CFACTOR

! Compute the reaction quotients:
      Q = 1.0
      do ir = 1,NR
         do is = 1,NS
            !Equilibrium constant def's exclude H2O:
            if (is==lh2O) cycle
            if(C_N(is) > 0.0) then
               Q(ir) = Q(ir)* C_N(is)**STOICH_MX(ir,is)
            endif
         enddo
      enddo

!Mass balance (should be zero)
      BALANCE(1)= 1.0 - sum(X_N(1:NS))
!Mea balance
      BALANCE(2)= C_N(lrnh2) + C_N(rnh3p) + C_N(rnhco2m) &
         -(C_NM1(lrnh2) + C_NM1(rnh3p) + C_NM1(rnhco2m))
!Carbon balance
      BALANCE(3)= C_N(lco2)+C_N(hco3m)+C_N(co3m2)+C_N(rnhco2m) &
         -(C_NM1(lco2)+C_NM1(hco3m)+C_NM1(co3m2)+C_NM1(rnhco2m))
!Charge balance
      BALANCE(4)= C_N(rnh3p)+C_N(h3op)-C_N(hco3m)-C_N(ohm)&
         -2.0*C_N(co3m2)-C_N(rnhco2m)

! Output the current state...
      write(*,'(a)') repeat('-',72)
      write(*,1002) '|','BALANCE:','|','  MASS    ','|' &
         ,'     MEA   ','|','    CARBON ','|','    CHARGE  ','|'
      write(*,1001) '|','','|'&
        ,BALANCE(1),'|',BALANCE(2),'|',BALANCE(3),'|',BALANCE(4),'|'
      write(*,'(a)') repeat('-',72)
      write(*,1002) '|','REACTION','|','     K     ','|' &
         ,'     Q     ','|','    Q/K    ','|','    RATE   ','|'
      do ir = 1,NR
      write(*,1001) '|',REACTION_NAME(ir),'|'&
        ,K(ir),'|',Q(ir),'|',Q(ir)/K(ir),'|',R(ir),'|'
      enddo
      write(*,'(a)') repeat('-',72)

1001 format (a1,a22,a1,ES11.2,a1,ES11.2,a1,ES11.2,a1,ES11.2,a1)
1002 format (a1,a22,a1,a11,a1,a11,a1,a11,a1,a11,a1)
1005 format (a6,ES11.4)
      END SUBROUTINE REPORT_EQ_STATUS

!----------------------------------------------------------------------!
! Update the species mass fractions given a set of rates for each
! equilibrium reaction, R [mol/vol].
!----------------------------------------------------------------------!
      SUBROUTINE UPDATE_SPECIES_CO2_MEA(X_NP1,X_N,MW,RO,R)
      implicit none
      ! Mass Fractions
      DOUBLE PRECISION, INTENT(INOUT):: X_NP1(NS), X_N(NS)
      ! Net reaction rate (FWD -  REV)
      DOUBLE PRECISION, INTENT(IN) :: R(NR)
      ! Molecular weights of species
      DOUBLE PRECISION, INTENT(IN):: MW(NS)
      ! Liquid Density
      DOUBLE PRECISION, INTENT(IN):: RO
      !-----
      DOUBLE PRECISION:: XTMP(NS)
      INTEGER:: is !species index)
      !-----

! Save x_n
      XTMP = X_N

! Update each species:
      do is = 1,NS
         X_NP1(is) = X_N(is) + &
           (MW(is)/(RO*CFACTOR))*sum(STOICH_MX(1:NR,is)*R(1:NR))
      enddo

! Shift time level
      X_N = XTMP

      END SUBROUTINE UPDATE_SPECIES_CO2_MEA

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
!
!  subroutine name: ludcmp(a,n,np,indx,d, calledFrom)
!  Purpose: Replaces matrix a (n,n) by the LU decomposition of a rowwise
!           permutation of itself. Used in combination with lubksb.
!
!  Literature/Document References:
!     Numerical Recipies in Fortran 77, page 38-39
!
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!
      subroutine ludcmp(a,n,np,indx,d,calledFrom)
      USE compar
      USE exit, only: mfix_exit

      implicit none
!-----------------------------------------------
! Dummy arguments
!-----------------------------------------------
      integer, intent(in) :: n
      double precision, intent(inout) :: a(n,n)
      integer :: np
      integer, intent(out) :: indx(n)
      double precision, intent(out) :: d
      CHARACTER(len=*), intent(in) :: calledFrom
!-----------------------------------------------
! Local parameters
!-----------------------------------------------
      integer :: nmax
      double precision :: TINY
      parameter (NMAX=500, TINY=1.0D-20)
!-----------------------------------------------
! Local variables
!-----------------------------------------------
      integer :: i, j, k, imax
      double precision :: vv(NMAX)
      double precision :: aamax, sum, dum
!-----------------------------------------------

      d = 1.0d0
      do i = 1,n
         aamax=0.0d0
         do j = 1,n
            if (dabs(a(i,j)).gt.aamax) aamax = dabs(a(i,j))
         enddo
         if (aamax.eq.0.0d0) then
           if(myPE==PE_IO) write(*,*) &
              'Singular Matrix in ludcmp called from ', calledFrom
           call mfix_exit(myPE)
         endif
         vv(i) = 1.d0/aamax
      enddo
      do j = 1,n
         do i = 1,j-1
            sum = a(i,j)
            do k = 1,i-1
               sum = sum-a(i,k)*a(k,j)
            enddo
            a(i,j) = sum
         enddo
         aamax = 0.0d0
         do i = j,n
            sum = a(i,j)
            do k = 1,j-1
               sum = sum-a(i,k)*a(k,j)
            enddo
            a(i,j) = sum
            dum = vv(i)*dabs(sum)
            if (dum.ge.aamax) then
               imax = i
               aamax = dum
            endif
         enddo
         if (j.ne.imax) then
            do k = 1,n
               dum = a(imax,k)
               a(imax,k) = a(j,k)
               a(j,k) = dum
            enddo
            d = -d
            vv(imax) = vv(j)
         endif
         indx(j) = imax
         if (a(j,j).eq.0.0d0) a(j,j) = TINY
         if (j.ne.n) then
            dum = 1.0d0/a(j,j)
            do i = j+1,n
               a(i,j) = a(i,j)*dum
            enddo
         endif
      enddo

      return
      end subroutine ludcmp

!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
!
!  subroutine name: lubksb (a,n,np,indx,b)
!  Purpose: solves the set of n linear equations A(n,n).X(n) = B(n).
!
!
!  Literature/Document References:
!     Numerical Recipies in Fortran 77, page 39.
!
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

      subroutine lubksb (a,n,np,indx,b)

      implicit none
!-----------------------------------------------
! Dummy arguments
!-----------------------------------------------
      integer, intent(in) :: n
      double precision, intent(in) :: a(n,n)
      integer :: np
      integer, intent(in) :: indx(n)
      double precision, intent(inout) :: b(n)
!-----------------------------------------------
! Local variables
!-----------------------------------------------
      integer :: i, ii, j, ll
      double precision :: sum
!-----------------------------------------------

      ii=0
      do i=1,n
        ll=indx(i)
        sum=b(ll)
        b(ll)=b(i)
        if (ii.ne.0) then
          do j=ii,i-1
            sum=sum-a(i,j)*b(j)
          enddo
         elseif (sum.ne.0.d0) then
           ii=i
         endif
         b(i)=sum
      enddo
       do i=n,1,-1
         sum=b(i)
         if (i.lt.n) then
           do j=i+1,n
             sum=sum-a(i,j)*b(j)
           enddo
         endif
         b(i)=sum/a(i,i)
       enddo
       return
       end subroutine lubksb

      END MODULE CO2_MEA_EQUILIBRIUM


!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
! Purpose: Loops through the fluid cells and calls the equilbrium      C
! chemistry solver                                                     C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE LIQUID_CHEM_EQ

! Modules
!--------------------------------------------------------------------//
      use run, only: units
      use geometry, only: imax,jmax,kmax
      use compar, only: ijkstart3, ijkend3
      use functions, only: fluid_at
      use fldvar, only: t_s, x_s, ro_s, ep_s
      use usr, only: index_liq
      use physprop, only: mw_s
      use toleranc, only: tmax
      use error_manager, only: init_err_msg, err_msg
      use error_manager, only: flush_err_msg,finl_err_msg
      use co2_mea_equilibrium, only: NR,NS
      use co2_mea_equilibrium, only: SOLVE_CO2_MEA_EQ
      use co2_mea_equilibrium, only: CO2_MEA_ERRORS
      IMPLICIT NONE

! Local variables
!--------------------------------------------------------------------//
! Minimum phase volume fraction required to enforce equilibrium
      DOUBLE PRECISION, parameter :: v_Limiter = 1.0d-6
! Tolerance for equilibrium solution.
      DOUBLE PRECISION, parameter:: TOL_EQ = 1e-11
! Minimum mass fraction to be passed to the solver
      DOUBLE PRECISION, parameter:: SMALL_X = 1e-8
! Error indicator
      INTEGER:: SUCCESS
! Cell index
      INTEGER:: IJK
! Bounded liquid phase temperature
      DOUBLE PRECISION:: xTl
! Mass fractions passed to/from the equilibrium chem solver
      DOUBLE PRECISION :: X_EQ(NS)
! Flag for verbose output from the chem solver.
      LOGICAL:: VERBOSE_CHEM = .FALSE.

!--------------------------------------------------------------------//

      include 'species.inc'
!--------------------------------------------------------------------//

! Loop through fluid cells, and solve for equilibrium mass fractions of
! the 9 species involved in the aqueous CO2-MEA solution.
      DO IJK=IJKSTART3,IJKEND3
         IF(FLUID_AT(IJK)) THEN

! If liquid volume fraction is very small (or zero) ignore this cell
            if(ep_s(ijk,index_liq) < v_limiter) cycle

! X_EQ is the mass fraction to be passed in/out of the solver.  For
! good convergence, we want to ensure that these mass fractions are
! non-zero on input.  So, a minimum mass fraction (SMALL_X) is given
! for each species.  Mass fraction of H2O is then adjusted to ensure
! mass fractions sum to unity.
            X_EQ = max(X_S(IJK,index_liq,:),SMALL_X)
            X_EQ(lh2o) = X_EQ(lh2o) + (1.0 - sum(X_EQ))

! bounded liquid phase temperature
            xTl = min(TMAX,T_s(IJK,index_liq))

! For 1 cell, let the chem solver write some extra debug info
            IF (IMAX==1 .AND. JMAX==1 .AND. KMAX==1) then
               VERBOSE_CHEM = .TRUE.
            endif

! Call the solver, which returns eq mass fractions in X_EQ
            CALL SOLVE_CO2_MEA_EQ(X_EQ,xTl&
            ,ro_s(ijk,index_liq),MW_S(index_liq,1:NS),trim(UNITS)&
            ,TOL_EQ,VERBOSE_CHEM,SUCCESS)

! Check for errors before updating mass fractions:
            if (SUCCESS==0)  then
               X_S(IJK,index_liq,1:NS) = X_EQ(1:NS)
            else
               CALL INIT_ERR_MSG('LIQUID_CHEM_EQ')
               WRITE(ERR_MSG,7001) CO2_MEA_ERRORS(SUCCESS),IJK
               CALL FLUSH_ERR_MSG(ABORT=.FALSE.)
 7001 FORMAT('WARN 7001: SOLVE_CO2_MEA_EQ returned error: ',/,A &
      ,/,'Will not update liquid mass fractions for IJK=',i8)
              CALL FINL_ERR_MSG()
           endif

         ENDIF
      ENDDO

      END SUBROUTINE LIQUID_CHEM_EQ

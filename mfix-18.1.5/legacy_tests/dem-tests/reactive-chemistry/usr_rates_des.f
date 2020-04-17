!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
!                                                                      !
!  Subroutine: USR_RATES_DES                                           !
!  Author: J.Musser                                   Date: 10-Oct-12  !
!                                                                      !
!  Purpose: Hook for user defined reaction rates.                      !
!                                                                      !
!  Comments: Write reaction rates in amount of chemical substance      !
!  per time. For CGS this has units mol/s. For _MFIX_ SI this has      !
!  units kmol/s.                                                       !
!                                                                      !
!  WARNING: Only discrete phase reactions should be specifed here.     !
!  Homogeneous gas phase reactions in DEM simulations must be given    !
!  in the continuum reaction hook (usr_rates.f).                       !
!                                                                      !
!  The call to usr_rates_des is made from inside a particle loop which !
!  is nested inside an IJK loop. Fluid grid calculations independent   !
!  of particle properties can be carried out in des/usr4_des.f to      !
!  reduce redundant calculations.                                      !
!                                                                      !
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
      SUBROUTINE USR_RATES_DES(NP, pM, IJK, DES_RATES)

      use constant, only: C
      use des_rxns, only: NO_OF_DES_RXNS
      use des_rxns, only: DES_X_s
      use run, only: dt
      use physprop, only: mw_s
      use discretelement, only: dtsolid, des_explicitly_coupled
      use discretelement, only: pmass

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: NP  ! Index of particle
      INTEGER, INTENT(IN) :: pM  ! Solid phase index of particle NP
      INTEGER, INTENT(IN) :: IJK ! Fluid cell index containing NP

! Calculated reaction rates. (reacted moles per sec)
      DOUBLE PRECISION, INTENT(OUT) :: DES_RATES(NO_OF_DES_RXNS)
! local time step, inverse time step
      DOUBLE PRECISION :: lDt, lOoDT
! max rate
      DOUBLE PRECISION :: lrate

      INCLUDE 'species.inc'

      lDT = merge(DT, DTSOLID, DES_EXPLICITLY_COUPLED)
      lOoDT = 1.d0/lDT

! EX_RXN:    A(g) + 2B(s) --> C(g) + D(s)
!`````````````````````````````````````````````````````````````````````\\

! (moles/sec)  specified in mfix.dat
      IF(NP == 2 .AND. DES_X_s(2,Bs) > 0.0d0) THEN
         DES_RATES(EX_RXN) = C(1)
! bound the rate as to not exceed the amount of solids reactant available
! for the given time step
         lrate = pmass(NP)*des_x_s(NP,Bs)*lOoDt/MW_S(pM,Bs)
         IF (C(1)>lrate) DES_RATES(EX_RXN) = lrate
      ELSE
         DES_RATES(EX_RXN) = 0.0d0
      ENDIF

      RETURN
      END SUBROUTINE USR_RATES_DES

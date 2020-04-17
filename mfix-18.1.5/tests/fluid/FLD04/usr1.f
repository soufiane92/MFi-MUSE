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
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
      SUBROUTINE USR1

      use geometry, only: oDx, oDy
      use fldvar, only  : u_g, v_g, w_g
      use functions
      use fun_avg
      use compar
      use usr
      IMPLICIT NONE
!-----------------------------------------------

! Looping indices
      integer :: i, j, k
      integer :: imjmk, imj_k, imjpk
      integer :: i_jmk, i_j_k, i_jpk
      integer :: ipjmk, ipj_k, ipjpk
! Face-centered velocities
      double precision :: UgN, UgS, VgE, VgW

      do k=kstart2, kend2
         do i=istart2, iend2
            do j=jstart2, jend2

               imjmk = bound_funijk(i-1,j-1,k)
               imj_k = bound_funijk(i-1,j,  k)
               imjpk = bound_funijk(i-1,j+1,k)

               i_jmk = bound_funijk(i,  j-1,k)
               i_j_k = bound_funijk(i,  j,  k)
               i_jpk = bound_funijk(i,  j+1,k)

               ipjmk = bound_funijk(i+1,j-1,k)
               ipj_k = bound_funijk(i+1,j,  k)
               ipjpk = bound_funijk(i+1,j+1,k)

               UgN = AVG_Y(AVG_X_E(U_G(imj_k),U_G(i_j_k),i),  &
                  AVG_X_E(U_G(imjpk),U_G(i_jpk),I),j)

               UgS = AVG_Y(AVG_X_E(U_G(imjmk),U_G(i_jmk),I),  &
                  AVG_X_E(U_G(imj_k),U_G(i_j_k),I),j-1)

               VgE = AVG_X(AVG_Y_N(V_G(i_jmk),V_G(i_j_k)),    &
                  AVG_Y_N(V_G(ipjmk),V_G(ipj_k)),i)

               VgW = AVG_X(AVG_Y_N(V_G(imjmk),V_G(imj_k)),    &
                  AVG_Y_N(V_G(i_jmk),V_G(i_j_k)),i-1)

               W_g(i_j_k) = (VgE-VgW)*oDX(i) - (UgN-UgS)*oDy(j)

            enddo
         enddo
      enddo

      RETURN
      END SUBROUTINE USR1

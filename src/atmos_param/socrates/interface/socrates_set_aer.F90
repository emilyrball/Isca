! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
MODULE socrates_set_aer
IMPLICIT NONE
CONTAINS

! Subroutine to set the aerosol fields for the core radiation code.
!------------------------------------------------------------------------------
SUBROUTINE set_aer(control, dimen, spectrum, aer, n_profile)

USE rad_pcf,      ONLY: ip_aersrc_classic_ron
USE def_spectrum, ONLY: StrSpecData
USE def_dimen,    ONLY: StrDim
USE def_control,  ONLY: StrCtrl
USE def_aer,      ONLY: StrAer, allocate_aer, allocate_aer_prsc
use soc_constants_mod,   only: i_def, r_def

IMPLICIT NONE

integer :: i, j, k

! Control options:
TYPE(StrCtrl),      INTENT(IN)  :: control

! Dimensions:
TYPE(StrDim),       INTENT(IN)  :: dimen

! Spectral data:
TYPE (StrSpecData), INTENT(IN)  :: spectrum

! Aerosol properties:
TYPE(StrAer),       INTENT(OUT) :: aer

INTEGER(i_def), INTENT(IN) :: n_profile
!   Number of atmospheric profiles for radiation calculations


! Allocate structure for the core radiation code interface
CALL allocate_aer(aer, dimen, spectrum)
CALL allocate_aer_prsc(aer, dimen, spectrum)

aer%mr_source = ip_aersrc_classic_ron

DO i=1, spectrum%aerosol%n_aerosol
  aer%mr_type_index(i)=i
  DO j=1, dimen%nd_profile
    DO k=1, dimen%nd_layer
      aer%mix_ratio(:,:,i)=1.0e-06
    END DO
  END DO
END DO
END SUBROUTINE set_aer
END MODULE socrates_set_aer

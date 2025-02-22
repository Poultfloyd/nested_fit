MODULE MOD_METADATA
  IMPLICIT NONE
  ! Module for program runtime metadata
  
  CHARACTER(LEN=5), PARAMETER :: version_full = '4.5.6'
  CHARACTER(LEN=3), PARAMETER :: version = '4.5'
  CHARACTER(LEN=18), PARAMETER :: nf_child_proc_name = 'nf_child_proc4.5.6'
  
#ifdef FUNC_TARGET
  CHARACTER(LEN=20), PARAMETER :: exec_target_name = 'nested_fit_func4.5.6'
#else
  CHARACTER(LEN=15), PARAMETER :: exec_target_name = 'nested_fit4.5.6'
#endif
  
  
END MODULE MOD_METADATA

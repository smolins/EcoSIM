!
!There needs to be a wrapper for the eocsim f90 driver
!as there are differences between how gfortran and intel compilers
!handle mangling conventions.
!
! Copied from the alquimia wrapper:
! **************************************************************************** !
!
! PFloTran Alquimia Inteface Wrappers
!
! Author: Benjamin Andre
!
! Different fortran compilers use different name mangling conventions
! for fortran modules:
!
!    gfortran : ___modulename_MOD_procedurename
!
!    intel : _modulename_mp_procedurename_
!
!    as a consequence we can't put the alquimia interface into a
!    module and call it directly from C/C++. Instead we use
!    some simple wrapper functions.
!
! Notes:
!
!  * Function call signatures are dictated by the alquimia API!
!
!  * alquimia data structures defined in AlquimiaContainers_module
!    (alquimia_containers.F90) are dictated by the alquimia API.
!
! **************************************************************************** !

subroutine EcoSIM_DataTest() bind(c)
    use, intrinsic :: iso_c_binding
    !Simple test for wrapper functionality
    implicit none

    write(*,*) "in the data test"

  end subroutine EcoSIM_DataTest

! **************************************************************************** !

subroutine EcoSIM_Setup(properties, state, istate, sizes, num_iterations,&
                        num_columns, ncells_per_col_) bind(C)

  use, intrinsic :: iso_c_binding

  use BGCContainers_module
  use ATSCPLMod, only : ATS2EcoSIMData, Init_EcoSIM
  use ATSStateRegistryMod, only : PackInternalState

  implicit none

  ! function parameters
  type (BGCSizes), intent(in) :: sizes
  type (BGCState), intent(inout) :: state
  type (BGCInternalState), intent(inout) :: istate
  type (BGCProperties), intent(in) :: properties
  integer(c_int), VALUE :: num_columns
  integer(c_int), VALUE :: num_iterations
  integer(c_int), VALUE :: ncells_per_col_

  call ATS2EcoSIMData(num_columns, state, properties, sizes)

  call Init_EcoSIM(sizes)

  ! give ATS the initialized carried state; outputs are computed by the first advance
  call PackInternalState(istate, sizes, .false.)

end subroutine EcoSIM_Setup

! **************************************************************************** !

subroutine EcoSIM_Shutdown() bind(C)

  !For now this does nothing, but it should clear all
  !the data structures
  use, intrinsic :: iso_c_binding

  use BGCContainers_module

  implicit none

  ! function parameters
  !character(kind=c_char), dimension(*), intent(in) :: input_filename
  !type (BGCSizes), intent(out) :: sizes
  !type (BGCState), intent(in) :: state
  !type (BGCAuxiliaryData), intent(in) :: aux_data
  !type (BGCProperties), intent(in) :: properties
  !integer :: num_columns, jz, js
  !integer, intent(in) :: num_iterations

end subroutine EcoSIM_Shutdown

! **************************************************************************** !

subroutine EcoSIM_Advance( &
     delta_t, &
     properties, &
     state, &
     istate, &
     sizes, &
     num_iterations, &
     num_columns) bind(C)

  use, intrinsic :: iso_c_binding
  use BGCContainers_module
  use ATSCPLMod, only : Run_EcoSIM_one_step, ATS2EcoSIMData, EcoSIM2ATSData
  use ATSStateRegistryMod, only : PackInternalState, UnpackInternalState

  implicit none

  ! function parameters
  real (c_double), value, intent(in) :: delta_t
  type (BGCProperties), intent(in) :: properties
  type (BGCState), intent(inout) :: state
  type (BGCInternalState), intent(inout) :: istate
  type (BGCSizes), intent(in) :: sizes
  integer(c_int), value, intent(in) :: num_iterations
  integer(c_int), value, intent(in) :: num_columns

  call ATS2EcoSIMData(num_columns, state, properties, sizes)

  ! restore carried state from ATS (identical unless ATS restarted)
  call UnpackInternalState(istate, sizes)

  call Run_EcoSIM_one_step(sizes)

  call EcoSIM2ATSData(num_columns, state, sizes)

  call PackInternalState(istate, sizes, .true.)

end subroutine EcoSIM_Advance

! **************************************************************************** !
!
! Layout of the EcoSIM internal state (see ATSStateRegistryMod). Static: it
! depends only on sizes, so ATS can query it before EcoSIM_Setup.
!
! **************************************************************************** !

function EcoSIM_Internal_State_Layout_Version() &
     bind(C, name="ecosim_internal_state_layout_version") result(version)

  use, intrinsic :: iso_c_binding
  use ATSStateRegistryMod, only : kLayoutVersion

  implicit none
  integer(c_int) :: version

  version = kLayoutVersion

end function EcoSIM_Internal_State_Layout_Version

! **************************************************************************** !

function EcoSIM_Internal_State_Num_Entries(sizes) &
     bind(C, name="ecosim_internal_state_num_entries") result(num_entries)

  use, intrinsic :: iso_c_binding
  use BGCContainers_module, only : BGCSizes
  use ATSStateRegistryMod, only : NumStateEntries

  implicit none
  type (BGCSizes), intent(in) :: sizes
  integer(c_int) :: num_entries

  num_entries = NumStateEntries(sizes)

end function EcoSIM_Internal_State_Num_Entries

! **************************************************************************** !

subroutine EcoSIM_Internal_State_Entry(i, sizes, ats_name, ecosim_name, units, &
     description, ncomp, role) bind(C, name="ecosim_internal_state_entry")

  ! i is 0-based. The character buffers must hold kNameLen, kNameLen,
  ! kUnitsLen and kDescLen characters; they are returned null-terminated.
  use, intrinsic :: iso_c_binding
  use BGCContainers_module, only : BGCSizes
  use ATSStateRegistryMod, only : GetStateEntry, kNameLen, kUnitsLen, kDescLen

  implicit none
  integer(c_int), value, intent(in) :: i
  type (BGCSizes), intent(in) :: sizes
  character(kind=c_char), intent(out) :: ats_name(kNameLen), ecosim_name(kNameLen)
  character(kind=c_char), intent(out) :: units(kUnitsLen), description(kDescLen)
  integer(c_int), intent(out) :: ncomp, role

  character(len=kNameLen) :: f_ats_name, f_ecosim_name
  character(len=kUnitsLen) :: f_units
  character(len=kDescLen) :: f_description
  integer :: f_ncomp, f_role

  call GetStateEntry(i+1, sizes, f_ats_name, f_ecosim_name, f_units, f_description, &
       f_ncomp, f_role)
  call to_c_string(f_ats_name, ats_name)
  call to_c_string(f_ecosim_name, ecosim_name)
  call to_c_string(f_units, units)
  call to_c_string(f_description, description)
  ncomp = f_ncomp
  role = f_role

contains

  subroutine to_c_string(fstr, cstr)
    character(len=*), intent(in) :: fstr
    character(kind=c_char), intent(out) :: cstr(:)
    integer :: k, n
    n = min(len_trim(fstr), size(cstr)-1)
    do k = 1, n
      cstr(k) = fstr(k:k)
    enddo
    cstr(n+1:) = c_null_char
  end subroutine to_c_string

end subroutine EcoSIM_Internal_State_Entry

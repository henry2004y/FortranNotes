program BATSRUS

  use modules

  !----------------------------------------------------------------------------
  !\
  ! Initialization of MPI/parallel message passing.
  !/
  call MPI_INIT(iError)

  !\
  ! Read input parameter file. Provide the default restart file for #RESTART
  !/
  call read_file('PARAM.in',iComm,trim(NameRestartInDir)//'restart.H')

  SESSIONLOOP: do
     call read_init
     !\
     ! Set and check input parameters for this session
     !/
     call set_parameters

     !\
     ! Time execution (timing parameters were set by MH_set_parameters)
     !/
     if(iSession==1)then
        call BATS_setup
        call BATS_init_session
     else
        call BATS_init_session
     end if

     TIMELOOP: do
        !\
        ! Stop this session if stopping conditions are fulfilled
        !/
        if (stop_condition_true()) EXIT TIMELOOP
        if(is_time_to_stop())EXIT SESSIONLOOP

        call timing_step(n_step+1)

        call BATS_advance(t_max)

     end do TIMELOOP

     if(IsLastRead)then
        EXIT SESSIONLOOP
     else
        iSession=iSession+1
     end if

  end do SESSIONLOOP

  call BATS_save_files('FINALWITHRESTART')

  call timing_stop('BATSRUS')

  call BATS_finalize

  call MPI_finalize(iError)

end program BATSRUS

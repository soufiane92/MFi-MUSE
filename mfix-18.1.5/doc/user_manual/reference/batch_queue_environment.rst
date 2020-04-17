Batch Queue
===========

MFIX can be used on systems where code execution is controlled through batch
queue submission system instead of interactive or background job type methods
shown in the previous section. Usually the user specifies the wall clock time
duration of the job and batch queuing system prioritize incoming jobs based on
their resource allocation requests. In order for MFIX to avoid abrupt and
abnormal termination at the end of the batch job session, several keywords need
to be entered in mfix.dat. Controlled and clean termination in environments with
batch queue is important as the system may terminate the batch job while MFIX is
writing out ``*.SP`` files, which may corrupt the files or cause loss of data.

For this purpose, MFIX checks whether the user-specified termination criteria is
reached at the beginning of each time step. However, to avoid performance
bottlenecks on small systems where the user is running their jobs without a
batch queue, this feature is disabled by default. In order to enable this
feature the following block of commands need to be entered in ``mfix.dat``.

.. code-block:: fortran

    CHK_BATCHQ_END = .TRUE.  ! Enable the controlled termination feature
    BATCH_WALLCLOCK = 3600.0 ! Specify the total wall clock duration
                             ! of your job in seconds
    TERM_BUFFER = 300.0      ! Specify a buffer time to start
                             ! clean termination of MFIX

Setting ``CHK_BATCH_END = .TRUE.`` in ``mfix.dat`` will enable the checking of
the termination criteria at the beginning of each time step. In the above
example, the user has set the total wall clock time for the duration of the
batch session to 1 hour (this is specified in seconds in mfix.dat) and a buffer
of 300 seconds has been set so that MFIX has sufficient time to terminate
cleanly by writing out all ``*.SP`` and ``*.RES`` files before the batch session
terminates. The duration of the buffer is critical for simulations with large
files. MFIX will check if ``elapsed time >= (BATCH_WALLCLOCK - TERM_BUFFER)`` to
start clean termination.

Another way to gracefully terminate MFIX as soon as possible is to create an
empty file named MFIX.STOP (filename all uppercase) in the working directory
where MFIX runs. At the beginning of each time step if MFIX.STOP file is
detected to exist, then MFIX will terminate gracefully by saving ``*.RES`` files.
CHK_BATCHQ_END flag must be set to .TRUE. in order to activate this feature.

The following command can be used to gracefully terminate MFIX:

.. code-block:: fortran

  > touch MFIX.STOP

  Remember to erase the file once MFIX terminates, otherwise the next time MFIX is run
  in the same directory it will terminate immediately.

.. code-block:: fortran

  > rm -f -r ./MFIX.STOP

.. include:: /user_manual/reference/batch_queue_environment_gen.rst

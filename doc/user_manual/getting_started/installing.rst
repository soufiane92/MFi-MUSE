.. _setup-guide:

===========
Setup Guide
===========

This section explains how to install binary packages for MFiX.

The MFiX GUI requires an installation of Python. The Anaconda distribution of
Python is the recommended way of installing MFiX.

If Python isn't available, the MFiX solver can still be used from a source
tarball (as in previous releases). See :ref:`developers` and
:ref:`batch_solver`.

Register
--------

MFiX is an open-source multiphase flow solver and is free to download and use. A
one-time no-cost registration is required prior to downloading the software. To
register, go to https://mfix.netl.doe.gov/register and submit your account
information. Your application will be manually reviewed and accepted, so
please allow for 2-3 business days for your registration to be processed. You
will receive a confirmation email when you can login.

Potential users may find reviewing the `Frequently Asked
Questions <https://mfix.netl.doe.gov/mfix/faq/>`__ section of the MFiX
website useful before downloading the code.


OS Support
----------

MFiX has been developed and tested on the following operating systems:

-  Linux

   - Ubuntu 17.10
   - openSUSE 10.4
   - openSUSE Leap 42.2
   - CentOS 7

-  macOS

   - 10.13 (latest)

-  Windows

   - Windows 7
   - Windows 10

Other recent releases of Linux and macOS are likely to work. MFiX has not been
tested on Windows 8, but it is likely to work there as well. If you find MFiX
works on other platforms, please report your experiences to :ref:`mailing-list`.


Building the MFiX solver requires:

    - Fortran 2003 compiler (GFortran 4.8 or later)
    - GNU Make
    - CMake
    - An MPI implementation for DMP support (mainly tested with OpenMPI)

Importing the MFiX environment will install the conda packages for CMake, GNU Make, and GCC/GFortran.

For building with other compilers, or for building with DMP, see :ref:`customsolver`.

Install Anaconda
----------------

**Download** `Anaconda <https://www.anaconda.com/download>`__ for your platform
(Linux, macOS, or Windows). The 64-bit version is required.

During the Anaconda installation:

**On Mac/Linux**, add the conda ``bin`` directory to your ``PATH`` at the end of
the installer. Restart your shell and run ``conda`` to verify it is in your ``PATH``.

**On Windows**, use the default settings during installation.
You can leave "Add Anaconda to my PATH environment variable" unchecked and
"Register Anaconda as my default python 3.6" checked.
You can skip the installation of MS VSCode.
You can skip the "Learn more about Anaconda cloud".
After installation, under the Start Menu, type "Anaconda Prompt", select the
Anaconda Prompt to open the Anaconda prompt. The prompt will be used to
enter commands to install additional dependencies and to install mfix.

.. _create-environment:

Activate Environment
--------------------

Creating a conda environment is optional, but for compatibility we recommend
using the `MFiX conda environments <https://anaconda.org/mfix/environments>`_
defined in Anaconda Cloud. This environment file install all MFiX dependencies,
but not MFiX itself.

.. note:: On Linux and Mac, if you are using a shell different from bash, switch
          to ``bash`` before activating the environment.

Run the corresponding commands for your platform to create a conda environment for MFiX:

.. code-block:: bash

      # on Linux
      > conda env create -n mfix-18.1 mfix/mfix-linux64
      > source activate mfix-18.1
      (mfix-18.1) >

      # on Mac
      > conda env create -n mfix-18.1 mfix/mfix-osx64
      > source activate mfix-18.1
      (mfix-18.1) >

      # on Windows
      C:\> conda env create -n mfix-18.1 mfix/mfix-win64
      C:\> activate mfix-18.1
      (mfix-18.1) C:\>

The ``mfix-18.1`` environment includes all dependencies of MFiX, but not
MFiX itself, which is installed in the next section.

.. note::

    Activating a conda environment sets certain environment variables such as
    PATH in the current shell. It does not create a new shell session.

    You will need to activate the environment every time before running MFIX.


.. _install-build-dependencies:

Install Build Dependencies
--------------------------

Installing build dependencies is needed for building a :ref:`customsolver`.

On Linux, you can use your system's package for GCC/GFortran if it is GCC 4.8 or
later. Otherwise install a newer GCC. GNU Make is also required. Or you could
install the Anaconda GCC and GNU Make with ``conda install gcc make``. CMake is
also required, and should already be in your :ref:`conda environment
<create-environment>`.

On Windows, the GFortran compiler should already be installed by
:ref:`create-environment`. The Conda package is ``m2w64-toolchain``.

On Mac, Homebrew is the easiest way to install MFiX build dependencies. Go to
`the Homebrew website <https://brew.sh>`__ and follow the installation
instructions.

Once Homebrew is installed, install MFiX build dependencies with the following
commands:

.. code:: shell

    > brew install gcc boost
    > brew install open-mpi  # for DMP



.. _install-mfix:

Install MFiX
------------

  - Copy the conda install command from `MFiX Download <https://mfix.netl.doe.gov/mfix/download-mfix>`__ (requires registration)

  - Paste and run in in the `environment you created <create-environment>`_.

The ``mfix`` conda package will be installed. The process will take a few
minutes to complete. When it finishes, you can now proceed to ready to proceed
to :ref:`user-guide`.

Deactivate Environment
----------------------

To deactivate a conda environment:

On Windows:

.. code-block:: bash

    > deactivate

On Linux/Mac:

.. code-block:: bash

    > source deactivate


This will return to the base conda environment.


Uninstall MFiX
--------------

To uninstall MFiX from a conda environment:

.. code-block:: bash

    > conda uninstall mfix

To view all conda environments (``*`` indicates current environment):

.. code-block:: bash

    (mfix-18.1) > conda env list
    # conda environments:
    #
    base           /home/user/anaconda3
    mfix-18.1   *  /home/user/anaconda3/envs/mfix-18.1

To uninstall the ``mfix-18.1`` conda environment:

.. code-block:: bash

    (mfix-18.1) > conda deactivate
    > conda env remove -n mfix-18.1


.. note::

    To learn more about managing conda environments, visit the `conda documentation <https://conda.io/docs/user-guide/tasks/manage-environments.html#managing-environments.>`__ .

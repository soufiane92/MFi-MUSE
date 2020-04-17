.. _about:

=====
About
=====


MFiX
----

MFiX is an open-source multiphase flow solver and is free to download
and use. A one-time, no-cost registration is required prior to
downloading the source code. To register, go to
https://mfix.netl.doe.gov/ and click on the "Register" button in the
upper right corner. Once you have read the notice, you can submit your
application by clicking on "REGISTER." After your application has been
reviewed and accepted, you will receive an email notification and
instructions on how to download the code. Please allow for 2-3 business
days for your registration to be processed.

Potential users may find reviewing the `Frequently Asked
Questions <https://mfix.netl.doe.gov/mfix/faq/>`__ section of the MFiX
website useful before downloading the code.


Development state of MFiX models
--------------------------------

MFiX provides a suite of models that treat the carrier phase (typically
the gas phase) and disperse phase (typically the solids phase)
differently. Their current state of development is summarized in the
tables below.

Symbol description for the following tables:

.. |c| replace:: :math:`\circ`
.. |b| replace:: :math:`\bullet`
.. |s| replace:: :math:`\square`

+----------+----------------------------------------------------+
|  Symbol  |Description                                         |
|          |                                                    |
+==========+====================================================+
| |b|      |Implemented and fully tested                        |
+----------+----------------------------------------------------+
| |c|      |Implemented with limited testing                    |
+----------+----------------------------------------------------+
| |s|      |Not tested or status unknown                        |
+----------+----------------------------------------------------+
| †        |Models not extended to DMP-parallel are only        |
|          |available for serial runs                           |
+----------+----------------------------------------------------+
| ‡        |Models not extended to SMP-parallel are available   |
|          |for SMP runs but do not scale with thread count     |
+----------+----------------------------------------------------+

MFiX-TFM (Two-Fluid Model)
~~~~~~~~~~~~~~~~~~~~~~~~~~

**MFiX-TFM (Two-Fluid Model)** is an Eulerian-Eulerian model, which
supports a broad range of capabilities for dense, reacting, multiphase
flows by representing the fluid and solids as interpenetrating continua.
This is the most mature MFiX model and is capable of modeling multiphase
reactors ranging in size from benchtop to industry-scale. Approximation
of the solid phase as a continuum typically allows for faster simulation
time than Lagrangian techniques; however, it also introduces the need
for accurate mathematical models to capture realistic solids phase
behavior. This includes transport properties, heterogeneous reaction
kinetics, and constitutive relations for interaction between fluid and
solid phases, e.g., solids phase drag and interphase heat transfer.

+----------------------+----------+--------+---------+
| Feature              | Serial   | †DMP   | ‡SMP    |
+======================+==========+========+=========+
| Momentum Equations   | |b|      | |b|    | |b|     |
+----------------------+----------+--------+---------+
| Energy Equations     | |b|      | |b|    | |b|     |
+----------------------+----------+--------+---------+
| Species Equations    | |b|      | |b|    | |b|     |
+----------------------+----------+--------+---------+
| Chemical Reactions   | |b|      | |b|    |         |
+----------------------+----------+--------+---------+
| Cartesian cut-cell   | |b|      | |b|    | **□**   |
+----------------------+----------+--------+---------+

MFiX-DEM (Discrete Element Model)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**MFiX-DEM (Discrete Element Model)** is an Eulerian-Lagrangian model
that treats the fluid phase as a continuum and models the individual
particles of the solid phase. This is a relatively new variation on
MFiX. While the treatment of individual particles can provide higher
fidelity over a broad range of flow regimes (from dilute to packed), it
becomes very challenging (in terms of computational resources)
when dealing with very large numbers of particles
for large-scale simulations. These large-scale applications will require
high performance computing (HPC) resources and large amounts of computer
time. Code optimization and speed up are critical research fronts to
support industrial scale applications.

+----------------------+----------+--------+--------+
| Feature              | Serial   | †DMP   | ‡SMP   |
+======================+==========+========+========+
| Momentum Equations   | |b|      | |b|    | |b|    |
+----------------------+----------+--------+--------+
| Energy Equations     | |b|      | |b|    |        |
+----------------------+----------+--------+--------+
| Species Equations    | |b|      | |b|    |        |
+----------------------+----------+--------+--------+
| Chemical Reactions   | |b|      | |b|    |        |
+----------------------+----------+--------+--------+
| Cartesian cut-cell   | |c|      | |c|    |        |
+----------------------+----------+--------+--------+

MFiX-PIC (Multiphase Particle in Cell)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**MFiX-PIC (Multiphase Particle in Cell)** is another
Eulerian-Lagrangian model that represents the fluid as a continuum while
using "parcels" to represent groups of real particles with similar
physical characteristics. The MFiX-PIC approach offers reduced
computational cost over MFiX-DEM as there are typically fewer parcels to
track and parcel collisions are not resolved. However, the added
modeling approximations influence the overall accuracy of the method.
Development, validation, and optimization of modeling approximations are
critical research fronts.

+----------------------+----------+--------+--------+
| Feature              | Serial   | †DMP   | ‡SMP   |
+======================+==========+========+========+
| Momentum Equations   | |b|      |        | |c|    |
+----------------------+----------+--------+--------+
| Energy Equations     |          |        |        |
+----------------------+----------+--------+--------+
| Species Equations    |          |        |        |
+----------------------+----------+--------+--------+
| Chemical Reactions   |          |        |        |
+----------------------+----------+--------+--------+
| Cartesian cut-cell   | |c|      | |s|    |        |
+----------------------+----------+--------+--------+

.. note::
   Creating MFiX-PIC models in the GUI is not yet supported.

MFiX-Hybrid (Eulerian-Lagrangian-Eulerian)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**MFiX-Hybrid (Eulerian-Lagrangian-Eulerian)** is a blend of MFiX-TFM
and MFiX-DEM that represents the fluid as a continuum and models solids
as either a continuous phase (TFM) or discrete particles (DEM). This
technique is presently restricted to solving only the momentum equations
to yield hydrodynamic predictions. This model is still in its infancy
and has seen only limited testing.

+----------------------+----------+--------+--------+
| Feature              | Serial   | †DMP   | ‡SMP   |
+======================+==========+========+========+
| Momentum Equations   | |c|      | |c|    | |c|    |
+----------------------+----------+--------+--------+
| Energy Equations     |          |        |        |
+----------------------+----------+--------+--------+
| Species Equations    |          |        |        |
+----------------------+----------+--------+--------+
| Chemical Reactions   |          |        |        |
+----------------------+----------+--------+--------+
| Cartesian cut-cell   | |c|      | |c|    | |c|    |
+----------------------+----------+--------+--------+

.. note::
   Creating MFiX-Hybrid models in the GUI is not yet supported.


GUI
---

The MFiX GUI is a front-end tool that allows users to quickly set-up MFiX
models, run the developed models, and provide post-processing tools
with the goal of making MFiX easy to use.

The GUI is written in Python using the cross-platform Qt framework. The Anaconda
platform is used for MFiX dependencies, which allows us to support MFiX on
Linux, Windows, and Mac. The `Visualization Toolkit (VTK)
<https://www.vtk.org/>`__ is used to visualize and manipulate the input geometry.

.. _mailing-list:

Mailing List
------------

Several mailing lists are available to communicate among MFIX users and
developers. When your subscription to MFIX is accepted, you are automatically
added to the mfixnews mailing list, where important announcements about MFIX are
shared with the MFIX community.

The most widely used mailing list is mfix-help, which allows users to post
questions and eventually help other users with similar issues.

The mailing list home page is located at https://mfix.netl.doe.gov/sympa. Click
on the "List of lists" tab to view all available mailing lists. Most of them
have a very low bandwidth, and most users only subscribe to the mfix-help list.

Once you subscribe to a list, you can send/receive messages to/from the MFIX
community. You can also search archived messages to see if there is already a
solution to a common problem.

There are many options to manage your subscription, including subscribing,
unsubscribing, and choosing the delivery mode.

Please visit https://mfix.netl.doe.gov/sympa/help/user to view the mailing list user guide.

Mailing list etiquette:

  1) Please allow sufficient time (say 2 to 3 business days) for MFIX developers and
     users to reply before posting unanswered questions again.
  2) Unless prior arrangement has been made with a given MFIX developer, do not
     send requests directly to the developer, but send the request to the appropriate
     mailing list instead. This ensures proper archiving of the thread and provides better
     opportunity for everyone to reply. Follow-up questions should also be sent to the
     mailing list.
  3) Do not ask for a copy of a reference, e.g., a journal article.
  4) Prior to submitting help requests regarding MFIX installation or compilation issues,
     please check the archives of mfix-help and if you are still having a problem, email
     mfix-help@mfix.netl.doe.gov by providing the following important details in your
     message after the description of the problem encountered:

        a. MFIX version you are trying to install or run
        b. Some details on your operating system environment (for Linux: copy and
           paste the output of the command "uname –a", Linux distribution name and
           version also)
        c. Your compiler name and version number (e.g. ifort -v will give the version
           number for Intel Fortran compiler)
        d. Output for your $PATH environment (in csh type echo $PATH)
        e. Your MPI library name and version number (if compilations problem with
           DMP mode encountered but make sure you can compile and run a simple
           hello world type MPI program with your current installation) Also please
           provide hardware details such as number of cores per socket in your
           system (or send the output for "cat /proc/cpuinfo" and how many cores
           you are trying to utilize.

.. note::

  Common reasons you may not receive an answer to your request

  1. You did not subscribe to the mailing list.
  2. You sent the request to an individual and not to the mailing list.
  3. Your question has already been answered and is available in the archive.
  4. You did not provide sufficient description of your problem
     (saying "It doesn’t work" is not useful).
  5. Your question is outside the scope of the mailing list.

User contribution
-----------------

If you wish to contribute to the development of MFIX, please contact the MFIX
team at admin@mfix.netl.doe.gov. We are looking for simulation results (figures,
animations, input files, user-defined subroutines), and new models that could
benefit the entire MFIX community. If you have written or know any publication
that uses MFIX, please let us know and we will post the citation on the website
(https://mfix.netl.doe.gov/publications/publications-citations/). Proper credit
will be given to all contributors.

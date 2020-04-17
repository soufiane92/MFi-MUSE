Running Interactive Solver Job in Queue
=======================================

MFiX solver jobs can also be submitted to a queue through the :ref:`run-dialog`.
This functionality needs to be added to the GUI by the user by creating a
`Queue Template`. This template allows users to customize the functionality of
the GUI for their specific system.

Currently only a queue template that supports queue submissions on
`Joule <https://hpc.netl.doe.gov/about/supercomputer>`__ is included. Joule uses
gridengine as the queueing system.

If you do not want to use the GUI or interactive features with your batch job,
you can build a :ref:`batch_solver` and run as in MFiX 2016 and earlier.

Queue Templates
~~~~~~~~~~~~~~~

Queue templates included with the source
can be found in the ``MFIX_HOME\queue_templates`` directory. Queue
templates are files that contain two sections. The first section is the
configuration section that tells the GUI what widgets to display as well
as various options for those widgets. The second section is the actual
script that is executed.

Variables can be used throughout the template, including with the
widgets, and are reference by ``${my_variable}``. There are a couple of
built in variables including:

+-----------------+-------------------------------------------------+
| Variable        | Description                                     |
+=================+=================================================+
| SCRIPT          | the path of this script, in the run directory   |
+-----------------+-------------------------------------------------+
| PROJECT\_NAME   | the name of the project                         |
+-----------------+-------------------------------------------------+
| JOB\_ID         | the job id extracted from the submit command    |
+-----------------+-------------------------------------------------+
| COMMAND         | the command that executes mfix                  |
+-----------------+-------------------------------------------------+

The configuration section starts with ``## CONFIG`` and ends with
``## END CONFIG``. This section uses the Microsoft Windows `INI file <https://en.wikipedia.org/wiki/INI_file>`__
format. Sections are defined with a ``[section]`` header, followed by a
collection of ``key=value`` or ``key: value`` entries for defining
parameters. For example:

::

    ## CONFIG
    [My Section]
    foodir: %(dir)s/whatever
    dir=frob
    long: this value continues
       in the next line
    ## END CONFIG

The configuration section has a special section called ``[options]``
where the following options can be specified:

+----------------+----------------------------------------------------------------------------------+
|Key             |Description                                                                       |
+================+==================================================================================+
|name            |name of the template, this is displayed in the template drop-down box in the GUI  |
+----------------+----------------------------------------------------------------------------------+
|job\_id\_regex  |regular expression to extract the job id from the output of the submit command    |
+----------------+----------------------------------------------------------------------------------+
|status\_regex   |regular expression to extract the job status from the status command              |
+----------------+----------------------------------------------------------------------------------+
|submit          |the submission command                                                            |
+----------------+----------------------------------------------------------------------------------+
|delete          |the job cancel or delete command                                                  |
+----------------+----------------------------------------------------------------------------------+
|status          |the job status command                                                            |
+----------------+----------------------------------------------------------------------------------+

An example of values that work with Grid Engine:

::

    [options]
    name: Joule
    job_id_regex: (\d+)
    status_regex: ([rqw])
    submit: qsub ${SCRIPT}
    delete: qdel ${JOB_ID}
    status: qstat -j ${JOB_ID}

To define a new variable and widget to edit that variable in the GUI,
create a new section:

::

    [my_value]

The widget and options for that widget can then be selected by
specifying various parameters including:

+-------------+-------------------------------+-------------------------------------+
|  Parameter  |Description                    |Values                               |
|             |                               |                                     |
+=============+===============================+=====================================+
|   widget    |the widget to be used          | ``lineedit``, ``combobox``,         |
|             |                               | ``checkbox``, ``spinbox``,          |
|             |                               | ``doublespinbox``, ``listwidget``   |
+-------------+-------------------------------+-------------------------------------+
|    label    |text to be placed beside the   | ``any string``                      |
|             |widget                         |                                     |
+-------------+-------------------------------+-------------------------------------+
|    value    |default value                  | a value such as ``1``, ``10.3``,    |
|             |                               | ``True``, ``some text``             |
+-------------+-------------------------------+-------------------------------------+
|    items    |list of items for the combobox | items delimited by \| character     |
|             |or listwidget                  |                                     |
+-------------+-------------------------------+-------------------------------------+
|    help     |text to be displayed in the    | ``this widget does this``           |
|             |tooltip for that widget        |                                     |
+-------------+-------------------------------+-------------------------------------+
|    true     |value to be returned if a      | a value such as ``1``, ``10.3``,    |
|             |checkbox is checked            | ``True``, ``some text``             |
+-------------+-------------------------------+-------------------------------------+
|    false    |value to be returned if a      | a value such as ``1``, ``10.3``,    |
|             |checkbox is un-checked         | ``True``, ``some text``             |
+-------------+-------------------------------+-------------------------------------+

An example defining a combo box:

::

    [my_email]
    widget: combobox
    label: email
    value: you@mail.com
    items: you@mail.com|
           me@mail.net|
           hi@mail.org
    help: email to send notifications to

The value of this widget can now be referenced throughout the template
with ``${my_email}``

The rest of the configuration file, outside of the ``## CONFIG`` to
``## END CONFIG`` section is the script that needs to be executed to
submit a job to your specific queue system. For example, with Grid
Engine on Joule, the following script specifies the job name, job type,
cores, and queue as well as loads the required modules and finally runs
mfix with ``${COMMAND}``.

::

    ## Change into the current working directory
    #$ -cwd
    ##
    ## The name for the job. It will be displayed this way on qstat
    #$ -N ${JOB_NAME}
    ##
    ## Number of cores to request
    #$ -pe ${JOB_TYPE} ${CORES}
    ##
    ## Queue Name
    #$ -q ${QUEUE}
    ##

    ##Load Modules
    module load ${MODULES}
    ##Run the job
    ${COMMAND}

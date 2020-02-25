#!/usr/bin/env python

# This module is only intended to be used with 'pymfix' since it depends
#  on pymfix to start and manage the mfix thread, etc.
# This code in a separate module so that pymfix can optionally run without
#  the web interface, in which case we skip loading this module

import simplejson # make sure Flask uses 'simplejson' backend
from flask import Flask, jsonify, request

import os
import sys
import random
import string

# Prevent logging every request to stdout
import logging
log = logging.getLogger('werkzeug')
log.setLevel(logging.WARN)

#Globals
F = Flask('pymfix')
secret_key = ''.join(random.choice(string.digits + string.ascii_letters)
                     for x in range(0, 20))

run_name = 'unset' # set by pymfix
mfix_thread = None # set by pymfix
M = None # set by pymfix
options = None #set by pymfix

# Views
@F.route('/')
def default():
    return "hello"

@F.route('/exit')
def exit():
    # Cause the entire server to stop
    shutdown = request.environ.get('werkzeug.server.shutdown')
    if shutdown:
        shutdown() # will finish current request, then shutdown
    else:
        sys.exit(0)
    return "goodbye"

@F.route('/stop')
def stop():
    M.exit.exit_flag = True
    M.pause.pause_flag = False
    # Write an 'MFIX.STOP' file?  kill the process?
    # If the mfix thread is daemonic, 'join' not needed
    #if mfix_thread:
    #    mfix_thread.thread.join()
    return "stop OK"

@F.route('/pause')
def pause():
    M.pause.pause_flag = True
    return "pause OK"

@F.route('/unpause')
def unpause():
    M.pause.pause_flag = False
    return "unpause OK"

@F.route('/status')
def status():
    # Conversions to float/int are because the f2py objects are 1-element np arrays
    tstop = float(M.run.tstop)
    time = float(M.run.time)
    time_start = float(M.time_cpu.time_start)
    wall_start = float(M.time_cpu.wall_start)
    wall_now = float(M.machine.wall_time())
    wall_elapsed = wall_now - wall_start if wall_start else 0
    running = mfix_thread and mfix_thread.isAlive()
    paused = bool(M.pause.pause_flag)
    finished = bool(M.exit.exit_flag or time>=tstop)
    wall_paused = float(M.time_cpu.wall_paused)

    d = { 'paused': paused,
          'finished': finished,
          'time': time,
          'tstop': tstop,
          'dt': float(M.run.dt),
          'nit': int(M.iterate.nit),
          'pid': os.getpid(),
          'run_name': run_name,
          'cpu0': float(M.time_cpu.cpu0),
          'wall0': float(M.time_cpu.wall0),
          'wall_start ': wall_start,
          'walltime': wall_now,
          'walltime_paused': wall_paused,
          'running': running
    }
    """
         WALL_LEFT = (WALL_NOW-WALL_START)*(TSTOP-TIME)/               &
            max(TIME-TIME_START,1.0d-6)
         CALL GET_TUNIT(WALL_LEFT, UNIT_LEFT)
    """
    if not running: # Job never started
        wall_left = 0
    else:
        wall_left = (wall_elapsed - wall_paused) * (tstop - time) / (max(time - time_start, 1e-6))
    wall_left = max(wall_left, 0)
    d['walltime_remaining'] = wall_left
    d['walltime_elapsed'] = wall_elapsed # includes paused time

    R = M.residual_pub
    if R.group_resid:
        get_count, get_name, get_val = (R.get_resid_grp_string_count,
                                        R.get_resid_grp_string,
                                        R.get_resid_grp)
    else:
        get_count, get_name, get_val = (R.get_resid_string_count,
                                        R.get_resid_string,
                                        R.get_resid)

    residuals = [(get_name(i).strip(), get_val(i)) for i in range(1,1+get_count())]

    d['residuals'] = residuals

    # If stdout/stderr has been redirected to log file (eg on batch system)
    #  the fstat calls return nonzero, even if -d mode not enabled
    if options.daemon:
        output_bytes = {'stdout_bytes':  os.fstat(sys.stdout.fileno()).st_size,
                        'stderr_bytes':  os.fstat(sys.stderr.fileno()).st_size}
    else:
        output_bytes = {'stdout_bytes': 0,
                        'stderr_bytes': 0}

    return jsonify(mfix_status=d,
                   output_bytes=output_bytes)


@F.route('/reinit', methods=['POST'])
def reinit():
    if not M.pause.pause_flag:
        return("model not paused", 403)

    try:
        data = request.get_json(force=True)
        mfix_dat = data.get('mfix_dat')
        autostart = data.get('autostart')
        # It's really too bad that we allowed utf-8 into mfix.dat,
        # can we undo that decision?
        mfix_dat = mfix_dat.encode('utf-8')
        # Note, assigning directly to reinit_data does not work
        M.pause.set_reinit_data(mfix_dat)
        M.pause.autostart_flag = bool(autostart)
        M.pause.reinit_flag = True
        return "reinit OK"
    except Exception as e:
        return(str(e), 500)


@F.route('/help')
def help():
    views = [v for v in F.view_functions.keys()
             if v not in ('default', 'static')]
    return '<br>'.join(views)


@F.route('/stdout', methods=['GET'])
@F.route('/stdout/<nbytes>', methods=['GET'])
def get_stdout(nbytes=0):
    max_size = 1024*1024
    stdout_fd = sys.stdout.fileno()
    try:
        nbytes = int(nbytes)
        os.lseek(stdout_fd, nbytes, 0)
        data = os.read(stdout_fd, max_size)
    except Exception as e:
        return (str(e), 500)


    return (jsonify(stdout=data.decode('utf-8', errors='ignore'),
                    stdout_bytes=nbytes+len(data)), 200)


@F.route('/stderr', methods=['GET'])
@F.route('/stderr/<nbytes>', methods=['GET'])
def get_stderr(nbytes=0):
    max_size = 1024*1024
    stderr_fd = sys.stderr.fileno()
    try:
        nbytes = int(nbytes)
        os.lseek(stderr_fd, nbytes, 0)
        data = os.read(stderr_fd, max_size)
    except Exception as e:
        return (str(e), 500)

    return (jsonify(stderr=data.decode('utf-8', errors='ignore'),
                    stderr_bytes=nbytes+len(data)),
            200)


def start(port=None):
    F.run(host='0.0.0.0', port=port,
              debug=False, use_reloader=False)

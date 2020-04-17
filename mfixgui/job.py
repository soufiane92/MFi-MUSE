# -*- coding: utf-8 -*-

from subprocess import Popen, PIPE
import errno
import simplejson as json
import os
import re
import signal
import socket
import time

import psutil

from qtpy.QtCore import QObject, QTimer, QUrl, Signal
from qtpy.QtNetwork import QNetworkAccessManager, QNetworkRequest, QNetworkReply

from mfixgui.tools import replace_with_dict


def get_process_info(filename):
    """Read contents of provided MFiX job pid file and set supported
    key-value pairs as dictionary members
    """
    d = {}
    try:
        with open(filename, encoding='utf-8', errors='replace') as f:
            for line in f:
                if line.startswith('#'):
                    continue
                tok = [x.strip() for x in line.split('=', 1)]
                if len(tok) != 2:
                    raise ValueError(line)
                k, v = tok
                if v.isdigit():
                    v = int(v)
                else:
                    if v == 'None':
                        v = None
                d[k] = v
    except OSError as e:
        if e.errno != errno.ENOENT: # Return empty dict if no file
            raise
    return d


class PymfixAPI(QNetworkAccessManager):

    def __init__(self, parent):

        super(PymfixAPI, self).__init__()
        self.parent = parent
        self.warning = parent.warning
        self.error = parent.error
        self.pidfile = parent.pidfile
        self.process_info = get_process_info(self.pidfile)
        self.mfix_pid = self.process_info.get('pid')
        self.job_id = self.process_info.get('job_id')
        self.run_cmd = self.process_info.get('run_cmd')
        self.api_available = False
        self.stderr_bytes = self.stdout_bytes = 0
        self.stderr_collected = self.stdout_collected = False
        self.methods = {'put': super(PymfixAPI, self).put,
                        'get': super(PymfixAPI, self).get,
                        'post': super(PymfixAPI, self).post,
                        'delete': super(PymfixAPI, self).deleteResource}

    def test_connection(self, response, error=None):
        self.get_status()

    def get_status(self):
        url = self.process_info.get('url')
        host = self.process_info.get('host') # could get from url
        if not url:
            errmsg = 'cannot connect, no URL found for mfix process'
            self.error(errmsg)
            self.parent.status = {'error': errmsg}
            #self.get_status_error()
            self.parent.parent.parent.slot_update_status()
        # issues/412 circumvent network check if job is local
        if host in ('127.0.0.1', 'localhost'):
            self.setNetworkAccessible(True)
        request = QNetworkRequest(QUrl(url + '/status'))
        reply = self.methods['get'](request) # ugh
        reply.error.connect(lambda error, reply=reply, self=self: self.get_status_error(error, reply)) # pylint: disable=undefined-variable
        reply.finished.connect(lambda reply=reply, self=self: self.get_status_finished(reply)) # pylint: disable=undefined-variable


    def get_status_finished(self, reply):
        try:
            content_type = reply.header(QNetworkRequest.ContentTypeHeader)
            if not content_type:
                return
            response = reply.readAll()
            if not response:
                return

            self.parent.pending = False

            data = response.data().decode('utf8', errors='ignore')
            if 'application/json' in content_type:
                data = json.loads(data)

                mfix_status = data.get('mfix_status',
                                       {'error': 'no status returned'})

                self.parent.status = mfix_status # leads to stale status
                self.parent.parent.parent.slot_update_status()
                # let's change to passing always-fresh status as a parameter
                #self.parent.parent.parent.slot_update_status(mfix_status)

                # Collect remote stderr/stdout
                output_bytes = data.get('output_bytes')
                if output_bytes:
                    self.fetch_output(output_bytes)

            elif 'text/html' in content_type:
                #self.parent.parent.parent.error(data, popup=False)
                #no, too many popups!
                self.show_response(data)

            else:
                print("Unknown content type:", content_type)

        finally:
            reply.deleteLater()

    def show_response(self, text): #
        try:
            self.parent.parent.parent.ui.responses.setText(text) # TODO clean this up
            if '200' not in text:
                self.parent.parent.parent.error(text)

        except Exception as e:
            print(e, text)


    def get_status_error(self, error_code, reply):
        try:
            if error_code == QNetworkReply.ConnectionRefusedError:
                ## todo,  increment error count
                return
            self.show_response("%s %s" % (error_code, reply.errorString()))
        finally:
            reply.deleteLater()



    def fetch_output(self, output_bytes):
        # fetch stdout/stderr via HTTP
        stdout_bytes = output_bytes['stdout_bytes']
        stderr_bytes = output_bytes['stderr_bytes']

        if stdout_bytes > self.stdout_bytes:
            self.stdout_collected = False
            self.get_stdout(self.stdout_bytes)
            self.stdout_bytes = stdout_bytes
        elif stdout_bytes < self.stdout_bytes: # File was truncated?
            self.get_stdout(0)
            self.stdout_bytes = 0
            self.stdout_collected = True
        else:
            self.stdout_collected = True # Nothing to collect

        if stderr_bytes > self.stderr_bytes:
            self.stderr_collected = False
            self.get_stderr(self.stderr_bytes)
            self.stderr_bytes = stderr_bytes
        elif stderr_bytes < self.stderr_bytes: # File was truncated?
            self.get_stderr(0)
            self.stderr_bytes = 0
            self.stderr_collected = True
        else:
            self.stderr_collected = True # Nothing to collect


    def get_stdout(self, last_nbytes):
        self.stdout_collected = False
        url = self.process_info.get('url')
        host = self.process_info.get('host') # could get from url
        # issues/412 circumvent network check if job is local
        if host in ('127.0.0.1', 'localhost'):
            self.setNetworkAccessible(True)

        request = QNetworkRequest(QUrl(url + '/stdout/%s'%last_nbytes))
        reply = self.methods['get'](request) # ugh
        reply.error.connect(lambda error, reply=reply, self=self: # pylint: disable=undefined-variable
                            self.get_stdout_error(error, reply))
        reply.finished.connect(lambda reply=reply, self=self: # pylint: disable=undefined-variable
                               self.get_stdout_finished(reply))

    def get_stdout_error(self, error_code, reply):
        print("Error %s %s\n" % (error_code, reply))

    def get_stdout_finished(self, reply):
        content_type = reply.header(QNetworkRequest.ContentTypeHeader)
        if not content_type:
            return
        response = reply.readAll()
        if not response:
            return
        data = response.data().decode('utf8', errors='ignore')
        if 'application/json' in content_type:
            data = json.loads(data)
        output = data.get('stdout')
        if output is not None:
            self.parent.parent.parent.stdout_signal.emit(output) # ?
        stdout_bytes = data.get('stdout_bytes')
        if stdout_bytes is not None:
            self.stdout_bytes = stdout_bytes
        self.stdout_collected = True

    def get_stderr(self, last_nbytes):
        self.stderr_collected = False
        url = self.process_info.get('url')
        host = self.process_info.get('host') # could get from url
        # issues/412 circumvent network check if job is local
        if host in ('127.0.0.1', 'localhost'):
            self.setNetworkAccessible(True)
        request = QNetworkRequest(QUrl(url + '/stderr/%s'%last_nbytes))
        reply = self.methods['get'](request) # ugh
        reply.error.connect(lambda error, reply=reply, self=self: # pylint: disable=undefined-variable
                            self.get_stderr_error(error, reply))
        reply.finished.connect(lambda reply=reply, self=self: # pylint: disable=undefined-variable
                               self.get_stderr_finished(reply))


    def get_stderr_finished(self, reply):
        content_type = reply.header(QNetworkRequest.ContentTypeHeader)
        if not content_type:
            return
        response = reply.readAll()
        if not response:
            return
        data = response.data().decode('utf8', errors='ignore')
        if 'application/json' in content_type:
            data = json.loads(data)
        output = data.get('stderr')
        if output is not None:
            self.parent.parent.parent.stderr_signal.emit(output)
        stderr_bytes = data.get('stderr_bytes')
        if stderr_bytes is not None:
            self.stderr_bytes = stderr_bytes
        self.stderr_collected = True

    def request(self, method, endpoint, data=None, headers=None):
        if not headers:
            headers = {}
        url = self.process_info.get('url')
        token = self.process_info.get('token')
        host = self.process_info.get('host')
        if host in ('127.0.0.1', 'localhost'):
            self.setNetworkAccessible(True)
        req = QNetworkRequest(QUrl('%s/%s' % (url, endpoint)))
        method = self.methods[str(method).lower()]

        # assemble headers
        if 'content-type' not in [x.lower() for x in headers.keys() if isinstance(x, str)]:
            headers['content-type'] = 'text/plain'
        headers['content-length'] = str(len(data if data else ''))
        if token:
            (name, value) = token.split(':')
            headers[name] = value
        for k, v in headers.items():
            kb = k.encode('utf-8') # why?
            vb = v.encode('utf-8')
            req.setRawHeader(kb, vb)

        if data is not None:
            reply = method(req, data)
        else:
            reply = method(req)
        reply.error.connect(lambda error, reply=reply, self=self: # pylint: disable=undefined-variable
                            self.handle_error(error, reply))
        reply.finished.connect(lambda reply=reply, self=self: # pylint: disable=undefined-variable
                               self.print_reply(reply))
        return reply

    def print_reply(self, reply):
        try:
            content_type = reply.header(QNetworkRequest.ContentTypeHeader)
            if not content_type:
                return
            response = reply.readAll()
            if not response:
                return
            data =  response.data().decode('utf-8', errors='ignore')

            if '<title>' not in data: # don't mess up preformmated page
                http_code = reply.attribute(QNetworkRequest.HttpStatusCodeAttribute)
                data = "%s %s" % ( http_code, data)
            if 'text/plain' in content_type:
                self.show_response(data)
            elif 'text/html' in content_type:
                self.show_response(data)
            else:
                print("Unknown content_type", content_type)

        finally:
            reply.deleteLater() # Will python gc these?

    def handle_error(self, error_code, reply):
        try:
            if error_code == QNetworkReply.ConnectionRefusedError:
                if self.parent.parent.stopping:
                    return
            self.show_response("%s %s" % (error_code, reply.errorString()))
        finally:
            reply.deleteLater() # Will python gc these?

    def get(self, endpoint, handlers=None):
        return self.request('get', endpoint)

    def put(self, endpoint, data=b'', handlers=None):
        return self.request('put', endpoint, data=data)

    def post(self, endpoint, data=b'', headers=None):
        return self.request('post', endpoint, data=data, headers=headers)


class JobManager(QObject):

    sig_update_job_status = Signal()
    sig_change_run_state = Signal()

    def __init__(self, parent):
        super(JobManager, self).__init__()
        self.job = None
        self.parent = parent
        self.pidfile = None
        self.warning = parent.warning
        self.error = parent.error
        self.error_count = 0
        self.ERROR_LIMIT = 3 # just like baseball
        self.stopping = False
        self.pausing = False


    def cleanup_stale_pidfile(self, pidfile):
        """Delete pidfile if it refers to a job that is no longer running.
        Return True if file was stale/deleted"""
        try:
            process_info = get_process_info(pidfile)
        except Exception as e:
            self.error("Removing file %s due to exception %s" % (pidfile, e))
            os.unlink(pidfile)
            return True

        pid = process_info.get('pid')
        job_id = process_info.get('job_id')
        url = process_info.get('url')
        host = process_info.get('host')

        if host not in ('127.0.0.1', 'localhost', socket.gethostname()):
            # TODO check status of remote job using qstat
            return False # Assume remote job is alive

        if pid is not None and psutil.pid_exists(pid):
            return False

        try:
            #self.warning("Removing %s" % pidfile)
            os.unlink(pidfile)
        except Exception as e:
            self.error('%s: %s' % (pidfile, e))
        return True


    def try_to_connect(self, pidfile):
        if self.job: # Already connected
            return

        if pidfile is None:
            return

        if os.path.isfile(pidfile):
            self.reset_error_count()
            if self.cleanup_stale_pidfile(pidfile):
                return
            self.job = Job(self, pidfile)
            self.pidfile = pidfile
            self.job.connect()
            # connect Job signals
            self.job.sig_job_exit.connect(self.teardown_job)
            self.job.sig_update_job_status.connect(self.sig_update_job_status)
            self.job.sig_change_run_state.connect(self.sig_change_run_state)
            self.job.sig_error.connect(self.increment_error_count)
            self.job.sig_success.connect(self.reset_error_count)


    def reset_error_count(self):
        self.error_count = 0

    def increment_error_count(self):
        """Increment API error count. Signal job exit if limit has been exceeded."""
        if self.stopping:
            return

        self.error_count += 1
        count = self.error_count
        if count >= self.ERROR_LIMIT:
            self.error("MFiX process is not responding")
            self.teardown_job()

    def is_job_pending(self):
        return self.job and self.job.pending

    def is_job_ready(self):
        return self.job and self.job.api_available

    def is_job_paused(self):
        return self.job and self.job.is_paused()

    def is_output_pending(self):
        return self.job and self.job.is_output_pending()

    def pause_job(self):
        if self.job:
            self.pausing = True
            self.job.pause()

    def submit_command(self,
                       qscript,
                       submit_cmd,
                       delete_cmd, # XXX UNUSED
                       status_cmd,
                       job_id_regex,
                       replace_dict):

        project_dir = self.parent.get_project_dir()
        script_name = '.qsubmit_script'

        # write the script
        with open(os.path.join(project_dir, script_name), 'w', encoding='utf-8', errors='replace') as f:
            f.write(qscript)

        replace_dict['SCRIPT'] = script_name

        submit_cmd = replace_with_dict(submit_cmd, replace_dict)

        # submit the job
        self.parent.print_internal("Job submit CMD: {}".format(submit_cmd),
                                   color='blue')

        proc = Popen(submit_cmd, shell=True, stdout=PIPE, stderr=PIPE,
                     cwd=self.parent.get_project_dir(), env=dict(os.environ, LD_PRELOAD=""))
        out, err = proc.communicate()
        if job_id_regex is not None and out:
            job_id = re.findall(job_id_regex, str(out))
        else:
            job_id = []
        if job_id:
            job_id = job_id[0]
            self.parent.print_internal("Job successfully submitted with job id: {}".format(job_id),
                                       color='blue')
        else:
            self.parent.error('Could not determine job id')
            job_id = None
        if err:
            self.parent.error('Error with submission:\n{}'.format(str(err)))

        #TODO:
        # use status_cmd to see if it queued, running, etc.

    def exit_mfix(self):
        if self.job is not None:
            self.job.mtime = 0
            self.job.exit_mfix()


    def stop_mfix(self):
        if self.job is not None:
            self.stopping = True
            self.job.mtime = 0
            self.job.stop_mfix()


    def force_stop_mfix(self):
        if self.job is not None:
            self.stopping = True
            self.job.mtime = 0
            self.job.force_stop_mfix()
            self.teardown_job()


    def disconnect(self):
        """disconnect job"""
        if self.job:
            self.job.stop_timers() # cleanup_and_exit recurses
        self.job = None
        self.sig_update_job_status.emit()
        self.sig_change_run_state.emit()

    def teardown_job(self):
        """Job ended or exited. Destroy Job object and remove pidfile"""
        # TODO: this handles local only, not queued jobs
        pidfile = self.pidfile
        if pidfile:
            try:
                process_info = get_process_info(pidfile)
                pid = process_info.get('pid')
                job_id = process_info.get('qjobid')

            except Exception as e:
                self.error("Cannot get PID: %s" % e)
                pid = job_id = None

            finally:
                # update GUI
                if self.job:
                    self.job.stop_timers() # cleanup_and_exit recurses
                self.job = None
                self.sig_update_job_status.emit()
                self.sig_change_run_state.emit()



class Job(QObject):
    """Class for managing and monitoring an MFiX job. This class contains
    methods for issuing requests to and handling responses from the MFiX API.

    pidfile: Name of file containing MFiX API connection information.
    """

    # Signals
    sig_job_exit = Signal()
    sig_response = Signal(str, dict)
    sig_error = Signal()
    sig_success = Signal()
    sig_handle_test = Signal(str, dict)
    sig_handle_test_error = Signal(str, QObject)
    sig_update_job_status = Signal()
    sig_change_run_state = Signal()

    def __init__(self, parent, pidfile):
        super(Job, self).__init__()
        self.parent = parent
        self.warning = parent.warning
        self.error = parent.error
        self.mfix_pid = None
        self.job_id = None
        self.mtime = time.time() # Job start time
        self.status = {}
        self.pidfile = pidfile
        self.api = None
        self.api_available = False
        self.pending = True

        self.sig_handle_test.connect(self.slot_handle_test)
        self.sig_handle_test_error.connect(self.slot_handle_test_error)

        self.api = PymfixAPI(self)

        self.mfix_pid = self.api.mfix_pid
        self.job_id = self.api.job_id
        self.run_cmd = self.api.run_cmd

        # test API before starting status loop
        # Timer will be stopped by slot_handle_test
        self.api_test_timer = QTimer()
        self.api_test_timer.setInterval(100)
        self.api_test_timer.timeout.connect(self.test_connection)
        self.api_test_timer.start()
        # don't start status timer here, it is started once API is responsive
        self.api_status_timer = QTimer()
        self.api_status_timer.setInterval(100)
        self.api_status_timer.timeout.connect(self.update_job_status)


    def update_job_status(self):
        self.api.get_status()

    def connect(self):
        self.test_connection()

    def stop_timers(self):
        if self.api_test_timer and self.api_test_timer.isActive():
            self.api_test_timer.stop()
        if self.api_status_timer and self.api_status_timer.isActive():
            self.api_status_timer.stop()

    def cleanup_and_exit(self):
        self.stop_timers()
        self.api_available = False
        self.sig_job_exit.emit()

    def test_connection(self):
        """Test API connection.
        Start API test timer if it is not already running. Check current test
        count and signal job exit if timeout has been exceeded."""
        try:
            self.api.get_status()
        except Exception as e: #
            #print("SIG_ERROR", e) #increments error count
            self.sig_error.emit()

    def slot_handle_test_error(self, req_id, reply):
        # this can be hit multiple times: until JobManager
        # error limit is reached
        self.error('Network error: request id %s' % req_id)


    def slot_handle_test(self, req_id, response):
        """Parse response data from API test call. If response is in JSON
        format and does not contain an error message, then stop API test timer
        and start API status timer.

        If API response includes a permenant failure or is not well formed,
        emit :class:`Job.sig_error` signal.
        """
        # this can be hit multiple times: until we hit JobManager error limit
        try:
            if response.get('internal_error'):
                self.error('Internal error %s' % response)
                self.sig_error.emit()
                return
        except Exception as e:
            # response was not parsable, API is not functional
            self.error('Response format error: %s, response=%s' % (e, response))
            self.sig_error.emit()
            return

        # reset error count and mark API as available
        self.api_available = True
        # stop test timer and start status timer
        self.api_test_timer.stop()
        self.api_status_timer.start()
        self.sig_update_job_status.emit()
        self.sig_change_run_state.emit()

    def reinit(self, project_file_contents, autostart=False):
        """reinitialize job. Sanity checks (paused, gui.unsaved_flag, etc)
        must be handled by caller"""

        data = json.dumps({'mfix_dat': project_file_contents,
                           'autostart': autostart})

        # TODO move unicode conversion into 'post' method
        data = data.encode('utf-8')
        headers = {'content-type': 'application/json'}
        self.api.post('reinit', data=data, headers=headers)
        # TODO:  handlers for reinit success/failure (don't unpause job if reinit failed)
        # TODO restore GUI values to values matching running job if reinit failed (how?)


    def is_paused(self):
        return self.status.get('paused')

    def pause(self):
        self.api.get('pause')

    def unpause(self):
        self.api.get('unpause')

    def stop_mfix(self):
        self.api.get('stop')

    def exit_mfix(self):
        self.api.get('exit')

    def force_stop_mfix(self):
        """Send stop request"""
        if self.job_id:
            # Queued job; TODO qkill
            return

        if not self.mfix_pid:
            # Nothing to kill
            return

        try:
            p = psutil.Process(self.mfix_pid)
        except Exception as e:
            if isinstance(e, psutil.NoSuchProcess):
                return # Nothing to do
            else:
                raise

        is_mpi = False
        kill_pid = self.mfix_pid
        if self.run_cmd and 'mpirun' in self.run_cmd:
            while p:
                is_mpi = any('mpirun' in x for x in p.cmdline()) # cmdline is list of strings
                if is_mpi:
                    kill_pid = p.pid
                    break
                p = p.parent()
            else:
                return

        if is_mpi:
            sig = signal.SIGTERM
        else:
            sig = signal.SIGTERM
        try:
            os.kill(kill_pid, sig)
        except Exception as e:
            self.error("force_kill: %s" % e)


    def handle_stop_mfix(self, req_id, response):
        """Handler for responses to stop requests"""
        self.api_available = False
        self.api_status_timer.stop()
        self.handle_response(req_id, response)
        self.sig_change_run_state.emit()
        self.cleanup_and_exit()


    def is_output_pending(self):
        return not (self.api.stdout_collected and
                    self.api.stderr_collected)

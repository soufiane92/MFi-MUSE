
from qtpy import QtCore, QtWidgets
import os
import copy
import numpy as np
import time
import re

try:
    import pandas as pd
    PANDAS_AVAILABLE = True
except ImportError:
    PANDAS_AVAILABLE = False

NO_FILE = 0
HEADER_REGEX = re.compile(r'(?:[^\s,"]|"(?:\\.|[^"])*")+')


def safe_float(v, default=np.nan):
    try:
        return float(v)
    except ValueError:
        return default


def parse_header(line):
    """parse the header could be:
        # "Time", "value", "x_s(1,1)", "x_s(1,2)", "T_s(1)", "x_g(2)"
        "Time","ep_g","p_g","u_g","v_g","w_g","t_g","p_star","rop_s(1)","u_s(1)","v_s(1)","w_s(1)","t_s(1)","theta_m(1)"
    """
    line = line.replace('#', '').strip()
    return [s.strip().replace('"', '') for s in HEADER_REGEX.findall(line)]


class MonitorReader(QtCore.QObject):
    update = QtCore.Signal(str, dict)

    def __init__(self, backend='csv', gui=None):
        QtCore.QObject.__init__(self)

        # on NFS, QFileSystemWatcher does not work, create a timer to poll the
        # directory for files
        self.file_timer = QtCore.QTimer(self)
        self.file_timer.timeout.connect(self.check_dir)
        self.file_timer.start(1000)

        self.thread_pool = QtCore.QThreadPool()
        self.thread_pool.setMaxThreadCount(10)
        self.threads = {}
        self.backend = backend
        self.gui = gui
        self.reset()
        self.connections = 0
        self.paths = []

    def connect(self, method):
        for thread in self.threads.values():
            thread.pause = False
        self.update.connect(method)
        self.connections += 1

    def disconect(self, method):
        self.update.disconnect(method)
        self.connections = max(0, self.connections-1)

        if not self.connections:
            for thread in self.threads.values():
                thread.pause = True

    def reset(self):
        """call to reset when opening a new project"""
        self.stop_threads()
        self.data = {}
        self.find_file_list = []
        self.threads = {}
        self.paths = []
        self.resume_paths = []

    def updateValue(self, key, value, args=None):
        """receive keyword changed from project manager"""
        prj_dir = None
        if self.gui is not None:
            prj_dir = self.gui.get_project_dir()
            if prj_dir is not None:
                self.add_file(os.path.join(prj_dir, value))

    def stop_threads(self):
        self.resume_paths = list(self.threads.keys())
        for thread in self.threads.values():
            thread.stop = True
        self.threads = {}

        # wait a max of 1 second for threads to exit
        self.thread_pool.waitForDone(1000)

    def resume_threads(self):
        for path in self.resume_paths:
            self.add_file(path)

    def add_file(self, path):
        """add a monitor file to watch, it does not have to exist yet"""

        path = os.path.abspath(path)
        base = os.path.splitext(os.path.basename(path))[0].upper()
        path = os.path.dirname(path)

        if not base.endswith('.csv'):
            base += '.csv'

        path = os.path.join(path, base)
        if not os.path.exists(path):
            self.find_file_list.append(base)
            self.add_dir(os.path.dirname(path))
        else:
            self.create_thread(path)

    def add_dir(self, path):
        if path not in self.paths:
            self.paths.append(path)

    def create_thread(self, path):
        if path in self.threads:
            return
        self.thread_pool.setMaxThreadCount(len(self.threads)+2)
        p = ParserThread(path, self.backend)
        p.signals.finished.connect(self.update_data)
        p.signals.error.connect(self.thread_error)
        self.threads[path] = p
        self.thread_pool.start(p)

    def update_data(self, path, header, data):
        self.data[path] = {'header': header, 'data': data}
        self.update.emit(path, self.data)

    def check_dir(self):
        """look for non-exisiting files"""
        tmp = copy.deepcopy(self.find_file_list)
        for path in self.paths:
            for f in tmp:
                fp = os.path.join(path, f)
                if os.path.exists(fp):
                    self.add_file(fp)
                    try:
                        self.find_file_list.remove(f)
                    except ValueError:
                        # already removed, how?
                        pass

    def thread_error(self, error, path):
        if error == NO_FILE:
            if path in self.threads:
                self.threads.pop(path)
            self.add_file(path)


class ParserThreadSignals(QtCore.QObject):
    finished = QtCore.Signal(str, list, object)
    error = QtCore.Signal(int, str)


class ParserThread(QtCore.QRunnable):
    def __init__(self, path, backend='pandas'):
        QtCore.QRunnable.__init__(self)
        self.signals = ParserThreadSignals()
        self.path = path
        self.data = None
        self.pos = None
        self.backend = backend
        self.stop = False
        self.pause = False
        self.mt = 0
        self.master_header = None
        self.current_header = None
        self.previous_header = None

    def parse_pandas(self, csvfile):
        # FIXME: get pandas to work
        data = None

        header = [s.strip() for s in csvfile.readline().replace('#', '').split(',')]
        data = pd.read_csv(csvfile, names=header,
                           index_col=0, comment='#',
                           error_bad_lines=False, warn_bad_lines=False)

        if self.data is not None and isinstance(self.data, pd.DataFrame):
            data = self.data.append(data)

        return data

    def parse_csv(self, csvfile):
        data = []
        expected_len = None
        header_changed_in_block = False
        if self.current_header is not None:
            expected_len = len(self.current_header)

        for line in csvfile:
            line = line.strip()
            if line.startswith('#'):
                if 'time' not in line.lower():
                    continue
                self.previous_header = self.current_header
                self.current_header = parse_header(line)
                expected_len = len(self.current_header)

                if self.master_header is None:
                    self.master_header = self.current_header
                else:
                    s = set(self.master_header)
                    diff = [x for x in self.current_header if x not in s]
                    if diff:
                        self.master_header.extend(diff)
                        if self.data is not None:
                            # extend previous array with nans
                            nans = np.empty((self.data.shape[0], len(diff)))
                            nans[:] = np.nan
                            self.data = np.hstack((self.data, nans))
                if len(data) > 0:
                    # header changed in the middle of parsing a block
                    header_changed_in_block = True
                    break
            else:
                row_data = [safe_float(n.strip()) for n in line.split(',')]
                row_len = len(row_data)
                if row_len == expected_len:
                    data.append(row_data)
                elif row_len != expected_len:
                    break

        if not data:
            return None
        data = np.asarray(data)

        if self.data is None:
            # might need to extend self.data if header changed and self.data is None
            diff = len(self.master_header) - data.shape[1]
            if diff > 0:
                nans = np.empty((data.shape[0], diff))
                nans[:] = np.nan
                data = np.hstack((data, nans))

            self.data = data

        else:
            # stack the data
            nans = np.empty((data.shape[0], len(self.master_header)))
            nans[:] = np.nan
            header = self.previous_header if header_changed_in_block else self.current_header
            for idx, h in enumerate(header):
                new_idx = self.master_header.index(h)
                nans[:, new_idx] = data[:, idx]
            self.data = np.vstack((self.data, nans))

        if header_changed_in_block:

            return self.parse_csv(csvfile)
        else:
            return self.data


    @QtCore.Slot()
    def run(self):

        # check if the file still exists
        if not os.path.exists(self.path):
            self.signals.error.emit(NO_FILE, self.path)
            return

        with open(self.path, 'r', encoding='ascii') as csvfile:
            while not self.stop:

                # check if the file still exists
                if not os.path.exists(self.path):
                    self.signals.error.emit(NO_FILE, self.path)
                    break

                mt = os.stat(self.path).st_mtime
                if self.pause or mt == self.mt:
                    time.sleep(.1)
                    continue
                self.mt = mt

                # pandas parser
                if PANDAS_AVAILABLE and self.backend == 'pandas':
                    #data = self.parse_pandas(csvfile)
                    pass

                # csv parser
                else:
                    data = self.parse_csv(csvfile)

                if data is not None:
                    self.signals.finished.emit(self.path, self.master_header, data)

def test_random_write():
    import random

    def test_write_csv(fname, method='w', line_len=5):
        # write a csv file
        if type(line_len) != type(int):
            line_len = np.random.random_integers(1, 5)

        var = random.choice(['epg', 'u', 'v', 'p'])

        header = ','.join(['# Time'] + [var + '{}'.format(i) for i in range(line_len)]) + '\n'

        rs = np.random.random_integers(1, 100)
        with open(fname, method, encoding='ascii') as csvfile:
            csvfile.write(header)
            for i in range(rs, rs+10):
                csvfile.write(','.join([str(i)] + ['1'] * line_len) + '\n')

    # write a file with variable header lengths
    test_write_csv('test.csv', 'w', 3)
    test_write_csv('test.csv', 'a', True)
    test_write_csv('test.csv', 'a', True)
    test_write_csv('test.csv', 'a', True)

    # create the QApplication
    qapp = QtWidgets.QApplication([])

    def test(f, d):
        print('update', ', '.join(str(v['header']) for v in d.values()))

    mr = MonitorReader('csv')  # TODO: fix pandas
    mr.update.connect(test)

    mr.add_file('test.csv')
    mr.add_file('test.csv')
    mr.add_file('test_not_there.csv')
    mr.add_file('test_write_later.csv')

    # write 10 new lines every n msec with variable headers
    timer = QtCore.QTimer()
    timer.timeout.connect(lambda: test_write_csv('test_write_later.csv', 'a', True))
    timer.start(100)

    # quit after 5 seconds
    QtCore.QTimer.singleShot(5000, qapp.quit)
    qapp.exec_()

    # clean up
    for f in ['test.csv', 'test_not_there.csv', 'test_write_later.csv']:
        if os.path.exists(f):
            os.remove(f)

    mr.reset()  # stop the threads

def test_header_parser():
    for l in ['# "Time", "value", "x_s(1,1)", "x_s(1,2)", "T_s(1)", "x_g(2)"',
              '"Time","ep_g","p_g","u_g","v_g","w_g","t_g","p_star","rop_s(1)","u_s(1)","v_s(1)","w_s(1)","t_s(1)","theta_m(1)"']:
        print(parse_header(l))

def parse_file(fname):
    # create the QApplication
    qapp = QtWidgets.QApplication([])

    def test(f, d):
        print('update', d)

    mr = MonitorReader('csv')  # TODO: fix pandas
    mr.update.connect(test)

    mr.add_file(fname)

    # quit after 1 second
    QtCore.QTimer.singleShot(1000, qapp.quit)
    qapp.exec_()

if __name__ == "__main__":
    test_header_parser()
    parse_file('C:/Users/test/Documents/FB2D/Entire_Domain.csv')

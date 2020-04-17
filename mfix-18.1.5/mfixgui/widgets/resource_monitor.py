from qtpy import QtWidgets, QtCore
from mfixgui.tools.qt import get_separator

try:
    import psutil
    PSUTIL_AVAILABLE = True
except ImportError:
    PSUTIL_AVAILABLE = False

RESOURCE_MONITOR_RATE = 500  # refresh rate of monitor in msec


class ResourceMonitor(QtWidgets.QWidget):

    def __init__(self, settings, parent=None):
        QtWidgets.QWidget.__init__(self, parent)

        self.settings = settings
        self.cpu_ram_timer = None
        self.setSizePolicy(QtWidgets.QSizePolicy.Maximum,
                           QtWidgets.QSizePolicy.Preferred)
        self.min_width = 0

        # labels
        cl = QtWidgets.QLabel('CPU:')
        rl = QtWidgets.QLabel('RAM:')
        clp = self.label_cpu = QtWidgets.QLabel('00%')
        rlp = self.label_ram = QtWidgets.QLabel('00%')
        for lb in [cl, rl, clp, rlp]:
            lb.setSizePolicy(QtWidgets.QSizePolicy.Maximum,
                             QtWidgets.QSizePolicy.Preferred)

        # bars
        cp = self.progress_bar_cpu = QtWidgets.QProgressBar()
        rp = self.progress_bar_ram = QtWidgets.QProgressBar()

        for pb in [cp, rp]:
            pb.setMaximumWidth(40)
            pb.setTextVisible(False)

        # layout
        bl = QtWidgets.QHBoxLayout(self)
        bl.setContentsMargins(0, 0, 0, 0)

        for w in [get_separator(), cl, clp, get_separator(), rl, rlp]:
            bl.addWidget(w)

    def show(self, enable):

        self.setVisible(PSUTIL_AVAILABLE and enable)
        self.settings.setValue('show_resources', int(enable))

        if PSUTIL_AVAILABLE and enable:
            if self.cpu_ram_timer is not None:
                self.cpu_ram_timer.stop()
            timer = self.cpu_ram_timer = QtCore.QTimer()
            timer.timeout.connect(self.get_cpu_ram)
            timer.start(int(self.settings.value('resource_update_rate',
                                                RESOURCE_MONITOR_RATE)))
        elif self.cpu_ram_timer is not None:
            self.cpu_ram_timer.stop()
            self.cpu_ram_timer = None

    def get_cpu_ram(self, cpu=None, ram=None):

        if not self.isVisible():
            return

        if PSUTIL_AVAILABLE:
            if cpu is None:
                cpu = psutil.cpu_percent()
            if ram is None:
                ram = psutil.virtual_memory().percent

        if cpu is not None:
            self.label_cpu.setText('{0:.0f}%'.format(cpu))
            # self.progress_bar_cpu.setValue(cpu)
        if ram is not None:
            self.label_ram.setText('{0:.0f}%'.format(ram))
            # self.progress_bar_ram.setValue(ram)

        # allow increasing width but not decreasing width to keep the widgets
        # from continously changing widths
        self.min_width = max([self.label_cpu.width(), self.label_ram.width(), self.min_width])
        self.label_cpu.setMinimumWidth(self.min_width)
        self.label_ram.setMinimumWidth(self.min_width)

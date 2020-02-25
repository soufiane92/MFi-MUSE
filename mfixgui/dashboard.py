# -*- coding: utf-8 -*-

from mfixgui.tools import num_to_time
from mfixgui.tools.qt import get_icon
from mfixgui.graphic_tabs import PYQTGRAPH_AVAILABLE

class Dashboard(object):
    # Dashboard Task Pane

    def init_dashboard(self):

        ui = self.ui.dashboard

        for plt, btn in [('Elapsed Time', ui.toolbutton_plot_time),
                         ('DT', ui.toolbutton_plot_dt),
                         ('NIT', ui.toolbutton_plot_nit)]:
            btn.setIcon(get_icon('timeline.svg'))
            btn.pressed.connect(lambda plt=plt:self.dashboard_create_plot(plt))
            btn.setEnabled(PYQTGRAPH_AVAILABLE)


        for pb in [ui.progressbar_dt, ui.progressbar_nit]:
            pb.setRange(0, 100)

        self.reset_dashboard()

    def dashboard_create_plot(self, plot):

        if plot not in self.plot_dict:
            tab = self.create_tab()
            tab.create_plot_widget(name=plot)

    def update_dashboard(self, status):
        ui = self.ui.dashboard

        # How many digits to show after decimal point in time values
        digits = 1 # show 0.1 seconds

        elapsed_time = status.get('walltime_elapsed', 0)
        paused_time = status.get('walltime_paused', 0)
        remaining_time = status.get('walltime_remaining', 0)
        paused = status.get('paused', False)

        ui.lineedit_elapsed_time.setText(num_to_time(elapsed_time, digits=digits))
        ui.lineedit_paused_time.setText(num_to_time(paused_time, digits=digits))
        # I don't like to see the remaining time estimate jump around while
        # paused.  That happens due to small latencies in reporting paused time,
        # vs total wall time, which affects the calculation.  So when we're paused,
        # just freeze the updating of remaining time estimate - cgw
        if not paused or (paused and paused_time == 0):
            ui.lineedit_remaining_time.setText(num_to_time(remaining_time, digits=digits))

        ui.lineedit_time.setText(
            '{:.3f}'.format(status.get('time', 0)))

        # Hide paused time if never paused
        visible = (paused_time != 0)
        for w in (ui.label_paused_time,
                  ui.lineedit_paused_time,
                  ui.label_paused_time_units):
            w.setVisible(visible)

        dt = status.get('dt', None)
        if dt is not None:
            ui.lineedit_dt.setText(
                '{:.2E}'.format(dt))
            self.dt_min = min(self.dt_min, dt)
            ui.lineedit_dt_min.setText(
                '{:.2E}'.format(self.dt_min))
            self.dt_max = max(self.dt_max, dt)
            ui.lineedit_dt_max.setText(
                '{:.2E}'.format(self.dt_max))

            rg = (self.dt_max-self.dt_min)
            if rg > 0:
                ui.progressbar_dt.setValue((dt-self.dt_min)/rg*100)

        nit = status.get('nit', None)
        if nit is not None:
            ui.lineedit_nit.setText(
                str(nit))
            self.nit_min = min(self.nit_min, nit)
            ui.lineedit_nit_min.setText(str(self.nit_min))
            self.nit_max = max(self.nit_max, nit)
            ui.lineedit_nit_max.setText(str(self.nit_max))

            rg = (self.nit_max-self.nit_min)
            if rg > 0:
                ui.progressbar_nit.setValue((nit-self.nit_min)/rg*100)

    def reset_dashboard(self):
        ui = self.ui.dashboard

        self.dt_min, self.nit_min = [99999999]*2
        self.dt_max, self.nit_max = [0]*2
        for pb in [ui.progressbar_dt, ui.progressbar_nit]:
            pb.setValue(0)
        self.update_dashboard({})

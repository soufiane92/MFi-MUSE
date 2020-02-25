from PyQt5.QtWidgets import QMainWindow

import mfixgui.widgets.species_popup

def test_species_popup(qtbot):
    qmain = QMainWindow()
    qmain.keyword_doc = {}
    species_popup = mfixgui.widgets.species_popup.SpeciesPopup(qmain, qmain, phases='GL')
    species_popup.show()

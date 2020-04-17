import pytest
import mfixgui.colormaps.color_maps

@pytest.mark.xfail(run=False, reason="None")
def test_color_maps():
    mfixgui.colormaps.color_maps.main()

import os

from qtpy.QtCore import Qt, QSize
from qtpy.QtGui import QImage, QPainter

from mfixgui.tools.qt import get_image_path


def create_thumbnail(name='./.thumbnail',
                     model='tfm',
                     geometry=False,
                     chemistry=False,
                     background=None):
    if background is not None and os.path.exists(background):
        img = QImage(background)
        base = img.scaled(300, 300).scaled(128, 128, Qt.IgnoreAspectRatio)
    else:
        base = QImage(QSize(128, 128), QImage.Format_ARGB32)
        base.fill(Qt.white)

    painter = QPainter(base)

    # add images
    model = QImage(get_image_path(model + '.svg'))
    painter.drawImage(0, 128 - 24, model)

    if geometry:
        geo = QImage(get_image_path('geometry.svg'))
        painter.drawImage(24, 128 - 24, geo)

    if chemistry:
        geo = QImage(get_image_path('chemistry.svg'))
        painter.drawImage(24 * 2, 128 - 24, geo)

    base.save(name, "PNG")
    del painter


def main():
    create_thumbnail('.thumbnail', 'dem', True, True, 'setup.png')

if __name__ == '__main__':
    main()

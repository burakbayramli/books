#!/usr/bin/env python3

import logging

logger = logging.getLogger(__name__)


def afgen(e: float, k: float, alpha: float, m: int):
    """Generates van de Vooren Airfoil. Refer to Section 6.7

    :param e: Thickness coefficient (between 0 and 1)
    :type e: float
    :param ak: Trailing edge angle coefficient
    :type ak: float
    :param alpha: Angle of attack (deg)
    :type alpha: float
    :param m: Number of panels
    :type m: int
    """
    logger.info(
        "Starting van de Vooren Transformation with "
        + f"e = '{e}', ak = '{k}', alpha = '{alpha}', m = '{m}'"
    )
    tl = 1.0  # l
    a = 2 * tl * (e + 1) ** (k - 1) / (2 ** k)  # Eq (6.73) ref Eq. (6.63)
    al = alpha / 57.2957795131
    try:
        m = int(m)
    except ValueError:
        logger.critical(f"Expected int or int-like, not m = '{m}'")
        raise

    theta = range(0, 1)

    print(a, al, theta)


if __name__ == "__main__":
    afgen(1, 1, 1, 1)

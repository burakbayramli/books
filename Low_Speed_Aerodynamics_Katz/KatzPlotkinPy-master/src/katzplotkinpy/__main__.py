import logging

from katzplotkinpy.cli import parse_args

logger = logging.getLogger(__name__)

if __name__ == "__main__":
    logging.basicConfig(format="%(name)s [%(levelname)s] %(message)s")

    args = parse_args(
        description="Python companion to Low Speed Aerodynamics",
        version="to fix",
        get_logger=logger,
    )

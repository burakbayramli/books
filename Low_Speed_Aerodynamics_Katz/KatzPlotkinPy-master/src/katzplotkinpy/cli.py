#!/usr/bin/env python3

import argparse
import logging
import sys
from pathlib import Path
from typing import List, Tuple

logger = logging.getLogger(__name__)


# Prog listing should map name to description to function
# Consider using function type annotations to power cli
prog_listing = {
    "afgen": "Grid generator for van de Vooren airfoil shapes",
    #
    "vor2d": "Discrete vortex, thin wing method",
    "sor2dc": "Constant strength source method",
    "dub2dc": "Constant strength doublet method",
    "vor2dc": "Constant strength vortex method",
    "sor2dl": "Linear strength source method",
    "vor2dl": "Linear strength vortex method",
    #
    "phicd": "Constant strength doublet method",
    "phicsd": "Constant strength source/doublet method",
    "phild": "Linear strength doublet method",
    "phiqd": "Quadratic strength doublet method",
    #
    "dub3dc": "Influence of constant strength source/doublet",
    "voring": "VLM for rectilinear surfaces (with ground effect)",
    "panel": "Constant strength sources and doublets (Dirichlet BC)",
    #
    "wake": "Acceleration of flat plate using a lumped vortex",
    "uvlm": "Unsteady motion of a thin rectangular lifting surface",
}


def existing_file(path: str, extensions: Tuple[str] = None) -> Path:
    """Check if a file exists and (optionally) is of a given extension type

    :param path: Path to check
    :type path: str
    :param extensions: List of allowed file extensions, defaults to None
    :type extensions: Tuple[str], optional
    :raises FileNotFoundError: File does not exist
    :raises TypeError: Wrong file type
    :return: Path to the file
    :rtype: Path
    """

    file_path = Path(path)
    if not file_path.is_file():
        raise FileNotFoundError(f"No such file: `{file_path}`")
    if extensions is not None and file_path.suffix.lower() not in [
        ext.lower() for ext in extensions
    ]:
        raise TypeError(f"Wrong file format: `{file_path}`. Expected `{extensions}`")
    return file_path


def existing_dir(path: str) -> Path:
    """Check if input path points to an existing directory

    :param path: Path to check
    :type path: str
    :raises NotADirectoryError: Path is not a directory
    :return: Path with expanded user
    :rtype: Path
    """
    dir_path = Path(path).expanduser()
    if not dir_path.is_dir():
        raise NotADirectoryError(f"Not a directory: `{path}`")
    return dir_path


def add_subparser(
    name: str,
    subparsers: argparse._SubParsersAction,
    parents: List[argparse.ArgumentParser],
) -> argparse.ArgumentParser:
    """Template creation of parsers for cli programs

    :param name: program name
    :type name: str
    :param subparsers: programs added to this object
    :type subparsers: argparse._SubParsersAction
    :param parents: inherit features of these parent parsers
    :type parents: List[argparse.ArgumentParser]
    :return: subparser with new program added to it
    :rtype: argparse.ArgumentParser
    """

    return subparsers.add_parser(
        name, parents=parents, description=prog_listing[name], help=prog_listing[name]
    )


def parse_args(
    argv: List[str] = sys.argv,
    description: str = __package__,
    version: str = "0.1.0",
    get_logger: logging.Logger = None,
) -> argparse.Namespace:
    """Parse arguments and provide CLI help if no arguments parsed.
    Sets the logger level based on argv flags.

    :param argv: vector of args to parse, defaults to sys.argv
    :type argv: List[str], optional
    :param description: cli main description, defaults to __package__
    :type description: str, optional
    :param version: version printed out via CLI, defaults to "0.1.0"
    :type version: str, optional
    :param get_logger: set the level of this logger, defaults to None
    :type get_logger: logging.Logger, optional
    :return: args as namespace
    :rtype: argparse.Namespace
    """

    # Main parser
    formatter_class = argparse.RawTextHelpFormatter
    main_parser = argparse.ArgumentParser(
        description=description,
        formatter_class=formatter_class,
        epilog="Source: https://github.com/AlwinW/KatzPlotkinPy",
    )
    main_parser.add_argument(
        "-V",
        "--version",
        action="version",
        version="%(prog)s {}".format(version),
        help="show the version and exit",
    )
    subparsers = main_parser.add_subparsers(
        title="programs",
        description="Programs from Appendix D of 'Low Speed Aerodynamics'",
        dest="program",
    )

    # Inheritance parser
    parent_parser = argparse.ArgumentParser(add_help=False)
    parent_parser.add_argument(
        "-v",
        "--verbose",
        action="count",
        default=0,
        required=False,
        help="Increase log verbosity (max -vvv)",
        dest="verbose_count",
    )
    parent_parser.add_argument(
        "-d",
        "--debug",
        action="store_true",
        required=False,
        help="Show debugging messages (eqv. to -vv, overrides verbosity flag)",
    )
    parent_parser.add_argument(
        "-s",
        "--silent",
        action="store_true",
        required=False,
        help="Suppress log warning and lower messages (overrides other verbosity flags)",
    )

    # Program parsers
    afgen_parser = add_subparser("afgen", subparsers, [parent_parser])

    vor2d_parser = add_subparser("vor2d", subparsers, [parent_parser])
    sor2dc_parser = add_subparser("sor2dc", subparsers, [parent_parser])
    dub2dc_parser = add_subparser("dub2dc", subparsers, [parent_parser])
    vor2dc_parser = add_subparser("vor2dc", subparsers, [parent_parser])
    sor2dl_parser = add_subparser("sor2dl", subparsers, [parent_parser])
    vor2dl_parser = add_subparser("vor2dl", subparsers, [parent_parser])

    phicd_parser = add_subparser("phicd", subparsers, [parent_parser])
    phicsd_parser = add_subparser("phicsd", subparsers, [parent_parser])
    phild_parser = add_subparser("phild", subparsers, [parent_parser])
    phiqd_parser = add_subparser("phiqd", subparsers, [parent_parser])

    dub3dc_parser = add_subparser("dub3dc", subparsers, [parent_parser])
    voring_parser = add_subparser("voring", subparsers, [parent_parser])
    panel_parser = add_subparser("panel", subparsers, [parent_parser])

    wake_parser = add_subparser("wake", subparsers, [parent_parser])
    uvlm_parser = add_subparser("uvlm", subparsers, [parent_parser])

    if len(argv) == 1:
        main_parser.print_help(sys.stderr)
        sys.exit(1)
    args = main_parser.parse_args(argv[1:])

    if get_logger is not None:
        if args.silent:
            logger.setLevel(logging.ERROR)
        elif args.debug:
            logger.setLevel(logging.DEBUG)
        else:
            logger.setLevel(max(3 - args.verbose_count, 1) * 10)
    return args

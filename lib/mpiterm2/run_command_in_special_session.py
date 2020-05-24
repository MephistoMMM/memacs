#!/usr/bin/env python3

import sys, os
import iterm2
import asyncio

from .lib import find_session_by_path

if len(sys.argv) < 3:
    print("USAGE: ./run_command_in_special_session.py CMD PATH [cmd]")
    os.exit(1)

CMD = sys.argv[1]
PATH = sys.argv[2]

def get_cmd() -> str:
    """
    Get content of cmd from arguments if arguments available,
    otherwise stdin.
    """
    if len(sys.argv) > 3:
        return " ".join(sys.argv[3:])
    else:
        return sys.stdin.read()

async def run_in(connection):
    app = await iterm2.async_get_app(connection)
    session = await find_session_by_path(app, PATH)
    await session.async_send_text(get_cmd()+"\n")
    await session.async_activate(True, True)
    print("Done")

async def go_to(connection):
    app = await iterm2.async_get_app(connection)
    session = await find_session_by_path(app, PATH)
    await session.async_activate(True, True)
    print("Done")

async def main(connection):
    if "RUN_IN" == CMD.upper():
        return await run_in(connection)
    if "GO_TO" == CMD.upper():
        return await go_to(connection)

iterm2.run_until_complete(main)

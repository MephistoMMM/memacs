#!/usr/bin/env python3

import sys, os
import iterm2
import asyncio

from mpiterm2.lib.session import get_session_by_path

PATH = None
if len(sys.argv) >= 2:
    PATH = sys.argv[1]

if PATH == "--help" or PATH == "-h":
    print("USAGE: ./go_to_session.py [PATH]")
    os.exit(1)

async def go_to(connection):
    app = await iterm2.async_get_app(connection)
    if PATH is None:
        session = app.current_terminal_window.current_tab.current_session
    else:
        session = await get_session_by_path(app, PATH.rstrip("/"))

    await session.async_activate(True, True)
    await app.async_activate(True, True)
    print("Done")

iterm2.run_until_complete(go_to)

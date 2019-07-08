#!/usr/bin/env python3

import sys, os
import iterm2
import asyncio

from mpiterm2.lib.session import get_session_by_path

if len(sys.argv) < 2:
    print("USAGE: ./go_to_session.py PATH")
    os.exit(1)

PATH = sys.argv[1]

async def go_to(connection):
    app = await iterm2.async_get_app(connection)
    session = await get_session_by_path(app, PATH.rstrip("/"))
    await session.async_activate(True, True)
    await app.async_activate(True, True)
    print("Done")

iterm2.run_until_complete(go_to)

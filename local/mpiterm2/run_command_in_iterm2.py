#!/usr/bin/env python3

import sys
import iterm2
import asyncio

def get_cmd():
    """
    Get content of cmd from arguments if arguments available,
    otherwise stdin.
    """
    if len(sys.argv) > 1:
        return " ".join(sys.argv[1:])
    else:
        return sys.stdin.read()

async def main(connection):
    app = await iterm2.async_get_app(connection)
    session = app.current_terminal_window.current_tab.current_session
    await session.async_send_text(get_cmd()+"\n")
    await app.async_activate(True, True)
    print("Done")

iterm2.run_until_complete(main)

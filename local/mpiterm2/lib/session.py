#!/usr/bin/env python3

import iterm2
import asyncio

async def collect_session_by_path(app:iterm2.App) -> dict:
    async def collect(session_dict:dict, session:iterm2.Session):
        session_dict[await session.async_get_variable("path")] = session

    result = dict()
    tasks = list()
    for window in app.terminal_windows:
        for tab in window.tabs:
            for session in tab.sessions:
                tasks.append(collect(result, session))

    done, _ = await asyncio.wait(tasks)
    return result

async def find_session_by_path(
        app:iterm2.App, target_path:str) -> iterm2.Session:
    session_dict = await collect_session_by_path(app)
    print(target_path)
    print(session_dict)
    return session_dict.get(target_path, None)

async def create_new_session_in_new_tab(app:iterm2.App) -> iterm2.Session:
    currentWindow = app.current_terminal_window
    newTab = await currentWindow.async_create_tab()
    newSession = newTab.current_session
    return newSession


async def get_session_by_path(
        app:iterm2.App, target_path:str) -> iterm2.Session :
    session = await find_session_by_path(app, target_path)
    if session is not None:
        return session

    session = await create_new_session_in_new_tab(app)
    await session.async_send_text("cd {}\n".format(target_path))
    return session

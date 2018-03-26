youtube-queue-thing
===================

Simple YouTube party queue written in PureScript.

Building & Running:
-------------------

Configure which host/port backend listens on in `src/Main.purs`.
Remember to change where the frontend connects to in `src/Frontend.purs`!

1. `yarn`
2. `bower install`
3. `yarn start`
4. `yarn fe-build`
5. Serve up the `dist/` directory with your favorite HTTP server.

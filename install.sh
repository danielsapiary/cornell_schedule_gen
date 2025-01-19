#!/bin/bash

# Prerequisites Check
command -v ocaml >/dev/null 2>&1 || { echo >&2 "OCaml is not installed. Please install it from https://ocaml.org/docs/install.html"; exit 1; }
command -v dune >/dev/null 2>&1 || { echo >&2 "Dune is not installed. Please install it from https://dune.build/install"; exit 1; }
command -v node >/dev/null 2>&1 || { echo >&2 "Node.js is not installed. Please install it from https://nodejs.org/"; exit 1; }
command -v npm >/dev/null 2>&1 || { echo >&2 "npm is not installed. Please install it from https://www.npmjs.com/"; exit 1; }

# Install OCaml Dependencies
echo "Installing OCaml dependencies..."
opam install lwt cohttp-lwt-unix yojson csv --yes || { echo >&2 "Failed to install OCaml dependencies."; exit 1; }

# Setup Frontend Dependencies
FRONTEND_DIR="frontend"
if [ -d "$FRONTEND_DIR" ]; then
  echo "Installing frontend dependencies..."
  cd $FRONTEND_DIR || { echo >&2 "Failed to navigate to frontend directory."; exit 1; }
  npm install react-big-calendar date-fns || { echo >&2 "Failed to install frontend dependencies."; exit 1; }
  cd .. || { echo >&2 "Failed to return to root directory."; exit 1; }
else
  echo "Frontend directory '$FRONTEND_DIR' not found. Please ensure it exists."
  exit 1
fi

# Build OCaml Project
echo "Building OCaml project..."
dune build || { echo >&2 "Failed to build OCaml project."; exit 1; }

# Start OCaml Server in the background
echo "Starting OCaml server..."
dune exec bin/main.exe SP25 0.0.0.0 9000 &
OCAML_SERVER_PID=$!
echo "OCaml server started with PID $OCAML_SERVER_PID"

# Start React Frontend in the background
echo "Starting React frontend..."
cd $FRONTEND_DIR || { echo >&2 "Failed to navigate to frontend directory."; exit 1; }
npm run start &
FRONTEND_PID=$!
cd .. || { echo >&2 "Failed to return to root directory."; exit 1; }
echo "Frontend started with PID $FRONTEND_PID"

# Trap Ctrl+C (SIGINT) to stop both servers
trap 'echo "Shutting down..."; kill $OCAML_SERVER_PID $FRONTEND_PID; exit 0' INT

# Wait for both background processes
wait $OCAML_SERVER_PID $FRONTEND_PID

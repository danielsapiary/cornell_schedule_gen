# Installation Instructions

## Prerequisites

Before installing and running the project, make sure the following are installed on your system:

- [OCaml](https://ocaml.org/docs/install.html)
- [dune](https://dune.build/install) (OCaml's build system)
- [Node.js](https://nodejs.org/) (version 12 or higher)
- [npm](https://www.npmjs.com/)

## Automated Installation

For a streamlined setup process, you can use the provided installation script. Run the following command from the root directory of the project:
Note that grabbing courses is expected to take ~90 seconds.

```bash
bash ./install.sh
```

This script will automatically install all dependencies, build the project, and start the necessary services. It may need `sudo`. Alternatively, you can install the do this manually by following the instructions below.

## Dependencies

This project requires both OCaml libraries and Node.js packages. Follow the steps below to install the necessary dependencies.

### OCaml Dependencies

The OCaml part of this project relies on the following libraries:

- **lwt**: For cooperative threading.
- **cohttp-lwt-unix**: For handling HTTP requests using `lwt` on Unix-based systems.
- **yojson**: For JSON parsing and manipulation.
- **csv**: For reading and writing CSV files.

To install these libraries, use `opam` (OCaml's package manager). If `opam` is not installed, follow the instructions [here](https://opam.ocaml.org/doc/Install.html).

#### Installing OCaml Libraries

Once `opam` is set up, run the following command to install the necessary dependencies:

```bash
opam install lwt cohttp-lwt-unix yojson csv
```

This will ensure all required libraries are installed and ready for use in the project.

### React Frontend Dependencies

The frontend uses **React** and the following additional libraries:

- **react-big-calendar**: To display schedules in a calendar view.
- **date-fns**: For date handling within `react-big-calendar`.

To install these packages:

1. Navigate to the `frontend` directory:

   ```bash
   cd frontend
   ```

2. Install the dependencies:

   ```bash
   npm install react-big-calendar date-fns
   ```

## Building the Project

After installing all dependencies, you can build the OCaml portion using `dune`.

1. Navigate to the root directory of the project:

   ```bash
   cd ..
   ```

2. Run the build command:

   ```bash
   dune build
   ```

## Running the Project

### Running the OCaml Server

After building, you can run the OCaml server. This server handles course schedules and provides data for frontend interaction.

```bash
dune exec bin/main.exe SP25 0.0.0.0 9000 
```

The server will start on [http://localhost:9000](http://localhost:9000).

### Running the React Frontend

To start the frontend:

1. Navigate to the `frontend` directory:

   ```bash
   cd frontend
   ```

2. Build the frontend:

   ```bash
   npm run build
   ```

3. If you receive an error during the build, try installing dependencies again:

   ```bash
   npm install
   ```

4. Start the frontend:

   ```bash
   npm run start
   ```

The frontend will run on [http://localhost:3000](http://localhost:3000) by default. Now, you should be able to access the site at that address.

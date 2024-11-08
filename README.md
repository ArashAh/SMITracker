<!-- badges: start -->

[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)

<!-- badges: end -->

# SMITracker


This page contains the source code for Single Molecule Interaction Tracker (SMITracker) platform. SMITracker is an interactive analysis platform that enables the users to detect and track fluorescently-labeled single molecules scanning linear substrates. Prior to analysis using SMITracker, all the existing signals in the recorded images need to be localized in ‘FIJI’ using a single molecule localization plug-in called ‘ThunderSTORM’. This can be done manually using the ThunderSTORM interface, or it can be done automatically using the script provided in this page under the name `localization_analysis.ijm`.

SMITracker can be installed and run as an **R package** in R or it can be run inside a **Docker container**. For thorough information on preprocessing and analysis of data using SMITracker, read the corresponding paper (link to the published paper). Here, we only provide instructions for retrieving and running the platform both as an R package and as a Docker image.

### Install and run SMITracker inside R (for R users)

Open an R console and run the following commands: 

1. **install devtools**(if not installed already):

``` r
install.packages("devtools")

```
2. **Install SMITracker from GitHub:** 

``` r
devtools::install_github("ArashAh/testSMITracker")

```
3. **Initiate the interface and analyze the data:**


``` r
SMITracker::run_app()

```

### Build and run SMITracker inside a container (for Docker users)

Make sure you have Git and Docker installed on your machine. Open a terminal, 
navigate to a directory to clone this repository and run the following commands: 

1. **Clone the repository**:

```sh
git clone https://github.com/ArashAh/testSMITracker
cd testSMITracker/deploy/
```

2. **Build the base Docker image**:

```sh
docker build -f Dockerfile_base --progress=plain -t smitracker_base .
```

3. **Build the Docker image of the platform**:

```sh
docker build -f Dockerfile --progress=plain -t smitracker:latest .
```

4. **Run the Docker container: **:

```sh
docker run --rm -p 80:80 --mount type=bind,source=<directory_in_the_local_computer>,target=/home smitracker:latest
```

Replace `<directory_in_the_local_computer>` with the actual path to the directory you want to mount; this directory should contain your data. 

5. **Open the interface inside a browser and analyze the data**
```link
127.0.0.1:80 or localhost 
```


#### Stopping the Docker Container from Running

1. **List running Docker containers**:

    ```sh
    docker ps
    ```
If the same terminal running the container is non-responsive, open another terminal and run the command there. 

2. **Copy the `CONTAINER ID`**.

3. **Stop the container**:

    ```sh
    docker stop <CONTAINER_ID>
    ```

Replace `<CONTAINER_ID>` with the actual container ID you copied.


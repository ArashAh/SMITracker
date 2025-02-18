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

4. **Run the Docker container: **

    ```sh
    docker run -p 80:80 smitracker:latest
    ```

5. **Open the interface inside a browser and analyze the data**

    ```link
    127.0.0.1:80 or localhost 
    ```


#### Stopping the Docker Container from Running

Either use the Docker Desktop interface or the following in instruction:

1. **List running Docker containers**:

    open a new terminal and run: 

    ```sh
    docker ps
    ```

2. **Copy the `CONTAINER ID`**.

3. **Stop the container**:

    ```sh
    docker stop <CONTAINER_ID>
    ```

Replace `<CONTAINER_ID>` with the actual container ID you copied.


### Test SMITracker using a sample dataset 


1. **Clone the Repository:** 
   Clone the  [repository](https://github.com/ArashAh/SMITracker_sample_data) containing the sample datasets to your local machine.
   ```sh
   git clone https://github.com/ArashAh/SMITracker_sample_data
   ```
   
2. **Run the Platform:** 
   Run the SMITracker platform either as an R package or a Docker container, following the instructions above. 
   
3. **Input Data:**
   In the first tab of the application, set the path to the `input_data/` directory from the cloned repository for the sample dataset.
   
4. **processing Data** 
   Use the provided pictures in the `screenshots/` directory as a guide for setting the variable in the SMITracker interface. For detailed information about processing data read the published paper [here](https://github.com/ArashAh/SMITracker)

5. **Output Data:** 
   Save the results of your analysis in the `output_data/` directory. File names will be labeled with the analysis ID you provide and the date of the analysis. Note that using the same analysis ID on the same date will overwrite previous outputs.


### Directory Structure and content

The repository includes three main sub-directories:

- **`input_data/`:** This directory contains `.JSON` files generated during the signal localization process using Fiji.

- **`output_data/`:** This directory includes `.rds` files produced during data processing with the SMITracker application. The file names start with numbers 1-5, corresponding to the outputs of tabs 1-5 in the application. Additionally, there are three exemplary files named `plot.1.1.data` to `plot.1.3.data` representing the data behind plots in the *Scanning speed* section of tab 6.

- **`visual_log/`:** This directory contains screenshots of different tabs of the SMITracker interface while processing data and generating output data. The files are named `tab1` to `tab6`, corresponding to each tab in the application interface. There are 13 versions for tab 2, `tab2.1`-`tab2.12` represent screenshots related to spatial filtering of specific files, and `tab2.13` shows intensity distribution and applied intensity filters. `tab4.1`, `tab4.3`, and `tab4.5` are examples of normal interactions that can be approved in tab 4. In contrast, `tabs4.2`, `tab4.4`, and `tab4.6` represent examples of noise in the data that should be removed from the optimization dataset in tab 4. `tab5.1` shows the visualization of results in the presence of noise, and `tab5.2` visualizes the results in the absence of noise.


access the repository containing the sample datasets [here](https://github.com/ArashAh/SMITracker_sample_data).

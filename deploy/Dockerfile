FROM smitracker_base
COPY renv.lock.prod renv.lock
RUN R -e 'renv::restore()'
COPY SMITracker_1.0.0.tar.gz /app.tar.gz
RUN R -e 'remotes::install_local("/app.tar.gz",upgrade="never")'
RUN rm /app.tar.gz
EXPOSE 3838
CMD R -e "options('shiny.port'=3838,shiny.host='0.0.0.0');library(SMITracker);SMITracker::run_app()"

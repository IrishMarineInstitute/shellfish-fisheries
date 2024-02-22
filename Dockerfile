FROM rocker/shiny:4.2.1
MAINTAINER Marine Institute

# first install ssl and gdal (needed for soem of the packaage we will install in a moment)
RUN sudo apt-get update && apt-get install -y libssl-dev libudunits2-0 libudunits2-dev libproj-dev libgdal-dev && apt-get clean && rm -rf /var/lib/apt/lists/ && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

# Install required packages (add additional package installs after these lines)
RUN sudo chown -R shiny:shiny /var/lib/shiny-server/
RUN Rscript -e "install.packages(c('shinyBS','shinydashboard','shinythemes','shinyjs','shinyWidgets','leaflet','dplyr','tidyr'), repos='https://cran.rstudio.com/')" && rm -rf /tmp/downloaded_packages/ /tmp/*.rds
RUN Rscript -e "install.packages(c('ggplot2','lubridate','plotly','sp','sf','shinycssloaders'), repos='https://cran.rstudio.com/')" && rm -rf /tmp/downloaded_packages/ /tmp/*.rds
RUN Rscript -e "install.packages(c('ggsci','colorRamps','ggrepel','rintrojs'), repos='https://cran.rstudio.com/')" && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

# Copy required files
COPY www /srv/shiny-server/shellfish/www
COPY data /srv/shiny-server/shellfish/data
COPY lib /srv/shiny-server/shellfish/lib
COPY README.md /srv/shiny-server/shellfish/
COPY global.R /srv/shiny-server/shellfish/
COPY server.R /srv/shiny-server/shellfish/
COPY ui.R /srv/shiny-server/shellfish/
COPY intro_text.html /srv/shiny-server/shellfish/

EXPOSE 3838

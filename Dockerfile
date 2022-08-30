FROM rocker/shiny:4.2.1
MAINTAINER Marine Institute

# Install required packages
RUN sudo chown -R shiny:shiny /var/lib/shiny-server/
RUN Rscript -e "install.packages(c('shinyBS','shinydashboard','shinythemes','shinyjs','shinyWidgets','leaflet','dplyr','tidyr','ggplot2','lubridate','plotly','sp','sf','rgdal','shinycssloaders','ggsci','colorRamps','ggrepel','rintrojs'), repos='https://cran.rstudio.com/')" && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

# Copy required files
COPY www /srv/shiny-server/shellfish/www
COPY Data /srv/shiny-server/shellfish/data
COPY README.md /srv/shiny-server/shellfish/
COPY global.R /srv/shiny-server/shellfish/
COPY server.R /srv/shiny-server/shellfish/
COPY ui.R /srv/shiny-server/shellfish/

EXPOSE 3838

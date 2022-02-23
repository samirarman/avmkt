FROM rocker/verse:4.0.0

RUN R -e "install.packages(c('zoo', 'DT', 'dygraphs', 'xts', 'RColorBrewer', 'lubridate'))"

RUN mkdir /home/avmkt

COPY *.Rmd /home/avmkt/

COPY make_site.R /home/avmkt/make_site.R

COPY _site.yml /home/avmkt/_site.yml

COPY *.rds /home/avmkt/

CMD cd /home/avmkt && \
    Rscript make_site.R && \
    touch .nojekyll && \
    rm *.rds && \
    rm *.csv

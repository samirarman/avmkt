FROM rocker/r-ver:4.0.0

RUN apt-get update && apt-get install -y libxml2-dev && apt-get install pandoc

RUN R -e "install.packages(c('plotly', 'zoo'))" 

RUN mkdir /home/avmkt

COPY make_site.R /home/avmkt/make_site.R
COPY *.Rmd /home/avmkt/

CMD cd /home/avmkt && \
    Rscript -e make_site.R && \
    touch .nojekyll &&\
    rm data.csv

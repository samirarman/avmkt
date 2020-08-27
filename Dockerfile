FROM rocker/verse:4.0.0

RUN R -e "install.packages(c('zoo', 'DT', 'dygraphs', 'xts'))"

RUN mkdir /home/avmkt

COPY *.Rmd /home/avmkt/

COPY make_site.R /home/avmkt/make_site.R

COPY _site.yml /home/avmkt/_site.yml

CMD cd /home/avmkt && \
    Rscript make_site.R && \
    touch .nojekyll && \
    rm data.zip

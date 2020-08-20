FROM rocker/verse:4.0.0

RUN R -e "install.packages(c('plotly', 'zoo'))" 

RUN mkdir /home/avmkt

COPY *.Rmd /home/avmkt/

COPY _site.yml /home/avmkt/

COPY *.R /home/avmkt/

CMD cd /home/avmkt && \
    Rscript -e make_site.R && \
    touch .nojekyll &&\
    rm data.csv

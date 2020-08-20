FROM rocker/verse:4.0.0

RUN R -e "install.packages(c('plotly', 'zoo', 'DT'))" 

RUN mkdir /home/avmkt

COPY index.Rmd /home/avmkt/index.Rmd

COPY make_site.R /home/avmkt/make_site.R

COPY _site.yml /home/avmkt/_site.yml

CMD cd /home/avmkt && \
    Rscript make_site.R && \
    touch .nojekyll && \
    rm data.zip

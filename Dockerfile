FROM rocker/verse:4.0.0

RUN R -e "install.packages(c('plotly', 'zoo'))" 

RUN mkdir /home/avmkt

COPY index.Rmd /home/avmkt/index.Rmd

COPY make_site.R /home/avmkt/make_site.R

COPY _site.yml /home/avmkt/_site.yml

CMD cd /home/avmkt && \
    touch .nojekyll

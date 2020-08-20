FROM rocker/verse:4.0.0

RUN R -e "install.packages(c('plotly', 'zoo'))" 

RUN mkdir /home/avmkt

COPY *.Rmd /home/avmkt/

COPY _site.yml /home/avmkt/_site.yml

CMD cd /home/avmkt && \
    Rscript -e "library(tidyverse); \
                download.file('https://www.anac.gov.br/assuntos/dados-e-estatisticas/dados-estatisticos/arquivos/DadosEstatsticos.csv', destfile = 'data.csv'); \
                rmarkdown::render_site();" && \
    touch .nojekyll &&\
    rm data.csv

FROM opencpu/base
RUN R -e 'remotes::install_github("zdk123/SpiecEasi")'
RUN R -e 'remotes::install_github("resplab/patientsimulatorPrism")'
RUN echo "opencpu:opencpu" | chpasswd


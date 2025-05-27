FROM ubuntu:24.04

# USER root
RUN apt-get update && apt-get -y install libomp-dev
RUN apt-get -y install libnetcdf-dev
RUN apt-get -y install git

RUN git clone https://github.com/rqthomas/ELCOM_FCR.git

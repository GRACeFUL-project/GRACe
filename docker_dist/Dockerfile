FROM ubuntu
ADD bins/ .
ADD libminizinc /libminizinc
ADD release-5.0.0 /release-5.0.0
COPY ["install-mzn-gecode.sh", "/"]
COPY ["run.sh", "/"]
RUN ./install-mzn-gecode.sh
CMD ./run.sh

